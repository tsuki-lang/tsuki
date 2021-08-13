mod codegen;
mod expression;
mod libc;

use std::path::{Path, PathBuf};
use std::process::Command;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{
   CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::{AddressSpace, OptimizationLevel};
use tsuki_frontend::backend;
use tsuki_frontend::common::{self, Error, ErrorKind, Errors, SourceFile, Span};

use codegen::CodeGen;

/// Struct representing the LLVM compilation backend and options passed to it.
pub struct LlvmBackend {
   cache_dir: PathBuf,
   executable_name: String,
   target_triple: TargetTriple,
}

/// Options for creating an LLVM backend instance.
pub struct LlvmBackendConfig<'c, 'e, 't> {
   pub cache_dir: &'c Path,
   pub executable_name: &'e str,
   pub target_triple: Option<&'t str>,
}

impl LlvmBackend {
   /// Creates a new instance of the LLVM compilation backend, with the provided options.
   pub fn new(config: LlvmBackendConfig) -> Self {
      Self {
         cache_dir: config.cache_dir.to_owned(),
         executable_name: config.executable_name.to_owned(),
         target_triple: match config.target_triple {
            Some(triple) => TargetTriple::create(triple),
            None => TargetMachine::get_default_triple(),
         },
      }
   }

   fn to_errors<T, E>(r: Result<T, E>) -> Result<T, Errors>
   where
      E: ToString,
   {
      r.map_err(|e| {
         common::single_error(Error {
            filename: "internal error".into(),
            span: Span::default(),
            kind: ErrorKind::CodeGen(e.to_string()),
         })
      })
   }
}

impl backend::Backend for LlvmBackend {
   type Executable = LlvmExecutable;

   /// Compiles the given source file to an executable.
   fn compile(&self, root: SourceFile) -> Result<Self::Executable, Errors> {
      let (ast, root_node) = tsuki_frontend::analyze(&root)?;
      let context = Context::create();
      let mut state = CodeGen::new(root, &context);

      // TODO: Make this more generic by putting this in the code generator.
      // Right now this is here for testing purposes.
      let i32_type = context.i32_type();
      let string_type =
         context.i8_type().ptr_type(AddressSpace::Generic).ptr_type(AddressSpace::Generic);
      let main_type = i32_type.fn_type(&[i32_type.into(), string_type.into()], false);

      let main_fn = state.module.add_function("main", main_type, None);
      let main_builder = context.create_builder();
      let main_block = context.append_basic_block(main_fn, "entry");
      main_builder.position_at_end(main_block);

      state
         .generate_statement(&ast, root_node, &main_builder)
         .map_err(|e| common::single_error(e))?;

      main_builder.build_return(Some(&i32_type.const_zero()));

      let pass_manager = PassManager::create(&state.module);
      pass_manager.initialize();

      println!("———");
      println!(":: LLVM IR");
      println!("———");
      println!("{:?}", state);

      // Cross-compilation support, anyone?
      // Right now we initialize the native target only.
      Self::to_errors(Target::initialize_native(&InitializationConfig {
         // I'm not sure we need _all_ the features, so let's initialize the base and machine_code
         // generation only.
         asm_parser: true,
         asm_printer: true,
         base: true,
         disassembler: true,
         info: true,
         machine_code: true,
      }))?;

      Self::to_errors(state.module.verify())?;

      // Set up the target machine. We won't be enabling any special features here for now.
      let target = Self::to_errors(Target::from_triple(&self.target_triple))?;
      let machine = Self::to_errors(
         target
            .create_target_machine(
               &self.target_triple,
               "generic",
               "",
               OptimizationLevel::Default,
               RelocMode::Default,
               CodeModel::Default,
            )
            .ok_or("target triple is not supported"),
      )?;
      state.module.set_data_layout(&machine.get_target_data().get_data_layout());
      state.module.set_triple(&self.target_triple);

      // Create all the needed directories.
      let object_dir = self.cache_dir.join("object");
      Self::to_errors(std::fs::create_dir_all(&self.cache_dir))?;
      Self::to_errors(std::fs::create_dir_all(&object_dir))?;

      // Do some path manipulation to figure out where the object file should be placed.
      let object_name = format!("{}.o", &self.executable_name);
      let object_path = object_dir.join(&object_name);
      // Delete the old object file so that LLVM isn't going to complain when writing the new one.
      // The result is ignored because we don't care if the file exists or not.
      // If we lack sufficient permissions, then LLVM will point that out anyways.
      let _ = std::fs::remove_file(&object_path);

      // Fire away!
      Self::to_errors(machine.write_to_file(&state.module, FileType::Object, &object_path))?;

      let executable_path = self.cache_dir.join(&self.executable_name);
      Ok(LlvmExecutable {
         path: executable_path,
      })
   }
}

/// Struct representing a complete executable built using LLVM.
pub struct LlvmExecutable {
   path: PathBuf,
}

impl backend::Executable for LlvmExecutable {
   /// Runs the executable.
   fn run(&self, args: &[&str]) -> Result<i32, String> {
      Ok(Command::new(&self.path)
         .args(args.iter())
         .spawn()
         .map_err(|e| e.to_string())?
         .wait()
         .map_err(|e| e.to_string())?
         .code()
         .unwrap_or(0))
   }
}
