//! The root of the LLVM backend. This implements high-level functionality - compiling code into
//! object files, and linking those object files into executables.

mod blocks;
mod codegen;
mod expression;
mod libc;

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{
   CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::OptimizationLevel;
use thiserror::Error;
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
   pub package_name: &'e str,
   pub target_triple: Option<&'t str>,
}

impl LlvmBackend {
   /// Creates a new instance of the LLVM compilation backend, with the provided options.
   pub fn new(config: LlvmBackendConfig) -> Self {
      Self {
         cache_dir: config.cache_dir.to_owned(),
         executable_name: config.package_name.to_owned(),
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
   type Target = ObjectFile;

   /// Compiles the given source file to an executable.
   fn compile(&self, root: SourceFile) -> Result<Self::Target, Errors> {
      let ir = tsuki_frontend::analyze(&root)?;
      let context = Context::create();
      let mut state = CodeGen::new(root, &context);

      // Construct all the types.
      let i32_type = state.context.i32_type();
      let main_fn_type = i32_type.fn_type(&[], false);

      // Create the function, and an entry block.
      let main_fn = state.module.add_function("main", main_fn_type, None);
      let entry = context.append_basic_block(main_fn, "entry");
      state.builder.position_at_end(entry);

      // Compile the modules' code.
      state.generate_statement(&ir, ir.root_node);

      // Return the zero exit code.
      state.builder.build_return(Some(&i32_type.const_zero()));

      let pass_manager = PassManager::create(&state.module);
      pass_manager.initialize();

      println!(":: LLVM IR");
      println!("{:?}", state);
      println!();

      // Cross-compilation support, anyone?
      // Right now we initialize the native target only.
      Self::to_errors(Target::initialize_native(&InitializationConfig {
         // Honestly, I'm not sure we need _all_ the features.
         // TODO: Check which ones can be disabled.
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

      // Compile the object file.
      Self::to_errors(machine.write_to_file(&state.module, FileType::Object, &object_path))?;

      Ok(ObjectFile { path: object_path })
   }
}

/// Struct representing an object file built using LLVM.
pub struct ObjectFile {
   path: PathBuf,
}

/// Struct representing an executable file built from linked object files.
pub struct ExecutableFile {
   path: PathBuf,
}

#[derive(Debug, Error)]
pub enum LinkError {
   #[error("no linker found; check the $TSUKI_LD and $LD environment variables.")]
   NoLinker,
   #[error("the linker exited with an error (code {0}):\n{1}")]
   Failure(i32, String),
   #[error("I/O error: {0}")]
   Io(#[from] std::io::Error),
}

impl ExecutableFile {
   /// Links the provided object files into an executable.
   ///
   /// This launches the standard compiler pointed to by the `$TSUKI_CC` or `$CC` environment
   /// variables (in that order), or the `cc` executable found in `$PATH`.
   pub fn link(backend: LlvmBackend, objects: &[ObjectFile]) -> Result<Self, LinkError> {
      use std::env;

      let linker = env::var_os("TSUKI_CC")
         .or_else(|| env::var_os("CC"))
         .or_else(|| Some("cc".into()))
         .ok_or(LinkError::NoLinker)?;
      let output_path = backend.cache_dir.join(&backend.executable_name);

      let mut cmd = Command::new(linker);
      // Pass the output path to the linker.
      cmd.arg("-o");
      cmd.arg(&output_path);
      for object in objects {
         cmd.arg(&object.path);
      }

      let output = cmd.output()?;
      if let Some(exit_code) = output.status.code() {
         if exit_code != 0 {
            let errors = std::str::from_utf8(&output.stderr).ok().unwrap_or("<invalid UTF-8>");
            return Err(LinkError::Failure(exit_code, errors.into()));
         }
      }
      Ok(Self { path: output_path })
   }

   /// Runs the executable, passing the given arguments to it. The output contains captured
   /// stdout and stderr, as well as the exit code.
   pub fn run(&self, args: &[&str]) -> Result<Output, std::io::Error> {
      Command::new(&self.path).args(args).output()
   }
}
