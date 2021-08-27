//! The root of the LLVM backend. This implements high-level functionality - compiling code into
//! object files, and linking those object files into executables.

mod blocks;
mod codegen;
mod expression;
mod functions;
mod libc;
mod variables;

use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::str::FromStr;

use inkwell::context::Context;
use inkwell::passes::PassManager;
use inkwell::targets::{
   CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use thiserror::Error;
use tsuki_frontend::backend;
use tsuki_frontend::common::{self, Error, ErrorKind, Errors, SourceFile, Span};

use codegen::CodeGen;

use crate::functions::Function;

/// Debug options for the backend.
pub struct DebugOptions {
   pub dump_ir: bool,
}

impl Default for DebugOptions {
   fn default() -> Self {
      Self { dump_ir: false }
   }
}

/// Struct representing the LLVM compilation backend and options passed to it.
pub struct LlvmBackend {
   cache_dir: PathBuf,
   executable_name: String,
   target_triple: TargetTriple,
   optimization_level: OptimizationLevel,
   frontend_debug_options: tsuki_frontend::DebugOptions,
   backend_debug_options: DebugOptions,
}

/// Options for creating an LLVM backend instance.
pub struct LlvmBackendConfig<'c, 'e, 't> {
   pub cache_dir: &'c Path,
   pub package_name: &'e str,
   pub target_triple: Option<&'t str>,
   pub optimization_level: OptimizationLevel,
   pub frontend_debug_options: tsuki_frontend::DebugOptions,
   pub backend_debug_options: DebugOptions,
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
         optimization_level: config.optimization_level,
         frontend_debug_options: config.frontend_debug_options,
         backend_debug_options: config.backend_debug_options,
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
      let ir = tsuki_frontend::analyze(&root, &self.frontend_debug_options)?;
      let context = Context::create();
      let module = context.create_module(&root.filename);

      // Set up the pass manager.
      let pm = PassManager::create(&module);

      // Make sure that the code we generate makes sense.
      pm.add_verifier_pass();

      if self.optimization_level >= OptimizationLevel::Essential {
         // Constant folding passes run twice: once at startup, and once after CFG simplicifation
         // and mem2reg, such that constant folding is also performed after simplifying the IR to
         // use more SSA and less allocas.
         pm.add_instruction_combining_pass();
         pm.add_reassociate_pass();

         // TODO: Figure out what GVN (global value numbering) is. The LLVM docs for passes don't
         // really say much about it.
         // (https://llvm.org/docs/Passes.html)
         pm.add_gvn_pass();

         // These passes simplify the control flow graph and turn memory operations into SSA form
         // wherever possible.
         pm.add_cfg_simplification_pass();
         pm.add_basic_alias_analysis_pass();
         pm.add_promote_memory_to_register_pass();

         // As said before, constant folding is performed twice.
         pm.add_instruction_combining_pass();
         pm.add_reassociate_pass();
      }

      pm.initialize();

      // Construct all the types.
      let i32_type = context.i32_type();
      let main_fun_type = i32_type.fn_type(&[], false);

      // Create the function and the codegen state.
      let main_fun = Function::new(&context, &module, "main", main_fun_type);
      let state = CodeGen::new(root, &context, &pm, module, main_fun);

      // Compile the modules' code.
      state.generate_statement(&ir, ir.root_node);

      // Return the zero exit code.
      state.finish_function(Some(&i32_type.const_zero()));

      if self.backend_debug_options.dump_ir {
         eprintln!(":: LLVM IR");
         eprintln!("{:?}", state);
         eprintln!();
      }

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
               inkwell::OptimizationLevel::Default,
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

/// Specifies the amount of optimizations to apply when compiling.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptimizationLevel {
   /// Perform no optimizations at all.
   None,
   Essential,
   Release,
}

/// Returned when an invalid optimization level is used.
#[derive(Clone, Copy, Debug)]
pub struct InvalidOptimizationLevel;

impl Display for InvalidOptimizationLevel {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      write!(f, "invalid optimization level")
   }
}

impl FromStr for OptimizationLevel {
   type Err = InvalidOptimizationLevel;

   /// Converts a string to an optimization level.
   ///
   /// Valid values include:
   /// - `"none"` → `None`
   /// - `"essential"` → `Essential`
   /// - `"release"` → `Release`
   ///
   /// Any other levels result in an `InvalidOptimizationLevel` error.
   fn from_str(s: &str) -> Result<Self, Self::Err> {
      match s {
         "none" => Ok(Self::None),
         "essential" => Ok(Self::Essential),
         "release" => Ok(Self::Release),
         _ => Err(InvalidOptimizationLevel),
      }
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
