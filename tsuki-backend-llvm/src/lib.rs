mod expression;
mod libc;
mod state;

use std::path::{Path, PathBuf};
use std::process::Command;

use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use tsuki_frontend::common::{self, Error, ErrorKind, Errors, SourceFile, Span};
use tsuki_frontend::backend;

use state::CodeGen;

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
      where E: ToString,
   {
      r.map_err(|e| common::single_error(Error {
         filename: "internal error".into(),
         span: Span::default(),
         kind: ErrorKind::CodeGen(e.to_string()),
      }))
   }
}

impl backend::Backend for LlvmBackend {
   type Executable = LlvmExecutable;

   /// Compiles the given source file to an executable.
   fn compile(&self, root: SourceFile) -> Result<Self::Executable, Errors> {
      let (ast, root_node) = tsuki_frontend::analyze(&root)?;
      let context = Context::create();
      let mut state = CodeGen::new(root, &context);

      state.generate(&ast, root_node).map_err(|e| common::single_error(e))?;

      // Cross-compilation support, anyone?
      // Right now we initialize the native target only.
      Self::to_errors(Target::initialize_native(&InitializationConfig {
         // I'm not sure we need _all_ the features, so let's initialize the base and machine_code
         // generation only.
         asm_parser: false,
         asm_printer: false,
         base: true,
         disassembler: false,
         info: false,
         machine_code: true,
      }))?;

      let executable_path = self.cache_dir.join(&self.executable_name);
      Ok(LlvmExecutable { path: executable_path })
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
