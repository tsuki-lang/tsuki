mod expression;
mod state;

use inkwell::context::Context;
use tsuki_frontend::common::{Errors, SourceFile};
use tsuki_frontend::types::{FloatSize, IntegerSize};
use tsuki_frontend::{astdump, backend, common, lexer, parser, sem};

use state::CodeGen;

/// Unit struct representing the LLVM compilation backend.
pub struct LlvmBackend;

impl backend::Backend for LlvmBackend {
   type Executable = LlvmExecutable;

   /// Compiles the given source file to an executable.
   fn compile(root: SourceFile) -> Result<Self::Executable, Errors> {
      let (ast, root_node) = tsuki_frontend::analyze(&root)?;
      let context = Context::create();
      let mut state = CodeGen::new(&root, &context);
      state.generate(&ast, root_node).map_err(|e| {
         let mut errs = Errors::new();
         errs.push(e);
         errs
      })?;

      Ok(LlvmExecutable {})
   }
}

/// Struct representing a complete executable built using LLVM.
pub struct LlvmExecutable {}

impl backend::Executable for LlvmExecutable {
   /// Runs the executable.
   fn run(&self, args: &[&str]) {
      println!("Soon:tm:");
   }
}
