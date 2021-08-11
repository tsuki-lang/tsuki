mod expression;

use tsuki_frontend::common::{Errors, SourceFile};
use tsuki_frontend::types::{FloatSize, IntegerSize};
use tsuki_frontend::{astdump, backend, common, lexer, parser, sem};

pub struct LlvmBackend;

impl backend::Backend for LlvmBackend {
   type Executable = LlvmExecutable;

   fn compile(root: SourceFile) -> Result<Self::Executable, Errors> {
      let ast = tsuki_frontend::analyze(&root)?;

      Ok(LlvmExecutable {

      })
   }
}

pub struct LlvmExecutable {
}

impl backend::Executable for LlvmExecutable {
   fn run(&self, args: &[&str]) {
      println!("Soon:tm:");
   }
}
