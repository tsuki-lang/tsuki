//! Module for common backend functionality.
//! This can be used by CLIs and other programs that need to compile tsuki source code.

use crate::common::{Errors, SourceFile};

/// Common functionality for executables.
pub trait Executable {
   /// Runs the executable with the provided arguments.
   ///
   /// Upon failure, this should return a string with an error message describing the failure.
   fn run(&self, args: &[&str]) -> Result<i32, String>;
}

/// Trait implemented by all backends that can compile and run tsuki source code.
pub trait Backend {
   /// The executable emitted by the backend.
   type Executable: Executable;

   /// Compiles a source file to an executable.
   ///
   /// # Errors
   /// `Err` should return an error of kind `CodeGen`, together with a diagnostic message.
   /// These errors should only be thrown in dire cases where something went wrong in earlier stages
   /// of compilation, and the backend can't make sense of what the frontend produced.
   fn compile(&self, root: SourceFile) -> Result<Self::Executable, Errors>;
}
