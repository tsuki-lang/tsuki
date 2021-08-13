//! Module for common backend functionality.
//! This can be used by CLIs and other programs that need to compile tsuki source code.

use crate::common::{Errors, SourceFile};

/// Trait implemented by all backends that can compile and run tsuki source code.
pub trait Backend {
   /// The kind of code emitted by the backend. This can be an object file, an executable file,
   /// or even a JIT-compiled function; you name it.
   type Target;

   /// Compiles a source file to a target.
   ///
   /// # Errors
   /// `Err` should return an error of kind `CodeGen`, together with a diagnostic message.
   /// These errors should only be thrown in dire cases where something went wrong in earlier stages
   /// of compilation, and the backend can't make sense of what the frontend produced.
   fn compile(&self, root: SourceFile) -> Result<Self::Target, Errors>;
}
