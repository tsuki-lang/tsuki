//! The root of semantic analysis.

use smallvec::SmallVec;

use crate::ast::{Ast, NodeData, NodeHandle, NodeKind};
use crate::common::{Error, Errors, ErrorKind, Span};
use crate::sem_types::SemTypes;
use crate::types::{Types, BuiltinTypes};

/// A semantic analyzer.
pub(crate) trait Sem {
   type Result;

   /// Analyzes the node with the given handle.
   fn analyze(&mut self, ast: &mut Ast, node: NodeHandle) -> Self::Result;

   /// Returns the filename string.
   fn filename(&self) -> &str;
   /// Returns a reference to the list of errors.
   fn errors(&self) -> &Errors;
   /// Returns a mutable reference to the list of errors.
   fn errors_mut(&mut self) -> &mut Errors;
   /// Consumes `self` to return the list of errors.
   fn into_errors(self) -> Errors;

   /// Emits an error of the given kind, at the given span.
   fn emit_error(&mut self, kind: ErrorKind, span: Span) {
      let filename = self.filename().to_owned();
      self.errors_mut().push(Error {
         filename,
         kind,
         span,
      });
   }
}

/// Semantic analysis state.
struct Analyzer {
   ast: Ast,
   root_node: NodeHandle,
}

impl Analyzer {
   /// Performs the given semantic pass, and returns the errors, if any.
   fn perform(&mut self, mut sem: impl Sem) -> Result<(), Errors> {
      sem.analyze(&mut self.ast, self.root_node);

      if sem.errors().len() > 0 {
         Err(sem.into_errors())
      } else {
         Ok(())
      }
   }
}

/// Analyzes and lowers the AST to a representation ready to be used by the backend.
pub fn analyze(filename: &str, ast: Ast, root_node: NodeHandle) -> Result<Ast, Errors> {
   let mut state = Analyzer {
      ast,
      root_node,
   };

   let mut types = Types::new();
   let builtin_types = BuiltinTypes::add_to(&mut types);

   state.perform(SemTypes::new(filename, &mut types, &builtin_types))?;

   Ok(state.ast)
}