//! The root of semantic analysis.

use smallvec::SmallVec;

use crate::ast::{Ast, NodeData, NodeHandle, NodeKind};
use crate::common::{Error, Errors, ErrorKind, Span};
use crate::sem_types::SemTypes;
use crate::types::Types;

/// A semantic analyzer.
pub(crate) trait Sem {
   /// Analyzes the node with the given handle.
   fn analyze(&mut self, ast: &mut Ast, node: NodeHandle);
   /// Returns a reference to the list of errors.
   fn errors(&self) -> &Errors;
   /// Returns a mutable reference to the list of errors.
   fn errors_mut(&mut self) -> &mut Errors;
   /// Consumes `self` to return the list of errors.
   fn into_errors(self) -> Errors;

   fn error(&mut self, kind: ErrorKind, span: Span) {
      self.errors_mut().push(Error {
         filename: "TODO: Sem Error filename".into(),
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
      let errors = SmallVec::<[Error; 8]>::new();

      sem.analyze(&mut self.ast, self.root_node);

      if errors.len() > 0 {
         Err(errors)
      } else {
         Ok(())
      }
   }
}

/// Analyzes and lowers the AST to a representation ready to be used by the backend.
pub fn analyze(ast: Ast, root_node: NodeHandle) -> Result<Ast, Errors> {
   let mut state = Analyzer {
      ast,
      root_node,
   };

   let mut types = Types::new();

   state.perform(SemTypes::new(&mut types))?;

   Ok(state.ast)
}