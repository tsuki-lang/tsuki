//! The root of semantic analysis.

use crate::ast::{Ast, Mutation, NodeData, NodeHandle, NodeKind};
use crate::common::{Error, Errors, ErrorKind, Span};
use crate::types::{Types, BuiltinTypes};

use crate::sem_types::SemTypes;
use crate::sem_literals::SemLiterals;

/// A semantic analyzer.
pub(crate) trait Sem {
   type Result;

   /// Analyzes the node with the given handle.
   fn analyze(&mut self, ast: &Ast, node: NodeHandle) -> Self::Result;

   /// Returns the filename string.
   fn filename(&self) -> &str;
   /// Returns a reference to the list of errors.
   fn errors(&self) -> &Errors;
   /// Returns a mutable reference to the list of errors.
   fn errors_mut(&mut self) -> &mut Errors;
   /// Consumes `self` to return the list of errors.
   fn into_errors(self) -> Errors;

   /// Returns the mutations queued up by the analyzer.
   fn mutations(&self) -> &[Mutation];

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

      // All mutations are committed before we check for errors. This should reduce the risk of
      // phases in the future getting invalid AST.
      for mutation in sem.mutations() {
         mutation.commit(&mut self.ast);
      }

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

   // NOTE: Maybe split errors into normal and fatal?
   // Normal errors would be accumulated into the existing error list, but would not halt the
   // analysis completely. Fatal errors would halt the analysis, and would occur if something really
   // goes wrong inside of a phase, yielding AST that might break the phase after it.
   // Also, warnings anyone?
   state.perform(SemLiterals::new(filename))?;
   state.perform(SemTypes::new(filename, &mut types, &builtin_types))?;

   Ok(state.ast)
}