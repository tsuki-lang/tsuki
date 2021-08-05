//! The root of semantic analysis.

use crate::ast::{Ast, Mutation, NodeHandle};
use crate::common::{Error, ErrorKind, Errors, Span};
pub use crate::types::DefaultTypes;
use crate::types::{BuiltinTypes, Types};

use crate::sem_literals::SemLiterals;
use crate::sem_types::SemTypes;

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

/// Common information shared by the semantic pass.
pub(crate) struct SemCommon {
   pub filename: String,
   pub source: String,
   pub default_types: DefaultTypes,
}

impl SemCommon {
   /// Returns the source code substring pointed to by the node's `first..second`.
   pub fn get_source_range_from_node(&self, ast: &Ast, node: NodeHandle) -> &str {
      let source_range = ast.first(node)..ast.second(node);
      &self.source[source_range]
   }
}

/// The options passed to `analyze`.
pub struct AnalyzeOptions<'f, 's> {
   pub filename: &'f str,
   pub source: &'s str,
   pub ast: Ast,
   pub root_node: NodeHandle,
   pub default_types: DefaultTypes,
}

/// Analyzes and lowers the AST to a representation ready to be used by the backend.
pub fn analyze(options: AnalyzeOptions) -> Result<Ast, Errors> {
   let AnalyzeOptions {
      filename,
      source,
      ast,
      root_node,
      default_types,
   } = options;
   let mut state = Analyzer { ast, root_node };

   let common = SemCommon {
      filename: filename.into(),
      source: source.into(),
      default_types,
   };
   let mut types = Types::new();
   let builtin_types = BuiltinTypes::add_to(&mut types, &common.default_types);

   // NOTE: Maybe split errors into normal and fatal?
   // Normal errors would be accumulated into the existing error list, but would not halt the
   // analysis completely. Fatal errors would halt the analysis, and would occur if something really
   // goes wrong inside of a phase, yielding AST that might break the phase after it.
   // Also, warnings anyone?
   state.perform(SemLiterals::new(&common))?;
   state.perform(SemTypes::new(&common, &mut types, &builtin_types))?;

   Ok(state.ast)
}
