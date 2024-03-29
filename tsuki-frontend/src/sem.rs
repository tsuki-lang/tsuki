//! The root of semantic analysis.

use std::path::Path;

use crate::ast::{Ast, NodeId};
use crate::common::{Error, ErrorKind, Errors, SourceFile, Span};
use crate::functions::Functions;
use crate::scope::{Scopes, Symbols};
pub use crate::types::DefaultTypes;
use crate::types::{BuiltinTypes, TypeLog, Types};

use crate::sem_literals::SemLiterals;
use crate::sem_types::{SemTypes, SemTypesBorrows};

/// A semantic analyzer.
pub(crate) trait SemPass {
   type Result;

   /// Analyzes the AST from the given root node, and returns the modified version of the AST.
   fn analyze(&mut self, ast: Ast, root_node: NodeId) -> Ast;

   /// Returns the filename string.
   fn filename(&self) -> &Path;
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
   root_node: NodeId,
}

impl Analyzer {
   /// Performs the given semantic pass, and returns the errors, if any.
   fn perform(mut self, mut sem: impl SemPass) -> Result<Self, Errors> {
      self.ast = sem.analyze(self.ast, self.root_node);

      if sem.errors().len() > 0 {
         Err(sem.into_errors())
      } else {
         Ok(self)
      }
   }
}

/// Common information shared by the semantic pass.
pub(crate) struct SemCommon<'s> {
   pub file: &'s SourceFile,
   pub default_types: DefaultTypes,
}

impl<'s> SemCommon<'s> {
   /// Returns the source code substring pointed to by the node's `first..second`.
   pub fn get_source_range_from_node(&self, ast: &Ast, node: NodeId) -> &str {
      let source_range = ast.first(node)..ast.second(node);
      &self.file.source[source_range]
   }
}

/// The intermediate representation output by the analyzer. This contains the AST and type
/// information.
pub struct Ir {
   pub ast: Ast,
   pub root_node: NodeId,
   pub types: Types,
   pub symbols: Symbols,
   pub functions: Functions,
}

/// The options passed to `analyze`.
pub struct AnalyzeOptions<'s> {
   pub file: &'s SourceFile,
   pub ast: Ast,
   pub root_node: NodeId,
   pub default_types: DefaultTypes,
}

/// Analyzes and lowers the AST to a representation ready to be used by the backend.
pub fn analyze(options: AnalyzeOptions) -> Result<Ir, Errors> {
   let AnalyzeOptions {
      file,
      ast,
      root_node,
      default_types,
   } = options;
   let mut state = Analyzer { ast, root_node };

   let common = SemCommon {
      file,
      default_types,
   };
   let mut types = Types::new();
   let mut type_log = TypeLog::new();
   let builtin_types = BuiltinTypes::add_to(&mut types, &common.default_types);
   let mut scopes = Scopes::new();
   let mut symbols = Symbols::new();
   let mut functions = Functions::new();

   // NOTE: Maybe split errors into normal and fatal?
   // Normal errors would be accumulated into the existing error list, but would not halt the
   // analysis completely. Fatal errors would halt the analysis, and would occur if something really
   // goes wrong inside of a phase, yielding AST that might break the phase after it.
   // Also, warnings anyone?
   state = state.perform(SemLiterals::new(&common))?;
   state = state.perform(SemTypes::new(SemTypesBorrows {
      common: &common,
      types: &mut types,
      log: &mut type_log,
      builtin: &builtin_types,
      scopes: &mut scopes,
      symbols: &mut symbols,
      functions: &mut functions,
   }))?;

   Ok(Ir {
      ast: state.ast,
      root_node,
      types,
      symbols,
      functions,
   })
}
