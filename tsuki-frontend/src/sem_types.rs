//! Semantic analyzer for types.

use crate::ast::{Ast, Mutation, NodeHandle};
use crate::common::{ErrorKind, Errors, Span};
use crate::sem::{Sem, SemCommon};
use crate::types::{BuiltinTypes, TypeId, Types};

pub(crate) struct SemTypes<'c, 't, 'bt> {
   common: &'c SemCommon,
   errors: Errors,
   mutations: Vec<Mutation>,

   types: &'t mut Types,
   builtin: &'bt BuiltinTypes,
}

impl<'c, 't, 'bt> SemTypes<'c, 't, 'bt> {
   /// Creates a new instance of the `SemTypes` analysis phase.
   pub fn new(common: &'c SemCommon, types: &'t mut Types, builtin: &'bt BuiltinTypes) -> Self {
      SemTypes {
         common,
         errors: Errors::new(),
         mutations: Vec::new(),
         types,
         builtin,
      }
   }

   /// Emits an error of the given kind, also returning the error type.
   fn error(&mut self, kind: ErrorKind, span: Span) -> TypeId {
      self.emit_error(kind, span);
      self.builtin.t_error
   }
}

impl Sem for SemTypes<'_, '_, '_> {
   type Result = TypeId;

   /// Performs type analysis for the given AST node. This annotates the node with a concrete type.
   fn analyze(&mut self, ast: &Ast, node: NodeHandle) -> TypeId {
      // let span = ast.span(node);
      // let typ = self.error(ErrorKind::Nyi("wip".into()), span.clone());
      // self.types.set_node_type(node, typ);
      // typ
      self.builtin.t_unit
   }

   fn filename(&self) -> &str {
      &self.common.filename
   }

   fn errors(&self) -> &Errors {
      &self.errors
   }

   fn errors_mut(&mut self) -> &mut Errors {
      &mut self.errors
   }

   fn into_errors(self) -> Errors {
      self.errors
   }

   fn mutations(&self) -> &[Mutation] {
      &self.mutations
   }
}