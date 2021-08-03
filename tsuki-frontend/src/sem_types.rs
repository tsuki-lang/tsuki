//! Semantic analyzer for types.

use crate::ast::{Ast, NodeHandle};
use crate::common::Errors;
use crate::sem::Sem;
use crate::types::Types;

pub struct SemTypes<'t> {
   errors: Errors,
   types: &'t mut Types,
}

impl<'t> SemTypes<'t> {
   pub fn new(types: &'t mut Types) -> Self {
      SemTypes {
         errors: Errors::new(),
         types,
      }
   }
}

impl<'t> Sem for SemTypes<'t> {
   /// Performs type analysis for the given AST node. This annotates the node with a concrete type.
   fn analyze(&mut self, ast: &mut Ast, node: NodeHandle) {

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
}