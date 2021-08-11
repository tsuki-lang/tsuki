//! Code generation for expressions.

use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};

use crate::state::CodeGen;

impl<'s, 'c> CodeGen<'s, 'c> {
   /// Generates code for any expression node.
   fn generate_expression(&self, ast: &Ast, node: NodeHandle) {
      match ast.kind(node) {
         _ => unreachable!(),
      }
   }
}
