//! Code generation for expressions.

use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};

use crate::codegen::CodeGen;

impl<'c> CodeGen<'c> {
   /// Generates code for any expression node.
   pub(crate) fn generate_expression(&self, ast: &Ast, node: NodeHandle) {
      match ast.kind(node) {
         _ => unreachable!(),
      }
   }
}
