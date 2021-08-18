//! Code generation for block-like structures (statement lists and the like).

use inkwell::values::BasicValueEnum;
use tsuki_frontend::ast::{NodeHandle, NodeKind};
use tsuki_frontend::common::Error;
use tsuki_frontend::Ir;

use crate::codegen::CodeGen;

impl<'c> CodeGen<'c> {
   /// Generates code for a list of statements.
   pub(crate) fn generate_statements(&self, ir: &Ir, node: NodeHandle) {
      ir.ast.walk_node_list(node, |ast, _index, node| {
         self.generate_statement(ir, node);
      });
   }

   /// Generates code for a list of statements with a tail expression.
   fn generate_statements_with_tail_expression(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum {
      let mut tail = None;
      for (index, &child) in ir.ast.extra(node).unwrap_node_list().iter().enumerate() {
         if ir.ast.is_last_child(node, index) {
            tail = Some(self.generate_expression(ir, child))
         } else {
            self.generate_statement(ir, child);
         }
      }
      if let Some(tail) = tail {
         tail
      } else {
         self.generate_unit_literal(&ir.ast, node).into()
      }
   }

   /// Generates code for a `do` expression or a `do` statement.
   ///
   /// If the node is a `DoExpression`, returns `Some` with the tail expression. Otherwise
   /// if the kind is `DoStatement`, returns `None`.
   pub(crate) fn generate_do(&self, ir: &Ir, node: NodeHandle) -> Option<BasicValueEnum> {
      match ir.ast.kind(node) {
         NodeKind::DoExpression => Some(self.generate_statements_with_tail_expression(ir, node)),
         NodeKind::DoStatement => {
            self.generate_statements(ir, node);
            None
         }
         _ => unreachable!(),
      }
   }
}
