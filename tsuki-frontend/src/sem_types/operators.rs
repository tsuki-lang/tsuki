//! Semantic analysis for operators and compiler intrinsics.

use crate::ast::{Ast, NodeId, NodeKind};
use crate::common::ErrorKind;
use crate::types::TypeLogEntry;

use super::{NodeContext, SemTypes};

impl<'s> SemTypes<'s> {
   // Currently, this does some rather simplistic analysis just to Make it Work™, but in the
   // future when operators will be lowered to trait instance function calls, this will be
   // replaced by much simpler logic and compiler intrinsics inside the stdlib.

   /// Annotates a unary operator with types.
   pub(super) fn annotate_unary_operator(&mut self, ast: &mut Ast, node: NodeId) -> TypeLogEntry {
      let log_entry = self.annotate_node(ast, ast.first_handle(node), NodeContext::expression());
      let right = self.log.type_id(log_entry);
      let right_kind = self.types.kind(right);
      let typ = match ast.kind(node) {
         NodeKind::Not if right == self.builtin.t_bool => right,
         NodeKind::BitNot if right_kind.is_integer() => right,
         NodeKind::Neg if right_kind.is_numeric() => right,
         _ => {
            let right_name = self.types.name(right);
            let kind = ErrorKind::InvalidUnaryOperator(right_name.into());
            return self.error(ast, node, kind);
         }
      };
      self.annotate(ast, node, typ)
   }

   /// Annotates a binary operator with types.
   pub(super) fn annotate_binary_operator(&mut self, ast: &mut Ast, node: NodeId) -> TypeLogEntry {
      let (left, right) = (ast.first_handle(node), ast.second_handle(node));
      let left_entry = self.annotate_node(ast, left, NodeContext::expression());
      let left_type = self.log.type_id(left_entry);
      let right_entry = self.annotate_node(ast, right, NodeContext::expression_of_type(left_type));
      let right_type = self.log.type_id(right_entry);
      let conversion = self.try_perform_implicit_conversion(ast, right, right_type, left_type);
      let left_type_kind = self.types.kind(left_type);
      let typ = match ast.kind(node) {
         // Arithmetic operators always evaluate to the same type as the LHS.
         NodeKind::Plus | NodeKind::Minus | NodeKind::Mul | NodeKind::Div
            if conversion.is_some() =>
         {
            left_type
         }

         // Comparison operators always evaluate to `Bool`.
         NodeKind::Equal | NodeKind::NotEqual
            if conversion.is_some() && left_type_kind.is_bool() =>
         {
            self.builtin.t_bool
         }
         | NodeKind::Equal
         | NodeKind::NotEqual
         | NodeKind::Less
         | NodeKind::LessEqual
         | NodeKind::Greater
         | NodeKind::GreaterEqual
            if conversion.is_some() && left_type_kind.is_numeric() =>
         {
            self.builtin.t_bool
         }

         // Other operators, and failed conversions, raise a type mismatch error.
         _ => {
            return self.type_mismatch(ast, node, left_type, right_type);
         }
      };
      self.annotate(ast, node, typ)
   }
}
