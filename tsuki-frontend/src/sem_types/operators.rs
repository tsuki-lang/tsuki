//! Semantic analysis for operators and compiler intrinsics.

use crate::ast::{Ast, NodeHandle, NodeKind};
use crate::common::ErrorKind;
use crate::types::TypeLogEntry;

use super::{NodeContext, SemTypes};

impl<'s> SemTypes<'s> {
   // Currently, this does some rather simplistic analysis just to Make it Work™, but in the
   // future when operators will be lowered to trait instance function calls, this will be
   // replaced by much simpler logic and compiler intrinsics inside the stdlib.

   /// Annotates a unary operator with types.
   pub(super) fn annotate_unary_operator(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
   ) -> TypeLogEntry {
      let log_entry = self.annotate_node(ast, ast.first_handle(node), NodeContext::Expression);
      let right = self.log.typ(log_entry);
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
   pub(super) fn annotate_binary_operator(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
   ) -> TypeLogEntry {
      let (left, right) = (ast.first_handle(node), ast.second_handle(node));
      let left_entry = self.annotate_node(ast, left, NodeContext::Expression);
      let right_entry = self.annotate_node(ast, right, NodeContext::Expression);
      let left_type = self.log.typ(left_entry);
      let right_type = self.log.typ(right_entry);
      let conversion = self.perform_implicit_conversion(ast, right, right_type, left_type);
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

   /// Annotates a function call.
   pub(super) fn annotate_call(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      // Because function calls aren't really supported yet, the function call syntax is reused
      // for backend intrinsics. This will someday be replaced by a `compiler_intrinsic` pragma.
      let callee = ast.first_handle(node);
      if ast.kind(callee) != NodeKind::Identifier {
         return self.error(ast, node, ErrorKind::NonIntrinCall);
      }
      let name = self.common.get_source_range_from_node(ast, callee);
      let expected_argument_count;
      match name {
         "__intrin_print_int32" => {
            expected_argument_count = 1;
            ast.convert_preserve(node, NodeKind::PrintInt32);
         }
         "__intrin_print_float32" => {
            expected_argument_count = 1;
            ast.convert_preserve(node, NodeKind::PrintFloat32);
         }
         _ => return self.error(ast, node, ErrorKind::NonIntrinCall),
      }
      let arguments = ast.extra(node).unwrap_node_list();
      if arguments.len() != expected_argument_count {
         let kind = ErrorKind::NArgumentsExpected(expected_argument_count, arguments.len());
         return self.error(ast, node, kind);
      }
      // I don't like that I have to use normal indices. Give me back my inline iterators :(
      for i in 0..arguments.len() {
         // TODO: Argument type checking.
         let argument = ast.extra(node).unwrap_node_list()[i];
         let _ = self.annotate_node(ast, argument, NodeContext::Expression);
      }
      self.annotate(ast, node, self.builtin.t_unit)
   }
}
