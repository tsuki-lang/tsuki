//! Type analysis for control flow constructs.

use crate::ast::{Ast, NodeId, NodeKind};
use crate::common::ErrorKind;
use crate::sem::SemPass;
use crate::types::TypeLogEntry;

use super::{NodeContext, SemTypes};

impl<'s> SemTypes<'s> {
   /// Annotates a "pass" (`_`) statement.
   pub(super) fn annotate_pass(&mut self, ast: &mut Ast, node: NodeId) -> TypeLogEntry {
      self.annotate(ast, node, self.builtin.t_statement)
   }

   /// Annotates a prefix `do` block.
   pub(super) fn annotate_do(
      &mut self,
      ast: &mut Ast,
      node: NodeId,
      context: NodeContext,
   ) -> TypeLogEntry {
      let scope = self.scope_stack.push(self.scopes.create_scope());
      ast.set_scope(node, Some(scope));
      let log_entry = self.annotate_statement_list(ast, node, context);
      self.scope_stack.pop();
      ast.convert_preserve(
         node,
         match context {
            NodeContext::Expression => NodeKind::DoExpression,
            NodeContext::Statement => NodeKind::DoStatement,
         },
      );
      log_entry
   }

   /// Annotates an `if` expression or `if` statement.
   pub(super) fn annotate_if(
      &mut self,
      ast: &mut Ast,
      node: NodeId,
      context: NodeContext,
   ) -> TypeLogEntry {
      let mut typ = None;
      ast.walk_node_list_mut(node, |ast, _index, branch| {
         // The scope is introduced before the condition is analyzed to have proper scoping behavior
         // in `if val`.
         let scope = self.scope_stack.push(self.scopes.create_scope());
         ast.set_scope(branch, Some(scope));
         // Only check the condition if it's an `if` branch. `else` branches do not have
         // the condition.
         if ast.kind(branch) == NodeKind::IfBranch {
            let condition = ast.first_handle(branch);
            let condition_entry = self.annotate_node(ast, condition, context);
            let condition_type = self.log.type_id(condition_entry);
            if !self.types.kind(condition_type).is_bool() {
               self.emit_error(
                  ErrorKind::IfConditionMustBeBool,
                  ast.span(condition).clone(),
               );
            }
         }
         let body_entry = self.annotate_statement_list(ast, branch, context);
         let body_type = self.log.type_id(body_entry);
         if context == NodeContext::Expression {
            match typ {
               None => typ = Some(body_type),
               Some(typ) if body_type != typ => {
                  // The type log entry is discarded here, because more mismatch errors may
                  // arise later in the `if` statement.
                  let _ = self.type_mismatch(ast, node, typ, body_type);
               }
               _ => (),
            }
         }
         self.scope_stack.pop();
      });
      ast.convert_preserve(
         node,
         match context {
            NodeContext::Expression => NodeKind::IfExpression,
            NodeContext::Statement => NodeKind::IfStatement,
         },
      );
      self.annotate(ast, node, typ.unwrap_or(self.builtin.t_statement))
   }

   /// Annotates a `while` loop.
   pub(super) fn annotate_while(&mut self, ast: &mut Ast, node: NodeId) -> TypeLogEntry {
      let condition_node = ast.first_handle(node);
      let condition_entry = self.annotate_node(ast, condition_node, NodeContext::Expression);
      let condition_type = self.log.type_id(condition_entry);
      if !self.types.kind(condition_type).is_bool() {
         return self.error(ast, condition_node, ErrorKind::WhileConditionMustBeBool);
      }
      let _ = self.annotate_statement_list(ast, node, NodeContext::Statement);
      self.annotate(ast, node, self.builtin.t_statement)
   }

   /// Annotates a `return` statement.
   pub(super) fn annotate_return(&mut self, ast: &mut Ast, node: NodeId) -> TypeLogEntry {
      // `return` can only be used in a function.
      if let Some(function_id) = self.current_function {
         let expected_return_type = self.functions.parameters(function_id).return_type;
         let value_node = ast.first_handle(node);
         let return_log = if value_node != NodeId::null() {
            // For `return`s that do actually return something, the path is straightforward.
            self.annotate_node(ast, value_node, NodeContext::Expression)
         } else {
            // For `return`s that _don't_ return a value, we need to duplicate the empty node such
            // that it gets a unique ID that we can attach the unit type to.
            let value_node = ast.duplicate(value_node);
            ast.set_first_handle(node, value_node);
            self.annotate(ast, value_node, self.builtin.t_unit)
         };
         let provided_return_type = self.log.type_id(return_log);
         let return_log = self
            .perform_implicit_conversion(ast, node, provided_return_type, expected_return_type)
            .unwrap_or(return_log);
         let provided_type = self.log.type_id(return_log);
         if provided_type != expected_return_type {
            return self.type_mismatch(ast, node, expected_return_type, provided_type);
         }
         self.annotate(ast, node, self.builtin.t_noreturn)
      } else {
         self.error(ast, node, ErrorKind::ReturnOutsideOfFunction)
      }
   }
}
