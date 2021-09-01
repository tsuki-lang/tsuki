//! Type analysis for variables, object fields, pointers, etc.

use crate::ast::{Ast, NodeHandle, NodeKind};
use crate::common::ErrorKind;
use crate::scope::{SymbolKind, Variable, VariableKind};
use crate::types::TypeLogEntry;

use super::{NodeContext, SemTypes};

impl<'c, 't, 'tl, 'bt, 's, 'sy> SemTypes<'c, 't, 'tl, 'bt, 's, 'sy> {
   /// Annotates a location expression, ie. variables `a`, members `.x`.
   pub(super) fn annotate_location(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
      match ast.kind(node) {
         NodeKind::Identifier => {
            let name = self.common.get_source_range_from_node(ast, node);
            if let Some(variable) = self.scope_stack.lookup(self.scopes, name) {
               let typ = self.symbols.type_id(variable);
               ast.convert_to_symbol(node, variable);
               let log = self.annotate(ast, node, typ);
               ast.wrap(node, NodeKind::Variable);
               log
            } else {
               self.error(ast, node, ErrorKind::UndeclaredSymbol(name.into()))
            }
         }
         // TODO: Make this into a better error. This would require slicing the source string,
         // which we can't do because spans don't store direct byte indices to it at the moment.
         _ => self.error(ast, node, ErrorKind::InvalidLhsOfAssignment),
      }
   }

   /// Annotates an assignment.
   pub(super) fn annotate_assignment(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
      // TODO: Pointers and assigning values to them.
      let (left, right) = (ast.first_handle(node), ast.second_handle(node));
      let left_entry = self.annotate_location(ast, left);
      let left_type = self.log.typ(left_entry);
      let right_entry = self.annotate_node(ast, right, NodeContext::Expression);
      let right_type = self.log.typ(right_entry);
      // Check types.
      if right_type != left_type {
         return self.type_mismatch(ast, node, left_type, right_type);
      }
      // Check mutability.
      // TODO: This could maybe be moved into a different check, shoving this logic into assignments
      // doesn't seem very clean.
      let target_is_mutable = match ast.kind(left) {
         NodeKind::Variable => {
            let symbol = ast.first_handle(left);
            let variable = self.symbols.kind(ast.symbol_id(symbol)).unwrap_variable();
            variable.kind == VariableKind::Var
         }
         _ => unreachable!(),
      };
      if !target_is_mutable {
         return self.error(ast, left, ErrorKind::CannotAssignImmutableLocation);
      }
      match context {
         NodeContext::Expression => self.annotate(ast, node, left_type),
         NodeContext::Statement => self.annotate(ast, node, self.builtin.t_statement),
      }
   }

   /// Annotates a variable declaration.
   pub(super) fn annotate_variable_declaration(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
   ) -> TypeLogEntry {
      let kind = match ast.kind(node) {
         NodeKind::Val => VariableKind::Val,
         NodeKind::Var => VariableKind::Var,
         _ => unreachable!(),
      };
      let variable = Variable { kind };

      // Figure out the name and expected type. This expected type can be `None`, and in that case,
      // should be inferred from context.
      let left_node = ast.first_handle(node);
      let (name_node, expected_type) = match ast.kind(ast.first_handle(node)) {
         NodeKind::VariableType => {
            let name = ast.first_handle(left_node);
            let typ = ast.second_handle(left_node);
            (name, Some(self.lookup_type(ast, typ)))
         }
         _ => (left_node, None),
      };

      // Annotate the value.
      let value_node = ast.second_handle(node);
      let value_log = self.annotate_node(ast, value_node, NodeContext::Expression);
      let value_type = self.log.typ(value_log);
      match ast.kind(name_node) {
         NodeKind::Discard => {
            // A discarding assignment is converted to an AssignDiscard node containing
            // the original value.
            ast.convert(node, NodeKind::AssignDiscard);
            ast.set_first_handle(node, value_node);
         }
         NodeKind::Identifier => {
            let name = self.common.get_source_range_from_node(ast, name_node);
            let symbol =
               self.symbols.create(name, node, value_type, SymbolKind::Variable(variable));
            ast.convert_to_symbol(name_node, symbol);
            let scope = self.scope_stack.top();
            self.scopes.insert(scope, name, symbol);
            // The variable type annotation is less relevant to error reporting than the fact that
            // it's a statement. This sounds counterintuitive at first, but note that we're
            // requested to annotate the Val/Var node, not the variable name node, so the calling
            // function likely expects a statement instead of an expression.
            let _ = self.annotate(ast, name_node, value_type);
         }
         _ => unreachable!(),
      }
      self.annotate(ast, node, self.builtin.t_statement)
   }
}
