//! Type analysis for variables, object fields, pointers, etc.

use crate::ast::{Ast, NodeHandle, NodeKind};
use crate::common::ErrorKind;
use crate::scope::{SymbolId, SymbolKind, Variable, VariableKind};
use crate::types::{TypeLogEntry, TypeLogResult};

use super::{NodeContext, SemTypes};

impl<'s> SemTypes<'s> {
   /// Annotates a location expression, ie. variables `a`, members `.x`.
   pub(super) fn annotate_location(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogResult {
      match ast.kind(node) {
         NodeKind::Identifier => {
            let symbol = self.lookup_variable(ast, node)?;
            Ok(self.annotate_location_symbol(ast, node, symbol))
         }
         // TODO: Make this into a better error. This would require slicing the source string,
         // which we can't do because spans don't store direct byte indices to it at the moment.
         _ => Err(self.error(ast, node, ErrorKind::InvalidLocation)),
      }
   }

   /// Annotates a symbol that refers to a location.
   fn annotate_location_symbol(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      symbol: SymbolId,
   ) -> TypeLogEntry {
      match self.symbols.kind(symbol) {
         SymbolKind::Variable(_variable) => {
            let typ = self.symbols.type_id(symbol);
            ast.convert_to_symbol(node, symbol);
            let log = self.annotate(ast, node, typ);
            ast.wrap(node, NodeKind::Variable);
            log
         }
         _ => self.error(ast, node, ErrorKind::InvalidLocation),
      }
   }

   /// Annotates an assignment.
   pub(super) fn annotate_assignment(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogResult {
      // TODO: Pointers and assigning values to them.
      let (left, right) = (ast.first_handle(node), ast.second_handle(node));
      let left_entry = self.annotate_location(ast, left)?;
      let left_type = self.log.typ(left_entry);
      let right_entry = self.annotate_node(ast, right, NodeContext::Expression);
      let right_type = self.log.typ(right_entry);
      // Check types.
      if right_type != left_type {
         return Err(self.type_mismatch(ast, node, left_type, right_type));
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
         _ => false,
      };
      if !target_is_mutable {
         return Err(self.error(ast, left, ErrorKind::CannotAssignImmutableLocation));
      }
      Ok(match context {
         NodeContext::Expression => self.annotate(ast, node, left_type),
         NodeContext::Statement => self.annotate(ast, node, self.builtin.t_statement),
      })
   }

   /// Annotates a variable declaration.
   pub(super) fn annotate_variable_declaration(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
   ) -> TypeLogResult {
      let kind = match ast.kind(node) {
         NodeKind::Val => VariableKind::Val,
         NodeKind::Var => VariableKind::Var,
         _ => unreachable!(),
      };
      let variable = Variable { kind };

      // Figure out the name and expected type. This expected type can be `None`, and in that case,
      // should be inferred from context.
      let left_node = ast.first_handle(node);
      let (name_node, expected_type) = match ast.kind(left_node) {
         NodeKind::VariableType => {
            let name_node = ast.first_handle(left_node);
            let type_node = ast.second_handle(left_node);
            let typ = self.lookup_type(ast, type_node)?;
            (name_node, Some(typ))
         }
         _ => (left_node, None),
      };
      // Normalize the LHS to the name only.
      ast.set_first_handle(node, name_node);

      // Annotate the value.
      let value_node = ast.second_handle(node);
      let value_log = self.annotate_node(ast, value_node, NodeContext::Expression);
      let value_type = self.log.typ(value_log);

      // Check if the type matches if an explicit type was provided.
      let value_type = match expected_type {
         Some(typ) => {
            if let Some(log) = self.perform_implicit_conversion(ast, value_node, value_type, typ) {
               self.log.typ(log)
            } else {
               let expected_name = self.types.name(typ).to_owned();
               let value_name = self.types.name(value_type).to_owned();
               return Err(self.error(
                  ast,
                  node,
                  ErrorKind::TypeMismatch(expected_name, value_name),
               ));
            }
         }
         None => value_type,
      };

      // Add to scope.
      match ast.kind(name_node) {
         NodeKind::Discard => {
            // A discarding assignment is converted to an AssignDiscard node containing
            // the original value.
            ast.convert(node, NodeKind::AssignDiscard);
            ast.set_first_handle(node, value_node);
         }
         NodeKind::Identifier => {
            // A simple symbol-binding assignment is converted into a Symbol node.
            let name = self.common.get_source_range_from_node(ast, name_node);
            let symbol =
               self.symbols.create(name, node, value_type, SymbolKind::Variable(variable));
            ast.convert_to_symbol(name_node, symbol);
            self.add_to_scope(name, symbol);
            // The variable type annotation is less relevant to error reporting than the fact that
            // it's a statement. This sounds counterintuitive at first, but note that we're
            // requested to annotate the Val/Var node, not the variable name node, so the calling
            // function likely expects a statement instead of an expression.
            let _ = self.annotate(ast, name_node, value_type);
         }
         _ => unreachable!(),
      }
      Ok(self.annotate(ast, node, self.builtin.t_statement))
   }
}
