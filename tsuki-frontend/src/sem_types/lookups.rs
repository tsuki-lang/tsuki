//! Identifier lookups, generic instantiations, mutability queries.

use crate::ast::{Ast, NodeId, NodeKind};
use crate::common::ErrorKind;
use crate::functions::FunctionId;
use crate::scope::{SymbolId, SymbolKind};
use crate::types::{TypeId, TypeLogEntry};

use super::{NodeContext, SemTypes};

impl<'s> SemTypes<'s> {
   // The difference between `find` and `lookup` is simple: `find` returns an Option,
   // `lookup` returns a Result.

   /// Performs a basic identifier search, looking for a symbol whose name matches the identifier
   /// stored in the node in the current scope.
   fn find_identifier(&self, ast: &Ast, node: NodeId) -> Option<SymbolId> {
      assert_eq!(ast.kind(node), NodeKind::Identifier);
      let name = self.common.get_source_range_from_node(ast, node);
      self.scope_stack.lookup(&self.scopes, name)
   }

   /// Performs an error-reporting identifier lookup.
   fn lookup_identifier(&mut self, ast: &Ast, node: NodeId) -> Result<SymbolId, TypeLogEntry> {
      self.find_identifier(ast, node).ok_or_else(|| {
         let name = self.common.get_source_range_from_node(ast, node);
         self.error(ast, node, ErrorKind::UndeclaredSymbol(name.into()))
      })
   }

   /// Finds the variable symbol referred to by the given identifier node.
   pub(super) fn lookup_variable(
      &mut self,
      ast: &Ast,
      node: NodeId,
   ) -> Result<SymbolId, TypeLogEntry> {
      let symbol = self.lookup_identifier(ast, node)?;
      if let SymbolKind::Variable(..) = self.symbols.kind(symbol) {
         Ok(symbol)
      } else {
         // TODO: Make this error not suck.
         Err(self.error(ast, node, ErrorKind::InvalidLocation))
      }
   }

   /// Finds the function referred to by the given identifier node.
   pub(super) fn lookup_function(
      &mut self,
      ast: &Ast,
      node: NodeId,
   ) -> Result<(SymbolId, FunctionId), TypeLogEntry> {
      let symbol = self.lookup_identifier(ast, node)?;
      if let &SymbolKind::Function(id) = self.symbols.kind(symbol) {
         Ok((symbol, id))
      } else {
         Err(self.error(ast, node, ErrorKind::ExpressionCannotBeCalled))
      }
   }

   /// Finds the type symbol referred to by the given node.
   ///
   /// The node can be any valid type as parsed by the parser. If the type is a generic type,
   /// then instantiations will be performed.
   pub(super) fn lookup_type(&mut self, ast: &Ast, node: NodeId) -> Result<TypeId, TypeLogEntry> {
      match ast.kind(node) {
         NodeKind::Identifier => {
            let symbol = self.lookup_identifier(ast, node)?;
            if let SymbolKind::Type(id) = self.symbols.kind(symbol) {
               Ok(*id)
            } else {
               let name = self.symbols.name(symbol).to_owned();
               Err(self.error(ast, node, ErrorKind::SymbolIsNotAType(name)))
            }
         }
         _ => {
            unreachable!("invalid node kind for type")
         }
      }
   }

   /// Adds a symbol to the current scope.
   pub(super) fn add_to_scope(&mut self, name: &str, symbol: SymbolId) {
      let scope = self.scope_stack.top();
      self.scopes.insert(scope, name, symbol);
   }

   /// Annotates a `pub` declaration.
   pub(super) fn annotate_pub(&mut self, ast: &mut Ast, node: NodeId) -> TypeLogEntry {
      let inner = ast.first_handle(node);
      let inner = self.annotate_node(ast, inner, NodeContext::Statement);
      let typ = self.log.type_id(inner);
      let symbol_id = self.types.kind(typ).as_declaration().unwrap();
      self.scopes.set_public(symbol_id);
      ast.unwrap(node);
      inner
   }
}
