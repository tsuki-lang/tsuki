//! Type declarations.

use crate::ast::{Ast, NodeId};
use crate::common::ErrorKind;
use crate::scope::SymbolId;
use crate::types::{TypeLogEntry, TypeLogResult};

use super::SemTypes;

impl<'s> SemTypes<'s> {
   /// Annotates the AST for a type alias declaration.
   pub(super) fn annotate_type_alias(&mut self, ast: &mut Ast, node: NodeId) -> TypeLogResult {
      let (lhs, rhs) = (ast.first_handle(node), ast.second_handle(node));
      let (constrained_type, pragmas) = Self::split_pragmas(ast, lhs);

      // TODO: generic parameters
      let type_name = ast.first_handle(constrained_type);
      let name_identifier = ast.first_handle(type_name);
      let name = self.common.get_source_range_from_node(ast, name_identifier);

      // Interpret the right-hand side.
      let mut aliased_type = if rhs != NodeId::null() {
         // TODO: Check constraints.
         Some(self.lookup_type(ast, rhs)?.0)
      } else {
         None
      };

      // Interpret pragmas.
      if let Some(pragmas) = pragmas {
         for i in 0..ast.extra(pragmas).as_node_list().unwrap().len() {
            let pragma = ast.extra(pragmas).as_node_list().unwrap()[i];
            aliased_type = self.type_alias_pragma(ast, pragma, aliased_type)?;
         }
      }

      // Unwrap the resulting type.
      let aliased_type =
         aliased_type.ok_or_else(|| self.error(ast, node, ErrorKind::EmptyTypeAlias))?;
      // Add the alias to scope.
      self.add_to_scope(name, aliased_type);

      todo!()
   }

   /// Interprets a pragma for a type alias declaration.
   fn type_alias_pragma(
      &mut self,
      ast: &mut Ast,
      pragma: NodeId,
      aliased_type: Option<SymbolId>,
   ) -> Result<Option<SymbolId>, TypeLogEntry> {
      let name_identifier = ast.first_handle(pragma);
      let name = self.common.get_source_range_from_node(ast, name_identifier);

      match name {
         "compiler_builtin_type" => (), // TODO
         other => return Err(self.error(ast, pragma, ErrorKind::UnknownPragma(other.into()))),
      }

      Ok(aliased_type)
   }
}
