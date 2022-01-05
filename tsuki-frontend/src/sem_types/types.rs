//! Type declarations.

use crate::ast::{Ast, NodeId, NodeKind};
use crate::common::ErrorKind;
use crate::scope::{SymbolId, SymbolKind};
use crate::types::{TypeInfo, TypeKind, TypeLogEntry, TypeLogResult};

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
         let underlying_type = self.lookup_type(ast, rhs)?;
         let alias = self.types.create_type(TypeInfo {
            name,
            kind: TypeKind::Alias(underlying_type),
         });
         let symbol = self.symbols.create(name, node, self.builtin.t_type, SymbolKind::Type(alias));
         Some(symbol)
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

      let declaration_type = self.create_declaration_type(aliased_type);
      Ok(self.annotate(ast, node, declaration_type))
   }

   /// Interprets a pragma for a type alias declaration.
   fn type_alias_pragma(
      &mut self,
      ast: &mut Ast,
      pragma: NodeId,
      #[allow(unused)] mut aliased_type: Option<SymbolId>,
   ) -> Result<Option<SymbolId>, TypeLogEntry> {
      let name_identifier = ast.first_handle(pragma);
      let name = self.common.get_source_range_from_node(ast, name_identifier);
      match name {
         "compiler_builtin_type" => {
            aliased_type = Some(self.pragma_compiler_builtin_type(ast, pragma)?);
         }
         other => return Err(self.error(ast, pragma, ErrorKind::UnknownPragma(other.into()))),
      }
      Ok(aliased_type)
   }

   /// Raises an error if a pragma does not have the provided number of arguments.
   fn pragma_expect_arguments(
      &mut self,
      ast: &Ast,
      pragma: NodeId,
      count: usize,
   ) -> Result<(), TypeLogEntry> {
      let nodes = ast.extra(pragma).as_node_list().unwrap();
      if nodes.len() != 1 {
         return Err(self.error(
            ast,
            pragma,
            ErrorKind::NArgumentsExpected(count, nodes.len()),
         ));
      }
      Ok(())
   }

   /// Handles the `compiler_builtin_type` pragma: creates a new symbol for a built-in type.
   fn pragma_compiler_builtin_type(
      &mut self,
      ast: &mut Ast,
      pragma: NodeId,
   ) -> Result<SymbolId, TypeLogEntry> {
      let nodes = ast.extra(pragma).as_node_list().unwrap();
      self.pragma_expect_arguments(ast, pragma, 1)?;
      let name_node = nodes[0];
      if ast.kind(name_node) != NodeKind::Atom {
         return Err(self.error(ast, name_node, ErrorKind::InvalidBuiltinTypeName));
      }
      let name = self.common.get_source_range_from_node(ast, name_node);
      let typ = match name {
         "noreturn" => self.builtin.t_noreturn,
         "bool" => self.builtin.t_bool,
         "uint8" => self.builtin.t_uint8,
         "uint16" => self.builtin.t_uint16,
         "uint32" => self.builtin.t_uint32,
         "uint64" => self.builtin.t_uint64,
         "int8" => self.builtin.t_int8,
         "int16" => self.builtin.t_int16,
         "int32" => self.builtin.t_int32,
         "int64" => self.builtin.t_int64,
         "float32" => self.builtin.t_float32,
         "float64" => self.builtin.t_float64,
         "size" => self.builtin.t_size,
         _ => return Err(self.error(ast, name_node, ErrorKind::InvalidBuiltinTypeName)),
      };
      let symbol = self.symbols.create(
         self.types.name(typ),
         pragma,
         self.builtin.t_type,
         SymbolKind::Type(typ),
      );
      Ok(symbol)
   }
}
