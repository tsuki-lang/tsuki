//! Annotating functions and introducing them into scope.

use smallvec::SmallVec;

use crate::ast::{Ast, NodeHandle, NodeKind};
use crate::common::ErrorKind;
use crate::scope::{Function, FunctionKind, SymbolKind, Variable, VariableKind};
use crate::types::{TypeKind, TypeLogEntry, TypeLogResult};

use super::{NodeContext, SemTypes};

impl<'s> SemTypes<'s> {
   fn mangle_name(&self, function_name: &str) -> String {
      format!("{}.{}", self.common.file.module_name(), function_name)
   }

   pub(super) fn annotate_function_declaration(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
   ) -> TypeLogResult {
      // Mangle the function name.
      let name_node = ast.first_handle(node);
      let name = self.common.get_source_range_from_node(ast, name_node);
      let mangled_name = self.mangle_name(&name);

      // Prepare all the nodes.
      let parameters_node = ast.second_handle(node);
      let formal_parameters_node = ast.second_handle(parameters_node);

      // Create a scope for the generic and formal parameters.
      let scope = self.scope_stack.push(self.scopes.create_scope());
      ast.set_scope(node, Some(scope));

      // Slurp all the parameters up into a vector.
      let mut parameters = SmallVec::new();
      for &named_parameters in ast.extra(formal_parameters_node).unwrap_node_list() {
         let type_node = ast.first_handle(named_parameters);
         let typ = self.lookup_type(ast, type_node)?;
         for &name_node in ast.extra(named_parameters).unwrap_node_list() {
            let name = self.common.get_source_range_from_node(ast, name_node);
            parameters.push((name.to_owned(), typ));
            // Also, make each parameter have its own identifier in the function body.
            // Function parameters are just variables, introduced by some external scope.
            let symbol = self.symbols.create(
               name,
               name_node,
               typ,
               SymbolKind::Variable(Variable {
                  kind: VariableKind::Val,
               }),
            );
            self.add_to_scope(name, symbol);
         }
      }

      // Look up what the return type should be.
      let return_type_node = ast.first_handle(formal_parameters_node);
      let return_type = if ast.kind(return_type_node) != NodeKind::Empty {
         self.lookup_type(ast, return_type_node)?
      } else {
         // In case no return type is provided, default to the unit type `()`.
         self.builtin.t_unit
      };

      // Sem'check the function's body.
      let returns_unit = self.types.kind(return_type).is_unit();
      let body_log = self.annotate_statement_list(
         ast,
         node,
         if returns_unit {
            NodeContext::Statement
         } else {
            NodeContext::Expression
         },
      );
      // Check that the body's return type is correct.
      let body_type = self.log.typ(body_log);
      if !returns_unit && body_type != return_type {
         return Ok(self.type_mismatch(ast, node, return_type, body_type));
      }

      // After all is done, pop the function's scope off.
      self.scope_stack.pop();

      let symbol_kind = SymbolKind::Function(Function {
         mangled_name,
         parameters,
         return_type,
         kind: FunctionKind::Local,
      });
      // TODO: Function/closure types. Right now we treat function symbols as having the
      // 'statement' type, which isn't exactly correct.
      let symbol = self.symbols.create(name, node, self.builtin.t_statement, symbol_kind);
      self.add_to_scope(name, symbol);

      Ok(self.annotate(ast, node, self.builtin.t_statement))
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
