//! Annotating functions and introducing them into scope.

use smallvec::SmallVec;

use crate::ast::{Ast, NodeHandle, NodeKind};
use crate::common::ErrorKind;
use crate::functions::{FunctionKind, Intrinsic, Parameters};
use crate::scope::{SymbolKind, Variable, VariableKind};
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
      for i in 0..ast.extra(formal_parameters_node).unwrap_node_list().len() {
         let named_parameters = ast.extra(formal_parameters_node).unwrap_node_list()[i];
         let type_node = ast.first_handle(named_parameters);
         let typ = self.lookup_type(ast, type_node)?;
         ast.walk_node_list_mut(named_parameters, |ast, _, name_node| {
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
            ast.convert_to_symbol(name_node, symbol);
            self.add_to_scope(name, symbol);
         });
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

      // Register the function in the registry and add it to scope.
      let function_id = self.functions.create(
         name.to_owned(),
         mangled_name,
         Parameters {
            formal: parameters,
            return_type,
         },
         FunctionKind::Local,
      );
      let symbol_kind = SymbolKind::Function(function_id);
      // TODO: Function/closure types. Right now we treat function symbols as having the
      // 'statement' type, which isn't exactly correct.
      let symbol = self.symbols.create(name, node, self.builtin.t_statement, symbol_kind);
      self.add_to_scope(name, symbol);

      Ok(self.annotate(ast, node, self.builtin.t_statement))
   }

   /// Annotates a function call.
   pub(super) fn annotate_call(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogResult {
      // Extract what is being called.
      let callee_node = ast.first_handle(node);
      // TODO: Method calls.
      if ast.kind(callee_node) != NodeKind::Identifier {
         return Ok(self.error(ast, callee_node, ErrorKind::ExpressionCannotBeCalled));
      }
      let function = self.lookup_function(ast, callee_node)?;

      // Check if we have the right amount of arguments.
      let given_parameter_count = ast.extra(node).unwrap_node_list().len();
      let declared_parameter_count = self.functions.parameters(function).formal.len();
      if given_parameter_count != declared_parameter_count {
         return Ok(self.error(
            ast,
            node,
            ErrorKind::NArgumentsExpected(declared_parameter_count, given_parameter_count),
         ));
      }
      // Check if all the arguments are of the correct type.
      // We don't immediately return after we error, so as to collect as many type mismatch
      // messages as we can.
      let mut last_error = None;
      ast.walk_node_list_mut(node, |ast, index, argument| {
         let parameters = self.functions.parameters(function);
         let expected_type = parameters.formal[index].1;

         let argument_log = self.annotate_node(ast, argument, NodeContext::Expression);
         let provided_type = self.log.typ(argument_log);

         // Perform implicit conversions on arguments.
         let argument_log = self
            .perform_implicit_conversion(ast, node, provided_type, expected_type)
            .unwrap_or(argument_log);
         // If there's a mismatch after the conversion, error.
         let provided_type = self.log.typ(argument_log);
         if provided_type != expected_type {
            last_error = Some(self.type_mismatch(ast, argument, expected_type, provided_type));
         }
      });
      if let Some(error) = last_error {
         return Ok(error);
      }

      if let &FunctionKind::Intrinsic(intrinsic) = self.functions.kind(function) {
         // Intrinsic calls have some transformation magic going on.
         self.annotate_intrinsic_call(ast, node, intrinsic);
      }

      let return_type = self.functions.parameters(function).return_type;
      Ok(self.annotate(ast, node, return_type))
   }

   /// Annotates an intrinsic function call.
   fn annotate_intrinsic_call(&mut self, ast: &mut Ast, node: NodeHandle, intrinsic: Intrinsic) {
      ast.convert_preserve(node, NodeKind::from(intrinsic));
   }
}
