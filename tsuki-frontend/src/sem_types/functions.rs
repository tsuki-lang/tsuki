//! Annotating functions and introducing them into scope.

use smallvec::SmallVec;

use crate::ast::{Ast, NodeHandle, NodeKind};
use crate::common::ErrorKind;
use crate::functions::{FunctionKind, Intrinsic, Parameters};
use crate::scope::{Mutability, SymbolKind, Variable};
use crate::types::{TypeId, TypeLogResult};

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
      // Check if the name is sem'd or not. If so, we are coming from a deferred sem'check,
      // so simply check the body and return.
      let name_node = ast.first_handle(node);
      if ast.kind(name_node) == NodeKind::Symbol {
         return self.annotate_function_body(ast, node);
      }

      // Get the function name.
      let name = self.common.get_source_range_from_node(ast, name_node);
      let mangled_name = self.mangle_name(&name);

      // Prepare all the nodes.
      let parameters_node = ast.second_handle(node);
      let formal_parameters_node = ast.second_handle(parameters_node);

      // Create a scope for the generic and formal parameters.
      // We save the declaration scope for later, as that's where we'll be adding the function
      // symbol itself.
      let declaration_scope = self.scope_stack.top();
      let scope = self.scope_stack.push(self.scopes.create_scope());
      ast.set_scope(node, Some(scope));

      // Slurp all the parameters up into a vector.
      let mut parameters = SmallVec::new();
      for i in 0..ast.extra(formal_parameters_node).unwrap_node_list().len() {
         let named_parameters = ast.extra(formal_parameters_node).unwrap_node_list()[i];
         let type_node = ast.first_handle(named_parameters);
         let typ = self.lookup_type(ast, type_node)?;
         ast.walk_node_list_mut(named_parameters, |ast, _, name_node| {
            // Make each parameter have its own identifier in the function body.
            // Semantically, function parameters are just variables, introduced by some
            // external scope.
            let name = self.common.get_source_range_from_node(ast, name_node);
            let symbol = self.symbols.create(
               name,
               name_node,
               typ,
               SymbolKind::Variable(Variable {
                  mutability: Mutability::Val,
               }),
            );
            parameters.push(symbol);
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

      // Register the function in the registry and add it to scope.
      // Registering the function _here_ allows for the referring to the function inside its
      // body, enabling recursion.
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
      self.scopes.insert(declaration_scope, name, symbol);
      ast.convert_to_symbol(name_node, symbol);

      // After all is done, pop the function's scope off.
      self.scope_stack.pop();

      if self.is_in_module_scope() {
         // If we're at the top-level scope, defer sem until all items in scope have already been
         // declared.
         self.defer(node, NodeContext::Statement);
         Ok(self.annotate(ast, node, TypeId::null()))
      } else {
         // If we're not top-level, check the function's body.
         self.annotate_function_body(ast, node)
      }
   }

   /// Annotates a function's body.
   pub(super) fn annotate_function_body(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
   ) -> TypeLogResult {
      let name_node = ast.first_handle(node);
      let symbol_id = ast.symbol_id(name_node);
      let function_id = self.symbols.kind(symbol_id).unwrap_function();

      let return_type = self.functions.parameters(function_id).return_type;

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
      let (symbol_id, function_id) = self.lookup_function(ast, callee_node)?;
      // Convert the callee to a symbol.
      ast.convert_to_symbol(callee_node, symbol_id);

      // Check if we have the right amount of arguments.
      let given_parameter_count = ast.extra(node).unwrap_node_list().len();
      let declared_parameter_count = self.functions.parameters(function_id).formal.len();
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
         let parameters = self.functions.parameters(function_id);
         let expected_type = self.symbols.type_id(parameters.formal[index]);

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

      if let &FunctionKind::Intrinsic(intrinsic) = self.functions.kind(function_id) {
         // Intrinsic calls have some transformation magic going on.
         self.annotate_intrinsic_call(ast, node, intrinsic);
      } else {
         // For other calls, we use the CallFunction node, which is a normalized version of `Call`
         // that takes the form of `function_name(params)`. Even for instance functions.
         ast.convert_preserve(node, NodeKind::CallFunction);
      }

      let return_type = self.functions.parameters(function_id).return_type;
      Ok(self.annotate(ast, node, return_type))
   }

   /// Annotates an intrinsic function call.
   fn annotate_intrinsic_call(&mut self, ast: &mut Ast, node: NodeHandle, intrinsic: Intrinsic) {
      ast.convert_preserve(node, NodeKind::from(intrinsic));
   }
}
