/// Code generation for variable declarations.
use inkwell::values::{BasicValueEnum, PointerValue};
use tsuki_frontend::ast::{NodeHandle, NodeKind};
use tsuki_frontend::scope::SymbolId;
use tsuki_frontend::sem::Ir;

use crate::CodeGen;

/// Storage for variable values. This maps symbol IDs to `Value`s from LLVM.
pub(crate) struct Variables<'c> {
   variables: Vec<Option<PointerValue<'c>>>,
}

impl<'c> Variables<'c> {
   /// Creates and initializes a new variable value storage.
   pub(crate) fn new() -> Self {
      Self {
         variables: Vec::new(),
      }
   }

   /// Inserts a new value under the given symbol ID.
   pub(crate) fn insert(&mut self, symbol: SymbolId, value: PointerValue<'c>) {
      if self.variables.len() <= symbol.id() {
         self.variables.resize(symbol.id() + 1, None);
      }
      self.variables[symbol.id()] = Some(value);
   }

   /// Retrieves the value under the given symbol ID.
   pub(crate) fn get(&self, symbol: SymbolId) -> Option<PointerValue<'c>> {
      if symbol.id() >= self.variables.len() {
         None
      } else {
         self.variables[symbol.id()]
      }
   }
}

impl<'src, 'c, 'pm> CodeGen<'src, 'c, 'pm> {
   /// Generates code for a variable reference.
   pub(crate) fn generate_variable_reference(
      &self,
      ir: &Ir,
      node: NodeHandle,
   ) -> BasicValueEnum<'c> {
      let symbol_node = ir.ast.first_handle(node);
      let symbol = ir.ast.symbol_id(symbol_node);

      let alloca =
         self.variables.borrow().get(symbol).expect("reference to undeclared variable in IR");
      self.builder.build_load(alloca, ir.symbols.name(symbol))
   }

   /// Generates code for variable declarations.
   pub(crate) fn generate_variable_declaration(&self, ir: &Ir, node: NodeHandle) {
      let symbol_node = ir.ast.first_handle(node);
      let symbol = ir.ast.symbol_id(symbol_node);

      let value_node = ir.ast.second_handle(node);
      let value = self.generate_expression(ir, value_node);

      // A variable declaration always performs an alloca for simplicity's sake.
      // These allocas, loads, and stores, are optimized by mem2reg later.
      // To make the allocas optimizable by mem2reg, they need to be placed in the entry block of
      // the function, and the most obvious place to put the allocas is the top, because then they
      // are available to every other instruction, including loads and stores.
      let builder = self.function.create_entry_block_builder(self.context);
      let alloca = builder.build_alloca(value.get_type(), ir.symbols.name(symbol));
      self.builder.build_store(alloca, value);

      self.variables.borrow_mut().insert(symbol, alloca);
   }

   /// Generates code for `AssignDiscard`.
   pub(crate) fn generate_discarding_assignment(&self, ir: &Ir, node: NodeHandle) {
      let value_node = ir.ast.first_handle(node);
      let _ = self.generate_expression(ir, value_node);
   }

   /// Generates code for assignments to variables.
   pub(crate) fn generate_assignment(
      &self,
      ir: &Ir,
      node: NodeHandle,
   ) -> Option<BasicValueEnum<'c>> {
      // When the assignment is not an expression, we do a little optimization where we don't
      // generate the load
      let result_type = ir.ast.type_id(node);
      let is_expression = !ir.types.kind(result_type).is_statement();

      let target_node = ir.ast.first_handle(node);
      let target = match ir.ast.kind(target_node) {
         NodeKind::Variable => {
            let symbol_node = ir.ast.first_handle(target_node);
            let symbol = ir.ast.symbol_id(symbol_node);
            self.variables.borrow().get(symbol).expect("reference to undeclared variable in IR")
         }
         _ => unreachable!(),
      };
      let value_node = ir.ast.second_handle(node);
      let value = self.generate_expression(ir, value_node);

      let result = if is_expression {
         let old_value = self.builder.build_load(target, "old");
         Some(old_value)
      } else {
         None
      };
      // Note that this does not care about mutability; that part is handled by SemTypes in the
      // frontend.
      self.builder.build_store(target, value);
      result
   }
}
