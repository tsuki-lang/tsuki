/// Code generation for variable declarations.
use inkwell::values::{BasicValue, BasicValueEnum};
use tsuki_frontend::ast::NodeHandle;
use tsuki_frontend::scope::SymbolId;
use tsuki_frontend::sem::Ir;

use crate::CodeGen;

/// Storage for variable values. This maps symbol IDs to `Value`s from LLVM.
pub(crate) struct Variables<'c> {
   variables: Vec<Option<BasicValueEnum<'c>>>,
}

impl<'c> Variables<'c> {
   /// Creates and initializes a new variable value storage.
   pub(crate) fn new() -> Self {
      Self {
         variables: Vec::new(),
      }
   }

   /// Inserts a new value under the given symbol ID.
   fn insert(&mut self, symbol: SymbolId, value: BasicValueEnum<'c>) {
      if self.variables.len() <= symbol.id() {
         self.variables.resize(symbol.id() + 1, None);
      }
      self.variables[symbol.id()] = Some(value);
   }

   /// Retrieves the value under the given symbol ID.
   fn get(&self, symbol: SymbolId) -> Option<BasicValueEnum<'c>> {
      if symbol.id() >= self.variables.len() {
         None
      } else {
         self.variables[symbol.id()]
      }
   }
}

impl<'c, 'pm> CodeGen<'c, 'pm> {
   /// Generates code for a variable reference.
   pub(crate) fn generate_variable_reference(
      &self,
      ir: &Ir,
      node: NodeHandle,
   ) -> BasicValueEnum<'c> {
      let variables = self.variables.borrow();

      let symbol_node = ir.ast.first_handle(node);
      let symbol = ir.ast.symbol_id(symbol_node);

      let alloca = variables.get(symbol).expect("reference to undeclared variable in IR");
      self.builder.build_load(alloca.into_pointer_value(), ir.symbols.name(symbol))
   }

   /// Generates code for variable declarations.
   pub(crate) fn generate_variable_declaration(&self, ir: &Ir, node: NodeHandle) {
      let mut variables = self.variables.borrow_mut();

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

      variables.insert(symbol, alloca.as_basic_value_enum());
   }
}
