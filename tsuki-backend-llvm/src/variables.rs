/// Code generation for variable declarations.
use inkwell::values::{BasicValue, BasicValueEnum};
use tsuki_frontend::ast::NodeHandle;
use tsuki_frontend::scope::SymbolId;
use tsuki_frontend::sem::Ir;

use crate::CodeGen;

pub(crate) struct Variables<'c> {
   variables: Vec<Option<BasicValueEnum<'c>>>,
}

impl<'c> Variables<'c> {
   pub(crate) fn new() -> Self {
      Self {
         variables: Vec::new(),
      }
   }

   fn insert(&mut self, symbol: SymbolId, value: BasicValueEnum<'c>) {
      if self.variables.len() <= symbol.id() {
         self.variables.resize(symbol.id() + 1, None);
      }
      self.variables[symbol.id()] = Some(value);
   }

   fn get(&self, symbol: SymbolId) -> Option<BasicValueEnum<'c>> {
      if symbol.id() >= self.variables.len() {
         None
      } else {
         self.variables[symbol.id()]
      }
   }
}

impl<'c> CodeGen<'c> {
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

   pub(crate) fn generate_variable_declaration(&self, ir: &Ir, node: NodeHandle) {
      let mut variables = self.variables.borrow_mut();
      let symbol_node = ir.ast.first_handle(node);
      let symbol = ir.ast.symbol_id(symbol_node);
      let value_node = ir.ast.second_handle(node);
      let value = self.generate_expression(ir, value_node);
      let alloca = self.builder.build_alloca(value.get_type(), ir.symbols.name(symbol));
      self.builder.build_store(alloca, value);
      variables.insert(symbol, alloca.as_basic_value_enum());
   }
}
