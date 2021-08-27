//! Code generation for functions.

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::FunctionType;
use inkwell::values::FunctionValue;

pub struct Function<'c> {
   pub value: FunctionValue<'c>,
   pub entry_block: BasicBlock<'c>,
}

impl<'c> Function<'c> {
   //! Creates a new function from the given name and type.
   pub fn new(
      context: &'c Context,
      module: &Module<'c>,
      name: &str,
      typ: FunctionType<'c>,
   ) -> Self {
      let value = module.add_function(name, typ, None);
      let entry_block = context.append_basic_block(value, "entry");
      Self { value, entry_block }
   }

   /// Positions the given builder at the start of the function.
   pub fn position_at_entry_block(&self, builder: Builder) {
      if let Some(instruction) = self.entry_block.get_first_instruction() {
         builder.position_before(&instruction);
      } else {
         builder.position_at_end(self.entry_block);
      }
   }
}
