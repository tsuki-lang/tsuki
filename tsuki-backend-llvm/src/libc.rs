use inkwell::context::Context;
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;

use crate::codegen::CodeGen;

impl<'c> CodeGen<'c> {
   pub(crate) fn load_libc(&mut self) {
      let string_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
      let i32_type = self.context.i32_type();
      let printf_fn_type = i32_type.fn_type(&[string_type.into()], true);
      self.module.add_function("printf", printf_fn_type, None);
   }
}
