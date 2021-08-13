use inkwell::AddressSpace;

use crate::codegen::CodeGen;

impl<'c> CodeGen<'c> {
   pub(crate) fn load_libc(&mut self) {
      let string_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
      let i32_type = self.context.i32_type();
      let printf_fn_type = i32_type.fn_type(&[string_type.into()], true);
      self.module.add_function("printf", printf_fn_type, None);

      let int_format = self.context.const_string(b"%i\n", true);
      let int_format_type = int_format.get_type();
      let printf_int_format = self.module.add_global(
         int_format_type,
         Some(AddressSpace::Generic),
         "printf_int_format",
      );
      printf_int_format.set_initializer(&int_format);
   }
}
