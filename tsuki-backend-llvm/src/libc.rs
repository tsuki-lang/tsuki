use inkwell::AddressSpace;

use crate::codegen::CodeGen;

// Named constants for string values. This should simplify refactoring if they ever need to
// be changed.
pub const FUN_PRINTF: &str = "printf";
pub const GLOBAL_PRINTF_INT_FORMAT: &str = "printf_int_format";
pub const GLOBAL_PRINTF_FLOAT_FORMAT: &str = "printf_float_format";

impl<'c> CodeGen<'c> {
   fn add_const_string(&mut self, name: &str, string: &[u8]) {
      let s = self.context.const_string(string, true);
      let typ = s.get_type();
      let global = self.module.add_global(typ, Some(AddressSpace::Generic), name);
      global.set_initializer(&s);
   }

   pub(crate) fn load_libc(&mut self) {
      // int printf(char *fmt, ...);
      let string_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
      let i32_type = self.context.i32_type();
      let printf_fn_type = i32_type.fn_type(&[string_type.into()], true);
      self.module.add_function(FUN_PRINTF, printf_fn_type, None);

      // printf format strings
      self.add_const_string(GLOBAL_PRINTF_INT_FORMAT, b"%i\n");
      self.add_const_string(GLOBAL_PRINTF_FLOAT_FORMAT, b"%g\n");
   }
}
