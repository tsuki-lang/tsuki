//! Conversion from tsuki types into LLVM types.

use inkwell::types::{BasicType, BasicTypeEnum, FloatType, IntType};
use inkwell::values::StructValue;
use tsuki_frontend::types::{FloatSize, IntegerSize, TypeId, TypeKind, Types};

use crate::codegen::CodeGen;

impl<'src, 'c, 'pm> CodeGen<'src, 'c, 'pm> {
   /// Generates code for a unit literal.
   pub(crate) fn generate_unit_literal(&self) -> StructValue<'c> {
      self.unit_type.const_zero()
   }

   /// Returns the integer type for the provided type, or panics if the type is not an integer type.
   fn get_integer_type(&self, types: &Types, typ: TypeId) -> IntType<'c> {
      if let TypeKind::Integer(size) = types.kind(typ) {
         match size {
            IntegerSize::U8 | IntegerSize::S8 => self.context.i8_type(),
            IntegerSize::U16 | IntegerSize::S16 => self.context.i16_type(),
            IntegerSize::U32 | IntegerSize::S32 => self.context.i32_type(),
            IntegerSize::U64 | IntegerSize::S64 => self.context.i64_type(),
         }
      } else {
         panic!("type is not an integer type")
      }
   }

   /// Returns the float type for the provided type, or panics if the type is not a float type.
   fn get_float_type(&self, types: &Types, typ: TypeId) -> FloatType<'c> {
      if let TypeKind::Float(size) = types.kind(typ) {
         match size {
            FloatSize::S32 => self.context.f32_type(),
            FloatSize::S64 => self.context.f64_type(),
         }
      } else {
         panic!("type is not a float type")
      }
   }

   pub(crate) fn get_type(&self, types: &Types, typ: TypeId) -> BasicTypeEnum<'c> {
      match types.kind(typ) {
         TypeKind::Missing => panic!("get_type called with missingtype"),
         TypeKind::Error => panic!("get_type called with errortype"),
         TypeKind::Statement => panic!("get_type called with statement type"),
         TypeKind::Declaration(id) => panic!("get_type called with declaration({:?}) type", id),
         TypeKind::Type => panic!("get_type called with type type"),
         TypeKind::Unit | TypeKind::NoReturn => self.unit_type.as_basic_type_enum(),
         TypeKind::Bool => self.context.bool_type().as_basic_type_enum(),
         TypeKind::Integer(_) => self.get_integer_type(types, typ).as_basic_type_enum(),
         TypeKind::Float(_) => self.get_float_type(types, typ).as_basic_type_enum(),
         TypeKind::Char => todo!(),
         TypeKind::Alias(alias) => self.get_type(types, *alias),
      }
   }
}
