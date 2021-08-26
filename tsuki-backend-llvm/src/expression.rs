//! Code generation for expressions.

use inkwell::types::{FloatType, IntType};
use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue, StructValue};
use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};
use tsuki_frontend::sem::Ir;
use tsuki_frontend::types::{FloatSize, IntegerSize, TypeId, TypeKind, Types};

use crate::codegen::CodeGen;
use crate::libc;

impl<'c> CodeGen<'c> {
   /// Generates code for a unit literal.
   pub(crate) fn generate_unit_literal(&self, _ast: &Ast, _node: NodeHandle) -> StructValue<'c> {
      self.unit_type.const_zero()
   }

   /// Returns the integer type for the provided type, or panics if the type is not an integer type.
   fn get_int_type(&self, types: &Types, typ: TypeId) -> IntType<'c> {
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

   /// Generates code for an integer literal.
   fn generate_int_literal(&self, ir: &Ir, node: NodeHandle) -> IntValue<'c> {
      let typ = self.get_int_type(&ir.types, ir.ast.type_id(node));
      typ.const_int(ir.ast.extra(node).unwrap_uint(), false)
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

   /// Generates code for a float literal.
   fn generate_float_literal(&self, ir: &Ir, node: NodeHandle) -> FloatValue<'c> {
      let typ = self.get_float_type(&ir.types, ir.ast.type_id(node));
      typ.const_float(ir.ast.extra(node).unwrap_float())
   }

   /// Generates code for integer math.
   fn generate_int_math(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum<'c> {
      // TODO: Panic on overflow. This can be done using LLVM's arithmetic intrinsics that return
      // an aggregate {T, i1}, where the second field is a flag signifying whether overflow occured.
      let left_value = self.generate_expression(ir, ir.ast.first_handle(node));
      let right_value = self.generate_expression(ir, ir.ast.second_handle(node));
      let (left, right) = (left_value.into_int_value(), right_value.into_int_value());
      let math = match ir.ast.kind(node) {
         NodeKind::Plus => self.builder.build_int_add(left, right, "addtmp"),
         NodeKind::Minus => self.builder.build_int_sub(left, right, "subtmp"),
         NodeKind::Mul => self.builder.build_int_mul(left, right, "multmp"),
         NodeKind::Div => {
            let is_signed = ir.types.kind(ir.ast.type_id(node)).unwrap_integer().is_signed();
            if is_signed {
               self.builder.build_int_signed_div(left, right, "sdivtmp")
            } else {
               self.builder.build_int_unsigned_div(left, right, "udivtmp")
            }
         }
         _ => unreachable!(),
      };
      math.as_basic_value_enum()
   }

   fn generate_float_math(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum<'c> {
      let left_value = self.generate_expression(ir, ir.ast.first_handle(node));
      let right_value = self.generate_expression(ir, ir.ast.second_handle(node));
      let (left, right) = (
         left_value.into_float_value(),
         right_value.into_float_value(),
      );
      let math = match ir.ast.kind(node) {
         NodeKind::Plus => self.builder.build_float_add(left, right, "faddtmp"),
         NodeKind::Minus => self.builder.build_float_sub(left, right, "fsubtmp"),
         NodeKind::Mul => self.builder.build_float_mul(left, right, "fmultmp"),
         NodeKind::Div => self.builder.build_float_div(left, right, "fdivtmp"),
         _ => unreachable!(),
      };
      math.as_basic_value_enum()
   }

   /// Generates code for integer and floating-point math operations.
   fn generate_math(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum<'c> {
      let typ = ir.types.kind(ir.ast.type_id(node));
      if typ.is_integer() {
         self.generate_int_math(ir, node)
      } else if typ.is_float() {
         self.generate_float_math(ir, node)
      } else {
         unreachable!()
      }
   }

   /// Generates code for an integer type conversion (`WidenUint` or `WidenInt`).
   fn generate_int_conversion(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum<'c> {
      let inner = ir.ast.first_handle(node);
      let inner_value = self.generate_expression(ir, inner).into_int_value();
      let dest_type = self.get_int_type(&ir.types, ir.ast.type_id(node));
      match ir.ast.kind(node) {
         NodeKind::WidenUint => self.builder.build_int_z_extend(inner_value, dest_type, "uwidened"),
         NodeKind::WidenInt => self.builder.build_int_s_extend(inner_value, dest_type, "swidened"),
         _ => unreachable!(),
      }
      .as_basic_value_enum()
   }

   /// Generates code for any expression node.
   pub(crate) fn generate_expression(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum<'c> {
      match ir.ast.kind(node) {
         // Literals
         | NodeKind::Uint8
         | NodeKind::Uint16
         | NodeKind::Uint32
         | NodeKind::Uint64
         | NodeKind::Int8
         | NodeKind::Int16
         | NodeKind::Int32
         | NodeKind::Int64 => self.generate_int_literal(ir, node).into(),
         NodeKind::Float32 | NodeKind::Float64 => self.generate_float_literal(ir, node).into(),

         // Variables
         NodeKind::Variable => self.generate_variable_reference(ir, node),

         // Operators
         NodeKind::Plus | NodeKind::Minus | NodeKind::Mul | NodeKind::Div => {
            self.generate_math(ir, node)
         }

         // Control flow
         NodeKind::DoExpression => self.generate_do(ir, node).unwrap(),

         // Intrinsics
         NodeKind::WidenUint | NodeKind::WidenInt => self.generate_int_conversion(ir, node),
         NodeKind::PrintInt32 | NodeKind::PrintFloat32 => {
            self.generate_call_like_intrinsic(ir, node)
         }
         other => unreachable!("invalid expression node: {:?}", other),
      }
   }

   /// Generates code for a function call-like intrinsic.
   fn generate_call_like_intrinsic(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum<'c> {
      let arguments = ir.ast.extra(node).unwrap_node_list();
      match ir.ast.kind(node) {
         kind @ (NodeKind::PrintInt32 | NodeKind::PrintFloat32) => {
            // This is not the, um, cleanest... piece of code here, but it'll get replaced
            // anyway once c_import is implemented.
            let printf = self.module.get_function(libc::FUN_PRINTF).expect("libc must be loaded");
            let zero = self.context.i32_type().const_zero();
            let global_name = if kind == NodeKind::PrintInt32 {
               libc::GLOBAL_PRINTF_INT_FORMAT
            } else {
               libc::GLOBAL_PRINTF_FLOAT_FORMAT
            };
            let format = self.module.get_global(global_name).unwrap();
            let format_ptr = unsafe {
               self.builder.build_in_bounds_gep(format.as_pointer_value(), &[zero, zero], "fmt")
            };
            let mut argument = self.generate_expression(ir, arguments[0]);
            // We need to convert `float` to `double` for passing to printf.
            if kind == NodeKind::PrintFloat32 {
               let f64_type = self.context.f64_type();
               argument = self
                  .builder
                  .build_float_cast(argument.into_float_value(), f64_type, "printf_dbl")
                  .as_basic_value_enum();
            }
            self.builder.build_call(printf, &[format_ptr.into(), argument.into()], "_");
         }
         _ => unreachable!(),
      }
      self.generate_unit_literal(&ir.ast, node).into()
   }
}
