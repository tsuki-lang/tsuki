//! Code generation for expressions.

use inkwell::types::IntType;
use inkwell::values::{BasicValue, BasicValueEnum, IntValue, StructValue};
use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};
use tsuki_frontend::types::{IntegerSize, TypeId, TypeKind, Types};
use tsuki_frontend::Ir;

use crate::codegen::CodeGen;

impl<'c> CodeGen<'c> {
   /// Returns the integer type for the provided type, or panics if the type is not an integer type.
   fn get_int_type(&self, types: &Types, typ: TypeId) -> IntType {
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
   fn generate_int_literal(&self, ir: &Ir, node: NodeHandle) -> IntValue {
      let typ = self.get_int_type(&ir.types, ir.ast.type_id(node));
      typ.const_int(ir.ast.extra(node).unwrap_uint(), false)
   }

   /// Generates code for a unit literal.
   fn generate_unit_literal(&self, _ast: &Ast, _node: NodeHandle) -> StructValue {
      self.unit_type.const_zero()
   }

   /// Generates code for integer math.
   fn generate_int_math(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum {
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

   /// Generates code for integer and floating-point math operations.
   fn generate_math(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum {
      let typ = ir.types.kind(ir.ast.type_id(node));
      if typ.is_integer() {
         self.generate_int_math(ir, node)
      } else if typ.is_float() {
         todo!()
      } else {
         unreachable!()
      }
   }

   /// Generates code for an integer type conversion (`WidenUint` or `WidenInt`).
   fn generate_int_conversion(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum {
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
   pub(crate) fn generate_expression(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum {
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

         // Operators
         NodeKind::Plus | NodeKind::Minus | NodeKind::Mul | NodeKind::Div => {
            self.generate_math(ir, node)
         }

         // Intrinsics
         NodeKind::WidenUint | NodeKind::WidenInt => self.generate_int_conversion(ir, node),
         NodeKind::PrintInt32 => self.generate_call_like_intrinsic(ir, node),
         other => unreachable!("invalid expression node: {:?}", other),
      }
   }

   /// Generates code for a function call-like intrinsic.
   fn generate_call_like_intrinsic(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum {
      let arguments = ir.ast.extra(node).unwrap_node_list();
      match ir.ast.kind(node) {
         NodeKind::PrintInt32 => {
            let printf = self.module.get_function("printf").expect("libc must be loaded");
            let zero = self.context.i32_type().const_zero();
            let format = self.module.get_global("printf_int_format").unwrap();
            let format_ptr = unsafe {
               self.builder.build_in_bounds_gep(format.as_pointer_value(), &[zero, zero], "fmt")
            };
            let argument = self.generate_expression(ir, arguments[0]);
            self.builder.build_call(printf, &[format_ptr.into(), argument.into()], "");
         }
         _ => unreachable!(),
      }
      self.generate_unit_literal(&ir.ast, node).into()
   }
}
