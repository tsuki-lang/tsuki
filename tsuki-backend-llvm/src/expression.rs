//! Code generation for expressions.

use inkwell::builder::Builder;
use inkwell::types::IntType;
use inkwell::values::{
   AnyValue, AnyValueEnum, BasicValueEnum, CallSiteValue, IntValue, PointerValue, StructValue,
   VectorValue,
};
use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};

use crate::codegen::CodeGen;

impl<'c> CodeGen<'c> {
   /// Returns the integer type for the provided node kind, panics if the node kind is not
   /// an integer.
   fn get_int_type(&self, node_kind: NodeKind) -> IntType {
      match node_kind {
         NodeKind::Uint8 | NodeKind::Int8 => self.context.i8_type(),
         NodeKind::Uint16 | NodeKind::Int16 => self.context.i16_type(),
         NodeKind::Uint32 | NodeKind::Int32 => self.context.i32_type(),
         NodeKind::Uint64 | NodeKind::Int64 => self.context.i64_type(),
         _ => panic!(),
      }
   }

   /// Generates code for an integer literal.
   fn generate_int_literal(&self, ast: &Ast, node: NodeHandle) -> IntValue {
      let typ = self.get_int_type(ast.kind(node));
      typ.const_int(ast.extra(node).unwrap_uint(), false)
   }

   fn generate_unit_literal(&self, ast: &Ast, node: NodeHandle) -> StructValue {
      self.unit_type.const_zero()
   }

   /// Generates code for any expression node.
   pub(crate) fn generate_expression(
      &self,
      ast: &Ast,
      node: NodeHandle,
      builder: &Builder,
   ) -> BasicValueEnum {
      match ast.kind(node) {
         | NodeKind::Uint8
         | NodeKind::Uint16
         | NodeKind::Uint32
         | NodeKind::Uint64
         | NodeKind::Int8
         | NodeKind::Int16
         | NodeKind::Int32
         | NodeKind::Int64 => self.generate_int_literal(ast, node).into(),
         NodeKind::IntrinPrintInt32 => self.generate_intrinsic(ast, node, builder),
         _ => unreachable!(),
      }
   }

   /// Generates code for an intrinsic function call node.
   fn generate_intrinsic(&self, ast: &Ast, node: NodeHandle, builder: &Builder) -> BasicValueEnum {
      let arguments = ast.extra(node).unwrap_node_list();
      match ast.kind(node) {
         NodeKind::IntrinPrintInt32 => {
            let printf = self
               .module
               .get_function("printf")
               .expect("libc must be loaded to use __intrin_print_int32");
            let zero = self.context.i32_type().const_zero();
            let format = self.module.get_global("printf_int_format").unwrap();
            let format_ptr = unsafe {
               builder.build_in_bounds_gep(format.as_pointer_value(), &[zero, zero], "fmt")
            };
            let argument = self.generate_expression(ast, arguments[0], builder);
            builder.build_call(printf, &[format_ptr.into(), argument.into()], "");
         }
         _ => unreachable!(),
      }
      self.generate_unit_literal(ast, node).into()
   }
}
