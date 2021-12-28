//! Code generation for expressions.

use inkwell::values::{BasicValue, BasicValueEnum, FloatValue, IntValue};
use inkwell::IntPredicate;
use tsuki_frontend::ast::{NodeId, NodeKind};
use tsuki_frontend::sem::Ir;

use crate::codegen::CodeGen;
use crate::libc;

impl<'src, 'c, 'pm> CodeGen<'src, 'c, 'pm> {
   /// Generates code for a Bool literal.
   fn generate_boolean_literal(&self, ir: &Ir, node: NodeId) -> IntValue<'c> {
      let typ = self.context.bool_type();
      let literal = (ir.ast.kind(node) == NodeKind::True) as u64;
      typ.const_int(literal, false)
   }

   /// Generates code for an integer literal.
   fn generate_integer_literal(&self, ir: &Ir, node: NodeId) -> IntValue<'c> {
      let typ = self.get_type(&ir.types, ir.ast.type_id(node)).into_int_type();
      typ.const_int(ir.ast.extra(node).unwrap_uint(), false)
   }

   /// Generates code for a float literal.
   fn generate_float_literal(&self, ir: &Ir, node: NodeId) -> FloatValue<'c> {
      let typ = self.get_type(&ir.types, ir.ast.type_id(node)).into_float_type();
      typ.const_float(ir.ast.extra(node).unwrap_float())
   }

   /// Generates code for boolean negation.
   fn generate_boolean_negation(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      let right = self.generate_expression(ir, ir.ast.first_handle(node));
      self.builder.build_not(right.into_int_value(), "nottmp").as_basic_value_enum()
   }

   /// Generates code for integer or float negation.
   fn generate_number_negation(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      let right = self.generate_expression(ir, ir.ast.first_handle(node));
      let typ = ir.ast.type_id(node);
      let kind = ir.types.kind(typ);
      if kind.is_integer() {
         let typ = self.get_type(&ir.types, typ).into_int_type();
         let zero = typ.const_zero();
         self.builder.build_int_sub(zero, right.into_int_value(), "negtmp").into()
      } else if kind.is_float() {
         self.builder.build_float_neg(right.into_float_value(), "fnegtmp").into()
      } else {
         unreachable!()
      }
   }

   /// Generates the LHS and RHS of a binary operator.
   fn generate_binary_operation(
      &mut self,
      ir: &Ir,
      node: NodeId,
   ) -> (BasicValueEnum<'c>, BasicValueEnum<'c>) {
      (
         self.generate_expression(ir, ir.ast.first_handle(node)),
         self.generate_expression(ir, ir.ast.second_handle(node)),
      )
   }

   /// Generates code for integer math.
   fn generate_integer_math(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      // TODO: Panic on overflow. This can be done using LLVM's arithmetic intrinsics that return
      // an aggregate {T, i1}, where the second field is a flag signifying whether overflow occured.
      let (left_value, right_value) = self.generate_binary_operation(ir, node);
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

   fn generate_float_math(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      let (left_value, right_value) = self.generate_binary_operation(ir, node);
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
   fn generate_math(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      let typ = ir.types.kind(ir.ast.type_id(node));
      if typ.is_integer() {
         self.generate_integer_math(ir, node)
      } else if typ.is_float() {
         self.generate_float_math(ir, node)
      } else {
         unreachable!()
      }
   }

   /// Generates code for an integer type conversion (`WidenUint` or `WidenInt`).
   fn generate_integer_conversion(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      let inner = ir.ast.first_handle(node);
      let inner_value = self.generate_expression(ir, inner).into_int_value();
      let dest_type = self.get_type(&ir.types, ir.ast.type_id(node)).into_int_type();
      match ir.ast.kind(node) {
         NodeKind::WidenUint => self.builder.build_int_z_extend(inner_value, dest_type, "uwidened"),
         NodeKind::WidenInt => self.builder.build_int_s_extend(inner_value, dest_type, "swidened"),
         _ => unreachable!(),
      }
      .as_basic_value_enum()
   }

   /// Generates code for a boolean comparison.
   fn generate_boolean_comparison(&mut self, ir: &Ir, node: NodeId) -> IntValue<'c> {
      let (left_value, right_value) = self.generate_binary_operation(ir, node);
      let (left, right) = (left_value.into_int_value(), right_value.into_int_value());
      let predicate = match ir.ast.kind(node) {
         NodeKind::Equal => IntPredicate::EQ,
         NodeKind::NotEqual => IntPredicate::NE,
         _ => unreachable!(),
      };
      self.builder.build_int_compare(predicate, left, right, "boolcmp")
   }

   /// Generates code for an integer comparison.
   fn generate_integer_comparison(&mut self, ir: &Ir, node: NodeId) -> IntValue<'c> {
      let (left_value, right_value) = self.generate_binary_operation(ir, node);
      let (left, right) = (left_value.into_int_value(), right_value.into_int_value());
      let left_type = ir.ast.type_id(ir.ast.first_handle(node));
      let is_signed = ir.types.kind(left_type).unwrap_integer().is_signed();
      let predicate = match ir.ast.kind(node) {
         NodeKind::Equal => IntPredicate::EQ,
         NodeKind::NotEqual => IntPredicate::NE,
         NodeKind::Less if is_signed => IntPredicate::SLT,
         NodeKind::LessEqual if is_signed => IntPredicate::SLE,
         NodeKind::Greater if is_signed => IntPredicate::SGT,
         NodeKind::GreaterEqual if is_signed => IntPredicate::SGE,
         NodeKind::Less if !is_signed => IntPredicate::ULT,
         NodeKind::LessEqual if !is_signed => IntPredicate::ULE,
         NodeKind::Greater if !is_signed => IntPredicate::UGT,
         NodeKind::GreaterEqual if !is_signed => IntPredicate::UGE,
         _ => unreachable!(),
      };
      self.builder.build_int_compare(predicate, left, right, "intcmp")
   }

   /// Generates code for integer comparisons.
   fn generate_comparison(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      let left_node = ir.ast.first_handle(node);
      let typ = ir.types.kind(ir.ast.type_id(left_node));
      if typ.is_integer() {
         self.generate_integer_comparison(ir, node)
      } else if typ.is_float() {
         todo!()
      } else if typ.is_bool() {
         self.generate_boolean_comparison(ir, node)
      } else {
         todo!()
      }
      .as_basic_value_enum()
   }

   /// Generates code for any expression node.
   pub(crate) fn generate_expression(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      match ir.ast.kind(node) {
         // Literals
         NodeKind::True | NodeKind::False => self.generate_boolean_literal(ir, node).into(),
         | NodeKind::Uint8
         | NodeKind::Uint16
         | NodeKind::Uint32
         | NodeKind::Uint64
         | NodeKind::Int8
         | NodeKind::Int16
         | NodeKind::Int32
         | NodeKind::Int64 => self.generate_integer_literal(ir, node).into(),
         NodeKind::Float32 | NodeKind::Float64 => self.generate_float_literal(ir, node).into(),

         // Variables
         NodeKind::Variable => self.generate_variable_reference(ir, node),

         // Operators
         NodeKind::Not => self.generate_boolean_negation(ir, node),
         NodeKind::Neg => self.generate_number_negation(ir, node),
         NodeKind::Plus | NodeKind::Minus | NodeKind::Mul | NodeKind::Div => {
            self.generate_math(ir, node)
         }
         | NodeKind::Equal
         | NodeKind::NotEqual
         | NodeKind::Less
         | NodeKind::LessEqual
         | NodeKind::Greater
         | NodeKind::GreaterEqual => self.generate_comparison(ir, node),
         NodeKind::Assign => self.generate_assignment(ir, node).unwrap(),

         // Control flow
         NodeKind::DoExpression => self.generate_do(ir, node).unwrap(),
         NodeKind::IfExpression => self.generate_if(ir, node).unwrap(),
         NodeKind::CallFunction => self.generate_call(ir, node),
         NodeKind::Return => self.generate_return(ir, node),

         // Intrinsics
         NodeKind::WidenUint | NodeKind::WidenInt => self.generate_integer_conversion(ir, node),
         NodeKind::PrintInt32 | NodeKind::PrintFloat32 => {
            self.generate_call_like_intrinsic(ir, node)
         }
         other => unreachable!("invalid expression node: {:?}", other),
      }
   }

   /// Generates code for a function call-like intrinsic.
   fn generate_call_like_intrinsic(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
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
      self.generate_unit_literal().into()
   }
}
