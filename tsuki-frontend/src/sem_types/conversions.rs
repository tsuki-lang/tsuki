//! Implicit conversions between types.

use crate::ast::{Ast, NodeData, NodeHandle, NodeKind};
use crate::types::{FloatSize, IntegerSize, TypeId, TypeLogEntry};

use super::SemTypes;

impl<'s> SemTypes<'s> {
   /// Widens the given integer node to the provided size.
   ///
   /// For literal nodes, this converts the literal directly. For other nodes, this wraps the node
   /// in a `WidenUint` or `WidenInt` with the type set to represent the new size.
   fn widen_integer(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      new_size: IntegerSize,
   ) -> TypeLogEntry {
      if ast.kind(node).is_integer() {
         // Shortcut path for literals.
         let as_uint = ast.extra(node).unwrap_uint();
         ast.convert(
            node,
            match new_size {
               IntegerSize::U8 => NodeKind::Uint8,
               IntegerSize::U16 => NodeKind::Uint16,
               IntegerSize::U32 => NodeKind::Uint32,
               IntegerSize::U64 => NodeKind::Uint64,
               IntegerSize::S8 => NodeKind::Int8,
               IntegerSize::S16 => NodeKind::Int16,
               IntegerSize::S32 => NodeKind::Int32,
               IntegerSize::S64 => NodeKind::Int64,
            },
         );
         ast.set_extra(
            node,
            match new_size {
               IntegerSize::U8 => NodeData::Uint8(as_uint as u8),
               IntegerSize::U16 => NodeData::Uint16(as_uint as u16),
               IntegerSize::U32 => NodeData::Uint32(as_uint as u32),
               IntegerSize::U64 => NodeData::Uint64(as_uint as u64),
               IntegerSize::S8 => NodeData::Int8(as_uint as i8),
               IntegerSize::S16 => NodeData::Int16(as_uint as i16),
               IntegerSize::S32 => NodeData::Int32(as_uint as i32),
               IntegerSize::S64 => NodeData::Int64(as_uint as i64),
            },
         );
      } else {
         // Backend path for other nodes.
         if ast.kind(node).is_unsigned_integer() {
            ast.wrap(node, NodeKind::WidenUint);
         } else {
            ast.wrap(node, NodeKind::WidenInt);
         }
      }
      self.annotate(
         ast,
         node,
         match new_size {
            IntegerSize::U8 => self.builtin.t_uint8,
            IntegerSize::U16 => self.builtin.t_uint16,
            IntegerSize::U32 => self.builtin.t_uint32,
            IntegerSize::U64 => self.builtin.t_uint64,
            IntegerSize::S8 => self.builtin.t_int8,
            IntegerSize::S16 => self.builtin.t_int16,
            IntegerSize::S32 => self.builtin.t_int32,
            IntegerSize::S64 => self.builtin.t_int64,
         },
      )
   }

   /// Widens a float node to the given size.
   ///
   /// Behavior with literals is similar to `widen_integer`.
   fn widen_float(&mut self, ast: &mut Ast, node: NodeHandle, new_size: FloatSize) -> TypeLogEntry {
      if ast.kind(node).is_float() {
         let as_float = ast.extra(node).unwrap_float();
         ast.convert(
            node,
            match new_size {
               FloatSize::S32 => NodeKind::Float32,
               FloatSize::S64 => NodeKind::Float64,
            },
         );
         ast.set_extra(
            node,
            match new_size {
               FloatSize::S32 => NodeData::Float32(as_float as f32),
               FloatSize::S64 => NodeData::Float64(as_float),
            },
         );
      } else {
         ast.wrap(node, NodeKind::WidenFloat);
      }
      self.annotate(
         ast,
         node,
         match new_size {
            FloatSize::S32 => self.builtin.t_float32,
            FloatSize::S64 => self.builtin.t_float64,
         },
      )
   }

   /// Attempts to convert the type `from` to type `to`. If an implicit conversion is not possible,
   /// returns `None`. Otherwise returns the converted type ID.
   pub(super) fn perform_implicit_conversion(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      from: TypeId,
      to: TypeId,
   ) -> Option<TypeLogEntry> {
      // If the two types are equal, there's need for conversion.
      if from == to {
         return Some(self.log.push(to, node));
      }
      // Otherwise, compare their kinds for various traits.
      let from_kind = self.types.kind(from);
      let to_kind = self.types.kind(to);

      // Widening integer conversions
      if from_kind.is_integer() && to_kind.is_integer() {
         // Integers are only implicitly convertible to wider types of the same signedness,
         // eg. Int8 -> Int16, Int32 -> Int64, but not Int64 -> Int32, or Uint32 -> Int32.
         let from_size = from_kind.unwrap_integer();
         let to_size = to_kind.unwrap_integer();
         if to_size >= from_size {
            return Some(self.widen_integer(ast, node, to_size));
         }
      }

      // Widening float conversions
      if from_kind.is_float() && to_kind.is_float() {
         // Floats are only implicitly convertible if the destination type is wider than the
         // source type (Float32 -> Float64).
         let from_size = from_kind.unwrap_float();
         let to_size = to_kind.unwrap_float();
         if to_size >= from_size {
            return Some(self.widen_float(ast, node, to_size));
         }
      }

      None
   }
}
