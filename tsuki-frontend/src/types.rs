//! Storage and logging of types.

use std::ops::Range;

use crate::ast::NodeHandle;

/// Data-oriented type storage.
pub struct Types {
   names: Vec<Range<usize>>,
   kinds: Vec<TypeKind>,

   name_data: String,
}

/// A unique ID representing a type.
pub struct TypeId(usize);

/// Structure containing data for creating a type.
pub struct TypeInfo<'n> {
   pub name: &'n str,
   pub kind: TypeKind,
}

impl Types {
   /// Creates a new, empty type registry.
   pub fn new() -> Self {
      Self {
         names: Vec::new(),
         kinds: Vec::new(),
         name_data: String::new(),
      }
   }

   /// Creates a new type with the given type info.
   pub fn create_type(&mut self, info: TypeInfo<'_>) -> TypeId {
      let id = self.names.len();
      let name_range = self.add_name(info.name);
      self.names.push(name_range);
      TypeId(id)
   }

   /// Adds a name into the local `name_data` storage.
   fn add_name(&mut self, name: &str) -> Range<usize> {
      let start = self.name_data.len();
      self.name_data.push_str(name);
      let end = self.name_data.len();
      start..end
   }
}

/// The kind of a type.
pub enum TypeKind {
   Unit,
   NoReturn,
   Bool,
   Integer(IntegerSize),
   Float(FloatSize),
}

/// The size of an integer. `S` sizes are signed, `U` sizes are unsigned.
pub enum IntegerSize {
   S8,
   S16,
   S32,
   S64,
   U8,
   U16,
   U32,
   U64,
}

/// The size of a float.
pub enum FloatSize {
   S32,
   S64,
}
