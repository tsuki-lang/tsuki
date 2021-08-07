//! Storage and logging of types.

// Note: Because `type` is a keyword in Rust, sometimes a truncated form `typ` is used to prevent
// conflicts.

use std::ops::Range;

use crate::ast::NodeHandle;

/// Data-oriented type storage.
pub struct Types {
   names: Vec<Range<usize>>,
   kinds: Vec<TypeKind>,

   name_data: String,

   node_types: Vec<TypeId>,
}

/// A unique ID representing a type.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TypeId(usize);

impl TypeId {
   /// Returns the null type ID, which maps to an error type.
   pub fn null() -> TypeId {
      TypeId(0)
   }
}

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
         node_types: Vec::new(),
      }
   }

   /// Creates a new type with the given type info.
   pub fn create_type(&mut self, info: TypeInfo<'_>) -> TypeId {
      let id = self.names.len();
      let name_range = self.add_name(info.name);
      self.names.push(name_range);
      self.kinds.push(info.kind);
      TypeId(id)
   }

   /// Returns the name of the type.
   pub fn name(&self, typ: TypeId) -> &str {
      &self.name_data[self.names[typ.0]]
   }

   /// Returns the type for the given node, or `TypeId::null()` if the node has no type.
   pub fn node_type(&self, node: NodeHandle) -> TypeId {
      self.node_types.get(node.id()).cloned().unwrap_or(TypeId::null())
   }

   /// Sets the type for the given node.
   pub fn set_node_type(&mut self, node: NodeHandle, typ: TypeId) {
      if self.node_types.len() <= node.id() {
         self.node_types.resize(node.id() + 1, TypeId::null());
      }
      self.node_types[node.id()] = typ;
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
   /// The error type is returned when type analysis fails for an AST node.
   Error,
   /// The statement type is assigned to AST nodes that do not return a value, such as loops.
   Statement,
   /// The unit type is a type with a single value `()`. It is the default return type for
   /// functions.
   Unit,
   /// The NoReturn type is assigned to expressions that do not return to the parent expression,
   /// eg. `return` expressions. Certain built-in functions also return `NoReturn`.
   /// It is implicitly convertible to any other type.
   NoReturn,
   // The rest of the primitive types is quite self-explanatory.
   Bool,
   Integer(IntegerSize),
   Float(FloatSize),
   Char,
}

/// The size of an integer. `S` sizes are signed, `U` sizes are unsigned.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IntegerSize {
   U8,
   U16,
   U32,
   U64,
   S8,
   S16,
   S32,
   S64,
}

/// The size of a float.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum FloatSize {
   S32,
   S64,
}

/// Configuration for "sensible default" types: `Int`, `Float`, and `Index`.
pub struct DefaultTypes {
   pub int_size: IntegerSize,
   pub float_size: FloatSize,
   pub index_size: IntegerSize,
}

/// A struct containing all the built-in types.
pub struct BuiltinTypes {
   // Special
   pub t_error: TypeId,
   pub t_unit: TypeId,
   pub t_noreturn: TypeId,

   // Boolean
   pub t_bool: TypeId,

   // Integers
   pub t_uint8: TypeId,
   pub t_uint16: TypeId,
   pub t_uint32: TypeId,
   pub t_uint64: TypeId,
   pub t_int8: TypeId,
   pub t_int16: TypeId,
   pub t_int32: TypeId,
   pub t_int64: TypeId,

   // Floats
   pub t_float32: TypeId,
   pub t_float64: TypeId,

   // Int/Float aliases
   // NOTE: These will later be implemented in the standard library and will be configurable with
   // compiler switches.
   pub t_int: TypeId,
   pub t_float: TypeId,
   pub t_index: TypeId,

   // Characters
   pub t_char: TypeId,
}

impl BuiltinTypes {
   /// Adds all the built-in types to the given `Types` and returns them.
   pub fn add_to(types: &mut Types, default_types: &DefaultTypes) -> Self {
      let t_uint8 = types.create_type(TypeInfo {
         name: "Uint8",
         kind: TypeKind::Integer(IntegerSize::U8),
      });
      let t_uint16 = types.create_type(TypeInfo {
         name: "Uint16",
         kind: TypeKind::Integer(IntegerSize::U16),
      });
      let t_uint32 = types.create_type(TypeInfo {
         name: "Uint32",
         kind: TypeKind::Integer(IntegerSize::U32),
      });
      let t_uint64 = types.create_type(TypeInfo {
         name: "Uint64",
         kind: TypeKind::Integer(IntegerSize::U64),
      });
      let t_int8 = types.create_type(TypeInfo {
         name: "Int8",
         kind: TypeKind::Integer(IntegerSize::S8),
      });
      let t_int16 = types.create_type(TypeInfo {
         name: "Int16",
         kind: TypeKind::Integer(IntegerSize::S16),
      });
      let t_int32 = types.create_type(TypeInfo {
         name: "Int32",
         kind: TypeKind::Integer(IntegerSize::S32),
      });
      let t_int64 = types.create_type(TypeInfo {
         name: "Int64",
         kind: TypeKind::Integer(IntegerSize::S64),
      });
      let t_float32 = types.create_type(TypeInfo {
         name: "Float32",
         kind: TypeKind::Float(FloatSize::S32),
      });
      let t_float64 = types.create_type(TypeInfo {
         name: "Float64",
         kind: TypeKind::Float(FloatSize::S64),
      });
      Self {
         t_error: types.create_type(TypeInfo {
            name: "Error",
            kind: TypeKind::Error,
         }),
         t_unit: types.create_type(TypeInfo {
            name: "()",
            kind: TypeKind::Unit,
         }),
         t_noreturn: types.create_type(TypeInfo {
            name: "NoReturn",
            kind: TypeKind::NoReturn,
         }),
         t_bool: types.create_type(TypeInfo {
            name: "Bool",
            kind: TypeKind::Bool,
         }),
         t_uint8,
         t_uint16,
         t_uint32,
         t_uint64,
         t_int8,
         t_int16,
         t_int32,
         t_int64,
         t_float32,
         t_float64,

         t_int: match default_types.int_size {
            IntegerSize::S8 => t_int8,
            IntegerSize::S16 => t_int16,
            IntegerSize::S32 => t_int32,
            IntegerSize::S64 => t_int64,
            _ => panic!("int_size must be signed"),
         },
         t_float: match default_types.float_size {
            FloatSize::S32 => t_float32,
            FloatSize::S64 => t_float64,
         },
         t_index: match default_types.index_size {
            IntegerSize::U8 => t_uint8,
            IntegerSize::U16 => t_uint16,
            IntegerSize::U32 => t_uint32,
            IntegerSize::U64 => t_uint64,
            _ => panic!("index_size must be unsigned"),
         },
         t_char: types.create_type(TypeInfo {
            name: "Char",
            kind: TypeKind::Char,
         }),
      }
   }
}
