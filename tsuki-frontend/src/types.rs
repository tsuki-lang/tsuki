//! Storage and logging of types.

// Note: Because `type` is a keyword in Rust, sometimes a truncated form `typ` is used to prevent
// conflicts.

use std::cmp::Ordering;
use std::ops::Range;

use crate::ast::NodeId;
use crate::scope::{ScopeId, Scopes, SymbolId, SymbolKind, Symbols};

/// Data-oriented type storage.
pub struct Types {
   names: Vec<Range<usize>>,
   kinds: Vec<TypeKind>,

   name_data: String,
}

/// A unique ID representing a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
      let mut types = Self {
         names: Vec::new(),
         kinds: Vec::new(),
         name_data: String::new(),
      };
      // Ensure the null slot is populated by the missing type.
      let _ = types.create_type(TypeInfo {
         name: "missingtype",
         kind: TypeKind::Missing,
      });
      types
   }

   /// Creates a new type with the given type info.
   #[must_use]
   pub fn create_type(&mut self, info: TypeInfo<'_>) -> TypeId {
      let id = self.names.len();
      let name_range = self.add_name(info.name);
      self.names.push(name_range);
      self.kinds.push(info.kind);
      TypeId(id)
   }

   /// Returns the name of the type.
   pub fn name(&self, typ: TypeId) -> &str {
      &self.name_data[self.names[typ.0].clone()]
   }

   /// Returns the kind of the given type.
   pub fn kind(&self, typ: TypeId) -> &TypeKind {
      &self.kinds[typ.0]
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
   /// The missing type is assigned to all nodes that don't get a type assigned explicitly.
   Missing,
   /// The error type is returned when type analysis fails for an AST node.
   Error,
   /// The statement type is assigned to AST nodes that do not return a value, such as loops.
   Statement,
   /// The declaration type is assigned to AST nodes that introduce a symbol into scope.
   Declaration(SymbolId),
   /// `type` is the type of all type symbols. It can't be instantiated by user code.
   Type,
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

   /// An alias for the type of the given ID.
   Alias(TypeId),
}

impl TypeKind {
   /// Returns whether the type kind represents an invalid type.
   pub fn is_invalid(&self) -> bool {
      matches!(self, TypeKind::Missing | TypeKind::Error)
   }

   /// Returns whether the type kind is the `NoReturn` type.
   pub fn is_noreturn(&self) -> bool {
      matches!(self, TypeKind::NoReturn)
   }

   /// Returns whether the type kind is the unit type.
   pub fn is_unit(&self) -> bool {
      matches!(self, TypeKind::Unit)
   }

   /// Returns whether the type kind represents the `Bool` type.
   pub fn is_bool(&self) -> bool {
      matches!(self, TypeKind::Bool)
   }

   /// Returns whether the type kind represents an integer type.
   pub fn is_integer(&self) -> bool {
      matches!(self, TypeKind::Integer(..))
   }

   /// Returns whether the type kind represents a float type.
   pub fn is_float(&self) -> bool {
      matches!(self, TypeKind::Float(..))
   }

   /// Returns whether the type kind represents a numeric (integer or float) type.
   pub fn is_numeric(&self) -> bool {
      self.is_integer() || self.is_float()
   }

   /// Returns whether the type kind is for a type that's valid when used as a statement.
   pub fn is_statement(&self) -> bool {
      self.is_invalid()
         || matches!(
            self,
            TypeKind::Statement | TypeKind::Declaration(_) | TypeKind::Unit | TypeKind::NoReturn
         )
   }

   /// Unwraps the integer size stored in the type kind, panics if the kind is not an integer.
   pub fn unwrap_integer(&self) -> IntegerSize {
      match self {
         TypeKind::Integer(size) => *size,
         _ => panic!("unwrap_integer called on a type kind that is not an integer"),
      }
   }

   /// Unwraps the float size stored in the type kind, panics if the kind is not an float.
   pub fn unwrap_float(&self) -> FloatSize {
      match self {
         TypeKind::Float(size) => *size,
         _ => panic!("unwrap_float called on a type kind that is not a float"),
      }
   }

   /// Returns `Some(symbol_id)` if the type kind represents a declaration, or `None` if it doesn't.
   pub fn as_declaration(&self) -> Option<SymbolId> {
      if let Self::Declaration(v) = self {
         Some(*v)
      } else {
         None
      }
   }
}

/// The size of an integer. `S` sizes are signed, `U` sizes are unsigned.
#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
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

impl IntegerSize {
   /// Returns whether the size represents an unsigned integer.
   pub fn is_unsigned(self) -> bool {
      // Can't use Self here?
      use IntegerSize::*;
      matches!(self, U8 | U16 | U32 | U64)
   }

   /// Returns whether the size represents a signed integer.
   pub fn is_signed(self) -> bool {
      !self.is_unsigned()
   }
}

impl PartialOrd for IntegerSize {
   /// Compares two integer sizes.
   fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      if self.is_signed() && other.is_unsigned() {
         None
      } else {
         (*self as u8).partial_cmp(&(*other as u8))
      }
   }
}

/// The size of a float.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FloatSize {
   S32,
   S64,
}

/// Configuration for "sensible default" types: `Int`, `Float`, and `Size`.
pub struct DefaultTypes {
   pub int_width: IntegerSize,
   pub float_width: FloatSize,
   pub size_width: IntegerSize,
}

/// A struct containing all the built-in types.
pub struct BuiltinTypes {
   // Special
   pub t_error: TypeId,
   pub t_unit: TypeId,
   pub t_noreturn: TypeId,
   pub t_statement: TypeId,
   pub t_type: TypeId,

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
   pub t_size: TypeId,

   // Characters
   pub t_char: TypeId,
}

impl BuiltinTypes {
   /// Adds all the built-in types to the given `Types` and returns them.
   pub fn add_to(types: &mut Types, default_types: &DefaultTypes) -> Self {
      let t_error = types.create_type(TypeInfo {
         // NOTE: Maybe look for better names than this?
         // Just like `statement`, the name is lowercase, but users may think that the occurrence
         // of errortype is an error in the compiler. Of course, it's not.
         // Maybe we should "unwrap" error types somehow, so that we never report errors
         // containing them?
         name: "errortype",
         kind: TypeKind::Error,
      });
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
         t_error,
         t_unit: types.create_type(TypeInfo {
            name: "()",
            kind: TypeKind::Unit,
         }),
         t_noreturn: types.create_type(TypeInfo {
            name: "NoReturn",
            kind: TypeKind::NoReturn,
         }),
         // Unlike all other types, the `statement` type is lowercase. This should let users know
         // that the "expression" in question isn't an expression after all.
         t_statement: types.create_type(TypeInfo {
            name: "statement",
            kind: TypeKind::Statement,
         }),
         t_type: types.create_type(TypeInfo {
            name: "type",
            kind: TypeKind::Type,
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

         t_int: match default_types.int_width {
            IntegerSize::S8 => t_int8,
            IntegerSize::S16 => t_int16,
            IntegerSize::S32 => t_int32,
            IntegerSize::S64 => t_int64,
            _ => panic!("int_size must be signed"),
         },
         t_float: match default_types.float_width {
            FloatSize::S32 => t_float32,
            FloatSize::S64 => t_float64,
         },
         t_size: match default_types.size_width {
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

   /// Registers named built-in types in the given scope.
   ///
   /// TODO: Remove this in favor of the stdlib declaring the types in the prelude.
   pub(crate) fn register_in(&self, scopes: &mut Scopes, symbols: &mut Symbols, scope: ScopeId) {
      macro_rules! add_type {
         ($field:tt, $name:tt) => {
            let symbol = symbols.create(
               $name,
               NodeId::null(),
               self.t_type,
               SymbolKind::Type(self.$field),
            );
            scopes.insert(scope, symbols.name(symbol), symbol);
         };
      }

      add_type!(t_noreturn, "NoReturn");
      add_type!(t_bool, "Bool");
      add_type!(t_uint8, "Uint8");
      add_type!(t_uint16, "Uint16");
      add_type!(t_uint32, "Uint32");
      add_type!(t_uint64, "Uint64");
      add_type!(t_int8, "Int8");
      add_type!(t_int16, "Int16");
      add_type!(t_int32, "Int32");
      add_type!(t_int64, "Int64");
      add_type!(t_float32, "Float32");
      add_type!(t_float64, "Float64");
      add_type!(t_char, "Char");

      add_type!(t_int, "Int");
      add_type!(t_float, "Float");
      add_type!(t_size, "Size");
   }
}

/// A unique ID identifying an entry in the type log.
#[derive(Clone, Copy, Debug)]
#[must_use]
pub struct TypeLogEntry(usize);

/// An alias for a result storing a log for either a valid or an erroneous type usage.
pub type TypeLogResult = Result<TypeLogEntry, TypeLogEntry>;

impl From<TypeLogResult> for TypeLogEntry {
   /// Unwraps a successful or erroneous type log from a result.
   ///
   /// This is used to simplify returning from functions when analysis errors occur.
   fn from(result: Result<TypeLogEntry, TypeLogEntry>) -> Self {
      match result {
         Ok(entry) | Err(entry) => entry,
      }
   }
}

/// A log storing the AST nodes from which different instances of types came from.
pub struct TypeLog {
   types: Vec<TypeId>,
   nodes: Vec<NodeId>,
}

impl TypeLog {
   /// Constructs a new type log.
   pub fn new() -> Self {
      Self {
         types: Vec::new(),
         nodes: Vec::new(),
      }
   }

   /// Inserts a new type into the log and returns its handle.
   pub fn push(&mut self, typ: TypeId, node: NodeId) -> TypeLogEntry {
      let id = self.types.len();
      self.types.push(typ);
      self.nodes.push(node);
      TypeLogEntry(id)
   }

   /// Returns the type stored in the log entry.
   pub fn type_id(&self, entry: TypeLogEntry) -> TypeId {
      self.types[entry.0]
   }

   /// Returns the source node stored in the log entry.
   pub fn node(&self, entry: TypeLogEntry) -> NodeId {
      self.nodes[entry.0]
   }
}
