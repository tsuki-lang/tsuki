//! Function registry.

use smallvec::{smallvec, SmallVec};

use crate::ast::{NodeHandle, NodeKind};
use crate::scope::{Mutability, ScopeId, Scopes, SymbolId, SymbolKind, Symbols, Variable};
use crate::types::{BuiltinTypes, TypeId};

/// The unique ID of a function in the registry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionId(usize);

/// The kind of a function.
pub enum FunctionKind {
   /// This function was declared in the current module.
   Local,
   /// This function was declared in a different module or package.
   External,
   /// This function is imported from C.
   ImportC { is_varargs: bool },
   /// This function is a compiler intrinsic.
   Intrinsic(Intrinsic),
}

impl FunctionKind {
   /// Returns whether the function kind is for a local function.
   pub fn is_local(&self) -> bool {
      matches!(self, Self::Local)
   }

   /// Returns whether the function kind is for a C varargs function.
   pub fn is_varargs(&self) -> bool {
      matches!(self, Self::ImportC { is_varargs: true })
   }
}

/// Function parameters.
pub struct Parameters {
   /// The names and types of formal parameters this function accepts.
   pub formal: SmallVec<[SymbolId; 8]>,
   /// The return type of the function.
   pub return_type: TypeId,
}

/// Data-oriented storage for functions.
pub struct Functions {
   names: Vec<String>,
   mangled_names: Vec<String>,
   parameters: Vec<Parameters>,
   kinds: Vec<FunctionKind>,
}

impl Functions {
   /// Creates a new function registry.
   pub fn new() -> Self {
      Self {
         names: Vec::new(),
         mangled_names: Vec::new(),
         parameters: Vec::new(),
         kinds: Vec::new(),
      }
   }

   /// Adds a function into the registry.
   pub fn create(
      &mut self,
      name: String,
      mangled_name: String,
      parameters: Parameters,
      kind: FunctionKind,
   ) -> FunctionId {
      let id = self.names.len();
      self.names.push(name);
      self.mangled_names.push(mangled_name);
      self.parameters.push(parameters);
      self.kinds.push(kind);
      FunctionId(id)
   }

   /// Returns the name of a function.
   pub fn name(&self, function: FunctionId) -> &str {
      &self.names[function.0]
   }

   /// Returns the mangled name of a function.
   pub fn mangled_name(&self, function: FunctionId) -> &str {
      &self.mangled_names[function.0]
   }

   /// Returns a reference to the function's parameters.
   pub fn parameters(&self, function: FunctionId) -> &Parameters {
      &self.parameters[function.0]
   }

   /// Returns the kind of the function.
   pub fn kind(&self, function: FunctionId) -> &FunctionKind {
      &self.kinds[function.0]
   }

   /// Returns an iterator over function IDs.
   pub fn iter(&self) -> FunctionsIter {
      FunctionsIter {
         current: 0,
         len: self.names.len(),
      }
   }
}

/// An intrinsic function.
///
/// Each kind of intrinsic has its own node kind;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intrinsic {
   // TODO: Remove these once `c_import` is implemented.
   /// Prints an `Int32` to stdout.
   PrintInt32,
   /// Prints a `Float32` to stdout.
   PrintFloat32,
}

impl From<Intrinsic> for NodeKind {
   /// Converts an intrinsic to its corresponding node kind.
   fn from(intrinsic: Intrinsic) -> Self {
      match intrinsic {
         Intrinsic::PrintInt32 => NodeKind::PrintInt32,
         Intrinsic::PrintFloat32 => NodeKind::PrintFloat32,
      }
   }
}

/// Registers intrinsic functions in the given scope, symbol, and function registries.
pub fn register_intrinsics(
   builtin: &BuiltinTypes,
   scopes: &mut Scopes,
   symbols: &mut Symbols,
   scope: ScopeId,
   functions: &mut Functions,
) {
   // TODO: replace this with stdlib declarations.
   macro_rules! add_intrinsic {
      ($name:tt, $params:tt, $return_type:expr, $intrinsic:expr $(,)?) => {
         let function_id = functions.create(
            $name.into(),
            String::new(),
            Parameters {
               formal: $params
                  .iter()
                  .map(|&(name, type_id)| {
                     symbols.create(
                        name,
                        NodeHandle::null(),
                        type_id,
                        SymbolKind::Variable(Variable {
                           mutability: Mutability::Val,
                        }),
                     )
                  })
                  .collect(),
               return_type: $return_type,
            },
            FunctionKind::Intrinsic($intrinsic),
         );
         let symbol_id = symbols.create(
            $name,
            NodeHandle::null(),
            builtin.t_statement,
            SymbolKind::Function(function_id),
         );
         scopes.insert(scope, $name, symbol_id);
      };
   }

   add_intrinsic!(
      "__intrin_print_int32",
      [("x", builtin.t_int32)],
      builtin.t_unit,
      Intrinsic::PrintInt32,
   );
   add_intrinsic!(
      "__intrin_print_float32",
      [("x", builtin.t_float32)],
      builtin.t_unit,
      Intrinsic::PrintFloat32,
   );
}

pub struct FunctionsIter {
   current: usize,
   len: usize,
}

impl Iterator for FunctionsIter {
   type Item = FunctionId;

   fn next(&mut self) -> Option<Self::Item> {
      if self.current < self.len {
         let i = self.current;
         self.current += 1;
         Some(FunctionId(i))
      } else {
         None
      }
   }
}
