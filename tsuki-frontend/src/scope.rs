//! Scoping and symbols.

use std::{collections::HashMap, num::NonZeroUsize};

use crate::{ast::NodeHandle, types::TypeId};

/// The kind of a symbol, as well as extra metadata attached to it.
pub enum SymbolKind {
   /// A symbol that represents a type.
   Type(TypeId),
}

pub struct Symbol {
   /// The AST node the symbol comes from.
   pub node: NodeHandle,
   /// The kind of the symbol.
   pub kind: SymbolKind,
   /// The symbol's type.
   pub typ: TypeId,
}

type SymbolMap = HashMap<String, Symbol>;

/// Represents a local scope.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ScopeId(NonZeroUsize);

impl ScopeId {
   /// Returns the index of the scope in a `Scopes`.
   fn index(self) -> usize {
      self.0.get() - 1
   }
}

/// Struct for module and local scope management.
pub struct Scopes {
   // FIXME: Maybe there's a more cache-friendly way of storing scopes?
   scopes: Vec<SymbolMap>,
}

impl Scopes {
   /// Creates a new scope manager.
   pub fn new() -> Self {
      Self { scopes: Vec::new() }
   }

   /// Creates a new scope and returns its ID.
   pub fn create_scope(&mut self) -> ScopeId {
      let id = self.scopes.len() + 1;
      self.scopes.push(SymbolMap::new());
      ScopeId(NonZeroUsize::new(id).unwrap())
   }

   /// Inserts a symbol to the scope, under the given identifier. If there already is a symbol with
   /// the given name, it's lost.
   pub fn insert(&mut self, scope: ScopeId, identifier: &str, symbol: Symbol) {
      let _ = self.scopes[scope.index()].insert(identifier.to_owned(), symbol);
   }

   /// Retrieves a reference to the provided identifier in the given scope, or `None` if the
   /// identifier is not in the given scope.
   pub fn get(&self, scope: ScopeId, identifier: &str) -> Option<&Symbol> {
      self.scopes[scope.index()].get(identifier)
   }

   /// Retrieves a mutable reference to a symbol in the scope, or `None` if there is no symbol with
   /// the given name.
   pub fn get_mut(&mut self, scope: ScopeId, identifier: &str) -> Option<&mut Symbol> {
      self.scopes[scope.index()].get_mut(identifier)
   }
}

/// A stack for tracking which local scopes are currently visible.
/// This can also be used for identifier lookups.
pub struct ScopeStack {
   scopes: Vec<ScopeId>,
}

impl ScopeStack {
   /// Creates a new scope stack.
   pub fn new() -> Self {
      Self { scopes: Vec::new() }
   }

   /// Pushes a scope onto the top of the stack. Returns the scope ID that was pushed.
   pub fn push(&mut self, scope: ScopeId) -> ScopeId {
      self.scopes.push(scope);
      scope
   }

   /// Returns the scope at the top of the stack.
   pub fn top(&self) -> ScopeId {
      *self.scopes.last().expect("the scope stack must not be empty")
   }

   /// Pops the topmost scope off the stack.
   pub fn pop(&mut self) {
      let _ = self.scopes.pop();
   }
}
