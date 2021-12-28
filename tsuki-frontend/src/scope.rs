//! Scoping and symbols.

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::num::NonZeroUsize;

use crate::ast::NodeId;
use crate::functions::FunctionId;
use crate::types::TypeId;

/// An ID uniquely identifying a symbol.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SymbolId(usize);

impl SymbolId {
   /// Returns the internal ID of the symbol.
   pub fn id(self) -> usize {
      self.0
   }

   /// Creates a symbol from an internal ID.
   pub(crate) fn new(id: usize) -> Self {
      Self(id)
   }
}

/// The kind of a symbol, as well as extra metadata attached to it.
pub enum SymbolKind {
   /// A symbol that represents a variable.
   Variable(Variable),
   /// A symbol that represents a type.
   Type(TypeId),
   /// A symbol that represents a function, be it free or associated.
   Function(FunctionId),
}

impl SymbolKind {
   /// Unwraps a variable symbol.
   pub fn unwrap_variable(&self) -> &Variable {
      if let SymbolKind::Variable(ref variable) = self {
         variable
      } else {
         panic!("unwrap_variable called on a non-variable symbol")
      }
   }

   /// Unwraps a function symbol.
   pub fn unwrap_function(&self) -> FunctionId {
      if let &SymbolKind::Function(function_id) = self {
         function_id
      } else {
         panic!("unwrap_function called on a non-function symbol")
      }
   }
}

/// The mutability of a variable.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mutability {
   /// A `val` (immutable) variable.
   Val,
   /// A `var` (mutable) variable.
   Var,
}

/// Symbol data for a variable declaration.
pub struct Variable {
   pub mutability: Mutability,
}

/// Symbol storage. Symbols are looked up identifiers.
pub struct Symbols {
   names: Vec<String>,
   nodes: Vec<NodeId>,
   types: Vec<TypeId>,
   kinds: Vec<SymbolKind>,
}

impl Symbols {
   /// Creates a new symbol storage.
   pub fn new() -> Symbols {
      Self {
         names: Vec::new(),
         nodes: Vec::new(),
         types: Vec::new(),
         kinds: Vec::new(),
      }
   }

   /// Creates a symbol from a name, handle, type, and kind.
   pub fn create(&mut self, name: &str, node: NodeId, typ: TypeId, kind: SymbolKind) -> SymbolId {
      let id = self.nodes.len();
      self.names.push(name.to_owned());
      self.nodes.push(node);
      self.types.push(typ);
      self.kinds.push(kind);
      SymbolId::new(id)
   }

   /// Returns the name of the symbol.
   pub fn name(&self, symbol: SymbolId) -> &str {
      &self.names[symbol.0]
   }

   /// Returns the symbol's ancestor node.
   pub fn node(&self, symbol: SymbolId) -> NodeId {
      self.nodes[symbol.0]
   }

   /// Returns the symbol's type.
   pub fn type_id(&self, symbol: SymbolId) -> TypeId {
      self.types[symbol.0]
   }

   /// Returns the symbol's associated data.
   pub fn kind(&self, symbol: SymbolId) -> &SymbolKind {
      &self.kinds[symbol.0]
   }

   pub fn kind_mut(&mut self, symbol: SymbolId) -> &mut SymbolKind {
      &mut self.kinds[symbol.0]
   }
}

/// Represents a local scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(NonZeroUsize);

/// Struct for module and local scope management.
pub struct Scopes {
   symbols: HashMap<(ScopeId, Cow<'static, str>), SymbolId>,
   breakable_scopes: HashSet<ScopeId>,
   scope_count: usize,
}

impl Scopes {
   /// Creates a new scope manager.
   pub fn new() -> Self {
      Self {
         symbols: HashMap::new(),
         breakable_scopes: HashSet::new(),
         scope_count: 1,
      }
   }

   /// Creates a new scope and returns its ID.
   pub fn create_scope(&mut self) -> ScopeId {
      let id = self.scope_count;
      self.scope_count += 1;
      ScopeId(NonZeroUsize::new(id).unwrap())
   }

   /// Inserts a symbol to the scope, under the given identifier. If there already is a symbol with
   /// the given name, it's lost.
   pub fn insert(&mut self, scope: ScopeId, identifier: &str, symbol: SymbolId) {
      let _ = self.symbols.insert((scope, Cow::Owned(identifier.to_owned())), symbol);
   }

   /// Retrieves a reference to the provided identifier in the given scope, or `None` if the
   /// identifier is not in the given scope.
   pub fn get(&self, scope: ScopeId, identifier: &str) -> Option<SymbolId> {
      self.symbols.get(&(scope, Cow::Borrowed(identifier))).map(|id| *id)
   }

   /// Returns whether a scope is breakable.
   pub fn is_breakable(&self, scope: ScopeId) -> bool {
      self.breakable_scopes.contains(&scope)
   }

   /// Marks a scope as breakable.
   pub fn set_breakable(&mut self, scope: ScopeId) {
      self.breakable_scopes.insert(scope);
   }
}

/// A stack for tracking which local scopes are currently visible.
/// This can also be used for identifier lookups.
#[derive(Debug, Clone)]
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

   /// Looks for symbols with the given name in scopes on the stack, and returns the innermost one.
   pub fn lookup(&self, scopes: &Scopes, name: &str) -> Option<SymbolId> {
      for &scope in self.scopes.iter().rev() {
         if let Some(symbol) = scopes.get(scope, name) {
            return Some(symbol);
         }
      }
      None
   }

   /// Returns an iterator over all scopes on the stack, from bottom to top.
   pub fn iter(&self) -> impl DoubleEndedIterator<Item = ScopeId> + '_ {
      self.scopes.iter().copied()
   }
}
