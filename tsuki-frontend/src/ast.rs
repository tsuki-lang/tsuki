//! Efficient, tightly-packet AST representation.
//!
//! Nodes in the tsuki AST are represented via indices in a struct of arrays, instead of being
//! scattered across the heap, which helps achieve better cache locality.
//! This also works much better with the borrow checker.

use crate::common::Span;
use crate::types::TypeId;

/// A handle to a single node in the AST. The actual AST is not stored next to the handle for
/// efficiency and appeasing the borrow checker.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct NodeHandle(usize);

impl NodeHandle {
   /// Returns the null node handle. This handle always points to an error node and denotes the lack
   /// of a usable node.
   pub fn null() -> NodeHandle {
      NodeHandle(0)
   }

   /// Returns the raw ID stored in the node handle.
   pub fn id(&self) -> usize {
      self.0
   }
}

/// The AST data.
pub struct Ast {
   kinds: Vec<NodeKind>,
   spans: Vec<Span>,
   /// The `first` and `second` vectors are used for storing user data inside of the nodes.
   /// The most common case for storing data in nodes is having two values inside of the node, eg.
   /// binary operators, `elif` branches, `while`.
   first: Vec<usize>,
   second: Vec<usize>,
   /// Nodes that need to store extra variable-length data can instead point to vectors inside of
   /// this one. These nodes will be much less cache-efficient, but these nodes also occur much more
   /// rarely than those that use `first` and `second`.
   extra: Vec<NodeData>,
   /// Converted nodes duplicate their old nodes and store them in this table.
   /// Nodes that don't have a valid ancestor have this set to the null node.
   ancestors: Vec<NodeHandle>,
   /// The type information is stored inside of the AST so that duplications also copy type info
   /// around.
   types: Vec<TypeId>,
}

impl Ast {
   /// Constructs a new AST.
   pub fn new() -> Self {
      let mut ast = Self {
         kinds: Vec::new(),
         spans: Vec::new(),
         first: Vec::new(),
         second: Vec::new(),
         extra: Vec::new(),
         ancestors: Vec::new(),
         types: Vec::new(),
      };
      // Create a Nil node at ID 0 so that if some invalid AST node reference is dumped, it'll
      // instead go to this Error node.
      let _ = ast.create_node(NodeKind::Error);
      ast
   }

   /// Creates a new node of the given kind and returns a handle to it.
   #[must_use]
   pub fn create_node(&mut self, kind: NodeKind) -> NodeHandle {
      let id = self.kinds.len();
      self.kinds.push(kind);
      self.spans.push(Span::default());
      self.first.push(0);
      self.second.push(0);
      self.extra.push(NodeData::None);
      self.ancestors.push(NodeHandle::null());
      self.types.push(TypeId::null());
      NodeHandle(id)
   }

   /// Returns the kind of the node behind the given handle.
   pub fn kind(&self, handle: NodeHandle) -> NodeKind {
      self.kinds[handle.0]
   }

   /// Returns the span of the given handle.
   pub fn span(&self, handle: NodeHandle) -> &Span {
      &self.spans[handle.0]
   }

   /// Sets the span of the given handle.
   pub fn set_span(&mut self, handle: NodeHandle, span: Span) {
      self.spans[handle.0] = span;
   }

   /// Returns the first value of the node with the given handle.
   pub fn first(&self, handle: NodeHandle) -> usize {
      self.first[handle.0]
   }

   /// Returns the first value of the node with the given handle. The stored value is interpreted
   /// as a node handle.
   pub fn first_handle(&self, handle: NodeHandle) -> NodeHandle {
      NodeHandle(self.first[handle.0])
   }

   /// Sets the first value of a node handle.
   pub fn set_first(&mut self, handle: NodeHandle, first: usize) {
      self.first[handle.0] = first;
   }

   /// Sets the first value of a node handle. The value stored is a node handle.
   pub fn set_first_handle(&mut self, handle: NodeHandle, first: NodeHandle) {
      self.first[handle.0] = first.0;
   }

   /// Returns the second value of the node with the given handle.
   pub fn second(&self, handle: NodeHandle) -> usize {
      self.second[handle.0]
   }

   /// Returns the second value of the node with the given handle. The stored value is interpreted
   /// as a node handle.
   pub fn second_handle(&self, handle: NodeHandle) -> NodeHandle {
      NodeHandle(self.second[handle.0])
   }

   /// Sets the second value of a node handle.
   pub fn set_second(&mut self, handle: NodeHandle, second: usize) {
      self.second[handle.0] = second;
   }

   /// Sets the second value of a node handle. The value stored is a node handle.
   pub fn set_second_handle(&mut self, handle: NodeHandle, second: NodeHandle) {
      self.second[handle.0] = second.0;
   }

   /// Returns a reference to the extra data for the node with the given handle.
   pub fn extra(&self, handle: NodeHandle) -> &NodeData {
      &self.extra[handle.0]
   }

   /// Sets the extra data for the node with the given handle.
   pub fn set_extra(&mut self, handle: NodeHandle, data: NodeData) {
      self.extra[handle.0] = data;
   }

   /// Returns the ancestor of the node.
   pub fn ancestor(&self, handle: NodeHandle) -> NodeHandle {
      self.ancestors[handle.0]
   }

   /// Returns the type ID of the given node.
   pub fn type_id(&self, handle: NodeHandle) -> TypeId {
      self.types[handle.0]
   }

   /// Sets the type ID of the given node.
   pub fn set_type_id(&mut self, handle: NodeHandle, typ: TypeId) {
      self.types[handle.0] = typ;
   }

   /// Duplicates the given node, and returns a handle to the new node.
   fn duplicate(&mut self, node: NodeHandle) -> NodeHandle {
      let new = self.create_node(self.kind(node));
      self.set_span(new, self.span(node).clone());
      self.set_first(new, self.first(node));
      self.set_second(new, self.second(node));
      // FIXME: This is maybe a bit on the expensive side.
      // Maybe move the extra data out of here instead, like `Option::take`?
      self.set_extra(new, self.extra(node).clone());
      self.set_type_id(new, self.type_id(node));
      new
   }

   /// Converts the given node to a new node of the given kind, preserving its metadata.
   pub fn convert_preserve(&mut self, node: NodeHandle, kind: NodeKind) {
      self.ancestors[node.0] = self.duplicate(node);
      self.kinds[node.0] = kind;
   }

   /// Converts the given node to a new node of the given kind.
   /// Field values are not preserved, and instead moved to a new node, which is set to be the
   /// ancestor of the existing node.
   pub fn convert(&mut self, node: NodeHandle, kind: NodeKind) {
      self.convert_preserve(node, kind);
      self.set_span(node, Span::default());
      self.set_first(node, 0);
      self.set_second(node, 0);
      self.set_extra(node, NodeData::None);
   }

   /// Wraps the old node in a new node of the given kind.
   ///
   /// That is, converts the given node to the given kind, and sets the `first` to the
   /// ancestor node.
   pub fn wrap(&mut self, node: NodeHandle, kind: NodeKind) {
      self.convert(node, kind);
      self.set_first_handle(node, self.ancestors[node.0]);
   }

   /// Returns an iterator over all node handles in the AST.
   pub fn node_handles(&self) -> NodeHandles {
      NodeHandles {
         // Start from 1 to skip the Error node at ID 0.
         i: 1,
         len: self.kinds.len(),
      }
   }
}

// As much as I don't like Rust's macro_rules!, I don't like duplicate code either, so the two
// functions `walk_node_list` and `walk` are "duplicated" using a couple of macros.

macro_rules! walk_node_list_impl {
   ($ast:expr, $node:expr, $then:expr) => {
      if matches!($ast.extra($node), NodeData::NodeList(..)) {
         let n = $ast.extra($node).unwrap_node_list().len();
         for i in 0..n {
            $then($ast, $ast.extra($node).unwrap_node_list()[i]);
         }
      }
   };
}

macro_rules! walk_impl {
   ($ast:expr, $node:expr, $then:expr, $walk_node_list:tt) => {
      if !$ast.kind($node).is_leaf() {
         if $ast.first($node) != 0 {
            $then($ast, $ast.first_handle($node));
            if $ast.second($node) != 0 {
               $then($ast, $ast.second_handle($node));
            }
         }
         $ast.$walk_node_list($node, |ast, child| {
            $then(ast, child);
         })
      }
   };
}

// These "iterators" are implemented as functions taking closures as arguments rather than proper
// Rusty iterators, because Rusty iterators quickly turn into a maintenance mess with more complex
// logic like shown here. Also, borrows, ownership, all that good stuff. This style plays better
// with the rules in this case.

impl Ast {
   /// Walks through the node data of a node whose `NodeData` is a node list.
   /// If the `NodeData` isn't a node list, this is a noop.
   pub fn walk_node_list(&self, node: NodeHandle, mut then: impl FnMut(&Self, NodeHandle)) {
      walk_node_list_impl!(self, node, then);
   }

   /// Same as `walk_node_list`, but `self` is mutable.
   pub fn walk_node_list_mut(
      &mut self,
      node: NodeHandle,
      mut then: impl FnMut(&mut Self, NodeHandle),
   ) {
      walk_node_list_impl!(self, node, then);
   }

   /// Walks through the given node's children.
   pub fn walk(&self, node: NodeHandle, mut then: impl FnMut(&Self, NodeHandle)) {
      walk_impl!(self, node, then, walk_node_list);
   }

   /// Walks through the given node's children, but the first argument of `then` is mutable.
   pub fn walk_mut(&mut self, node: NodeHandle, mut then: impl FnMut(&mut Self, NodeHandle)) {
      walk_impl!(self, node, then, walk_node_list_mut);
   }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub enum NodeKind {
   /*
    * Pre-sem'check
    */
   /// The error node is used for returning a valid, unique node handle in case an error occurs
   /// during parsing.
   Error,

   // Literals
   Nil,
   True,
   False,
   Integer,
   Float,
   Atom,
   Character,
   String,

   // Documentation and identification
   DocComment,
   Identifier,

   // Nullary operators
   FullRange,

   /// Separator for where leaf nodes end, and where branch nodes start.
   /// Nodes of this kind must not be constructed. This kind is only used for distinguishing whether
   /// a node can be walked through.
   _LastLeaf,

   // Grouping
   StatementList,

   // Unary operators
   Not,
   Neg,    // -
   BitNot, // ~
   Member, // .
   Ref,    // ^

   // Binary operators
   Dot,           // .
   Plus,          // +
   Minus,         // -
   Mul,           // *
   Div,           // /
   Pow,           // **
   Concat,        // ~
   Lshift,        // <<
   Rshift,        // >>
   BitAnd,        // &
   BitOr,         // |
   BitXor,        // ^^
   Equal,         // ==
   NotEqual,      // !=
   Less,          // <
   Greater,       // >
   LessEqual,     // <=
   GreaterEqual,  // >=
   Deref,         // ^
   Check,         // ?
   Unwrap,        // !
   UpTo,          // ..
   UpToInclusive, // ..=
   Assign,        // =
   PlusAssign,    // +=
   MinusAssign,   // -=
   MulAssign,     // *=
   DivAssign,     // /=
   Push,          // <-

   // Other operations
   Call,
   Index,
   IndexAlt,

   // Control flow
   // ---
   // *Expression kinds differ from *Statement kinds in that the last statement in the block must
   // be an expression statement. The parser tries to assume the kind from context, but might fail.
   // The kind can be corrected during sem'checking.
   Pass,
   Do,
   If,
   IfBranch,   // if, elif
   ElseBranch, // else

   /*
    * Post-sem'check
    */
   // Concrete literals
   Uint8,
   Uint16,
   Uint32,
   Uint64,
   Int8,
   Int16,
   Int32,
   Int64,
   Float32,
   Float64,

   // Intrinsics
   WidenUint,
   WidenInt,
   PrintInt32,
   PrintFloat32,
}

impl NodeKind {
   /// Returns whether the node kind is for a leaf node.
   pub fn is_leaf(self) -> bool {
      self < NodeKind::_LastLeaf
   }

   /// Returns whether the node kind is for a branch node.
   pub fn is_branch(self) -> bool {
      self > NodeKind::_LastLeaf
   }

   /// Returns whether the node kind is for a typed unsigned integer node.
   pub fn is_unsigned_integer(self) -> bool {
      matches!(
         self,
         Self::Uint8 | Self::Uint16 | Self::Uint32 | Self::Uint64
      )
   }

   /// Returns whether the node kind is for a typed signed integer node.
   pub fn is_signed_integer(self) -> bool {
      matches!(self, Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
   }

   /// Returns whether the node kind is for a typed integer node, be it signed, or unsigned.
   pub fn is_integer(self) -> bool {
      self.is_unsigned_integer() || self.is_signed_integer()
   }
}

/// Extra node data, for storing inside of the `extra` field.
#[derive(Clone, Debug)]
pub enum NodeData {
   None,
   NodeList(Vec<NodeHandle>),
   // Resolved Integer and Float literals are stored as NodeData, because `usize` doesn't
   // necessarily have to be a `u64` internally. This also avoids some `std::mem::transmute`s for
   // signed integers and floating-point numbers. Overall, I don't think integer literals are
   // accessed often enough to cause a major performance drop from being stored in `NodeData`.
   Uint8(u8),
   Uint16(u16),
   Uint32(u32),
   Uint64(u64),
   Int8(i8),
   Int16(i16),
   Int32(i32),
   Int64(i64),
   Float32(f32),
   Float64(f64),
}

impl NodeData {
   /// Unwraps a node list, or panics if the data aren't a node list.
   pub fn unwrap_node_list(&self) -> &[NodeHandle] {
      if let Self::NodeList(list) = self {
         &list
      } else {
         panic!("unwrap_node_list called on node data that aren't a node list");
      }
   }

   /// Unwraps `Uint` or `Int` data to the largest possible unsigned integer.
   pub fn unwrap_uint(&self) -> u64 {
      match self {
         &Self::Uint8(x) => x as u64,
         &Self::Uint16(x) => x as u64,
         &Self::Uint32(x) => x as u64,
         &Self::Uint64(x) => x,
         &Self::Int8(x) => x as u64,
         &Self::Int16(x) => x as u64,
         &Self::Int32(x) => x as u64,
         &Self::Int64(x) => x as u64,
         _ => panic!("unwrap_uint called on node data that isn't a u?int"),
      }
   }

   /// Unwraps `Float` data to the largest possible float.
   pub fn unwrap_float(&self) -> f64 {
      match self {
         &Self::Float32(x) => x as f64,
         &Self::Float64(x) => x,
         _ => panic!("unwrap_float called on node data that isn't a float"),
      }
   }
}

/// An iterator over node handles in an AST.
pub struct NodeHandles {
   i: usize,
   len: usize,
}

impl Iterator for NodeHandles {
   type Item = NodeHandle;

   fn next(&mut self) -> Option<Self::Item> {
      if self.i < self.len {
         let id = self.i;
         self.i += 1;
         Some(NodeHandle(id))
      } else {
         None
      }
   }
}
