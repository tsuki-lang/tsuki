//! Efficient, tightly-packet AST representation.
//!
//! Nodes in the tsuki AST are represented via indices in a struct of arrays, instead of being
//! scattered across the heap, which helps achieve better cache locality.
//! This also works much better with the borrow checker.

use crate::common::Span;

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
      NodeHandle(id)
   }

   /// Returns the kind of the node behind the given handle.
   #[inline(always)]
   pub fn kind(&self, handle: NodeHandle) -> NodeKind {
      self.kinds[handle.0]
   }

   /// Returns the span of the given handle.
   #[inline(always)]
   pub fn span(&self, handle: NodeHandle) -> &Span {
      &self.spans[handle.0]
   }

   /// Sets the span of the given handle.
   #[inline(always)]
   pub fn set_span(&mut self, handle: NodeHandle, span: Span) {
      self.spans[handle.0] = span;
   }

   /// Returns the first value of the node with the given handle.
   #[inline(always)]
   pub fn first(&self, handle: NodeHandle) -> usize {
      self.first[handle.0]
   }

   /// Returns the first value of the node with the given handle. The stored value is interpreted
   /// as a node handle.
   #[inline(always)]
   pub fn first_handle(&self, handle: NodeHandle) -> NodeHandle {
      NodeHandle(self.first[handle.0])
   }

   /// Sets the first value of a node handle.
   #[inline(always)]
   pub fn set_first(&mut self, handle: NodeHandle, first: usize) {
      self.first[handle.0] = first;
   }

   /// Sets the first value of a node handle. The value stored is a node handle.
   #[inline(always)]
   pub fn set_first_handle(&mut self, handle: NodeHandle, first: NodeHandle) {
      self.first[handle.0] = first.0;
   }

   /// Returns the second value of the node with the given handle.
   #[inline(always)]
   pub fn second(&self, handle: NodeHandle) -> usize {
      self.second[handle.0]
   }

   /// Returns the second value of the node with the given handle. The stored value is interpreted
   /// as a node handle.
   #[inline(always)]
   pub fn second_handle(&self, handle: NodeHandle) -> NodeHandle {
      NodeHandle(self.second[handle.0])
   }

   /// Sets the second value of a node handle.
   #[inline(always)]
   pub fn set_second(&mut self, handle: NodeHandle, second: usize) {
      self.second[handle.0] = second;
   }

   /// Sets the second value of a node handle. The value stored is a node handle.
   #[inline(always)]
   pub fn set_second_handle(&mut self, handle: NodeHandle, second: NodeHandle) {
      self.second[handle.0] = second.0;
   }

   /// Returns a reference to the extra data for the node with the given handle.
   #[inline(always)]
   pub fn extra(&self, handle: NodeHandle) -> &NodeData {
      &self.extra[handle.0]
   }

   /// Sets the extra data for the node with the given handle.
   #[inline(always)]
   pub fn set_extra(&mut self, handle: NodeHandle, data: NodeData) {
      self.extra[handle.0] = data;
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

/// A handle to a single node in the AST. The actual AST is not stored next to the handle for
/// efficiency and appeasing the borrow checker.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct NodeHandle(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NodeKind {
   /// The error node is used for returning a valid, unique node handle in case an error occurs
   /// during parsing.
   Error,

   // Grouping
   StatementList,

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
   // be an expression statement.
   DoExpression,
   DoStatement,
}

/// Extra node data, for storing inside of the `extra` field.
pub enum NodeData {
   None,
   NodeList(Vec<NodeHandle>),
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
