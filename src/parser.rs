//! The tsuki parser.

use std::ops::Range;

use smallvec::{smallvec, SmallVec};

use crate::ast::*;
use crate::common::{Error, ErrorKind, Span};
use crate::lexer::{Associativity, Lexer, Token, TokenKind};

pub type ParseErrors = SmallVec<[Error; 8]>;

/// The parser state.
struct Parser<'l, 's> {
   filename: String,

   ast: Ast,
   lexer: &'l mut Lexer<'s>,

   /// Parsing errors are accumulated into this list of errors. If an error occurs, it's saved
   /// in this vector, and the function that encountered the error creates and returns an error
   /// node.
   ///
   /// This vector is empty if no errors occur during parsing.
   errors: ParseErrors,
}

impl<'l, 's> Parser<'l, 's> {
   /// Creates a new parser from the given filename and source code string.
   fn new(lexer: &'l mut Lexer<'s>) -> Self {
      Self {
         filename: lexer.filename().to_owned(),
         ast: Ast::new(),
         lexer,
         errors: SmallVec::new(),
      }
   }

   /// Emits a parsing error and creates an error node.
   fn error(&mut self, kind: ErrorKind, span: Span) -> NodeHandle {
      self.errors.push(Error {
         // A bit inefficient wrt allocations, but errors don't occur very often.
         filename: self.filename.clone(),
         span,
         kind,
      });
      self.ast.create_node(NodeKind::Error)
   }

   /// Creates a node and copies the `first` and `second` arguments the `first` and
   /// `second` data fields of the node.
   fn create_node_with(&mut self, kind: NodeKind, first: usize, second: usize) -> NodeHandle {
      let handle = self.ast.create_node(kind);
      self.ast.set_first(handle, first);
      self.ast.set_second(handle, second);
      handle
   }

   /// Similar to `create_node_with`, but for storing nodes inside of other nodes.
   fn create_node_with_handles(
      &mut self,
      kind: NodeKind,
      first: NodeHandle,
      second: NodeHandle,
   ) -> NodeHandle {
      let handle = self.ast.create_node(kind);
      self.ast.set_first_handle(handle, first);
      self.ast.set_second_handle(handle, second);
      handle
   }

   /// Creates a node and copies the `start` and `end` of the given range to the `first` and
   /// `second` data fields of the node.
   fn create_node_with_range(&mut self, kind: NodeKind, range: Range<usize>) -> NodeHandle {
      self.create_node_with(kind, range.start, range.end)
   }

   /// Parses a literal token and returns the node corresponding to it.
   fn parse_literal(&mut self, token: Token) -> NodeHandle {
      let literal = match token.kind {
         TokenKind::Nil => self.ast.create_node(NodeKind::Nil),
         TokenKind::True => self.ast.create_node(NodeKind::True),
         TokenKind::False => self.ast.create_node(NodeKind::False),
         TokenKind::Integer(r) => self.create_node_with_range(NodeKind::Integer, r),
         TokenKind::Float(r) => self.create_node_with_range(NodeKind::Float, r),
         TokenKind::Atom(r) => self.create_node_with_range(NodeKind::Atom, r),
         TokenKind::Character(c) => self.create_node_with(NodeKind::Character, c as usize, 0),
         TokenKind::String(r) => self.create_node_with_range(NodeKind::String, r),
         _ => unreachable!(),
      };
      self.ast.set_span(literal, token.span);
      literal
   }

   /// Turns an identifier token into a node.
   fn create_identifier(&mut self, token: Token) -> NodeHandle {
      if let TokenKind::Identifier(range) = token.kind {
         let handle = self.create_node_with_range(NodeKind::Identifier, range);
         self.ast.set_span(handle, token.span);
         handle
      } else {
         panic!("token must be an identifier")
      }
   }

   /// Parses a prefix from the given token.
   fn parse_prefix(&mut self, token: Token) -> NodeHandle {
      let span = token.span.clone();
      match token.kind {
         | TokenKind::Nil
         | TokenKind::True
         | TokenKind::False
         | TokenKind::Integer(..)
         | TokenKind::Float(..)
         | TokenKind::Atom(..)
         | TokenKind::Character(..)
         | TokenKind::String(..) => self.parse_literal(token),
         TokenKind::Identifier(..) => self.create_identifier(token),
         _ => self.error(ErrorKind::UnexpectedPrefixToken(token), span),
      }
   }

   /// Parses the right-hand side of an infix operator.
   fn infix_operator(
      &mut self,
      lhs: NodeHandle,
      token: Token,
      node_kind: NodeKind,
   ) -> Result<NodeHandle, Error> {
      let right_associative = token.kind.associativity() == Associativity::Right;
      let precedence = token.kind.precedence() - right_associative as i8;
      let rhs = self.parse_expression(precedence)?;
      Ok(self.create_node_with_handles(node_kind, lhs, rhs))
   }

   /// Parses an infix, given the left node and infix token.
   fn parse_infix(&mut self, left: NodeHandle, token: Token) -> Result<NodeHandle, Error> {
      let span = token.span.clone();
      Ok(match token.kind {
         // What do you sacrifice for efficiency.
         // Honestly it's such a shame that repr(u8) only works for C-like enums that do not have
         // data associated with them. Who knows, maybe someday the lexer will become more
         // data-oriented just like the parser, but for now that's just the way it is.
         // I feel like by the point I get to big optimizations like that tsuki will already be
         // self-hosted.
         TokenKind::Dot => self.infix_operator(left, token, NodeKind::Dot)?,
         TokenKind::Plus => self.infix_operator(left, token, NodeKind::Plus)?,
         TokenKind::Minus => self.infix_operator(left, token, NodeKind::Minus)?,
         TokenKind::Mul => self.infix_operator(left, token, NodeKind::Mul)?,
         TokenKind::Div => self.infix_operator(left, token, NodeKind::Div)?,
         TokenKind::Pow => self.infix_operator(left, token, NodeKind::Pow)?,
         TokenKind::Tilde => self.infix_operator(left, token, NodeKind::Tilde)?,
         TokenKind::Lshift => self.infix_operator(left, token, NodeKind::Lshift)?,
         TokenKind::Rshift => self.infix_operator(left, token, NodeKind::Rshift)?,
         TokenKind::BitAnd => self.infix_operator(left, token, NodeKind::BitAnd)?,
         TokenKind::BitOr => self.infix_operator(left, token, NodeKind::BitOr)?,
         TokenKind::BitXor => self.infix_operator(left, token, NodeKind::BitXor)?,
         TokenKind::Equal => self.infix_operator(left, token, NodeKind::Equal)?,
         TokenKind::NotEqual => self.infix_operator(left, token, NodeKind::NotEqual)?,
         TokenKind::Less => self.infix_operator(left, token, NodeKind::Less)?,
         TokenKind::Greater => self.infix_operator(left, token, NodeKind::Greater)?,
         TokenKind::LessEqual => self.infix_operator(left, token, NodeKind::LessEqual)?,
         TokenKind::GreaterEqual => self.infix_operator(left, token, NodeKind::GreaterEqual)?,
         TokenKind::Pointer => self.infix_operator(left, token, NodeKind::Pointer)?,
         TokenKind::Check => self.infix_operator(left, token, NodeKind::Check)?,
         TokenKind::Unwrap => self.infix_operator(left, token, NodeKind::Unwrap)?,
         TokenKind::UpTo => self.infix_operator(left, token, NodeKind::UpTo)?,
         TokenKind::UpToInclusive => self.infix_operator(left, token, NodeKind::UpToInclusive)?,
         TokenKind::Assign => self.infix_operator(left, token, NodeKind::Assign)?,
         TokenKind::PlusAssign => self.infix_operator(left, token, NodeKind::PlusAssign)?,
         TokenKind::MinusAssign => self.infix_operator(left, token, NodeKind::MinusAssign)?,
         TokenKind::MulAssign => self.infix_operator(left, token, NodeKind::MulAssign)?,
         TokenKind::DivAssign => self.infix_operator(left, token, NodeKind::DivAssign)?,
         TokenKind::Push => self.infix_operator(left, token, NodeKind::Push)?,
         _ => self.error(ErrorKind::UnexpectedInfixToken(token), span),
      })
   }

   /// Parses a full expression, where infix operators must have at least the given
   /// precedence level.
   fn parse_expression(&mut self, precedence: i8) -> Result<NodeHandle, Error> {
      let mut token = self.lexer.next()?;
      let mut left = self.parse_prefix(token);

      while precedence < self.lexer.peek()?.kind.precedence() {
         token = self.lexer.next()?;
         if token.kind == TokenKind::Eof {
            break
         }
         left = self.parse_infix(left, token)?;
      }

      Ok(left)
   }

   /// Parses the entire source code of a module.
   fn parse_module(&mut self) -> Result<NodeHandle, Error> {
      Ok(self.parse_expression(0)?)
   }
}

/// Parses a source file with the given filename and source code. On success, returns the AST and
/// the handle to the root node. On failure, returns a list of errors.
pub fn parse(lexer: &mut Lexer) -> Result<(Ast, NodeHandle), ParseErrors> {
   let mut parser = Parser::new(lexer);
   let root_node = match parser.parse_module() {
      Ok(node) => node,
      Err(error) => return Err(smallvec![error]),
   };

   if parser.errors.len() > 0 {
      Err(parser.errors)
   } else {
      Ok((parser.ast, root_node))
   }
}
