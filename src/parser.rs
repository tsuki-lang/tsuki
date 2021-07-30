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

   /// Emits a parsing error.
   fn emit_error(&mut self, kind: ErrorKind, span: Span) {
      self.errors.push(Error {
         // A bit inefficient wrt allocations, but errors don't occur very often.
         filename: self.filename.clone(),
         span,
         kind,
      });
   }

   /// Emits a parsing error and creates an error node.
   #[must_use]
   fn error(&mut self, kind: ErrorKind, span: Span) -> NodeHandle {
      self.emit_error(kind, span);
      self.ast.create_node(NodeKind::Error)
   }

   /// Checks if the next token's kind matches `kind`. If not, emits an `error` of the given kind
   /// and returns `None`. Otherwise returns `Some` with the token.
   fn expect_token(&mut self, kind: TokenKind, error: ErrorKind) -> Result<Option<Token>, Error> {
      let token = self.lexer.next()?;
      if token.kind != kind {
         self.emit_error(error, token.span);
         Ok(None)
      } else {
         Ok(Some(token))
      }
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

   /// Similar to `create_node_with`, but for storing a single node inside of another node.
   fn create_node_with_handle(&mut self, kind: NodeKind, child: NodeHandle) -> NodeHandle {
      let handle = self.ast.create_node(kind);
      self.ast.set_first_handle(handle, child);
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

   /// Parses a list of comma-separated values.
   /// `start` is the starting token, used for error reporting.
   /// `end` is the kind of ending token that should be matched against.
   /// `next` is the _parsing rule_ - a function that, when called, parses the next element to be
   /// added to `dest`.
   fn parse_comma_separated(
      &mut self,
      dest: &mut Vec<NodeHandle>,
      start: &Token,
      end: TokenKind,
      mut next: impl FnMut(&mut Self) -> Result<NodeHandle, Error>,
   ) -> Result<Token, Error> {
      loop {
         match &self.lexer.peek()?.kind {
            TokenKind::Eof => {
               let error = ErrorKind::MissingClosingToken(end.clone());
               dest.push(self.error(error, start.span.clone()));
               return self.lexer.next();
            }
            kind if *kind == end => {
               return self.lexer.next();
            }
            _ => (),
         }
         dest.push(next(self)?);
         match self.lexer.next()? {
            Token {
               kind: TokenKind::Comma,
               ..
            } => (),
            t if t.kind == end => return Ok(t),
            other => {
               let error = ErrorKind::MissingCommaOrClosingToken(end.clone());
               dest.push(self.error(error, other.span.clone()));
               return Ok(other);
            }
         }
      }
   }

   /// Creates a node for a nullary operator.
   fn nullary_operator(&mut self, operator: Token, node_kind: NodeKind) -> NodeHandle {
      let node = self.create_node_with(node_kind, 0, 0);
      self.ast.set_span(node, operator.span);
      node
   }

   /// Parses the right-hand side of a unary prefix operator.
   fn unary_prefix(&mut self, operator: Token, node_kind: NodeKind) -> Result<NodeHandle, Error> {
      let operator_span = operator.span.clone();
      let token = self.lexer.next()?;
      let right = self.parse_prefix(token)?;
      let node = self.create_node_with_handle(node_kind, right);
      self.ast.set_span(node, Span::join(&operator_span, &self.ast.span(right)));
      Ok(node)
   }

   /// Parses a prefix from the given token.
   fn parse_prefix(&mut self, token: Token) -> Result<NodeHandle, Error> {
      let span = token.span.clone();
      Ok(match token.kind {
         | TokenKind::Nil
         | TokenKind::True
         | TokenKind::False
         | TokenKind::Integer(..)
         | TokenKind::Float(..)
         | TokenKind::Atom(..)
         | TokenKind::Character(..)
         | TokenKind::String(..) => self.parse_literal(token),
         TokenKind::Identifier(..) => self.create_identifier(token),
         TokenKind::UpTo => self.nullary_operator(token, NodeKind::FullRange),
         TokenKind::Not => self.unary_prefix(token, NodeKind::Not)?,
         TokenKind::Minus => self.unary_prefix(token, NodeKind::Neg)?,
         TokenKind::Tilde => self.unary_prefix(token, NodeKind::BitNot)?,
         TokenKind::Dot => self.unary_prefix(token, NodeKind::Member)?,
         TokenKind::Pointer => self.unary_prefix(token, NodeKind::Ref)?,
         _ => self.error(ErrorKind::UnexpectedPrefixToken(token), span),
      })
   }

   /// Creates a node from the left-hand side and operator token of a unary postfix operator.
   fn unary_postfix(&mut self, lhs: NodeHandle, token: Token, kind: NodeKind) -> NodeHandle {
      let node = self.create_node_with_handle(kind, lhs);
      self.ast.set_span(node, Span::join(self.ast.span(lhs), &token.span));
      node
   }

   /// Parses the right-hand side of an infix operator.
   fn binary_operator(
      &mut self,
      lhs: NodeHandle,
      token: Token,
      node_kind: NodeKind,
   ) -> Result<NodeHandle, Error> {
      let right_associative = token.kind.associativity() == Associativity::Right;
      let precedence = token.kind.precedence() - right_associative as i8;
      let rhs = self.parse_expression(precedence)?;
      let node = self.create_node_with_handles(node_kind, lhs, rhs);
      self.ast.set_span(node, Span::join(self.ast.span(lhs), self.ast.span(rhs)));
      Ok(node)
   }

   /// Parses the right-hand side of a function call.
   fn parse_call(&mut self, left: NodeHandle, lparen: Token) -> Result<NodeHandle, Error> {
      let node = self.create_node_with_handle(NodeKind::Call, left);
      let mut args = Vec::new();
      let rparen = self.parse_comma_separated(&mut args, &lparen, TokenKind::RightParen, |p| {
         p.parse_expression(0)
      })?;
      self.ast.set_extra(node, NodeData::NodeList(args));
      self.ast.set_span(node, Span::join(&lparen.span, &rparen.span));
      Ok(node)
   }

   /// Parses an indexing operator.
   fn parse_index(
      &mut self,
      left: NodeHandle,
      lparen: Token,
      rparen_kind: TokenKind,
      node_kind: NodeKind,
   ) -> Result<NodeHandle, Error> {
      let right = self.parse_expression(0)?;
      let error = ErrorKind::MissingClosingToken(rparen_kind.clone());
      let maybe_rparen = self.expect_token(rparen_kind, error)?;
      if let Some(rparen) = maybe_rparen {
         let node = self.create_node_with_handles(node_kind, left, right);
         self.ast.set_span(node, Span::join(self.ast.span(left), &rparen.span));
         Ok(node)
      } else {
         Ok(self.ast.create_node(NodeKind::Error))
      }
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
         // -----
         // Binary operators
         TokenKind::Dot => self.binary_operator(left, token, NodeKind::Dot)?,
         TokenKind::Plus => self.binary_operator(left, token, NodeKind::Plus)?,
         TokenKind::Minus => self.binary_operator(left, token, NodeKind::Minus)?,
         TokenKind::Mul => self.binary_operator(left, token, NodeKind::Mul)?,
         TokenKind::Div => self.binary_operator(left, token, NodeKind::Div)?,
         TokenKind::Pow => self.binary_operator(left, token, NodeKind::Pow)?,
         TokenKind::Tilde => self.binary_operator(left, token, NodeKind::Concat)?,
         TokenKind::Lshift => self.binary_operator(left, token, NodeKind::Lshift)?,
         TokenKind::Rshift => self.binary_operator(left, token, NodeKind::Rshift)?,
         TokenKind::BitAnd => self.binary_operator(left, token, NodeKind::BitAnd)?,
         TokenKind::BitOr => self.binary_operator(left, token, NodeKind::BitOr)?,
         TokenKind::BitXor => self.binary_operator(left, token, NodeKind::BitXor)?,
         TokenKind::Equal => self.binary_operator(left, token, NodeKind::Equal)?,
         TokenKind::NotEqual => self.binary_operator(left, token, NodeKind::NotEqual)?,
         TokenKind::Less => self.binary_operator(left, token, NodeKind::Less)?,
         TokenKind::Greater => self.binary_operator(left, token, NodeKind::Greater)?,
         TokenKind::LessEqual => self.binary_operator(left, token, NodeKind::LessEqual)?,
         TokenKind::GreaterEqual => self.binary_operator(left, token, NodeKind::GreaterEqual)?,
         TokenKind::UpTo => self.binary_operator(left, token, NodeKind::UpTo)?,
         TokenKind::UpToInclusive => self.binary_operator(left, token, NodeKind::UpToInclusive)?,
         TokenKind::Assign => self.binary_operator(left, token, NodeKind::Assign)?,
         TokenKind::PlusAssign => self.binary_operator(left, token, NodeKind::PlusAssign)?,
         TokenKind::MinusAssign => self.binary_operator(left, token, NodeKind::MinusAssign)?,
         TokenKind::MulAssign => self.binary_operator(left, token, NodeKind::MulAssign)?,
         TokenKind::DivAssign => self.binary_operator(left, token, NodeKind::DivAssign)?,
         TokenKind::Push => self.binary_operator(left, token, NodeKind::Push)?,
         // Postfix unary operators
         TokenKind::Pointer => self.unary_postfix(left, token, NodeKind::Deref),
         TokenKind::Check => self.unary_postfix(left, token, NodeKind::Check),
         TokenKind::Unwrap => self.unary_postfix(left, token, NodeKind::Unwrap),
         // Other operators
         TokenKind::LeftParen => self.parse_call(left, token)?,
         TokenKind::LeftBracket => {
            self.parse_index(left, token, TokenKind::RightBracket, NodeKind::Index)?
         }
         TokenKind::LeftBrace => {
            self.parse_index(left, token, TokenKind::RightBrace, NodeKind::IndexAlt)?
         }
         _ => self.error(ErrorKind::UnexpectedInfixToken(token), span),
      })
   }

   /// Parses a full expression, where infix operators must have at least the given
   /// precedence level.
   fn parse_expression(&mut self, precedence: i8) -> Result<NodeHandle, Error> {
      let mut token = self.lexer.next()?;
      let mut left = self.parse_prefix(token)?;

      while precedence < self.lexer.peek()?.kind.precedence() {
         token = self.lexer.next()?;
         if token.kind == TokenKind::Eof {
            break;
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
