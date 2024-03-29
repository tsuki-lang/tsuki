//! The parser. Reads tokens from the lexer and emits untyped AST.

use std::ops::Range;

use smallvec::SmallVec;

use crate::ast::*;
use crate::common::{Error, ErrorKind, Errors, Span};
use crate::lexer::{Associativity, Lexer, Token, TokenKind};

/// The parser state.
struct Parser<'s> {
   lexer: Lexer<'s>,
   ast: Ast,

   /// Parsing errors are accumulated into this list of errors. If an error occurs, it's saved
   /// in this vector, and the function that encountered the error creates and returns an error
   /// node.
   ///
   /// This vector is empty if no errors occur during parsing.
   errors: Errors,
}

impl<'s> Parser<'s> {
   /*
    * Common
    */

   /// Creates a new parser from the given filename and source code string.
   fn new(lexer: Lexer<'s>) -> Self {
      Self {
         lexer,
         ast: Ast::new(),
         errors: SmallVec::new(),
      }
   }

   /// Emits a parsing error.
   fn emit_error(&mut self, kind: ErrorKind, span: Span) {
      self.errors.push(Error {
         // A bit inefficient wrt allocations, but errors don't occur very often.
         filename: self.lexer.file().path.clone(),
         span,
         kind,
      });
   }

   /// Emits a parsing error and creates an error node.
   #[must_use]
   fn error(&mut self, kind: ErrorKind, span: Span) -> NodeId {
      self.emit_error(kind, span);
      self.ast.create_node(NodeKind::Error)
   }

   /// Peeks at the next token and checks if its kind matches `kind`.
   ///
   /// If yes, consumes the token and returns `Some(token)`.
   /// Otherwise returns `None`.
   fn match_token(&mut self, kind: TokenKind) -> Result<Option<Token>, Error> {
      let token = self.lexer.peek()?;
      if token.kind != kind {
         Ok(None)
      } else {
         let token = self.lexer.next()?;
         Ok(Some(token))
      }
   }

   /// Checks if the next token's kind matches `kind`. If not, emits an `error` of the given kind
   /// and returns `None`. Otherwise returns `Some` with the token.
   fn expect_token(
      &mut self,
      kind: TokenKind,
      error: impl FnOnce(Token) -> ErrorKind,
   ) -> Result<Option<Token>, Error> {
      let token = self.lexer.next()?;
      if token.kind != kind {
         let span = token.span.clone();
         self.emit_error(error(token), span);
         Ok(None)
      } else {
         Ok(Some(token))
      }
   }

   /// Creates a node and copies the `first` and `second` arguments the `first` and
   /// `second` data fields of the node.
   fn create_node_with(&mut self, kind: NodeKind, first: usize, second: usize) -> NodeId {
      let handle = self.ast.create_node(kind);
      self.ast.set_first(handle, first);
      self.ast.set_second(handle, second);
      handle
   }

   /// Similar to `create_node_with`, but for storing nodes inside of other nodes.
   fn create_node_with_handles(&mut self, kind: NodeKind, first: NodeId, second: NodeId) -> NodeId {
      let handle = self.ast.create_node(kind);
      self.ast.set_first_handle(handle, first);
      self.ast.set_second_handle(handle, second);
      handle
   }

   /// Similar to `create_node_with`, but for storing a single node inside of another node.
   fn create_node_with_handle(&mut self, kind: NodeKind, child: NodeId) -> NodeId {
      let handle = self.ast.create_node(kind);
      self.ast.set_first_handle(handle, child);
      handle
   }

   /// Creates a node and copies the `start` and `end` of the given range to the `first` and
   /// `second` data fields of the node.
   fn create_node_with_range(&mut self, kind: NodeKind, range: Range<usize>) -> NodeId {
      self.create_node_with(kind, range.start, range.end)
   }

   /// Creates a span spanning all the nodes in the given slice.
   fn span_all_nodes(&self, nodes: &[NodeId]) -> Span {
      if nodes.len() >= 2 {
         Span::join(self.ast.span(nodes[0]), self.ast.span(nodes[1]))
      } else if nodes.len() == 1 {
         self.ast.span(nodes[0]).clone()
      } else {
         Span::new()
      }
   }

   /// Turns an identifier token into a node.
   fn create_identifier(&mut self, token: Token) -> NodeId {
      if let TokenKind::Identifier(range) = token.kind {
         let handle = self.create_node_with_range(NodeKind::Identifier, range);
         self.ast.set_span(handle, token.span);
         handle
      } else {
         self.error(ErrorKind::IdentifierExpected(token.kind), token.span)
      }
   }

   /// Unwraps the `Option<Token>`, such that when it's `None`, the next token is read and returned.
   /// Otherwise the existing token is used.
   fn some_or_next(&mut self, maybe_token: Option<Token>) -> Result<Token, Error> {
      match maybe_token {
         Some(token) => Ok(token),
         None => self.lexer.next(),
      }
   }

   /// Parses a list of comma-separated values.
   ///
   /// `start` is the starting token, used for error reporting.
   /// `end` is the kind of ending token that should be matched against.
   /// `next` is the _parsing rule_ - a function that, when called, parses the next element to be
   /// added to `dest`.
   ///
   /// Returns the terminating token, which might _not_ be of `end` if erroneous input is
   /// encountered.
   fn parse_comma_separated(
      &mut self,
      dest: &mut Vec<NodeId>,
      start: &Token,
      end: TokenKind,
      mut next: impl FnMut(&mut Self) -> Result<NodeId, Error>,
   ) -> Result<Token, Error> {
      loop {
         match &self.lexer.peek()?.kind {
            TokenKind::Eof => {
               let error = ErrorKind::MissingClosingToken(end.clone(), start.clone());
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
               let error = ErrorKind::ExpectedCommaOrClosingToken(end.clone(), start.clone());
               dest.push(self.error(error, other.span.clone()));
               return Ok(other);
            }
         }
      }
   }

   /// Parses rules in an indented block.
   ///
   /// Each node parsed by a rule must be separated by a line break.
   ///
   /// `parent_token` specifies which token from which the indent level the tokens in the block must
   /// exceed should be taken from.
   fn parse_indented_block(
      &mut self,
      dest: &mut Vec<NodeId>,
      parent_token: &Token,
      mut next: impl FnMut(&mut Self) -> Result<NodeId, Error>,
      missing_line_break_error: impl FnOnce() -> ErrorKind,
   ) -> Result<(), Error> {
      let indent_level = {
         let next = self.lexer.peek()?;
         if next.indent_level <= parent_token.indent_level {
            let span = next.span.clone();
            self.emit_error(
               ErrorKind::IndentedBlockExpected(parent_token.indent_level),
               span,
            );
            return Ok(());
         }
         next.indent_level
      };
      let mut previous_line = parent_token.line();
      while self.lexer.peek()?.indent_level == indent_level {
         let next_token = self.lexer.peek()?;
         let line = next_token.line();
         if line == previous_line {
            let span = next_token.span.clone();
            self.emit_error(missing_line_break_error(), span);
            break;
         }
         dest.push(next(self)?);
         previous_line = line;
      }
      Ok(())
   }

   /*
    * Types
    */

   /// Parses a type.
   fn parse_type(&mut self) -> Result<NodeId, Error> {
      let token = self.lexer.next()?;
      Ok(match token.kind {
         TokenKind::Identifier(..) => self.create_identifier(token),
         other => self.error(ErrorKind::UnexpectedTypeToken(other), token.span),
      })
   }

   /// Parses a type name (in a declaration).
   ///
   /// A type name can have generic arguments attached to it.
   fn parse_type_name(&mut self) -> Result<NodeId, Error> {
      let identifier = self.lexer.next()?;
      let name = self.create_identifier(identifier);
      let type_name = self.create_node_with_handle(NodeKind::TypeName, name);
      self.ast.set_span(type_name, self.ast.span(name).clone());
      // TODO: generic parameters
      Ok(type_name)
   }

   /*
    * Expressions
    */

   /// Parses a literal token and returns the node corresponding to it.
   fn parse_literal(&mut self, token: Token) -> NodeId {
      let literal = match token.kind {
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

   /// Parses expressions in parentheses.
   fn parentheses(&mut self, _left: Token) -> Result<NodeId, Error> {
      // TODO: tuples and unit literal
      let inner = self.parse_expression(0)?;
      let _ = self.expect_token(TokenKind::RightParen, |t| {
         ErrorKind::MissingClosingToken(TokenKind::RightParen, t)
      })?;
      Ok(inner)
   }

   /// Creates a node for a nullary operator.
   fn nullary_operator(&mut self, operator: Token, node_kind: NodeKind) -> NodeId {
      let node = self.create_node_with(node_kind, 0, 0);
      self.ast.set_span(node, operator.span);
      node
   }

   /// Parses the right-hand side of a unary prefix operator.
   fn unary_prefix(&mut self, operator: Token, node_kind: NodeKind) -> Result<NodeId, Error> {
      let operator_span = operator.span.clone();
      let token = self.lexer.next()?;
      let right = self.parse_prefix(token)?;
      let node = self.create_node_with_handle(node_kind, right);
      self.ast.set_span(node, Span::join(&operator_span, &self.ast.span(right)));
      Ok(node)
   }

   /// Parses a prefix from the given token.
   fn parse_prefix(&mut self, token: Token) -> Result<NodeId, Error> {
      let span = token.span.clone();
      Ok(match token.kind {
         // Literals
         | TokenKind::True
         | TokenKind::False
         | TokenKind::Integer(..)
         | TokenKind::Float(..)
         | TokenKind::Atom(..)
         | TokenKind::Character(..)
         | TokenKind::String(..) => self.parse_literal(token),
         TokenKind::Identifier(..) => self.create_identifier(token),

         // Parentheses
         TokenKind::LeftParen => self.parentheses(token)?,

         // Nullary operators
         TokenKind::UpTo => self.nullary_operator(token, NodeKind::FullRange),

         // Unary prefix operators
         TokenKind::Not => self.unary_prefix(token, NodeKind::Not)?,
         TokenKind::Minus => self.unary_prefix(token, NodeKind::Neg)?,
         TokenKind::Tilde => self.unary_prefix(token, NodeKind::BitNot)?,
         TokenKind::Dot => self.unary_prefix(token, NodeKind::Member)?,
         TokenKind::Pointer => self.unary_prefix(token, NodeKind::Ref)?,

         // Control flow structures
         TokenKind::Do => self.parse_do_expression(Some(token))?,
         TokenKind::If => self.parse_if_expression(Some(token))?,
         TokenKind::Break => self.parse_break(token)?,
         TokenKind::Return => self.parse_return(token)?,

         // Unknown tokens
         _ => self.error(ErrorKind::UnexpectedPrefixToken(token.kind), span),
      })
   }

   /// Parses a prefix `do` expression (or statement).
   fn parse_do_expression(&mut self, token: Option<Token>) -> Result<NodeId, Error> {
      let token = self.some_or_next(token)?;
      let node = self.ast.create_node(NodeKind::Do);
      let mut statements = Vec::new();
      self.parse_indented_block(
         &mut statements,
         &token,
         |p| p.parse_statement(),
         || ErrorKind::MissingLineBreakAfterStatement,
      )?;
      self.ast.set_span(
         node,
         Span::join(&token.span, &self.span_all_nodes(&statements)),
      );
      self.ast.set_extra(node, NodeData::NodeList(statements));
      Ok(node)
   }

   /// Parses an `if` expression or statement.
   fn parse_if_expression(&mut self, token: Option<Token>) -> Result<NodeId, Error> {
      let mut branch_token = self.some_or_next(token)?;
      let mut branches = Vec::new();
      let mut is_elif = true;
      loop {
         // If it's an else, the condition is always the null node.
         let condition = if is_elif {
            self.parse_expression(0)?
         } else {
            NodeId::null()
         };
         // The branch body can be either `->` followed by an expression, or a block of code.
         let mut branch_body = Vec::new();
         if let Some(..) = self.match_token(TokenKind::Then)? {
            branch_body.push(self.parse_expression(0)?);
         } else {
            self.parse_indented_block(
               &mut branch_body,
               &branch_token,
               |p| p.parse_statement(),
               || ErrorKind::MissingLineBreakAfterStatement,
            )?;
         }
         // Construct the branch.
         let branch = self.ast.create_node(if is_elif {
            NodeKind::IfBranch
         } else {
            NodeKind::ElseBranch
         });
         let body_span = self.span_all_nodes(&branch_body);
         let span = if is_elif {
            Span::join(self.ast.span(condition), &body_span)
         } else {
            body_span
         };
         self.ast.set_span(branch, span);
         self.ast.set_first_handle(branch, condition);
         self.ast.set_extra(branch, NodeData::NodeList(branch_body));
         branches.push(branch);
         // If the current branch is an `elif` branch, look ahead for the next one.
         if is_elif {
            if let Some(token) = self.match_token(TokenKind::Elif)? {
               branch_token = token;
            } else if let Some(token) = self.match_token(TokenKind::Else)? {
               branch_token = token;
               is_elif = false;
            } else {
               break;
            }
         } else {
            break;
         }
      }
      let node = self.ast.create_node(NodeKind::If);
      self.ast.set_span(
         node,
         Span::join(&branch_token.span, &self.span_all_nodes(&branches)),
      );
      self.ast.set_extra(node, NodeData::NodeList(branches));

      Ok(node)
   }

   /// Parses a `break` expression.
   fn parse_break(&mut self, token: Token) -> Result<NodeId, Error> {
      let node = self.ast.create_node(NodeKind::Break);
      self.ast.set_span(node, token.span);
      Ok(node)
   }

   /// Parses a `return` expression.
   fn parse_return(&mut self, token: Token) -> Result<NodeId, Error> {
      let next = self.lexer.peek()?;
      let right = if next.line() > token.line() {
         // If the next token is placed on a new line, then we treat this `return` as one without
         // an expression to return.
         NodeId::null()
      } else {
         // Otherwise, we parse an expression.
         self.parse_expression(0)?
      };
      let node = self.create_node_with_handle(NodeKind::Return, right);
      self.ast.set_span(node, Span::join(&token.span, self.ast.span(right)));
      Ok(node)
   }

   /// Creates a node from the left-hand side and operator token of a unary postfix operator.
   fn unary_postfix(&mut self, lhs: NodeId, token: Token, kind: NodeKind) -> NodeId {
      let node = self.create_node_with_handle(kind, lhs);
      self.ast.set_span(node, Span::join(self.ast.span(lhs), &token.span));
      node
   }

   /// Parses the right-hand side of an infix operator.
   fn binary_operator(
      &mut self,
      lhs: NodeId,
      token: Token,
      node_kind: NodeKind,
   ) -> Result<NodeId, Error> {
      let right_associative = token.kind.associativity() == Associativity::Right;
      let precedence = token.kind.precedence() - right_associative as i8;
      let rhs = self.parse_expression(precedence)?;
      let node = self.create_node_with_handles(node_kind, lhs, rhs);
      self.ast.set_span(node, Span::join(self.ast.span(lhs), self.ast.span(rhs)));
      Ok(node)
   }

   /// Parses the right-hand side of a function call.
   fn parse_call(&mut self, left: NodeId, lparen: Token) -> Result<NodeId, Error> {
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
      left: NodeId,
      lparen: Token,
      rparen_kind: TokenKind,
      node_kind: NodeKind,
   ) -> Result<NodeId, Error> {
      let right = self.parse_expression(0)?;
      let maybe_rparen = self.expect_token(rparen_kind.clone(), |_| {
         ErrorKind::MissingClosingToken(rparen_kind, lparen.clone())
      })?;
      if let Some(rparen) = maybe_rparen {
         let node = self.create_node_with_handles(node_kind, left, right);
         self.ast.set_span(node, Span::join(self.ast.span(left), &rparen.span));
         Ok(node)
      } else {
         Ok(self.ast.create_node(NodeKind::Error))
      }
   }

   /// Parses an infix, given the left node and infix token.
   fn parse_infix(&mut self, left: NodeId, token: Token) -> Result<NodeId, Error> {
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

         _ => self.error(ErrorKind::UnexpectedInfixToken(token.kind), span),
      })
   }

   /// Parses a full expression, where infix operators must have at least the given
   /// precedence level.
   fn parse_expression(&mut self, precedence: i8) -> Result<NodeId, Error> {
      let mut token = self.lexer.next()?;
      let expr_indent_level = token.indent_level;
      let expr_line = token.line();
      let mut left = self.parse_prefix(token)?;

      while precedence < self.lexer.peek()?.kind.precedence() {
         // Operators continuing an expression on the next line must have their indent level greater
         // than the first token of the expression.
         let next = self.lexer.peek()?;
         if next.line() > expr_line && next.indent_level <= expr_indent_level {
            break;
         }

         token = self.lexer.next()?;
         if token.kind == TokenKind::Eof {
            break;
         }
         left = self.parse_infix(left, token)?;
      }

      Ok(left)
   }

   /*
    * Statements
    */

   /// Parses a pass (`_`) statement.
   fn parse_pass(&mut self) -> Result<NodeId, Error> {
      let token = self.lexer.next()?;
      let node = self.ast.create_node(NodeKind::Pass);
      self.ast.set_span(node, token.span);
      Ok(node)
   }

   /// Parses a `while` loop.
   fn parse_while_loop(&mut self) -> Result<NodeId, Error> {
      let token = self.lexer.next()?;
      let condition = self.parse_expression(0)?;
      let node = self.create_node_with_handle(NodeKind::While, condition);
      let mut statements = Vec::new();
      if let Some(..) = self.match_token(TokenKind::Then)? {
         statements.push(self.parse_expression(0)?);
      } else {
         self.parse_indented_block(
            &mut statements,
            &token,
            |p| p.parse_statement(),
            || ErrorKind::MissingLineBreakAfterStatement,
         )?;
      }
      self.ast.set_span(
         node,
         Span::join(&token.span, &self.span_all_nodes(&statements)),
      );
      self.ast.set_extra(node, NodeData::NodeList(statements));
      Ok(node)
   }

   /// Parses a comma-separated parameter list, like `(a: Int, b: Int, c, d: Int)`.
   fn parse_parameter_list(
      &mut self,
      dest: &mut Vec<NodeId>,
      start: &Token,
      end: TokenKind,
   ) -> Result<Token, Error> {
      self.parse_comma_separated(dest, start, end, |p| {
         let mut list = Vec::new();
         let type_node;
         loop {
            let name_token = p.lexer.next()?;
            let name = p.create_identifier(name_token);
            list.push(name);
            let next_token = p.lexer.next()?;
            match next_token.kind {
               TokenKind::Comma => {
                  // Continue the loop and parse another parameter name.
               }
               TokenKind::Colon => {
                  type_node = p.parse_type()?;
                  break;
               }
               other => {
                  return Ok(p.error(ErrorKind::ExpectedCommaOrColon(other), next_token.span));
               }
            }
         }
         let node = p.ast.create_node(NodeKind::NamedParameters);
         p.ast.set_span(
            node,
            Span::join(&p.span_all_nodes(&list), p.ast.span(type_node)),
         );
         p.ast.set_first_handle(node, type_node);
         p.ast.set_extra(node, NodeData::NodeList(list));
         Ok(node)
      })
   }

   /// Parses a pragma application.
   fn parse_pragma(&mut self) -> Result<NodeId, Error> {
      let identifier = self.lexer.next()?;
      let name = self.create_identifier(identifier);
      if let Some(left_paren) = self.expect_token(TokenKind::LeftParen, |token| {
         ErrorKind::PragmaArgsExpected(token.kind)
      })? {
         let mut arguments = Vec::new();
         self.parse_comma_separated(&mut arguments, &left_paren, TokenKind::RightParen, |p| {
            p.parse_expression(0)
         })?;
         let pragma = self.create_node_with_handle(NodeKind::Call, name);
         self.ast.set_span(
            pragma,
            Span::join(self.ast.span(name), &self.span_all_nodes(&arguments)),
         );
         self.ast.set_extra(pragma, NodeData::NodeList(arguments));
         Ok(pragma)
      } else {
         Ok(self.ast.create_node(NodeKind::Error))
      }
   }

   /// Attempts to parse pragmas, and if the `::` is present, wraps the given node in a `Pragmas`
   /// node and stores all the pragmas in the new node's extra.
   fn parse_pragmas(&mut self, node: NodeId) -> Result<NodeId, Error> {
      if let Some(colons) = self.match_token(TokenKind::Colons)? {
         self.ast.wrap(node, NodeKind::Pragmas);
         let mut pragmas = Vec::new();
         let first_pragma = self.parse_pragma()?;
         pragmas.push(first_pragma);
         while let Some(..) = self.match_token(TokenKind::Comma)? {
            pragmas.push(self.parse_pragma()?);
         }
         self.ast.set_span(
            node,
            Span::join(&colons.span, &self.span_all_nodes(&pragmas)),
         );
         self.ast.set_extra(node, NodeData::NodeList(pragmas));
      }
      Ok(node)
   }

   /// Parses a `val` or `var` declaration.
   fn parse_variable_declaration(&mut self) -> Result<NodeId, Error> {
      let var_token = self.lexer.next()?;
      let node_kind = match var_token.kind {
         TokenKind::Val => NodeKind::Val,
         TokenKind::Var => NodeKind::Var,
         _ => unreachable!(),
      };

      // TODO: Destructuring.
      // Similarly to a discarding assignment `val _ = x`, these should use a separate node kind for
      // the name node.
      let name_token = self.lexer.next()?;
      let mut left = match name_token.kind {
         TokenKind::Underscore => self.ast.create_node(NodeKind::Discard),
         // Don't use create_identifier here, neither do we need to set the span, nor
         // error when the token is not an identifier.
         TokenKind::Identifier(range) => self.create_node_with_range(NodeKind::Identifier, range),
         _ => return Ok(self.error(ErrorKind::VarNameExpected, name_token.span)),
      };
      self.ast.set_span(left, name_token.span);

      // Optional type after `:`.
      if let Some(..) = self.match_token(TokenKind::Colon)? {
         let name = left;
         let typ = self.parse_type()?;
         left = self.create_node_with_handles(NodeKind::VariableType, name, typ);
         self.ast.set_span(left, Span::join(self.ast.span(name), self.ast.span(typ)));
      }

      self.expect_token(TokenKind::Assign, |t| ErrorKind::VarMissingEquals(t.kind))?;
      let value = self.parse_expression(0)?;
      let node = self.create_node_with_handles(node_kind, left, value);
      self.ast.set_span(node, Span::join(&var_token.span, self.ast.span(value)));
      Ok(node)
   }

   /// Parses a function declaration.
   fn parse_function_declaration(&mut self) -> Result<NodeId, Error> {
      // TODO: anonymous functions.
      // Those are not in the spec yet, as I'm not sure how I want closures to be implemented.

      let token = self.lexer.next()?;
      let name_token = self.lexer.next()?;
      let name = self.create_identifier(name_token);

      // Parse the formal parameter list.
      let left_paren = match self.expect_token(TokenKind::LeftParen, |token| {
         ErrorKind::FunctionParametersExpected(token.kind)
      })? {
         Some(left_paren) => left_paren,
         // We have already emitted an error, so we only return an error node.
         None => return Ok(self.ast.create_node(NodeKind::Error)),
      };
      let mut formal_param_list = Vec::new();
      let right_paren =
         self.parse_parameter_list(&mut formal_param_list, &left_paren, TokenKind::RightParen)?;

      // Handle the optional return type.
      let mut return_type = NodeId::null();
      if let Some(..) = self.match_token(TokenKind::Colon)? {
         return_type = self.parse_type()?;
      }

      // Parse the body.
      let mut body = Vec::new();
      self.parse_indented_block(
         &mut body,
         &token,
         |p| p.parse_statement(),
         || ErrorKind::MissingLineBreakAfterStatement,
      )?;

      // Construct the AST.
      let generic_params = NodeId::null(); // TODO: generic parameters
      let formal_params = self.ast.create_node(NodeKind::FormalParameters);
      self.ast.set_span(
         formal_params,
         Span::join(&left_paren.span, &right_paren.span),
      );
      self.ast.set_first_handle(formal_params, return_type);
      self.ast.set_extra(formal_params, NodeData::NodeList(formal_param_list));

      let params =
         self.create_node_with_handles(NodeKind::Parameters, generic_params, formal_params);
      let params_span = if generic_params != NodeId::null() {
         Span::join(self.ast.span(generic_params), self.ast.span(formal_params))
      } else {
         self.ast.span(formal_params).clone()
      };
      self.ast.set_span(params, params_span);

      let fun = self.create_node_with_handles(NodeKind::Fun, name, params);
      self.ast.set_span(fun, Span::join(&token.span, self.ast.span(params)));
      self.ast.set_extra(fun, NodeData::NodeList(body));

      Ok(fun)
   }

   /// Parses a declaration for a type alias.
   fn parse_type_alias_declaraction(&mut self) -> Result<NodeId, Error> {
      let type_keyword = self.lexer.next()?;
      let name = self.parse_type_name()?;

      // TODO: constraints `: T`
      // For now we'll just keep them empty.
      let constraint = NodeId::null();
      let constrained = self.create_node_with_handles(NodeKind::ConstrainedType, name, constraint);
      self.ast.set_span(constrained, self.ast.span(name).clone());

      // Optional pragmas. Note that `parse_pragmas` mutates the node passed to it, rather than
      // creating a new one.
      self.parse_pragmas(constrained)?;

      // The `= B` RHS is optional and can be omitted if this is an associated type definition
      // in a trait, or a declaration for a built-in type.
      let aliased_type = if let TokenKind::Equal = self.lexer.peek()?.kind {
         let _equal = self.lexer.next()?;
         self.parse_type()?
      } else {
         NodeId::null()
      };

      let typ = self.create_node_with_handles(NodeKind::Type, constrained, aliased_type);
      self.ast.set_span(
         typ,
         Span::join(
            &type_keyword.span,
            self.ast.span(if aliased_type != NodeId::null() {
               aliased_type
            } else {
               constrained
            }),
         ),
      );
      Ok(typ)
   }

   /// Parses a `pub` symbol declaration.
   fn parse_pub_declaration(&mut self) -> Result<NodeId, Error> {
      let pub_token = self.lexer.next()?;
      let next_token = self.lexer.peek()?;
      let inner = match next_token.kind {
         TokenKind::Fun | TokenKind::Type => self.parse_statement()?,
         _ => {
            let span = Span::join(&pub_token.span, &next_token.span);
            self.error(ErrorKind::PubMustBeFollowedByDeclaration, span)
         }
      };
      let node = self.create_node_with_handle(NodeKind::Pub, inner);
      self.ast.set_span(node, Span::join(&pub_token.span, self.ast.span(inner)));
      Ok(node)
   }

   /// Parses a statement.
   fn parse_statement(&mut self) -> Result<NodeId, Error> {
      let token_kind = self.lexer.peek()?.kind.clone();
      let node = match token_kind {
         // Declarations
         TokenKind::Val | TokenKind::Var => self.parse_variable_declaration()?,
         TokenKind::Fun => self.parse_function_declaration()?,
         TokenKind::Type => self.parse_type_alias_declaraction()?,
         TokenKind::Pub => self.parse_pub_declaration()?,

         // Control flow
         TokenKind::Underscore => self.parse_pass()?,
         TokenKind::Do => self.parse_do_expression(None)?,
         TokenKind::If => self.parse_if_expression(None)?,
         TokenKind::While => self.parse_while_loop()?,

         // Expression statements
         _ => self.parse_expression(0)?,
      };
      Ok(node)
   }

   /// Parses the entire source code of a module.
   fn parse_module(&mut self) -> Result<NodeId, Error> {
      let node = self.ast.create_node(NodeKind::StatementList);
      let mut statements = Vec::new();
      let mut previous_line = Span::INVALID_LINE;
      while self.lexer.peek()?.kind != TokenKind::Eof {
         let next = self.lexer.peek()?;
         let (line, span) = (next.line(), next.span.clone());
         if next.line() == previous_line {
            return Ok(self.error(ErrorKind::MissingLineBreakAfterStatement, span));
         }
         if next.indent_level != 0 {
            let indent_level = next.indent_level;
            return Ok(self.error(
               ErrorKind::NoIndentationExpectedAtModuleLevel(indent_level),
               span,
            ));
         }
         let statement = self.parse_statement()?;
         statements.push(statement);
         previous_line = line;
      }
      self.ast.set_span(node, self.span_all_nodes(&statements));
      self.ast.set_extra(node, NodeData::NodeList(statements));
      Ok(node)
   }
}

/// Parses a source file with the given filename and source code. On success, returns the AST and
/// the handle to the root node. On failure, returns a list of errors.
pub fn parse(lexer: Lexer) -> Result<(Ast, NodeId), Errors> {
   let mut parser = Parser::new(lexer);
   let root_node = match parser.parse_module() {
      Ok(node) => node,
      Err(error) => {
         return Err({
            let mut errors = Errors::new();
            errors.push(error);
            errors
         })
      }
   };

   if parser.errors.len() > 0 {
      Err(parser.errors)
   } else {
      Ok((parser.ast, root_node))
   }
}
