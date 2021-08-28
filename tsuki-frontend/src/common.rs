//! Common functionality.

use std::fmt;

use smallvec::SmallVec;

use crate::{
   ast::NodeKind,
   lexer::{IndentLevel, Token, TokenKind},
};

/// Represents a source file.
pub struct SourceFile {
   pub filename: String,
   pub source: String,
}

/// Represents a span of text in a source file.
#[derive(Clone, Debug)]
pub struct Span {
   pub line_start: usize,
   pub column_start: usize,
   pub line_end: usize,
   pub column_end: usize,
}

impl Span {
   pub const FIRST_LINE: usize = 1;
   pub const FIRST_COLUMN: usize = 1;

   pub const INVALID_LINE: usize = 0;
   pub const INVALID_COLUMN: usize = 0;

   /// Creates and initializes a new span starting at the first possible position in a file.
   pub fn new() -> Self {
      // The first possible position is 1:1..1:1.
      Self {
         line_start: Self::FIRST_LINE,
         column_start: Self::FIRST_COLUMN,
         line_end: Self::FIRST_LINE,
         column_end: Self::FIRST_COLUMN,
      }
   }

   /// Sets the start of the span to its current end.
   pub fn start_over(&mut self) {
      self.line_start = self.line_end;
      self.column_start = self.column_end;
   }

   /// Increments the ending column by `n`.
   pub fn advance_column_by(&mut self, n: usize) {
      self.column_end += n;
   }

   /// Increments the ending line and resets the ending column to the first column.
   pub fn advance_line(&mut self) {
      self.line_end += 1;
      self.column_end = Self::FIRST_COLUMN;
   }

   /// Returns whether the span is an _invalid_ span, that is, its positions are `INVALID_LINE` and
   /// `INVALID_COLUMN`.
   pub fn is_invalid(&self) -> bool {
      self.line_start == Self::INVALID_LINE
         || self.column_start == Self::INVALID_COLUMN
         || self.line_end == Self::INVALID_LINE
         || self.column_end == Self::INVALID_COLUMN
   }

   /// Joins two spans into one. The span `a` must be placed earlier in the text than `b`.
   pub fn join(a: &Span, b: &Span) -> Span {
      // We want to find the minimal and maximal lines and columns. Note that `a` is always at an
      // earlier position than `b`.

      // In the first check, we use <=, because if the starting lines are equal, we want to pick the
      // column number from `a`.
      let (line_start, column_start) = if a.line_start <= b.line_start {
         (a.line_start, a.column_start)
      } else {
         (b.line_start, b.column_start)
      };

      // In the second check, we use <, because if the starting lines are equal, we want to pick the
      // column number from `b`.
      let (line_end, column_end) = if a.line_end < b.line_end {
         (a.line_end, a.column_end)
      } else {
         (b.line_end, b.column_end)
      };

      // Then we just join those into a final span.
      Span {
         line_start,
         column_start,
         line_end,
         column_end,
      }
   }
}

impl fmt::Display for Span {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      if f.alternate() {
         write!(
            f,
            "{}:{}..{}:{}",
            self.line_start, self.column_start, self.line_end, self.column_end
         )?;
      } else {
         write!(f, "{}:{}", self.line_start, self.column_start)?;
      }
      Ok(())
   }
}

impl Default for Span {
   /// Initializes a span at a default, _invalid_ position. This is _not_ the same as [`Span::new`]!
   fn default() -> Self {
      Self {
         line_start: Self::INVALID_LINE,
         column_start: Self::INVALID_COLUMN,
         line_end: Self::INVALID_LINE,
         column_end: Self::INVALID_COLUMN,
      }
   }
}

#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
   /*
    * Lexer errors
    */
   #[error("unexpected character: {0:?}")]
   UnexpectedCharacter(char),
   #[error("indentation too deep; something's wrong with Your program.")]
   IndentTooDeep,
   #[error("CRLF line endings are not supported")]
   CrlfNotSupported,
   #[error("invalid integer literal kind: {0:?}")]
   InvalidIntegerLiteral(char),
   #[error("invalid escape sequence kind: '\\{0}'")]
   InvalidEscapeSequence(char),
   #[error("incomplete '\\{0}' escape sequence")]
   IncompleteEscapeSequence(char),
   #[error("unclosed character literal")]
   UnclosedCharacterLiteral,
   #[error("unicode character U+{0:X} is out of range")]
   UnicodeEscapeOutOfRange(u32),
   #[error("unicode escape out of range of 32-bit integers")]
   UnicodeEscapeOutOfRange32,
   #[error("unclosed string literal")]
   UnclosedStringLiteral,

   /*
    * Parser errors
    */
   #[error("unexpected token in prefix position: '{0}'")]
   UnexpectedPrefixToken(TokenKind),
   #[error("unexpected token in infix position: '{0}'")]
   UnexpectedInfixToken(TokenKind),
   #[error("missing '{0}' to close {1}")]
   MissingClosingToken(TokenKind, Token),
   #[error("expected comma ',' or '{0}' to close {1}")]
   ExpectedCommaOrClosingToken(TokenKind, Token),
   #[error("statements must be separated by line breaks")]
   MissingLineBreakAfterStatement,
   #[error("module-level code must not be indented (got {0} spaces of indentation)")]
   NoIndentationExpectedAtModuleLevel(IndentLevel),
   #[error("indented block of level greater than {0} expected")]
   IndentedBlockExpected(IndentLevel),
   #[error("identifier expected, but got '{0}'")]
   IdentifierExpected(TokenKind),
   #[error("missing variable name (an identifier, or '_' to discard the value)")]
   VarNameExpected,
   #[error("expected '=' after variable name, but got '{0}'")]
   VarMissingEquals(TokenKind),

   /*
    * Sem'check errors
    */
   // SemLiterals
   #[error("invalid number literal suffix: '{0}'")]
   InvalidNumberLiteralSuffix(String),
   #[error("integer {0} is too big to fit in 64 bits")]
   IntegerTooBig(String),
   #[error("integer {0} is too big to fit in {1}")]
   UnsignedIntegerOverflowForType(u64, String),
   #[error("integer {0} is too big (or too small) to fit in {1}")]
   SignedIntegerOverflowForType(i64, String),
   #[error("unsigned integers cannot be negative")]
   UintCannotBeNegative,
   #[error("integer suffixes cannot be used on float literals")]
   InvalidFloatSuffix,

   // SemTypes
   #[error("'{0}' is not declared")]
   UndeclaredSymbol(String),
   #[error("invalid unary operator for {0}")]
   InvalidUnaryOperator(String),
   #[error("type mismatch: expected {0}, but got {1}")]
   TypeMismatch(String, String),
   #[error("only intrinsic \"function\" calls are supported right now")]
   NonIntrinCall,
   #[error("{0} arguments expected, got {1}")]
   NArgumentsExpected(usize, usize),
   #[error("missing result value in expression")]
   MissingResult,
   #[error("result value of expression is unused; use `val _ = x` to discard it")]
   UnusedValue,
   #[error("invalid location (left hand side of assignment)")]
   InvalidLhsOfAssignment,
   #[error("the target is immutable and cannot be assigned to")]
   CannotAssignImmutableLocation,
   #[error("`if` condition must be a Bool")]
   IfConditionMustBeBool,

   /*
    * Internal errors
    * ---
    * Every internal error must be prefixed by "<component> internal error:" to tell the user that
    * something went terribly wrong in the compiler, and that the error should be reported.
    */
   #[error("SemTypes internal error: invalid AST node passed to annotate_node(): {0:?}")]
   SemTypesInvalidAstNode(NodeKind),

   #[error("backend internal error: code generation error: {0}")]
   CodeGen(String),
   #[error("backend internal error: execution error: {0}")]
   ExecutableError(String),
}

/// An error that can occur during lexing, parsing, semantic analysis, or code generation.
#[derive(Debug)]
pub struct Error {
   // The filename is owned because errors don't occur very often, so allocations are fine here,
   // and using an owned String here simplifies a bunch of code.
   pub filename: String,
   pub span: Span,
   pub kind: ErrorKind,
}

impl fmt::Display for Error {
   /// The alternate format syntax `{:#}` can be used to display the full span of where the error
   /// occured, instead of its starting position only.
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      if f.alternate() {
         write!(f, "{}:{:#}: {}", self.filename, self.span, self.kind)?;
      } else {
         write!(f, "{}:{}: {}", self.filename, self.span, self.kind)?;
      }
      Ok(())
   }
}

pub type Errors = SmallVec<[Error; 8]>;

/// Creates an `Errors` from a single error.
pub fn single_error(error: Error) -> Errors {
   let mut errs = Errors::new();
   errs.push(error);
   errs
}
