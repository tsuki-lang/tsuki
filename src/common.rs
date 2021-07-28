//! Common functionality.

use std::fmt;

/// A structure representing a span of text in a source file.
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
   pub fn is_invalid(&mut self) -> bool {
      self.line_start == Self::INVALID_LINE
         || self.column_start == Self::INVALID_COLUMN
         || self.line_end == Self::INVALID_LINE
         || self.column_end == Self::INVALID_COLUMN
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
   fn default() -> Self {
      Self {
         line_start: Self::FIRST_LINE,
         column_start: Self::FIRST_COLUMN,
         line_end: Self::FIRST_LINE,
         column_end: Self::FIRST_COLUMN,
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
   // TODO: make Token implement Display so that error messages are more clear
   #[error("unexpected token in prefix position: {0:?}")]
   UnexpectedPrefixToken(crate::lexer::Token),
   #[error("unexpected token in infix position: {0:?}")]
   UnexpectedInfixToken(crate::lexer::Token),
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
