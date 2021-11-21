//! Common functionality.

use std::fmt;
use std::path::PathBuf;

use smallvec::SmallVec;

use crate::ast::NodeKind;
use crate::lexer::{IndentLevel, Token, TokenKind};

/// Represents a source file.
pub struct SourceFile {
   /// The package the source file resides in.
   pub package: String,
   /// The root of the package.
   pub package_root: PathBuf,
   /// The path to the source file, relative to the package's `src` folder.
   pub path: PathBuf,
   /// The module name; that is, the package name and file path concatenated together with a colon,
   /// with the extension stripped, and all path separators replaced with colons `:`.
   pub module_name: String,
   /// The source code itself.
   pub source: String,
}

impl SourceFile {
   pub fn new(
      package: String,
      package_root: PathBuf,
      path: PathBuf,
      source: String,
   ) -> Result<Self, Error> {
      let module_name = {
         let package_root = package_root
            .canonicalize()
            .map_err(|err| Error::spanless(path.clone(), ErrorKind::Io(err)))?;
         let path = path
            // Normalize the path into something that makes sense.
            .canonicalize()
            .map_err(|err| Error::spanless(path.clone(), ErrorKind::Io(err)))?
            // Remove the package_root prefix.
            .strip_prefix(&package_root)
            .map_err(|_| Error::spanless(path.clone(), ErrorKind::InvalidPackageRoot))?
            // Remove the .tsu extension.
            .with_extension("")
            // Convert it to a string.
            .to_str()
            .ok_or_else(|| Error::spanless(path.clone(), ErrorKind::InvalidUtf8InPath))?
            // Replace path separators with dots.
            .replace(std::path::MAIN_SEPARATOR, ".");
         // And pray to God it's correct.
         format!("{}:{}", package, path)
      };
      Ok(Self {
         package,
         package_root,
         path,
         module_name,
         source,
      })
   }
}

/// Represents a span of text in a source file.
#[derive(Clone, Debug)]
pub struct Span {
   pub byte_start: usize,
   pub line_start: usize,
   pub column_start: usize,
   pub byte_end: usize,
   pub line_end: usize,
   pub column_end: usize,
}

impl Span {
   pub const FIRST_BYTE: usize = 0;
   pub const FIRST_LINE: usize = 1;
   pub const FIRST_COLUMN: usize = 1;

   pub const INVALID_LINE: usize = 0;
   pub const INVALID_COLUMN: usize = 0;

   pub const INVALID: Self = Self {
      byte_start: 0,
      line_start: Self::INVALID_LINE,
      column_start: Self::INVALID_COLUMN,
      byte_end: 0,
      line_end: Self::INVALID_LINE,
      column_end: Self::INVALID_COLUMN,
   };

   /// Creates and initializes a new span starting at the first possible position in a file.
   pub fn new() -> Self {
      // The first possible position is 1:1..1:1.
      Self {
         byte_start: Self::FIRST_BYTE,
         line_start: Self::FIRST_LINE,
         column_start: Self::FIRST_COLUMN,
         byte_end: Self::FIRST_BYTE,
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
      self.byte_end += n;
      self.column_end += n;
   }

   /// Increments the ending line and resets the ending column to the first column.
   pub fn advance_line(&mut self) {
      self.byte_end += 1;
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
      // There's probably a simpler way of doing this.

      let byte_start = a.byte_start.min(b.byte_start);
      let byte_end = a.byte_end.max(b.byte_end);

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
         byte_start,
         line_start,
         column_start,
         byte_end,
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
         byte_start: Self::FIRST_BYTE,
         line_start: Self::INVALID_LINE,
         column_start: Self::INVALID_COLUMN,
         byte_end: Self::FIRST_BYTE,
         line_end: Self::INVALID_LINE,
         column_end: Self::INVALID_COLUMN,
      }
   }
}

#[derive(thiserror::Error, Debug)]
pub enum ErrorKind {
   /*
    * Non-compilation errors
    */
   #[error("invalid UTF-8 in path")]
   InvalidUtf8InPath,
   #[error("package root is not a prefix of the main file path")]
   InvalidPackageRoot,
   #[error("I/O error: {0}")]
   Io(#[from] std::io::Error),

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
   #[error("unexpected token in type position: '{0}'")]
   UnexpectedTypeToken(TokenKind),
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
   #[error("expected comma ',' or colon ':' in parameter list, but got '{0}'")]
   ExpectedCommaOrColon(TokenKind),
   #[error("function parameter list expected, but got '{0}'")]
   FunctionParametersExpected(TokenKind),

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
   #[error("'{0}' is not declared in this scope")]
   UndeclaredSymbol(String),
   #[error("invalid unary operator for {0}")]
   InvalidUnaryOperator(String),
   #[error("type mismatch: expected {0}, but got {1}")]
   TypeMismatch(String, String),
   #[error("{0} arguments expected, but got {1}")]
   NArgumentsExpected(usize, usize),
   #[error("missing result value in expression")]
   MissingResult,
   #[error("result value of expression is unused; use `val _ = x` to discard it")]
   UnusedValue,
   #[error("invalid location (left hand side of assignment)")]
   InvalidLocation,
   #[error("'{0}' is not a type")]
   SymbolIsNotAType(String),
   #[error("the target is immutable and cannot be assigned to")]
   CannotAssignImmutableLocation,
   #[error("`if` condition must be a Bool")]
   IfConditionMustBeBool,
   #[error("`while` condition must be a Bool")]
   WhileConditionMustBeBool,
   #[error("expression cannot be called; make sure it is a function")]
   ExpressionCannotBeCalled,

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
   pub filename: PathBuf,
   pub span: Span,
   pub kind: ErrorKind,
}

impl Error {
   /// Constructs an error with an invalid span, such that no span is displayed.
   pub fn spanless(filename: PathBuf, kind: ErrorKind) -> Self {
      Self {
         filename,
         span: Span::INVALID,
         kind,
      }
   }
}

impl fmt::Display for Error {
   /// The alternate format syntax `{:#}` can be used to display the full span of where the error
   /// occured, instead of its starting position only.
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      let filename = self.filename.to_str().unwrap();
      write!(f, "{}:", filename)?;
      if !self.span.is_invalid() {
         if f.alternate() {
            write!(f, "{:#}:", self.span)?;
         } else {
            write!(f, "{}:", self.span)?;
         }
      }
      write!(f, " {}", self.kind)?;
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
