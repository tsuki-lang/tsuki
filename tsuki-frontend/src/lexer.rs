//! The lexer.
//!
//! tsuki uses a flat data structure for storing tokens to avoid memory fragmentation and improve
//! performance.

use std::fmt;
use std::ops::Range;

use crate::common::{Error, ErrorKind, Span};

/// The kind of a token, along with some extra metadata for some token types.
#[allow(unused)]
#[derive(PartialEq, Eq, Clone, Debug)]
#[non_exhaustive]
#[repr(u8)]
pub enum TokenKind {
   // Literals
   Nil,
   True,
   False,
   /// Integer and float tokens store the actual number behind them in the source string, so as not
   /// to lose precision.
   Integer(Range<usize>),
   Float(Range<usize>),
   Atom(Range<usize>), // atom literal
   Character(u32),
   /// Strings store the range in which a string literal is stored within the `string_data` field
   /// inside of the lexer struct.
   String(Range<usize>),
   /// Doc comments also use the `string_data` field inside of the lexer.
   DocComment(Range<usize>),

   // Identifiers and keywords
   Identifier(Range<usize>),
   Underscore,
   And,
   AtomK, // 'atom' keyword
   Catch,
   Derive,
   Do,
   Elif,
   Else,
   For,
   Fun,
   If,
   Impl,
   In,
   Is,
   Macro,
   Match,
   Not,
   Object,
   Of,
   Or,
   Pub,
   Return,
   Try,
   Type,
   Union,
   While,
   Val,
   Var,

   // Non-keyword operators
   Dot,           // .
   Plus,          // +
   Minus,         // -
   Mul,           // *
   Div,           // /
   Pow,           // **
   Tilde,         // ~
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
   Pointer,       // ^
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

   // Punctuation
   LeftParen,    // (
   RightParen,   // )
   LeftBracket,  // [
   RightBracket, // ]
   LeftBrace,    // {
   RightBrace,   // }
   Comma,        // ,
   Colon,        // :
   Colons,       // ::
   Semicolon,    // ;
   Then,         // ->

   // EOF
   Eof,
}

/// The associativity of a token.
#[derive(Eq, PartialEq)]
pub enum Associativity {
   Left,
   Right,
}

impl TokenKind {
   /// Returns the substring of the given `source` string that this token points to, or `None`
   /// if the token's type does not store a source range.
   pub fn get_source<'s>(&self, source: &'s str) -> Option<&'s str> {
      match self {
         Self::Integer(r) | Self::Float(r) | Self::Atom(r) | Self::Identifier(r) => {
            Some(&source[r.clone()])
         }
         _ => None,
      }
   }

   /// Returns the string pointed to by the token, if it's a `String` or a `DocComment`. Otherwise
   /// returns `None`.
   ///
   /// Panics when the string contains invalid UTF-8.
   /// Thus, this function should only ever be used for debugging purposes.
   pub fn get_string<'s>(&self, lexer: &'s Lexer) -> Option<&'s str> {
      match self {
         Self::String(r) | Self::DocComment(r) => {
            let bytes = &lexer.string_data[r.clone()];
            Some(std::str::from_utf8(bytes).expect("invalid UTF-8"))
         }
         _ => None,
      }
   }

   /// Returns the precedence level of the token. A negative precedence level means that the token
   /// is not a valid infix token.
   pub fn precedence(&self) -> i8 {
      // The precedence _could_ be an Option, but I opted not to do that for efficiency sake.
      // The "default" precedence level for expressions is 0, which means that any expression can
      // appear at the given position. If a token's precedence level is -1, that means it cannot
      // ever appear in an infix position, because -1 is never going to be greater than or equal
      // to any valid precedence level set by the parser.
      match self {
         | Self::LeftParen
         | Self::LeftBrace
         | Self::LeftBracket
         | Self::Dot
         | Self::Pointer
         | Self::Check
         | Self::Unwrap => precedence::CHAIN,
         Self::Pow => precedence::EXPONENT,
         | Self::Mul
         | Self::Div
         | Self::Lshift
         | Self::Rshift
         | Self::BitAnd
         | Self::BitOr
         | Self::BitXor => precedence::PRODUCT,
         Self::Plus | Self::Minus | Self::Tilde => precedence::SUM,
         Self::UpTo | Self::UpToInclusive => precedence::RANGE,
         | Self::Equal
         | Self::NotEqual
         | Self::Less
         | Self::Greater
         | Self::LessEqual
         | Self::GreaterEqual
         | Self::Is
         | Self::Of
         | Self::In => precedence::COMPARISON,
         Self::And => precedence::AND,
         Self::Or => precedence::OR,
         | Self::Assign
         | Self::PlusAssign
         | Self::MinusAssign
         | Self::MulAssign
         | Self::DivAssign
         | Self::Push => precedence::ASSIGNMENT,
         _ => precedence::INVALID,
      }
   }

   /// Returns the associativity of the token.
   pub fn associativity(&self) -> Associativity {
      match self {
         Self::Pow => Associativity::Right,
         _ => Associativity::Left,
      }
   }
}

pub mod precedence {
   pub const INVALID: i8 = -1;
   pub const ASSIGNMENT: i8 = 1;
   pub const OR: i8 = 2;
   pub const AND: i8 = 3;
   pub const COMPARISON: i8 = 4;
   pub const RANGE: i8 = 5;
   pub const SUM: i8 = 6;
   pub const PRODUCT: i8 = 7;
   pub const EXPONENT: i8 = 8;
   pub const CHAIN: i8 = 9;
}

impl fmt::Display for TokenKind {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(
         f,
         "{}",
         match self {
            TokenKind::Nil => "nil",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Integer(_) => "<integer literal>",
            TokenKind::Float(_) => "<float literal>",
            TokenKind::Atom(_) => "<atom literal>",
            TokenKind::Character(_) => "<character literal>",
            TokenKind::String(_) => "<string literal>",
            TokenKind::DocComment(_) => "<documentation comment>",
            TokenKind::Identifier(_) => "<identifier>",
            TokenKind::Underscore => "_",
            TokenKind::And => "and",
            TokenKind::AtomK => "atom",
            TokenKind::Catch => "catch",
            TokenKind::Derive => "derive",
            TokenKind::Do => "do",
            TokenKind::Elif => "elif",
            TokenKind::Else => "else",
            TokenKind::For => "for",
            TokenKind::Fun => "fun",
            TokenKind::If => "if",
            TokenKind::Impl => "impl",
            TokenKind::In => "in",
            TokenKind::Is => "is",
            TokenKind::Macro => "macro",
            TokenKind::Match => "match",
            TokenKind::Not => "not",
            TokenKind::Object => "object",
            TokenKind::Of => "of",
            TokenKind::Or => "or",
            TokenKind::Pub => "pub",
            TokenKind::Return => "return",
            TokenKind::Try => "try",
            TokenKind::Type => "type",
            TokenKind::Union => "union",
            TokenKind::While => "while",
            TokenKind::Val => "val",
            TokenKind::Var => "var",
            TokenKind::Dot => ".",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Mul => "*",
            TokenKind::Div => "/",
            TokenKind::Pow => "**",
            TokenKind::Tilde => "~",
            TokenKind::Lshift => "<<",
            TokenKind::Rshift => ">>",
            TokenKind::BitAnd => "&",
            TokenKind::BitOr => "|",
            TokenKind::BitXor => "^^",
            TokenKind::Equal => "==",
            TokenKind::NotEqual => "!=",
            TokenKind::Less => "<",
            TokenKind::Greater => ">",
            TokenKind::LessEqual => "<=",
            TokenKind::GreaterEqual => ">=",
            TokenKind::Pointer => "^",
            TokenKind::Check => "?",
            TokenKind::Unwrap => "!",
            TokenKind::UpTo => "..",
            TokenKind::UpToInclusive => "..=",
            TokenKind::Assign => "=",
            TokenKind::PlusAssign => "+=",
            TokenKind::MinusAssign => "-=",
            TokenKind::MulAssign => "*=",
            TokenKind::DivAssign => "/=",
            TokenKind::Push => "<-",
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBracket => "[",
            TokenKind::RightBracket => "]",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Colon => ":",
            TokenKind::Colons => "::",
            TokenKind::Semicolon => ";",
            TokenKind::Then => "->",
            TokenKind::Eof => "<end of file>",
         }
      )
   }
}

/// A map of identifiers corresponding to keywords.
static KEYWORDS: phf::Map<&'static str, TokenKind> = phf::phf_map! {
   "_" => TokenKind::Underscore,
   "and" => TokenKind::And,
   "atom" => TokenKind::AtomK,
   "catch" => TokenKind::Catch,
   "derive" => TokenKind::Derive,
   "do" => TokenKind::Do,
   "elif" => TokenKind::Elif,
   "else" => TokenKind::Else,
   "false" => TokenKind::False,
   "for" => TokenKind::For,
   "fun" => TokenKind::Fun,
   "if" => TokenKind::If,
   "impl" => TokenKind::Impl,
   "in" => TokenKind::In,
   "is" => TokenKind::Is,
   "macro" => TokenKind::Macro,
   "match" => TokenKind::Match,
   "not" => TokenKind::Not,
   "nil" => TokenKind::Nil,
   "object" => TokenKind::Object,
   "of" => TokenKind::Of,
   "or" => TokenKind::Or,
   "pub" => TokenKind::Pub,
   "return" => TokenKind::Return,
   "true" => TokenKind::True,
   "try" => TokenKind::Try,
   "type" => TokenKind::Type,
   "union" => TokenKind::Union,
   "while" => TokenKind::While,
   "val" => TokenKind::Val,
   "var" => TokenKind::Var,
};

/// The integer type representing an indent level.
pub type IndentLevel = u32;

/// A token with an associated span.
#[derive(Clone, Debug)]
pub struct Token {
   pub kind: TokenKind,
   pub span: Span,
   pub indent_level: IndentLevel,
}

impl Token {
   /// Returns the line at which the token is located.
   ///
   /// This is a shortcut for `token.span.line_start`.
   pub fn line(&self) -> usize {
      self.span.line_start
   }
}

impl fmt::Display for Token {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "'{}' at {}", self.kind, self.span)
   }
}

/// Lexer state. This struct stores all the data needed for lexing, emitting tokens, and reporting
/// errors.
pub struct Lexer<'i> {
   filename: Option<String>,

   input: &'i [u8],
   position: usize,

   span: Span,
   indent_level: IndentLevel,

   /// This field is used for storing normalized string data - that is, strings without escapes.
   /// It's generally more cache-efficient to just have a single place where all strings live,
   /// rather than scattering potentially hundreds of strings over the heap.
   /// `String` tokens point to ranges of characters in this field.
   string_data: Vec<u8>,

   peek_cache: Option<(usize, Span, Token)>,
}

impl<'i> Lexer<'i> {
   const EOF_CHAR: u8 = 0;

   /// Creates a new lexer with the given filename and input string. The filename is used for error
   /// reporting.
   pub fn new(filename: &str, input: &'i str) -> Self {
      Self {
         filename: Some(filename.to_owned()),
         input: input.as_bytes(),
         position: 0,
         span: Span::new(),
         indent_level: 0,
         string_data: Vec::new(),
         peek_cache: None,
      }
   }

   /// Clones the filename out of the lexer. Panics if the filename was taken out by an error.
   pub fn filename(&self) -> &str {
      self.filename.as_ref().unwrap()
   }

   /// Returns the lexer's input string.
   pub fn input(&self) -> &str {
      // Safety: `self.input` is guaranteed to be valid UTF-8 as it is provided by an external
      // caller, and only interpreted as bytes by the lexer.
      unsafe { std::str::from_utf8_unchecked(self.input) }
   }

   /// Returns the lexer's string data.
   pub fn string_data(&self) -> &str {
      // Safety: `self.string_data` is guaranteed to be valid UTF-8 as it consists of parts of
      // `self.input` sliced at ASCII boundaries.
      unsafe { std::str::from_utf8_unchecked(&self.string_data) }
   }

   /// Returns whether the lexer still has more input to read.
   fn has_more(&self) -> bool {
      self.position < self.input.len()
   }

   /// Returns the character at the given _absolute_ position.
   fn get_at(&self, i: usize) -> u8 {
      // If all the logic for reading characters was only maybe handled in fn next(), this check
      // wouldn't be necessary. However, reading characters is done in many places, and we certainly
      // don't want to panic each time we reach the EOF and forget to put a bounds check for
      // reading from `self.input`. Since the bounds check would be required in each of these places
      // anyways, we may as well hoist it into this function without a significant performance
      // penalty.
      if i >= self.input.len() {
         Self::EOF_CHAR
      } else {
         self.input[i]
      }
   }

   /// Returns the character at the current position.
   fn get(&self) -> u8 {
      self.get_at(self.position)
   }

   /// Returns the character `n` characters ahead of the current position.
   fn get_ahead(&self, n: usize) -> u8 {
      self.get_at(self.position + n)
   }

   /// Advances the current position by `n` characters.
   fn advance_by(&mut self, n: usize) {
      self.position += n;
      self.span.advance_column_by(n);
   }

   /// Advances the current position by one character.
   fn advance(&mut self) {
      self.advance_by(1);
   }

   /// Constructs a new token of the given kind, using the current span stored in the lexer.
   fn token<'s>(&self, kind: TokenKind) -> Token {
      Token {
         kind,
         span: self.span.clone(),
         indent_level: self.indent_level,
      }
   }

   /// Creates an error result.
   /// This moves the filename out of the lexer, and subsequent calls to this will result in a
   /// panic. This means once the lexer throws an error, all lexis must be aborted.
   fn error(&mut self, kind: ErrorKind) -> Error {
      Error {
         filename: self
            .filename
            .take()
            .expect("an error may only be thrown once and all lexing must stop afterwards"),
         span: self.span.clone(),
         kind,
      }
   }

   /// Returns a slice of the input starting at `start`, ending at `end`. This function is unsafe
   /// because it assumes that the provided range contains valid UTF-8, which it doesn't have to.
   unsafe fn input_slice(&self, range: Range<usize>) -> &str {
      std::str::from_utf8_unchecked(&self.input[range.start..range.end])
   }

   /// Skips characters as long as the current character is `ch`.
   /// Returns the number of characters skipped.
   fn skip_chars(&mut self, ch: u8) -> usize {
      let mut skipped = 0;
      while self.get() == ch {
         self.advance();
         skipped += 1;
      }
      skipped
   }

   /// Skips all characters, until the current character is `ch`.
   fn skip_chars_except(&mut self, ch: u8) {
      while self.get() != ch && self.get() != Self::EOF_CHAR {
         self.advance();
      }
   }

   /// Reads indentation from the beginning of a line.
   /// Returns an error if the indent level exceeds the maximum supported level (256).
   fn read_indent(&mut self) -> Result<(), Error> {
      let space_count = self.skip_chars(b' ');
      if space_count >= IndentLevel::MAX as usize {
         return Err(self.error(ErrorKind::IndentTooDeep));
      }
      self.indent_level = space_count as IndentLevel;
      Ok(())
   }

   /// Checks whether there is a line break following the cursor. Also skips leading whitespace and
   /// comments.
   fn match_line_break(&mut self) -> Result<bool, Error> {
      let mut had_line_break = false;

      // Skip whitespace and comments.
      loop {
         match self.get() {
            b' ' => {
               self.skip_chars(b' ' as u8);
            }
            b'#' => {
               if self.get_ahead(1) == b'#' {
                  // We have a doc comment, don't read it accidentally as it's its own token.
                  return Ok(false);
               } else {
                  self.skip_chars_except(b'\n' as u8);
               }
            }
            _ => break,
         }
      }
      // Handle line breaks.
      loop {
         match self.get() {
            b'\n' => {
               had_line_break = true;
            }
            _ => break,
         }
         self.indent_level = 0;
         self.advance();
         if had_line_break {
            self.span.advance_line();
         }
      }

      // After handling all the line breaks, read how much indentation there is at the beginning
      // of the line.
      if had_line_break {
         self.read_indent()?;
      }

      Ok(had_line_break)
   }

   /// Skips all whitespace and comments following the current position. Returns whether it
   /// encountered 1 or more line break along the way.
   fn skip_whitespace_and_comments(&mut self) -> Result<bool, Error> {
      let mut had_line_break = false;
      while self.match_line_break()? {
         had_line_break = true;
      }
      Ok(had_line_break)
   }

   /// Reads an identifier, and returns a string slice pointing to it.
   fn read_identifier(&mut self) -> Range<usize> {
      let start = self.position;
      while matches!(self.get(), b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') {
         self.advance();
      }
      let end = self.position;
      start..end
   }

   /// Reads a binary, octal, decimal, or hexadecimal integer, or a float.
   fn read_number(&mut self) -> Result<TokenKind, Error> {
      #[derive(PartialEq, Eq)]
      enum LiteralKind {
         Binary,
         Octal,
         Decimal,
         Hexadecimal,
      }
      let mut kind = LiteralKind::Decimal;

      let start = self.position;
      if self.get() == b'0' && matches!(self.get_ahead(1), b'b' | b'o' | b'x') {
         self.advance(); // skip 0
         match self.get() {
            b'b' => kind = LiteralKind::Binary,
            b'o' => kind = LiteralKind::Octal,
            b'x' => kind = LiteralKind::Hexadecimal,
            other => return Err(self.error(ErrorKind::InvalidIntegerLiteral(other as char))),
         }
         self.advance(); // skip literal type specifier
      }

      // FIXME: Number parsing allows for multiple consecutive underscores '_' inside of the
      // literal. It's best to disallow this, as 1__000__000 looks pretty bad.
      match kind {
         LiteralKind::Binary => {
            while matches!(self.get(), b'0' | b'1' | b'_') {
               self.advance();
            }
         }
         LiteralKind::Octal => {
            while matches!(self.get(), b'0'..=b'7' | b'_') {
               self.advance();
            }
         }
         LiteralKind::Decimal => {
            while matches!(self.get(), b'0'..=b'9' | b'_') {
               self.advance();
            }
         }
         LiteralKind::Hexadecimal => {
            while matches!(self.get(), b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' | b'_') {
               self.advance();
            }
         }
      }

      let mut is_float = false;
      if kind == LiteralKind::Decimal
         && self.get() == b'.'
         && matches!(self.get_ahead(1), b'0'..=b'9')
      {
         is_float = true;
         self.read_float();
      }

      // If the last character parsed is an underscore, then this is a type-suffixed literal.
      // Note how the lexer allows for any arbitrary identifier here; this might be used later
      // to allow for custom literals, like 90_deg or -1.10_fixed.
      // The negative sign is not part of the literal, but if the operator is placed next to a
      // literal, it's recognized by SemLiterals, and edge cases like -128_i8 work correctly.
      if self.get_at(self.position - 1) == b'_' && matches!(self.get(), b'a'..=b'z' | b'A'..=b'Z') {
         let _ = self.read_identifier();
      }

      let end = self.position;
      let range = start..end;
      if is_float {
         Ok(TokenKind::Float(range))
      } else {
         Ok(TokenKind::Integer(range))
      }
   }

   /// Reads the floating point part of a number literal.
   fn read_float(&mut self) {
      self.advance(); // skip .
      while matches!(self.get(), b'0'..=b'9' | b'_') {
         self.advance();
      }
      if matches!(self.get(), b'e' | b'E') {
         self.advance();
         if matches!(self.get(), b'-' | b'+') {
            self.advance();
         }
         while matches!(self.get(), b'0'..=b'9' | b'_') {
            self.advance();
         }
      }
   }

   /// Reads a single character from a string.
   fn read_string_character(&mut self, allow_escapes: bool) -> Result<StringCharacter, Error> {
      if allow_escapes && self.get() == b'\\' {
         self.advance(); // skip \
         let kind = self.get();
         self.advance(); // skip kind, because it's always a single character
         match kind {
            // Common escapes
            b'n' => Ok(StringCharacter::Byte(b'\n')),
            b'r' => Ok(StringCharacter::Byte(b'\r')),
            b't' => Ok(StringCharacter::Byte(b'\t')),
            b'\'' => Ok(StringCharacter::Byte(b'\'')),
            b'"' => Ok(StringCharacter::Byte(b'"')),
            // Generic escapes
            b'x' => {
               let start = self.position;
               for _ in 0..2 {
                  if !matches!(self.get(), b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z') {
                     return Err(self.error(ErrorKind::IncompleteEscapeSequence('x')));
                  }
                  self.advance();
               }
               let end = self.position;
               // Using `input_slice` here is safe, because we know that the characters between
               // `start` and `end` are valid ASCII (in fact, they're hex digits).
               let byte = u8::from_str_radix(unsafe { self.input_slice(start..end) }, 16).unwrap();
               Ok(StringCharacter::Byte(byte))
            }
            b'u' => {
               if self.get() != b'{' {
                  return Err(self.error(ErrorKind::IncompleteEscapeSequence('u')));
               }
               self.advance();
               let start = self.position;
               while self.has_more() && self.get() != b'}' {
                  self.advance();
               }
               let end = self.position;
               self.advance(); // skip }
               if !self.has_more() || start == end {
                  return Err(self.error(ErrorKind::IncompleteEscapeSequence('u')));
               }
               // Again, using `input_slice` here is safe, because the characters between `start`
               // and `end` are valid ASCII.
               let digits = unsafe { self.input_slice(start..end) };
               if let Ok(char_code) = u32::from_str_radix(digits, 16) {
                  // If Unicode ever gets expanded beyond U+10FFFF, change this.
                  if char_code > 0x10FFFF {
                     Err(self.error(ErrorKind::UnicodeEscapeOutOfRange(char_code)))
                  } else {
                     Ok(StringCharacter::Unicode(char_code))
                  }
               } else {
                  Err(self.error(ErrorKind::UnicodeEscapeOutOfRange32))
               }
            }
            other => Err(self.error(ErrorKind::InvalidEscapeSequence(other as char))),
         }
      } else {
         // FIXME: Needs to support Unicode characters. Right now only ASCII characters are
         // supported, which is problematic for character literals.
         let ch = self.get();
         self.advance();
         Ok(StringCharacter::Byte(ch))
      }
   }

   /// Returns whether the lexer's position is currently at string quotes.
   /// If `long` is true, checks for long string quotes `"""`. Otherwise checks for
   /// single quotes `"`.
   fn at_string_quotes(&self, long: bool) -> bool {
      let first_char = self.get() == b'"';
      let next_chars = if long {
         self.get_ahead(1) == b'"' && self.get_ahead(2) == b'"'
      } else {
         true
      };
      first_char && next_chars
   }

   /// Reads and normalizes a string literal.
   fn read_string(&mut self, long: bool) -> Result<Range<usize>, Error> {
      self.advance_by(if long { 3 } else { 1 });
      let start = self.string_data.len();
      while self.has_more() && !self.at_string_quotes(long) {
         let ch = self.read_string_character(!long)?;
         match ch {
            StringCharacter::Byte(b) => {
               self.string_data.push(b);
            }
            StringCharacter::Unicode(u) => {
               let mut bytes = [0; 4];
               // Safety: tsuki characters are different from Rust characters, in that they're valid
               // code points and not scalar values.
               let ch = unsafe { char::from_u32_unchecked(u) };
               ch.encode_utf8(&mut bytes);
               self.string_data.extend(bytes[0..ch.len_utf8()].iter());
            }
         }
      }
      if !self.has_more() {
         return Err(self.error(ErrorKind::UnclosedStringLiteral));
      }
      self.advance_by(if long { 3 } else { 1 });
      let end = self.string_data.len();
      Ok(start..end)
   }

   /// Reads and normalizes a doc comment.
   fn read_doc_comment(&mut self) -> Result<Range<usize>, Error> {
      let start = self.string_data.len();
      while self.get() == b'#' {
         // We can safely assume that we can skip two characters, because the lookahead for the
         // second '#' is performed in `match_line_break`.
         self.advance_by(2);
         let line_start = self.position;
         self.skip_chars_except(b'\n');
         self.advance();
         let line_end = self.position;
         self.string_data.extend(self.input[line_start..line_end].iter());
         if self.skip_whitespace_and_comments()? {
            break;
         }
      }
      let end = self.string_data.len();
      Ok(start..end)
   }

   /// Parses a trivial single-character token.
   fn trivial_token(&mut self, kind: TokenKind) -> Token {
      self.advance();
      self.token(kind)
   }

   /// Parses an operator with an optional second character. If only one character is present,
   /// `one` is constructed, otherwise if the second character is present, `two` is constructed.
   fn operator_opt2(&mut self, second: u8, one: TokenKind, two: TokenKind) -> Token {
      self.advance(); // skip first character
      if self.get() == second {
         self.advance();
         self.token(two)
      } else {
         self.token(one)
      }
   }

   /// Parses an operator with optional second and third characters. Functions similarly to
   /// `operator_opt2`.
   fn operator_opt3(
      &mut self,
      second: u8,
      third: u8,
      one: TokenKind,
      two: TokenKind,
      three: TokenKind,
   ) -> Token {
      self.advance(); // skip first character
      if self.get() == second {
         self.advance();
         if self.get() == third {
            self.advance();
            self.token(three)
         } else {
            self.token(two)
         }
      } else {
         self.token(one)
      }
   }

   /// Returns the next token, or `Err` if a lexing error occured.
   pub fn next(&mut self) -> Result<Token, Error> {
      if let Some((position, span, token)) = self.peek_cache.take() {
         self.position = position;
         self.span = span;
         return Ok(token);
      }

      self.skip_whitespace_and_comments()?;

      self.span.start_over();
      match self.get() {
         // Identifiers and keywords
         b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
            let ident_range = self.read_identifier();
            let ident = unsafe { self.input_slice(ident_range.clone()) };
            let mut token_kind = TokenKind::Identifier(ident_range);
            if let Some(keyword) = KEYWORDS.get(ident) {
               token_kind = keyword.clone();
            }
            Ok(self.token(token_kind))
         }

         // Integers and floats
         b'0'..=b'9' => {
            let number = self.read_number()?;
            Ok(self.token(number))
         }

         // Atoms, :, ::
         b':' => {
            self.advance();
            if matches!(self.get(), b'a'..=b'z' | b'A'..=b'Z' | b'_') {
               let ident_range = self.read_identifier();
               Ok(self.token(TokenKind::Atom(ident_range)))
            } else {
               if self.get() == b':' {
                  self.advance();
                  Ok(self.token(TokenKind::Colons))
               } else {
                  Ok(self.token(TokenKind::Colon))
               }
            }
         }

         // Character literals and strings
         b'\'' => {
            self.advance();
            let ch = self.read_string_character(true)?.to_unicode();
            if self.get() != b'\'' {
               Err(self.error(ErrorKind::UnclosedCharacterLiteral))
            } else {
               self.advance();
               Ok(self.token(TokenKind::Character(ch)))
            }
         }
         b'"' => {
            let long = self.at_string_quotes(true);
            let string_range = self.read_string(long)?;
            Ok(self.token(TokenKind::String(string_range)))
         }

         // Doc comments
         b'#' => {
            let comment_range = self.read_doc_comment()?;
            Ok(self.token(TokenKind::DocComment(comment_range)))
         }

         // Operators
         b'.' => Ok(self.operator_opt3(
            b'.',
            b'=',
            TokenKind::Dot,
            TokenKind::UpTo,
            TokenKind::UpToInclusive,
         )),
         b'+' => Ok(self.operator_opt2(b'=', TokenKind::Plus, TokenKind::PlusAssign)),
         b'-' => {
            self.advance();
            let kind = match self.get() {
               b'>' => TokenKind::Then,
               b'=' => TokenKind::MinusAssign,
               _ => TokenKind::Minus,
            };
            if kind != TokenKind::Minus {
               self.advance();
            }
            Ok(self.token(kind))
         }
         b'*' => {
            self.advance(); // skip *
            let kind = match self.get() {
               b'*' => TokenKind::Pow,
               b'=' => TokenKind::MulAssign,
               _ => TokenKind::Mul,
            };
            if kind != TokenKind::Mul {
               self.advance();
            }
            Ok(self.token(kind))
         }
         b'/' => Ok(self.operator_opt2(b'=', TokenKind::Div, TokenKind::DivAssign)),
         b'~' => Ok(self.trivial_token(TokenKind::Tilde)),
         b'<' => {
            self.advance(); // skip <
            let kind = match self.get() {
               b'<' => TokenKind::Lshift,
               b'=' => TokenKind::LessEqual,
               b'-' => TokenKind::Push,
               _ => TokenKind::Less,
            };
            if kind != TokenKind::Less {
               self.advance();
            }
            Ok(self.token(kind))
         }
         b'>' => {
            self.advance(); // skip <
            let kind = match self.get() {
               b'>' => TokenKind::Rshift,
               b'=' => TokenKind::GreaterEqual,
               _ => TokenKind::Greater,
            };
            if kind != TokenKind::Greater {
               self.advance();
            }
            Ok(self.token(kind))
         }
         b'&' => Ok(self.trivial_token(TokenKind::BitAnd)),
         b'|' => Ok(self.trivial_token(TokenKind::BitOr)),
         b'^' => Ok(self.operator_opt2(b'^', TokenKind::Pointer, TokenKind::BitXor)),
         b'=' => Ok(self.operator_opt2(b'=', TokenKind::Assign, TokenKind::Equal)),
         b'!' => Ok(self.operator_opt2(b'=', TokenKind::Unwrap, TokenKind::NotEqual)),
         b'?' => Ok(self.trivial_token(TokenKind::Check)),

         // Punctuation
         b'(' => Ok(self.trivial_token(TokenKind::LeftParen)),
         b')' => Ok(self.trivial_token(TokenKind::RightParen)),
         b'[' => Ok(self.trivial_token(TokenKind::LeftBracket)),
         b']' => Ok(self.trivial_token(TokenKind::RightBracket)),
         b'{' => Ok(self.trivial_token(TokenKind::LeftBrace)),
         b'}' => Ok(self.trivial_token(TokenKind::RightBrace)),
         b',' => Ok(self.trivial_token(TokenKind::Comma)),
         b';' => Ok(self.trivial_token(TokenKind::Semicolon)),

         // Magic tokens (and magic smoke)
         Self::EOF_CHAR => {
            self.indent_level = 0;
            Ok(self.token(TokenKind::Eof))
         }
         b'\r' => Err(self.error(ErrorKind::CrlfNotSupported)),
         other => Err(self.error(ErrorKind::UnexpectedCharacter(other as char))),
      }
   }

   /// Peeks at what the next token is going to be.
   pub fn peek(&mut self) -> Result<&Token, Error> {
      if self.peek_cache.is_none() {
         let (old_position, old_span) = (self.position, self.span.clone());
         let token = self.next()?;
         let (new_position, new_span) = (self.position, self.span.clone());
         self.peek_cache = Some((new_position, new_span, token));
         self.position = old_position;
         self.span = old_span;
      }
      if let Some((_, _, token)) = self.peek_cache.as_ref() {
         Ok(token)
      } else {
         unreachable!()
      }
   }
}

/// A character in a string. This is used to differentiate between escape sequences, for values
/// returned from [`Lexer::read_string_character`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum StringCharacter {
   /// The escape sequence is not `\u{...}`, and should be represented verbatim in a string.
   Byte(u8),
   /// The escape sequence is `\u{...}`, and should be converted to UTF-8 before being stored in
   /// a string.
   Unicode(u32),
}

impl StringCharacter {
   /// Returns the unicode codepoint for the character, regardless of whether the character is a
   /// `Byte` or a `Unicode`.
   pub fn to_unicode(self) -> u32 {
      match self {
         Self::Byte(x) => x as u32,
         Self::Unicode(x) => x,
      }
   }
}
