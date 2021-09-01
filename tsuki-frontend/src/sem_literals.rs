//! Semantic analyzer for literal kinds.
//! This performs some basic initial analysis to convert literal kinds from generic `Integer` and
//! `Float` to concrete types `Int8`, `Int16`, etc., including negation.
//! Note that `SemTypes` may perform additional conversions later down the line.

use std::convert::{TryFrom, TryInto};

use smallvec::SmallVec;

use crate::ast::{Ast, NodeData, NodeHandle, NodeKind};
use crate::common::{ErrorKind, Errors, Span};
use crate::sem::{SemCommon, SemPass};
use crate::types::{FloatSize, IntegerSize};

/// State for the `SemLiterals` analysis phase.
pub(crate) struct SemLiterals<'c> {
   common: &'c SemCommon,
   errors: Errors,
}

/// Available suffixes for literals.
#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
enum LiteralSuffix {
   None,
   I,
   I8,
   I16,
   I32,
   I64,
   U,
   U8,
   U16,
   U32,
   U64,
   F,
   F32,
   F64,
}

impl From<IntegerSize> for LiteralSuffix {
   fn from(size: IntegerSize) -> Self {
      match size {
         IntegerSize::S8 => LiteralSuffix::I8,
         IntegerSize::S16 => LiteralSuffix::I16,
         IntegerSize::S32 => LiteralSuffix::I32,
         IntegerSize::S64 => LiteralSuffix::I64,
         IntegerSize::U8 => LiteralSuffix::U8,
         IntegerSize::U16 => LiteralSuffix::U16,
         IntegerSize::U32 => LiteralSuffix::U32,
         IntegerSize::U64 => LiteralSuffix::U64,
      }
   }
}

impl From<FloatSize> for LiteralSuffix {
   fn from(size: FloatSize) -> Self {
      match size {
         FloatSize::S32 => LiteralSuffix::F32,
         FloatSize::S64 => LiteralSuffix::F64,
      }
   }
}

static SUFFIXES: phf::Map<&'static str, LiteralSuffix> = phf::phf_map! {
   "i" => LiteralSuffix::I,
   "i8" => LiteralSuffix::I8,
   "i16" => LiteralSuffix::I16,
   "i32" => LiteralSuffix::I32,
   "i64" => LiteralSuffix::I64,
   "u" => LiteralSuffix::U,
   "u8" => LiteralSuffix::U8,
   "u16" => LiteralSuffix::U16,
   "u32" => LiteralSuffix::U32,
   "u64" => LiteralSuffix::U64,
   "f" => LiteralSuffix::F,
   "f32" => LiteralSuffix::F32,
   "f64" => LiteralSuffix::F64,
};

impl<'c> SemLiterals<'c> {
   /// Creates a new instance of the `SemTypes` analysis phase.
   pub fn new(common: &'c SemCommon) -> Self {
      SemLiterals {
         common,
         errors: Errors::new(),
      }
   }

   fn split_number<'s>(&mut self, source: &'s str, span: &Span) -> (&'s str, LiteralSuffix) {
      if let Some(underscore) = source.rfind('_') {
         // Check if the index is at least the character before the last character,
         // and the character after it is an identifier character.
         let bytes = source.as_bytes();
         if underscore <= source.len() - 2
            && matches!(bytes[underscore + 1], b'a'..=b'z' | b'A'..=b'Z')
         {
            let suffix_string = &source[underscore + 1..];
            if let Some(&suffix) = SUFFIXES.get(suffix_string) {
               return (&source[..underscore], suffix);
            } else {
               self.emit_error(
                  ErrorKind::InvalidNumberLiteralSuffix(source.into()),
                  span.clone(),
               );
            }
         }
      }
      (source, LiteralSuffix::None)
   }

   /// Converts an ASCII digit to a u64.
   fn digit_to_u64(digit: u8) -> u64 {
      assert!(matches!(digit, b'0'..=b'9'));
      (digit - b'0') as u64
   }

   fn overflow_error(&mut self, string: &str, span: Span) {
      self.emit_error(ErrorKind::IntegerTooBig(string.into()), span)
   }

   /// Parses the given string into a `u64`. The string must not be empty, otherwise an assertion is
   /// triggered.
   /// If an error is occured while parsing, `Err(())` is returned, and the error is added to the
   /// phase's error list.
   fn parse_integer(&mut self, string: &str, span: &Span) -> Result<u64, ()> {
      assert!(!string.is_empty());

      let bytes = string.as_bytes();
      let mut result: u64 = Self::digit_to_u64(bytes[0]);
      for &c in &bytes[1..] {
         if matches!(c, b'0'..=b'9') {
            // We don't want to panic on overflow here, instead report a nice error to the user.
            // Hence the usage of `checked_mul` and `checked_add`.
            let digit = Self::digit_to_u64(c);
            result =
               result.checked_mul(10).ok_or_else(|| self.overflow_error(string, span.clone()))?;
            result = result
               .checked_add(digit)
               .ok_or_else(|| self.overflow_error(string, span.clone()))?;
         } else if c == b'_' {
            // Continue, because _ is a valid separating character.
         } else {
            // If a different character is found, panic! because the lexer should've already sorted
            // the appropriate characters out for us.
            panic!("unexpected character in integer literal: {}", c);
         }
      }

      Ok(result)
   }

   /// Converts a `u64` to a smaller unsigned integer. `type_name` and `span` are used for
   /// emitting errors in case of overflow.
   fn convert_unsigned<R>(&mut self, x: u64, type_name: &str, span: &Span) -> R
   where
      R: Default + TryFrom<u64>,
   {
      match x.try_into() {
         Ok(ok) => ok,
         Err(..) => {
            let kind = ErrorKind::UnsignedIntegerOverflowForType(x.into(), type_name.into());
            self.emit_error(kind, span.clone());
            R::default()
         }
      }
   }

   /// Converts a `u64` to a signed integer. Emits an error using the given `type_name` and `span`,
   /// and returns `R::default()` in case of an overflow error.
   fn convert_signed<R>(&mut self, negative: bool, x: u64, type_name: &str, span: &Span) -> R
   where
      R: Default + TryFrom<i64>,
   {
      // i64 is the largest possible signed integer in tsuki, so we use that as the source for our
      // conversion. Note that if we converted straight from u64 to R, the minimum negative number
      // edge case -128_i8 would not work. The 128 would get converted into an i8, causing an
      // overflow, so instead we first need to convert to an i64, then apply the sign, and
      // afterwards convert the i64 to R.
      let mut signed: i64 = match x.try_into() {
         Ok(ok) => ok,
         Err(..) => {
            let kind = ErrorKind::UnsignedIntegerOverflowForType(x.into(), type_name.into());
            self.emit_error(kind, span.clone());
            return R::default();
         }
      };
      if negative {
         signed *= -1;
      }
      match signed.try_into() {
         Ok(ok) => ok,
         Err(..) => {
            let kind = ErrorKind::SignedIntegerOverflowForType(signed.into(), type_name.into());
            self.emit_error(kind, span.clone());
            R::default()
         }
      }
   }

   /// Converts a `u64` to an `f64`, optionally flipping its sign around.
   fn convert_to_float(&self, negative: bool, x: u64) -> f64 {
      if negative {
         -(x as f64)
      } else {
         x as f64
      }
   }

   /// Converts the abstract Integer `node` to a concretely typed node.
   fn convert_integer_node(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      negative: bool,
      number: u64,
      mut suffix: LiteralSuffix,
   ) {
      match suffix {
         LiteralSuffix::None | LiteralSuffix::I => {
            suffix = LiteralSuffix::from(self.common.default_types.int_width);
         }
         LiteralSuffix::U => {
            suffix = LiteralSuffix::from(self.common.default_types.size_width);
         }
         LiteralSuffix::F => {
            suffix = LiteralSuffix::from(self.common.default_types.float_width);
         }
         _ => (),
      }
      if matches!(
         suffix,
         LiteralSuffix::U8 | LiteralSuffix::U16 | LiteralSuffix::U32 | LiteralSuffix::U64
      ) && negative
      {
         self.emit_error(ErrorKind::UintCannotBeNegative, ast.span(node).clone());
         return;
      }
      let span = ast.span(node);
      let (kind, extra) = match suffix {
         LiteralSuffix::None | LiteralSuffix::I | LiteralSuffix::U | LiteralSuffix::F => {
            // These cases are canonicalized to types configured in `common.default_types`.
            unreachable!()
         }
         LiteralSuffix::I8 => (
            NodeKind::Int8,
            NodeData::Int8(self.convert_signed(negative, number, "Int8", span)),
         ),
         LiteralSuffix::I16 => (
            NodeKind::Int16,
            NodeData::Int16(self.convert_signed(negative, number, "Int16", span)),
         ),
         LiteralSuffix::I32 => (
            NodeKind::Int32,
            NodeData::Int32(self.convert_signed(negative, number, "Int32", span)),
         ),
         LiteralSuffix::I64 => (
            NodeKind::Int64,
            NodeData::Int64(self.convert_signed(negative, number, "Int64", span)),
         ),
         LiteralSuffix::U8 => (
            NodeKind::Uint8,
            NodeData::Uint8(self.convert_unsigned(number, "Uint8", span)),
         ),
         LiteralSuffix::U16 => (
            NodeKind::Uint16,
            NodeData::Uint16(self.convert_unsigned(number, "Uint16", span)),
         ),
         LiteralSuffix::U32 => (
            NodeKind::Uint32,
            NodeData::Uint32(self.convert_unsigned(number, "Uint32", span)),
         ),
         LiteralSuffix::U64 => (NodeKind::Uint64, NodeData::Uint64(number)),
         LiteralSuffix::F32 => (
            NodeKind::Float32,
            NodeData::Float32(self.convert_to_float(negative, number) as f32),
         ),
         LiteralSuffix::F64 => (
            NodeKind::Float64,
            NodeData::Float64(self.convert_to_float(negative, number)),
         ),
      };
      ast.convert(node, kind);
      ast.set_extra(node, extra);
   }

   /// Extracts the sign and number node from a potentially `Neg` node. The first value returned
   /// specifies whether the number is negative, and the second value is the actual number.
   fn extract_neg_node(ast: &Ast, node: NodeHandle) -> (bool, NodeHandle) {
      let negative = ast.kind(node) == NodeKind::Neg;
      let number_node = if negative {
         ast.first_handle(node)
      } else {
         node
      };
      (negative, number_node)
   }

   /// Parses an integer literal to one of the type-strict kinds `Int8`, `Int16`, etc.
   fn analyze_integer(&mut self, ast: &mut Ast, node: NodeHandle) {
      let (negative, number_node) = Self::extract_neg_node(ast, node);
      let source = self.common.get_source_range_from_node(ast, number_node);
      assert!(!source.is_empty());
      let (digits, suffix) = self.split_number(source, ast.span(node));
      match self.parse_integer(digits, ast.span(node)) {
         Ok(number) => self.convert_integer_node(ast, node, negative, number, suffix),
         Err(..) => ast.convert(node, NodeKind::Error),
      }
   }

   /// Parses a floating point literal to an `f64`. If an error occurs, the function panics, as
   /// floats are not susceptible to overflow; only precision loss on large scales.
   fn parse_float(string: &str) -> f64 {
      // Parsing floats is hard. That's why we're using the Rust standard library for this purpose.
      // However, the standard library expects floats without underscores `_`, which tsuki
      // allows for. Thus, all the digits have to be first accumulated into a separate string
      // without these underscores.
      // We use a SmallVec for this purpose, so as to allocate memory on the stack for relatively
      // small literals. I don't think there are many cases where people use more than 32 characters
      // in a literal, but in these cases the SmallVec is simply going to move over to the heap.
      let mut digits = SmallVec::<[u8; 32]>::new();
      for b in string.bytes() {
         if b != b'_' {
            digits.push(b);
         }
      }
      // Safety: Using `from_utf8_unchecked` is safe, as floating point literals cannot have any
      // UTF-8 characters in them.
      let filtered = unsafe { std::str::from_utf8_unchecked(&digits) };
      filtered.parse::<f64>().expect("the lexer must provide only valid digits")
      // Idea: emit a warning when the literal suffers significant precision loss.
   }

   /// Converts the abstract `Float` node to a concrete node of kind `Float32` or `Float64`.
   fn convert_float_node(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      negative: bool,
      mut number: f64,
      mut suffix: LiteralSuffix,
   ) {
      match suffix {
         LiteralSuffix::None | LiteralSuffix::F => {
            suffix = LiteralSuffix::from(self.common.default_types.float_width);
         }
         _ => (),
      }
      if negative {
         number *= -1.0;
      }
      let (kind, extra) = match suffix {
         LiteralSuffix::None | LiteralSuffix::F => unreachable!(),
         LiteralSuffix::F32 => (NodeKind::Float32, NodeData::Float32(number as f32)),
         LiteralSuffix::F64 => (NodeKind::Float64, NodeData::Float64(number)),
         _ => {
            self.emit_error(ErrorKind::InvalidFloatSuffix, ast.span(node).clone());
            ast.convert(node, NodeKind::Error);
            return;
         }
      };
      ast.convert(node, kind);
      ast.set_extra(node, extra);
   }

   /// Parses a float to a `Float32` or a `Float64`.
   fn analyze_float(&mut self, ast: &mut Ast, node: NodeHandle) {
      let (negative, number_node) = Self::extract_neg_node(ast, node);
      let source = self.common.get_source_range_from_node(ast, number_node);
      assert!(!source.is_empty());
      let (digits, suffix) = self.split_number(source, ast.span(node));
      let number = Self::parse_float(digits);
      self.convert_float_node(ast, node, negative, number, suffix);
   }

   /// Walks through the sub-nodes of a branch node.
   fn walk_branch(&mut self, ast: &mut Ast, node: NodeHandle) {
      let left = ast.first_handle(node);
      match ast.kind(node) {
         // The negation sign `-` is not included in the literal, so these extra cases ensure that
         // edge cases such as -128_u8 are handled correctly without causing an overflow error.
         NodeKind::Neg if ast.kind(left) == NodeKind::Integer => {
            self.analyze_integer(ast, node);
         }
         NodeKind::Neg if ast.kind(left) == NodeKind::Float => {
            self.analyze_float(ast, node);
         }
         _ => {
            ast.walk_mut(node, |ast, child| {
               self.analyze_node(ast, child);
            });
         }
      }
   }

   /// Analyzes the given syntax tree node.
   fn analyze_node(&mut self, ast: &mut Ast, node: NodeHandle) {
      match ast.kind(node) {
         NodeKind::Integer => self.analyze_integer(ast, node),
         NodeKind::Float => self.analyze_float(ast, node),
         kind if kind.is_branch() => self.walk_branch(ast, node),
         _ => (),
      }
   }
}

impl SemPass for SemLiterals<'_> {
   type Result = ();

   /// Performs literal resolution for the syntax tree.
   fn analyze(&mut self, mut ast: Ast, root_node: NodeHandle) -> Ast {
      self.analyze_node(&mut ast, root_node);
      ast
   }

   fn filename(&self) -> &str {
      &self.common.filename
   }

   fn errors(&self) -> &Errors {
      &self.errors
   }

   fn errors_mut(&mut self) -> &mut Errors {
      &mut self.errors
   }

   fn into_errors(self) -> Errors {
      self.errors
   }
}
