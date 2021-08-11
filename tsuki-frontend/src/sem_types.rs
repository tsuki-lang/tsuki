//! Semantic analyzer for types.

use crate::ast::{Ast, Mutation, NodeHandle, NodeKind};
use crate::common::{ErrorKind, Errors};
use crate::sem::{SemCommon, SemPass};
use crate::types::{BuiltinTypes, TypeId, TypeLog, TypeLogEntry, Types};

pub(crate) struct SemTypes<'c, 't, 'tl, 'bt> {
   common: &'c SemCommon,
   errors: Errors,
   mutations: Vec<Mutation>,

   types: &'t mut Types,
   log: &'tl mut TypeLog,
   builtin: &'bt BuiltinTypes,
}

impl<'c, 't, 'tl, 'bt> SemTypes<'c, 't, 'tl, 'bt> {
   /// Creates a new instance of the `SemTypes` analysis phase.
   pub fn new(
      common: &'c SemCommon,
      types: &'t mut Types,
      log: &'tl mut TypeLog,
      builtin: &'bt BuiltinTypes,
   ) -> Self {
      SemTypes {
         common,
         errors: Errors::new(),
         mutations: Vec::new(),
         types,
         log,
         builtin,
      }
   }

   /// Annotates the given AST with the given type, and returns the type.
   fn annotate(&mut self, node: NodeHandle, typ: TypeId) -> TypeLogEntry {
      self.types.set_node_type(node, typ);
      self.log.push(typ, node)
   }

   /// Annotates a literal with a concrete type.
   fn annotate_literal(&mut self, ast: &Ast, node: NodeHandle) -> TypeLogEntry {
      let typ = match ast.kind(node) {
         NodeKind::True => self.builtin.t_bool,
         NodeKind::False => self.builtin.t_bool,
         NodeKind::Uint8 => self.builtin.t_uint8,
         NodeKind::Uint16 => self.builtin.t_uint16,
         NodeKind::Uint32 => self.builtin.t_uint32,
         NodeKind::Uint64 => self.builtin.t_uint64,
         NodeKind::Int8 => self.builtin.t_int8,
         NodeKind::Int16 => self.builtin.t_int16,
         NodeKind::Int32 => self.builtin.t_int32,
         NodeKind::Int64 => self.builtin.t_int64,
         NodeKind::Float32 => self.builtin.t_float32,
         NodeKind::Float64 => self.builtin.t_float64,
         NodeKind::Character => self.builtin.t_char,
         _ => unreachable!(),
      };
      self.annotate(node, typ)
   }

   // Currently, this does some rather simplistic analysis just to Make it Work™, but in the
   // future when operators will be lowered to trait instance function calls, this will be
   // replaced by much simpler logic and compiler intrinsics inside the stdlib.

   /// Annotates a unary operator with types.
   fn annotate_unary_operator(&mut self, ast: &Ast, node: NodeHandle) -> TypeLogEntry {
      let log_entry = self.analyze(ast, ast.first_handle(node));
      let right = self.log.typ(log_entry);
      let right_kind = self.types.kind(right);
      let typ = match ast.kind(node) {
         NodeKind::Not if right == self.builtin.t_bool => right,
         NodeKind::BitNot if right_kind.is_integer() => right,
         NodeKind::Neg if right_kind.is_numeric() => right,
         _ => {
            let right_name = self.types.name(right);
            let kind = ErrorKind::InvalidUnaryOperator(right_name.into());
            return self.error(ast, node, kind);
         }
      };
      self.annotate(node, typ)
   }

   /// Attempts to convert the type `from` to type `tp`. If an implicit conversion is not possible,
   /// returns `None`. Otherwise returns the converted type ID.
   fn perform_implicit_conversion(
      &mut self,
      node: NodeHandle,
      from: TypeId,
      to: TypeId,
   ) -> Option<TypeLogEntry> {
      // If the two types are equal, there's need for conversion.
      if from == to {
         return Some(self.log.push(to, node));
      }
      // Otherwise, compare their kinds for various traits.
      let from_kind = self.types.kind(from);
      let to_kind = self.types.kind(to);

      // Widening integer conversions
      if from_kind.is_integer() && to_kind.is_integer() {
         // Integers are only implicitly convertible to wider types of the same signedness,
         // eg. Int8 -> Int16, Int32 -> Int64, but not Int64 -> Int32, or Uint32 -> Int32.
         let from_size = from_kind.unwrap_integer();
         let to_size = to_kind.unwrap_integer();
         if to_size >= from_size {
            return Some(self.log.push(to, node));
         }
      }

      // Widening float conversions
      if from_kind.is_float() && to_kind.is_float() {
         // Floats are only implicitly convertible if the destination type is wider than the
         // source type (Float32 -> Float64).
         let from_size = from_kind.unwrap_float();
         let to_size = to_kind.unwrap_float();
         if to_size >= from_size {
            return Some(self.log.push(to, node));
         }
      }

      None
   }

   /// Annotates a binary operator with types.
   fn annotate_binary_operator(&mut self, ast: &Ast, node: NodeHandle) -> TypeLogEntry {
      let (left, right) = (ast.first_handle(node), ast.second_handle(node));
      let left_entry = self.analyze(ast, left);
      let right_entry = self.analyze(ast, right);
      let left_type = self.log.typ(left_entry);
      let right_type = self.log.typ(right_entry);
      let conversion = self.perform_implicit_conversion(right, right_type, left_type);
      let typ = match ast.kind(node) {
         NodeKind::Plus | NodeKind::Minus | NodeKind::Mul | NodeKind::Div
            if conversion.is_some() =>
         {
            left_type
         }
         _ => {
            let left_name = self.types.name(left_type);
            let right_name = self.types.name(right_type);
            let kind = ErrorKind::TypeMismatch(left_name.into(), right_name.into());
            return self.error(ast, node, kind);
         }
      };
      self.annotate(node, typ)
   }

   /// Annotates statements in a list of statements.
   fn annotate_statement_list(&mut self, ast: &Ast, node: NodeHandle) -> TypeLogEntry {
      ast.walk(node, |ast, node| {
         // TODO: Don't ignore the type. Instead, enforce it to be ().
         // Right now this isn't done for testing purposes.
         let _ = self.analyze(ast, node);
      });
      self.annotate(node, self.builtin.t_statement)
   }

   /// Emits an error of the given kind, also returning the error type.
   fn error(&mut self, ast: &Ast, node: NodeHandle, kind: ErrorKind) -> TypeLogEntry {
      self.emit_error(kind, ast.span(node).clone());
      self.log.push(self.builtin.t_error, node)
   }
}

impl SemPass for SemTypes<'_, '_, '_, '_> {
   type Result = TypeLogEntry;

   /// Performs type analysis for the given AST node. This annotates the node with a concrete type.
   fn analyze(&mut self, ast: &Ast, node: NodeHandle) -> TypeLogEntry {
      match ast.kind(node) {
         // Literals
         | NodeKind::True
         | NodeKind::False
         | NodeKind::Uint8
         | NodeKind::Uint16
         | NodeKind::Uint32
         | NodeKind::Uint64
         | NodeKind::Int8
         | NodeKind::Int16
         | NodeKind::Int32
         | NodeKind::Int64
         | NodeKind::Float32
         | NodeKind::Float64
         | NodeKind::Character => self.annotate_literal(ast, node),

         // Unary operators
         // ---
         // The following operators were omitted from the generic rule:
         // NodeKind::Member - magic for field access in self
         // NodeKind::Ref - magic for creating pointers
         // NodeKind::Deref - magic for dereferencing
         NodeKind::Not | NodeKind::Neg | NodeKind::BitNot => {
            self.annotate_unary_operator(ast, node)
         }

         // Binary operators
         // ---
         // The following kinds were omitted from the generic rule:
         // NodeKind::Dot - magic for field access
         NodeKind::Plus | NodeKind::Minus | NodeKind::Mul | NodeKind::Div => {
            self.annotate_binary_operator(ast, node)
         }
         // Other operators are to be implemented later.

         // Control flow
         NodeKind::StatementList => self.annotate_statement_list(ast, node),

         // Other nodes are invalid.
         _ => self.error(ast, node, ErrorKind::SemTypesInvalidAstNode),
      }
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

   fn mutations(&self) -> &[Mutation] {
      &self.mutations
   }
}
