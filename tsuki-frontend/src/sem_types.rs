//! Semantic analyzer for types.

use crate::ast::{Ast, Mutation, NodeHandle, NodeKind};
use crate::common::{ErrorKind, Errors, Span};
use crate::sem::{Sem, SemCommon};
use crate::types::{BuiltinTypes, TypeId, Types};

pub(crate) struct SemTypes<'c, 't, 'bt> {
   common: &'c SemCommon,
   errors: Errors,
   mutations: Vec<Mutation>,

   types: &'t mut Types,
   builtin: &'bt BuiltinTypes,
}

impl<'c, 't, 'bt> SemTypes<'c, 't, 'bt> {
   /// Creates a new instance of the `SemTypes` analysis phase.
   pub fn new(common: &'c SemCommon, types: &'t mut Types, builtin: &'bt BuiltinTypes) -> Self {
      SemTypes {
         common,
         errors: Errors::new(),
         mutations: Vec::new(),
         types,
         builtin,
      }
   }

   /// Annotates the given AST with the given type, and returns the type.
   fn annotate(&mut self, ast: &Ast, node: NodeHandle, typ: TypeId) -> TypeId {
      self.types.set_node_type(node, typ);
      typ
   }

   // Currently, this does some rather simplistic analysis just to Make it Work™, but in the
   // future when operators will be lowered to trait instance function calls, this will be
   // replaced by much simpler logic and compiler intrinsics inside the stdlib.

   /// Annotates a unary operator with types.
   fn annotate_unary_operator(&mut self, ast: &Ast, node: NodeHandle) -> TypeId {
      let right = self.analyze(ast, ast.first_handle(node));
      let typ = match ast.kind(node) {
         _ => self.error(ErrorKind::BinaryOperatorNotYetSupported, ast.span(node).clone()),
      };
      self.annotate(ast, node, typ)
   }

   /// Annotates a binary operator with types.
   fn annotate_binary_operator(&mut self, ast: &Ast, node: NodeHandle) -> TypeId {
      let left = self.analyze(ast, ast.first_handle(node));
      let right = self.analyze(ast, ast.second_handle(node));
      let typ = match ast.kind(node) {
         _ => self.error(ErrorKind::BinaryOperatorNotYetSupported, ast.span(node).clone())
      };
      self.annotate(ast, node, typ)
   }

   /// Annotates statements in a list of statements.
   fn annotate_statement_list(&mut self, ast: &Ast, node: NodeHandle) -> TypeId {
      ast.walk(node, |ast, node| {
         // TODO: Don't ignore the type. Instead, enforce it to be ().
         // Right now this isn't done for testing purposes.
         let _ = self.analyze(ast, node);
      });
      self.builtin.t_statement
   }

   /// Emits an error of the given kind, also returning the error type.
   fn error(&mut self, kind: ErrorKind, span: Span) -> TypeId {
      self.emit_error(kind, span);
      self.builtin.t_error
   }
}

impl Sem for SemTypes<'_, '_, '_> {
   type Result = TypeId;

   /// Performs type analysis for the given AST node. This annotates the node with a concrete type.
   fn analyze(&mut self, ast: &Ast, node: NodeHandle) -> TypeId {
      match ast.kind(node) {
         // Literals
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

         // Unary operators
         // ---
         // The following operators were omitted from the generic rule:
         // NodeKind::Member - magic for field access in self
         // NodeKind::Ref - magic for creating pointers
         // NodeKind::Deref - magic for dereferencing
         | NodeKind::Not
         | NodeKind::Neg
         | NodeKind::BitNot => self.annotate_unary_operator(ast, node),

         // Binary operators
         // ---
         // The following kinds were omitted from the generic rule:
         // NodeKind::Dot - magic for field access
         | NodeKind::Plus
         | NodeKind::Minus
         | NodeKind::Mul
         | NodeKind::Div => self.annotate_binary_operator(ast, node),
         // Other operators are to be implemented later.

         // Control flow
         NodeKind::StatementList => self.annotate_statement_list(ast, node),

         // Other nodes are invalid.
         _ => self.error(ErrorKind::SemTypesInvalidAstNode, ast.span(node).clone()),
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
