//! Semantic analyzer for types.

mod control_flow;
mod conversions;
mod functions;
mod locations;
mod lookups;
mod operators;

use std::path::Path;

use crate::ast::{Ast, NodeHandle, NodeKind};
use crate::common::{ErrorKind, Errors};
use crate::scope::{ScopeStack, Scopes, Symbols};
use crate::sem::{SemCommon, SemPass};
use crate::types::{BuiltinTypes, TypeId, TypeLog, TypeLogEntry, Types};

pub(crate) struct SemTypes<'s> {
   common: &'s SemCommon<'s>,
   errors: Errors,

   types: &'s mut Types,
   log: &'s mut TypeLog,
   builtin: &'s BuiltinTypes,
   scopes: &'s mut Scopes,
   symbols: &'s mut Symbols,

   scope_stack: ScopeStack,
}

/// Values borrowed to `SemTypes`, used during its construction.
pub(crate) struct SemTypesBorrows<'s> {
   pub(crate) common: &'s SemCommon<'s>,
   pub(crate) types: &'s mut Types,
   pub(crate) log: &'s mut TypeLog,
   pub(crate) builtin: &'s BuiltinTypes,
   pub(crate) scopes: &'s mut Scopes,
   pub(crate) symbols: &'s mut Symbols,
}

/// Specifies whether a node should be annotated in expression or statement context.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum NodeContext {
   Expression,
   Statement,
}

impl<'s> SemTypes<'s> {
   /// Creates a new instance of the `SemTypes` analysis phase.
   pub fn new(borrows: SemTypesBorrows<'s>) -> Self {
      let SemTypesBorrows {
         common,
         types,
         log,
         builtin,
         scopes,
         symbols,
      } = borrows;
      let mut scope_stack = ScopeStack::new();
      // The scope stack is always initialized with a top-level module scope, such that there is
      // always a valid scope on top.
      let module_scope = scope_stack.push(scopes.create_scope());
      builtin.register_in(scopes, symbols, module_scope);
      SemTypes {
         common,
         errors: Errors::new(),

         types,
         log,
         builtin,
         scopes,
         symbols,

         scope_stack,
      }
   }

   /// Annotates the given AST with the given type, and returns the type.
   fn annotate(&mut self, ast: &mut Ast, node: NodeHandle, typ: TypeId) -> TypeLogEntry {
      ast.set_type_id(node, typ);
      self.log.push(typ, node)
   }

   /// Emits an error of the given kind, also returning the error type.
   fn error(&mut self, ast: &Ast, node: NodeHandle, kind: ErrorKind) -> TypeLogEntry {
      self.emit_error(kind, ast.span(node).clone());
      self.log.push(self.builtin.t_error, node)
   }

   /// Emits a type mismatch error.
   fn type_mismatch(
      &mut self,
      ast: &Ast,
      node: NodeHandle,
      expected: TypeId,
      got: TypeId,
   ) -> TypeLogEntry {
      let expected_name = self.types.name(expected);
      let provided_name = self.types.name(got);
      let kind = ErrorKind::TypeMismatch(expected_name.to_owned(), provided_name.to_owned());
      self.error(ast, node, kind)
   }

   /// Annotates a literal with a concrete type.
   fn annotate_literal(&mut self, ast: &mut Ast, node: NodeHandle) -> TypeLogEntry {
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
      self.annotate(ast, node, typ)
   }

   /// Annotates statements in a list of statements.
   fn annotate_statement_list(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
      let mut last_log = None;
      ast.walk_node_list_mut(node, |ast, index, child| {
         // Trailing expressions in expression context statement lists get special treatment.
         // They are resulting expressions of the these statement lists, and thus get analyzed as
         // proper expressions rather than statements. They are also not subject to triggering the
         // UnusedValue error, as the resulting value _is_ actually used - it's the result of the
         // statement list.
         let is_last_expression =
            ast.is_last_child(node, index) && context == NodeContext::Expression;
         let log_entry = self.annotate_node(
            ast,
            child,
            if is_last_expression {
               NodeContext::Expression
            } else {
               NodeContext::Statement
            },
         );
         let typ = self.log.typ(log_entry);
         let type_kind = self.types.kind(typ);
         // For expressions, we have some special cases.
         if !type_kind.is_statement() {
            if is_last_expression {
               // Trailing expressions get assigned to the `last_log`, so that we know what the
               // result of the statement list is.
               last_log = Some(log_entry);
            } else {
               // Other expressions are unused, which is invalid.
               self.emit_error(ErrorKind::UnusedValue, ast.span(child).clone());
            }
         }
      });
      // Statement lists in expression context must always have a trailing expression.
      if context == NodeContext::Expression && last_log.is_none() {
         return self.error(ast, node, ErrorKind::MissingResult);
      }
      // Nodes in expression context inherit their type from the last expression statement in
      // the list.
      if let Some(log) = last_log {
         self.annotate(ast, node, self.log.typ(log))
      } else {
         self.annotate(ast, node, self.builtin.t_statement)
      }
   }

   /// Annotates the given AST node.
   fn annotate_node(
      &mut self,
      ast: &mut Ast,
      node: NodeHandle,
      context: NodeContext,
   ) -> TypeLogEntry {
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

         // Locations
         NodeKind::Identifier => self.annotate_location(ast, node).into(),

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
         | NodeKind::Plus
         | NodeKind::Minus
         | NodeKind::Mul
         | NodeKind::Div
         | NodeKind::Equal
         | NodeKind::NotEqual
         | NodeKind::Less
         | NodeKind::LessEqual
         | NodeKind::Greater
         | NodeKind::GreaterEqual => self.annotate_binary_operator(ast, node),
         NodeKind::Call => self.annotate_call(ast, node),
         NodeKind::Assign => self.annotate_assignment(ast, node, context).into(),
         // Other operators are to be implemented later.

         // Control flow
         NodeKind::StatementList => self.annotate_statement_list(ast, node, context),
         NodeKind::Pass => self.annotate_pass(ast, node),
         NodeKind::Do => self.annotate_do(ast, node, context),
         NodeKind::If => self.annotate_if(ast, node, context),
         NodeKind::While => self.annotate_while(ast, node),

         // Declarations
         NodeKind::Val | NodeKind::Var => self.annotate_variable_declaration(ast, node).into(),
         NodeKind::Fun => self.annotate_function_declaration(ast, node).into(),

         // Other nodes are invalid (or not implemented yet).
         other => self.error(ast, node, ErrorKind::SemTypesInvalidAstNode(other)),
      }
   }
}

impl SemPass for SemTypes<'_> {
   type Result = TypeLogEntry;

   /// Performs type analysis for the given AST node. This annotates the node with a concrete type.
   fn analyze(&mut self, mut ast: Ast, root_node: NodeHandle) -> Ast {
      let _ = self.annotate_node(&mut ast, root_node, NodeContext::Statement);
      ast
   }

   fn filename(&self) -> &Path {
      &self.common.file.filename
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
