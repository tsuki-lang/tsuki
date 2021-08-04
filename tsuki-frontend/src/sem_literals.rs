//! Semantic analyzer for literal kinds.
//! This performs some basic initial analysis to convert literal kinds from generic `Integer` and
//! `Float` to concrete types `Int8`, `Int16`, etc., including negation.
//! Note that `SemTypes` may perform additional conversions later down the line.

use crate::ast::{Ast, Mutation, NodeHandle, NodeKind};
use crate::common::{ErrorKind, Errors, Span};
use crate::sem::Sem;

/// State for the `SemLiterals` analysis phase.
pub struct SemLiterals<'f> {
   filename: &'f str,
   errors: Errors,
   mutations: Vec<Mutation>,
}

impl<'f> SemLiterals<'f> {
   /// Creates a new instance of the `SemTypes` analysis phase.
   pub fn new(filename: &'f str) -> Self {
      SemLiterals {
         filename,
         errors: Errors::new(),
         mutations: Vec::new(),
      }
   }

   fn canonicalize_integer(&mut self, ast: &Ast, node: NodeHandle) {}

   fn canonicalize_float(&mut self, ast: &Ast, node: NodeHandle) {}

   fn walk_branch(&mut self, ast: &Ast, node: NodeHandle) {
      ast.walk(node, |ast, child| {
         self.analyze(ast, child);
      });
   }
}

impl<'f> Sem for SemLiterals<'f> {
   type Result = ();

   /// Performs literal resolution for the given AST node.
   fn analyze(&mut self, ast: &Ast, node: NodeHandle) {
      match ast.kind(node) {
         NodeKind::Integer => self.canonicalize_integer(ast, node),
         NodeKind::Float => self.canonicalize_float(ast, node),
         kind if kind.is_branch() => self.walk_branch(ast, node),
         _ => (),
      }
   }

   fn filename(&self) -> &str {
      self.filename
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
