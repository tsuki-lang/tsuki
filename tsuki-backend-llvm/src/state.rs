//! Common code generation state.

use inkwell::module::Module;
use inkwell::context::Context;
use inkwell::builder::Builder;
use tsuki_frontend::common::{Error, ErrorKind, Errors, SourceFile};
use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};

/// Code generation state shared across functions.
pub struct CodeGen<'s, 'c> {
   source: &'s SourceFile,
   context: &'c Context,
   module: Module<'c>,
   builder: Builder<'c>,
}

impl<'s, 'c> CodeGen<'s, 'c> {
   pub fn new(source: &'s SourceFile, context: &'c Context) -> Self {
      Self {
         source,
         context,
         // TODO: import, module resolution and names.
         module: context.create_module("main"),
         builder: context.create_builder(),
      }
   }

   /// Creates a code generation error.
   fn error(&mut self, ast: &Ast, node: NodeHandle, message: String) -> Error {
      Error {
         filename: self.source.filename.clone(),
         span: ast.span(node).clone(),
         kind: ErrorKind::CodeGen(message),
      }
   }

   /// Generates code for an arbitrary node.
   pub fn generate(&mut self, ast: &Ast, node: NodeHandle) -> Result<(), Error> {
      match ast.kind(node) {
         other => return Err(self.error(ast, node, format!("node kind not supported: {:?}", other))),
      }
      Ok(())
   }
}
