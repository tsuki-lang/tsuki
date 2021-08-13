//! Common code generation state.

use std::fmt;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};
use tsuki_frontend::common::{Error, ErrorKind, SourceFile};

/// Code generation state shared across functions.
pub struct CodeGen<'c> {
   pub(crate) source: SourceFile,
   pub(crate) context: &'c Context,
   pub(crate) module: Module<'c>,
   pub(crate) builder: Builder<'c>,
}

impl<'c> CodeGen<'c> {
   pub fn new(source: SourceFile, context: &'c Context) -> Self {
      let mut state = Self {
         source,
         context,
         // TODO: import, module resolution and names.
         module: context.create_module("main"),
         builder: context.create_builder(),
      };
      // Temporary: set up some libc functions.
      state.load_libc();
      state
   }

   /// Creates a code generation error.
   fn error(&self, ast: &Ast, node: NodeHandle, message: String) -> Error {
      Error {
         filename: self.source.filename.clone(),
         span: ast.span(node).clone(),
         kind: ErrorKind::CodeGen(message),
      }
   }

   /// Maps an LLVM error string to an Error.
   fn map_llvm_error<T>(
      &self,
      ast: &Ast,
      node: NodeHandle,
      r: Result<T, LLVMString>,
   ) -> Result<T, Error> {
      r.map_err(|e| self.error(ast, node, format!("LLVM error: {}", e)))
   }

   /// Generates code for an arbitrary node.
   pub fn generate(&mut self, ast: &Ast, node: NodeHandle) -> Result<(), Error> {
      match ast.kind(node) {
         NodeKind::StatementList => self.generate_statement_list(ast, node)?,
         other => {
            return Err(self.error(ast, node, format!("node kind not supported: {:?}", other)))
         }
      }
      Ok(())
   }

   fn generate_statement_list(&mut self, ast: &Ast, node: NodeHandle) -> Result<(), Error> {
      // For debugging purposes, the last statement in the list is interpreted as a Float32 to be
      // passed to printf.
      let statements = ast.extra(node).unwrap_node_list();
      if statements.len() > 0 {
         for i in 0..statements.len() - 1 {}
      } else {
         return Err(self.error(ast, node, "no statements to execute".into()));
      }
      Ok(())
   }
}

impl fmt::Debug for CodeGen<'_> {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", &self.module.print_to_string().to_str().unwrap())
   }
}
