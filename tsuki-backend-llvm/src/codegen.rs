//! Common code generation state.

use std::fmt;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::StructType;
use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};
use tsuki_frontend::common::{Error, ErrorKind, SourceFile};
use tsuki_frontend::Ir;

/// Code generation state shared across functions.
pub struct CodeGen<'c> {
   pub(crate) source: SourceFile,
   pub(crate) context: &'c Context,
   pub(crate) module: Module<'c>,
   pub(crate) builder: Builder<'c>,

   pub(crate) unit_type: StructType<'c>,
}

impl<'c> CodeGen<'c> {
   pub fn new(source: SourceFile, context: &'c Context) -> Self {
      let mut state = Self {
         source,
         context,
         // TODO: import, module resolution and names.
         module: context.create_module("main"),
         unit_type: context.struct_type(&[], false),
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
   pub fn generate_statement(&mut self, ir: &Ir, node: NodeHandle) -> Result<(), Error> {
      match ir.ast.kind(node) {
         NodeKind::StatementList => self.generate_statement_list(ir, node)?,
         _ => {
            let _ = self.generate_expression(ir, node);
         }
      }
      Ok(())
   }

   /// Generates code for a list of statements.
   fn generate_statement_list(&mut self, ir: &Ir, node: NodeHandle) -> Result<(), Error> {
      for &statement in ir.ast.extra(node).unwrap_node_list() {
         self.generate_statement(ir, statement)?;
      }
      Ok(())
   }
}

impl fmt::Debug for CodeGen<'_> {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", &self.module.print_to_string().to_str().unwrap())
   }
}
