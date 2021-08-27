//! Common code generation state.

use std::cell::RefCell;
use std::fmt;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::support::LLVMString;
use inkwell::types::StructType;
use inkwell::values::{BasicValue, FunctionValue};
use tsuki_frontend::ast::{Ast, NodeHandle, NodeKind};
use tsuki_frontend::common::{Error, ErrorKind, SourceFile};
use tsuki_frontend::sem::Ir;

use crate::functions::Function;
use crate::variables::Variables;

/// Code generation state shared across functions.
pub struct CodeGen<'c, 'pm> {
   pub(crate) source: SourceFile,
   pub(crate) context: &'c Context,
   pub(crate) module: Module<'c>,
   pub(crate) builder: Builder<'c>,
   pub(crate) pass_manager: &'pm PassManager<FunctionValue<'c>>,

   pub(crate) function: Function<'c>,
   pub(crate) variables: RefCell<Variables<'c>>,

   pub(crate) unit_type: StructType<'c>,
}

impl<'c, 'pm> CodeGen<'c, 'pm> {
   pub fn new(
      source: SourceFile,
      context: &'c Context,
      pass_manager: &'pm PassManager<FunctionValue<'c>>,
      module: Module<'c>,
      function: Function<'c>,
   ) -> Self {
      let mut state = Self {
         source,
         context,
         // TODO: import, module resolution and names.
         module,
         builder: context.create_builder(),
         pass_manager,

         function,
         variables: RefCell::new(Variables::new()),

         unit_type: context.struct_type(&[], false),
      };
      state.builder.position_at_end(state.function.entry_block);
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
   pub fn generate_statement(&self, ir: &Ir, node: NodeHandle) {
      match ir.ast.kind(node) {
         // Control flow
         NodeKind::Pass => (),
         NodeKind::StatementList => self.generate_statements(ir, node),
         NodeKind::DoStatement => {
            let _ = self.generate_do(ir, node);
         }
         // Declarations
         NodeKind::Val | NodeKind::Var => self.generate_variable_declaration(ir, node),
         NodeKind::AssignDiscard => self.generate_discarding_assignment(ir, node),
         // Expressions
         _ => {
            let _ = self.generate_expression(ir, node);
         }
      }
   }

   pub fn finish_function(&self, return_value: Option<&dyn BasicValue<'c>>) {
      self.builder.build_return(return_value);
      self.pass_manager.run_on(&self.function.value);
   }
}

impl fmt::Debug for CodeGen<'_, '_> {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", &self.module.print_to_string().to_str().unwrap())
   }
}
