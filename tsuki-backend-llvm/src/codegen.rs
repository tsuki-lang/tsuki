//! Common code generation state.

use std::fmt;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::StructType;
use inkwell::values::{BasicValueEnum, FunctionValue};
use tsuki_frontend::ast::{NodeId, NodeKind};
use tsuki_frontend::common::SourceFile;
use tsuki_frontend::sem::Ir;

use crate::functions::Function;
use crate::variables::Variables;

/// Code generation state shared across functions.
pub struct CodeGen<'src, 'c, 'pm> {
   pub(crate) source: &'src SourceFile,
   pub(crate) context: &'c Context,
   pub(crate) module: &'pm Module<'c>,
   pub(crate) builder: Builder<'c>,
   pub(crate) pass_manager: &'pm PassManager<FunctionValue<'c>>,

   pub(crate) function: Function<'c>,
   pub(crate) variables: Variables<'c>,

   pub(crate) unit_type: StructType<'c>,
}

impl<'src, 'c, 'pm> CodeGen<'src, 'c, 'pm> {
   pub fn new(
      source: &'src SourceFile,
      context: &'c Context,
      pass_manager: &'pm PassManager<FunctionValue<'c>>,
      module: &'pm Module<'c>,
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
         variables: Variables::new(),

         unit_type: context.struct_type(&[], false),
      };
      state.builder.position_at_end(state.function.entry_block);
      // Temporary: set up some libc functions.
      state.load_libc();
      state
   }

   /// Creates a new code generator, with the same source file, context, pass manager, and module,
   /// but with a different function.
   pub fn for_function(&self, function: Function<'c>) -> Self {
      Self {
         builder: self.context.create_builder(),
         function,
         variables: Variables::new(),
         unit_type: self.unit_type,
         ..*self
      }
   }

   /// Generates code for an arbitrary node.
   pub fn generate_statement(&mut self, ir: &Ir, node: NodeId) {
      match ir.ast.kind(node) {
         // Control flow
         NodeKind::Pass => (),
         NodeKind::StatementList => self.generate_statements(ir, node),
         NodeKind::DoStatement => {
            let _ = self.generate_do(ir, node);
         }
         NodeKind::IfStatement => {
            let _ = self.generate_if(ir, node);
         }
         NodeKind::While => self.generate_while(ir, node),

         // Declarations
         NodeKind::Val | NodeKind::Var => self.generate_variable_declaration(ir, node),
         NodeKind::AssignDiscard => self.generate_discarding_assignment(ir, node),
         NodeKind::Fun => self.generate_function(ir, node),

         // Expressions
         NodeKind::Assign => {
            let _ = self.generate_assignment(ir, node);
         }
         _ => {
            let _ = self.generate_expression(ir, node);
         }
      }
   }

   /// Finishes compiling a function, by inserting a `ret` instruction at the end, as well
   /// as running optimizations on it.
   pub fn finish_function(&self, return_value: Option<BasicValueEnum<'c>>) {
      // It seems like Rust can't really infer that I want to pass a &dyn when I .as_ref()
      // the option, so this requires some manual matching.
      match return_value {
         Some(v) => self.builder.build_return(Some(&v)),
         None => self.builder.build_return(None),
      };
      self.pass_manager.run_on(&self.function.value);
   }
}

impl fmt::Debug for CodeGen<'_, '_, '_> {
   fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      write!(f, "{}", &self.module.print_to_string().to_str().unwrap())
   }
}
