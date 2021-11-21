//! Code generation for functions.

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use smallvec::SmallVec;
use tsuki_frontend::ast::NodeHandle;
use tsuki_frontend::functions::FunctionId;
use tsuki_frontend::sem::Ir;

use crate::codegen::CodeGen;

/// Data associated with a single function.
pub struct Function<'c> {
   pub value: FunctionValue<'c>,
   pub entry_block: BasicBlock<'c>,
}

impl<'c> Function<'c> {
   /// Creates a new function from the given name and type.
   ///
   /// # Side effects
   ///
   /// This adds the new function to the module.
   pub fn add_to_module(
      context: &'c Context,
      module: &Module<'c>,
      name: &str,
      typ: FunctionType<'c>,
   ) -> Self {
      // Unfortunately this side effect of adding the function into the module is unavoidable,
      // as far as I know. I generally prefer code that is free of any side effects but I guess
      // some sacrifices have to be made.
      let value = module.add_function(name, typ, None);
      let entry_block = context.append_basic_block(value, "entry");
      Self { value, entry_block }
   }

   /// Creates a `Function` from an existing `FunctionValue`.
   pub fn from_value(value: FunctionValue<'c>) -> Self {
      Self {
         value,
         entry_block: value.get_first_basic_block().expect("function did not have a basic block"),
      }
   }

   /// Positions the given builder at the start of the function.
   pub fn position_at_entry_block(&self, builder: &Builder) {
      if let Some(instruction) = self.entry_block.get_first_instruction() {
         builder.position_before(&instruction);
      } else {
         builder.position_at_end(self.entry_block);
      }
   }

   /// Creates a builder that adds instructions to the top of the entry block.
   pub fn create_entry_block_builder(&self, context: &'c Context) -> Builder<'c> {
      let builder = context.create_builder();
      self.position_at_entry_block(&builder);
      builder
   }
}

impl<'src, 'c, 'pm> CodeGen<'src, 'c, 'pm> {
   /// Returns the function type, for the given function ID.
   fn get_function_type(&self, ir: &Ir, function_id: FunctionId) -> FunctionType<'c> {
      let parameters = ir.functions.parameters(function_id);
      let parameter_types: SmallVec<[BasicTypeEnum<'c>; 8]> = parameters
         .formal
         .iter()
         .map(|&symbol_id| {
            let typ = ir.symbols.type_id(symbol_id);
            self.get_type(&ir.types, typ)
         })
         .collect();
      // Well, that kind of sucks. AnyValueEnum does not have an fn_type method, so we need to
      // branch here.
      if ir.types.kind(parameters.return_type).is_unit() {
         self.context.void_type().fn_type(&parameter_types, false)
      } else {
         self.get_type(&ir.types, parameters.return_type).fn_type(&parameter_types, false)
      }
   }

   /// Adds functions from the IR to the module.
   pub fn add_functions(&self, ir: &Ir) {
      for function_id in ir.functions.iter() {
         // Skip non-local functions in the process.
         if ir.functions.kind(function_id).is_local() {
            let function_type = self.get_function_type(ir, function_id);
            let _ = Function::add_to_module(
               self.context,
               self.module,
               ir.functions.mangled_name(function_id),
               function_type,
            );
         }
      }
   }

   /// Generates code for a function.
   pub fn generate_function(&self, ir: &Ir, node: NodeHandle) {
      // Get the function ID from the AST.
      let name_node = ir.ast.first_handle(node);
      let symbol_id = ir.ast.symbol_id(name_node);
      let function_id = ir.symbols.kind(symbol_id).unwrap_function();

      // Obtain the function from the module.
      let function = self
         .module
         .get_function(ir.functions.mangled_name(function_id))
         .expect("function does not seem to exist");
      let function = Function::from_value(function);

      // Create a new CodeGen for generating the function's body.
      let code_gen = self.for_function(function);

      // Copy all the parameters into allocas.
      // I don't think this is _too_ terrible performance-wise, mem2reg will hopefully optimize
      // away most of the cases here.
      code_gen.function.position_at_entry_block(&code_gen.builder);
      let parameters = ir.functions.parameters(function_id);
      let mut allocas = SmallVec::<[PointerValue<'c>; 8]>::new();
      for (i, parameter) in code_gen.function.value.get_param_iter().enumerate() {
         // While we're at it, we give all the parameters names for more readable IR.
         let symbol_id = parameters.formal[i];
         let name = ir.symbols.name(symbol_id);
         parameter.set_name(name);
         let alloca = code_gen.builder.build_alloca(parameter.get_type(), name);
         code_gen.builder.build_store(alloca, parameter);
         allocas.push(alloca);
         // Also, store the alloca in the code generator's variables list.
         let mut variables = code_gen.variables.borrow_mut();
         variables.insert(symbol_id, alloca);
      }

      // Generate the function's body.
      let return_type = ir.functions.parameters(function_id).return_type;
      let return_value = if ir.types.kind(return_type).is_unit() {
         code_gen.generate_statements(ir, node);
         None
      } else {
         Some(code_gen.generate_statements_with_tail_expression(ir, node))
      };

      // Finish the function up.
      code_gen.finish_function(return_value);
   }

   /// Generates code for a function call.
   pub(crate) fn generate_call(&self, ir: &Ir, node: NodeHandle) -> BasicValueEnum<'c> {
      // Get the function we want to call.
      let callee_node = ir.ast.first_handle(node);
      let symbol_id = ir.ast.symbol_id(callee_node);
      let function_id = ir.symbols.kind(symbol_id).unwrap_function();
      let function = self
         .module
         .get_function(ir.functions.mangled_name(function_id))
         .expect("function does not seem to exist");

      // Generate code for all the arguments.
      let mut arguments = SmallVec::<[BasicValueEnum<'c>; 8]>::new();
      for &argument in ir.ast.extra(node).unwrap_node_list() {
         arguments.push(self.generate_expression(ir, argument));
      }
      let call = self.builder.build_call(function, &arguments, "calltmp");

      call.try_as_basic_value().either(|value| value, |_void| self.generate_unit_literal().into())
   }
}
