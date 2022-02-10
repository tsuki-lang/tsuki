//! Code generation for statement lists and control flow structures.

use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, IntValue};
use smallvec::SmallVec;
use tsuki_frontend::ast::{NodeId, NodeKind};
use tsuki_frontend::scope::ScopeId;
use tsuki_frontend::sem::Ir;

use crate::codegen::CodeGen;

impl<'src, 'c, 'pm> CodeGen<'src, 'c, 'pm> {
   /// Generates code for a list of statements.
   pub(crate) fn generate_statements(&mut self, ir: &Ir, node: NodeId) {
      ir.ast.walk_node_list(node, |_ast, _index, node| {
         self.generate_statement(ir, node);
      });
   }

   /// Generates code for a list of statements with a tail expression.
   pub(crate) fn generate_statements_with_tail_expression(
      &mut self,
      ir: &Ir,
      node: NodeId,
   ) -> BasicValueEnum<'c> {
      let mut tail = None;
      for (index, &child) in ir.ast.extra(node).as_node_list().unwrap().iter().enumerate() {
         if ir.ast.is_last_child(node, index) {
            tail = Some(self.generate_expression(ir, child))
         } else {
            self.generate_statement(ir, child);
         }
      }
      if let Some(tail) = tail {
         tail
      } else {
         self.generate_unit_literal().into()
      }
   }

   /// Generates code for a `do` expression or a `do` statement.
   ///
   /// If the node is a `DoExpression`, returns `Some` with the tail expression. Otherwise
   /// if the kind is `DoStatement`, returns `None`.
   pub(crate) fn generate_do(&mut self, ir: &Ir, node: NodeId) -> Option<BasicValueEnum<'c>> {
      match ir.ast.kind(node) {
         NodeKind::DoExpression => Some(self.generate_statements_with_tail_expression(ir, node)),
         NodeKind::DoStatement => {
            self.generate_statements(ir, node);
            None
         }
         _ => unreachable!(),
      }
   }

   /// Generates code for an `if` expression or an `if` statement.
   ///
   /// Return value behavior is similar to `generate_do`.
   pub(crate) fn generate_if(&mut self, ir: &Ir, node: NodeId) -> Option<BasicValueEnum<'c>> {
      /// This local struct stores information about the condition of an `if` branch.
      struct Condition<'c> {
         block: BasicBlock<'c>,
         value: IntValue<'c>,
         // We store the ending block of the condition, because it may be different than the
         // starting block.
         end_block: BasicBlock<'c>,
      }
      // This local struct stores information about a single `if` branch: its condition, condition
      // block, and body block.
      struct Branch<'c> {
         condition: Option<Condition<'c>>,
         body: BasicBlock<'c>,
         // Similarly to the condition, we store the ending block, because it may be different than
         // the starting block, and it's where we must emit the final `br` instructions.
         end_block: BasicBlock<'c>,
         result: Option<BasicValueEnum<'c>>,
      }
      let mut branches = SmallVec::<[Branch<'c>; 16]>::new();
      let is_expression = ir.ast.kind(node) == NodeKind::IfExpression;
      // Unwrapping here is safe, because we are coming from an existing block
      // (eg. the function's %entry).
      let entry_block = self.builder.get_insert_block().unwrap();

      // Generate code for each of the branches. The `br` instructions are added after the inner
      // code is generated, because all blocks have to be known beforehand.
      let branch_nodes = ir.ast.extra(node).as_node_list().unwrap();
      for (index, &branch) in branch_nodes.iter().enumerate() {
         let body_block = self.context.append_basic_block(
            self.function.value,
            // For easier debugging of the IR, the block's name is determined by the branch type.
            match ir.ast.kind(branch) {
               NodeKind::IfBranch => "elif",
               NodeKind::ElseBranch => "else",
               _ => unreachable!(),
            },
         );
         // The condition is only generated for `IfBranch`es, because the `ElseBranch` does not
         // have a condition.
         let mut condition = None;
         if ir.ast.kind(branch) == NodeKind::IfBranch {
            let condition_block = if index == 0 {
               // We don't need a new block if this is the first branch; we can simply fall through
               // from the current one.
               entry_block
            } else {
               self.context.prepend_basic_block(body_block, "condition")
            };
            self.builder.position_at_end(condition_block);
            let condition_value = self.generate_expression(ir, ir.ast.first_handle(branch));
            let end_block = self.builder.get_insert_block().unwrap();
            condition = Some(Condition {
               block: condition_block,
               value: condition_value.into_int_value(),
               end_block,
            });
         }
         // Then we generate the body.
         self.builder.position_at_end(body_block);
         let result = if is_expression {
            Some(self.generate_statements_with_tail_expression(ir, branch))
         } else {
            self.generate_statements(ir, branch);
            None
         };
         let end_block = self.builder.get_insert_block().unwrap();
         branches.push(Branch {
            condition,
            body: body_block,
            end_block,
            result,
         });
      }
      // Generate the terminating %end block. After a successfully executed branch, this block is
      // branched to unconditionally, and is where control flow continues after the if statement
      // ends.
      let end_block = self.context.append_basic_block(self.function.value, "end");

      // Now that we have all the blocks, we're ready to backpatch some `br` instructions into
      // the blocks.
      for (index, branch) in branches.iter().enumerate() {
         let &Branch {
            condition,
            body: body_block,
            end_block: branch_end_block,
            ..
         } = &branch;
         if let Some(condition) = condition {
            // The block to execute if the branch fails is dependent on whether there's a branch
            // after this one, and also if the branch after this one is an `else` branch without
            // a condition.
            let else_block = if let Some(next_branch) = branches.get(index + 1) {
               if let Some(next_condition) = &next_branch.condition {
                  next_condition.block
               } else {
                  next_branch.body
               }
            } else {
               end_block
            };
            self.builder.position_at_end(condition.end_block);
            self.builder.build_conditional_branch(condition.value, *body_block, else_block);
         }
         self.builder.position_at_end(*branch_end_block);
         self.builder.build_unconditional_branch(end_block);
      }

      // Compilation is resumed normally at the %end block.
      self.builder.position_at_end(end_block);
      // In case of an if expression, we have to generate a `phi` node at the end that's going to
      // contain our final value.
      if is_expression {
         let typ = branches[0].result.unwrap().get_type();
         let phi = self.builder.build_phi(typ, "ifresult");
         for Branch {
            end_block, result, ..
         } in branches
         {
            phi.add_incoming(&[(&result.unwrap(), end_block)]);
         }
         // It's a bit strange that `phi`'s function for this is not called `as_basic_value_enum`.
         Some(phi.as_basic_value())
      } else {
         None
      }
   }

   /// Generates code for a `while` loop.
   pub(crate) fn generate_while(&mut self, ir: &Ir, node: NodeId) {
      // Save the start block for generating the initial `br label %condition` instruction.
      let start_block = self.builder.get_insert_block().unwrap();

      // Generate the condition block and value.
      let condition_block = self.context.append_basic_block(self.function.value, "while");
      self.builder.position_at_end(condition_block);
      let condition_value = self.generate_expression(ir, ir.ast.first_handle(node));
      // Save the end of the condition value, in case it generates some extra blocks.
      let condition_end_block = self.builder.get_insert_block().unwrap();

      // Generate the loop body.
      let body_block = self.context.append_basic_block(self.function.value, "do");
      self.builder.position_at_end(body_block);
      self.generate_statements(ir, node);
      let body_end_block = self.builder.get_insert_block().unwrap();

      // Generate the final %end block.
      let end_block = self.context.append_basic_block(self.function.value, "end");

      // Now, insert all the branch instructions.
      // First we start with the unconditional branch to the condition block.
      self.builder.position_at_end(start_block);
      self.builder.build_unconditional_branch(condition_block);
      // Then, we build the conditional branch at the end of the condition block.
      self.builder.position_at_end(condition_end_block);
      self.builder.build_conditional_branch(
         condition_value.into_int_value(),
         body_block,
         end_block,
      );
      // Finally, we branch back to the condition at the end of the body.
      self.builder.position_at_end(body_end_block);
      self.builder.build_unconditional_branch(condition_block);

      let scope = ir.ast.scope(node).unwrap();
      self.generate_break_jumps(scope, end_block);

      // Continue generating code at the end block.
      self.builder.position_at_end(end_block);
   }

   /// Generates jumps at the ends of blocks, that are results of `break`s of the breaking scope
   /// with the given ID.
   fn generate_break_jumps(&mut self, scope: ScopeId, end_block: BasicBlock<'c>) {
      let builder = self.context.create_builder();
      let keys: SmallVec<[(ScopeId, usize); 4]> =
         self.break_blocks.keys().filter(|(scope_id, _)| *scope_id == scope).copied().collect();
      for key in keys {
         let block = self.break_blocks.remove(&key).unwrap();
         builder.position_at_end(block);
         builder.build_unconditional_branch(end_block);
      }
   }

   /// Generates code for a `break` expression.
   pub(crate) fn generate_break(&mut self, ir: &Ir, node: NodeId) -> BasicValueEnum<'c> {
      // Save the current block in the break_blocks table, such that it can be later referred to
      // by the enclosing `while` loop.
      let target_scope = ir.ast.scope(node).expect("Break node with no scope in IR");
      let id = self.break_blocks.len();
      let break_block = self.builder.get_insert_block().unwrap();
      self.break_blocks.insert((target_scope, id), break_block);

      // Continue generating in an unreachable block with no predecessors.
      let unreachable_block = self.context.append_basic_block(self.function.value, "unreachable");
      self.builder.position_at_end(unreachable_block);

      // Return a dummy value as a result of `break` being an expression.
      let result_type = self.get_type(&ir.types, ir.ast.type_id(node));
      result_type.const_zero()
   }
}
