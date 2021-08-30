//! Code generation for block-like structures (statement lists and the like).

use inkwell::basic_block::BasicBlock;
use inkwell::values::{BasicValueEnum, InstructionValue, IntValue};
use smallvec::SmallVec;
use tsuki_frontend::ast::{NodeHandle, NodeKind};
use tsuki_frontend::sem::Ir;

use crate::codegen::CodeGen;

impl<'c, 'pm> CodeGen<'c, 'pm> {
   /// Generates code for a list of statements.
   pub(crate) fn generate_statements(&self, ir: &Ir, node: NodeHandle) {
      ir.ast.walk_node_list(node, |_ast, _index, node| {
         self.generate_statement(ir, node);
      });
   }

   /// Generates code for a list of statements with a tail expression.
   fn generate_statements_with_tail_expression(
      &self,
      ir: &Ir,
      node: NodeHandle,
   ) -> BasicValueEnum<'c> {
      let mut tail = None;
      for (index, &child) in ir.ast.extra(node).unwrap_node_list().iter().enumerate() {
         if ir.ast.is_last_child(node, index) {
            tail = Some(self.generate_expression(ir, child))
         } else {
            self.generate_statement(ir, child);
         }
      }
      if let Some(tail) = tail {
         tail
      } else {
         self.generate_unit_literal(&ir.ast, node).into()
      }
   }

   /// Generates code for a `do` expression or a `do` statement.
   ///
   /// If the node is a `DoExpression`, returns `Some` with the tail expression. Otherwise
   /// if the kind is `DoStatement`, returns `None`.
   pub(crate) fn generate_do(&self, ir: &Ir, node: NodeHandle) -> Option<BasicValueEnum<'c>> {
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
   pub(crate) fn generate_if(&self, ir: &Ir, node: NodeHandle) -> Option<BasicValueEnum<'c>> {
      /// This local struct stores information about the condition of an `if` branch.
      struct Condition<'c> {
         block: BasicBlock<'c>,
         value: IntValue<'c>,
         // The `br` instruction at the end of the instruction. This is initially `unreachable`,
         // and is later replaced with a proper `br`.
         br_instruction: InstructionValue<'c>,
      }
      // This local struct stores information about a single `if` branch: its condition, condition
      // block, and body block.
      struct Branch<'c> {
         condition: Option<Condition<'c>>,
         body: BasicBlock<'c>,
         // The unconditional `br label %end` instruction at the end of a block.
         // Just like a condition's `br`, this is initially `unreachable`, and is later backpatched
         // into a proper `br`.
         br_end_instruction: InstructionValue<'c>,
         result: Option<BasicValueEnum<'c>>,
      }
      let mut branches = SmallVec::<[Branch<'c>; 16]>::new();
      let is_expression = ir.ast.kind(node) == NodeKind::IfExpression;
      // Unwrapping here is safe, because we are coming from an existing block
      // (eg. the function's %entry).
      let entry_block = self.builder.get_insert_block().unwrap();

      // Generate code for each of the branches. The `br` instructions are added after the inner
      // code is generated, because all blocks have to be known beforehand.
      let branch_nodes = ir.ast.extra(node).unwrap_node_list();
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
            // We generate the (initially `unreachable`) conditional `br`.
            let br_instruction = self.builder.build_unreachable();
            condition = Some(Condition {
               block: condition_block,
               value: condition_value.into_int_value(),
               br_instruction,
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
         // After the body, we generate the (again, initially `unreachable`) unconditional `br`.
         let br_end_instruction = self.builder.build_unreachable();
         branches.push(Branch {
            condition,
            body: body_block,
            br_end_instruction,
            result,
         })
      }

      // Generate the terminating %end block. After a successfully executed branch, this block is
      // branched to.
      let end_block = self.context.append_basic_block(self.function.value, "end");

      // Now that we have all the blocks, we're ready to backpatch our `unreachable`s into `br`s.
      // We use a separate builder without a position to create the `br` instructions.
      let builder = self.context.create_builder();
      for (index, branch) in branches.iter().enumerate() {
         let &Branch {
            condition,
            body: body_block,
            br_end_instruction,
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
            builder.position_at(condition.block, &condition.br_instruction);
            builder.build_conditional_branch(condition.value, *body_block, else_block);
            condition.br_instruction.erase_from_basic_block();
         }
         builder.position_at(*body_block, br_end_instruction);
         builder.build_unconditional_branch(end_block);
         br_end_instruction.erase_from_basic_block();
      }

      // Compilation is resumed normally at the %end block.
      self.builder.position_at_end(end_block);
      // In case of an if expression, we have to generate a `phi` node at the end that's going to
      // contain our final value.
      if is_expression {
         let typ = branches[0].result.unwrap().get_type();
         let phi = self.builder.build_phi(typ, "ifresult");
         for Branch { body, result, .. } in branches {
            phi.add_incoming(&[(&result.unwrap(), body)]);
         }
         // It's a bit strange that `phi`'s function for this is not called `as_basic_value_enum`.
         Some(phi.as_basic_value())
      } else {
         None
      }
   }
}
