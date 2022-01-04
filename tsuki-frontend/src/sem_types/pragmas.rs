//! Handling for pragma AST.

use crate::ast::{Ast, NodeId, NodeKind};

use super::SemTypes;

impl<'s> SemTypes<'s> {
   /// Splits a node that may have pragmas attached to it, to the inner part (first tuple field),
   /// and the pragmas (second tuple field).
   pub(crate) fn split_pragmas(ast: &Ast, node: NodeId) -> (NodeId, Option<NodeId>) {
      if ast.kind(node) == NodeKind::Pragmas {
         (ast.first_handle(node), Some(node))
      } else {
         (node, None)
      }
   }
}
