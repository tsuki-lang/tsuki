//! AST pretty printer.

use crate::ast::*;
use crate::lexer::Lexer;

#[derive(Debug)]
enum Prefix {
   L,
   R,
}

fn print_indentation(depth: usize) {
   for _ in 0..depth * 2 {
      print!(" ");
   }
}

fn print_source_range(lexer: &Lexer, start: usize, end: usize) {
   print!("{}", &lexer.input()[start..end]);
}

fn dump_node(lexer: &Lexer, ast: &Ast, node: NodeHandle, depth: usize, prefix: Option<Prefix>) {
   print_indentation(depth);

   let kind = ast.kind(node);

   // Optional prefix.
   if let Some(prefix) = prefix {
      print!("{:?} -> ", prefix);
   }

   // Node header: the name, and optionally some source code.
   print!("{:?} ", kind);
   match kind {
      | NodeKind::Integer
      | NodeKind::Float
      | NodeKind::Atom
      | NodeKind::String
      | NodeKind::DocComment
      | NodeKind::Identifier => {
         let (start, end) = (ast.first(node), ast.second(node));
         print_source_range(lexer, start, end);
      }
      NodeKind::Character => print!("{:?}", char::from_u32(ast.first(node) as u32)),
      _ => (),
   }
   println!();

   match kind {
      | NodeKind::Dot
      | NodeKind::Plus
      | NodeKind::Minus
      | NodeKind::Mul
      | NodeKind::Div
      | NodeKind::Pow
      | NodeKind::Tilde
      | NodeKind::Lshift
      | NodeKind::Rshift
      | NodeKind::BitAnd
      | NodeKind::BitOr
      | NodeKind::BitXor
      | NodeKind::Equal
      | NodeKind::NotEqual
      | NodeKind::Less
      | NodeKind::Greater
      | NodeKind::LessEqual
      | NodeKind::GreaterEqual
      | NodeKind::Pointer
      | NodeKind::Check
      | NodeKind::Unwrap
      | NodeKind::UpTo
      | NodeKind::UpToInclusive
      | NodeKind::Assign
      | NodeKind::PlusAssign
      | NodeKind::MinusAssign
      | NodeKind::MulAssign
      | NodeKind::DivAssign
      | NodeKind::Push => {
         let (left, right) = (ast.first_handle(node), ast.second_handle(node));
         dump_node(lexer, ast, left, depth + 1, Some(Prefix::L));
         dump_node(lexer, ast, right, depth + 1, Some(Prefix::R));
      },
      _ => (),
   }
}

/// Prints the AST to stdout, starting from the given root node.
pub fn dump_ast(lexer: &Lexer, ast: &Ast, root_node: NodeHandle) {
   dump_node(lexer, ast, root_node, 0, None);
}
