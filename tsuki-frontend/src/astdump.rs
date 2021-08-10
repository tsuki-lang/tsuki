//! AST pretty printer.

use crate::ast::*;
use crate::lexer::Lexer;
use crate::types::Types;

#[derive(Debug)]
enum Prefix {
   L,
   R,
   Fun,
   Cond,
}

fn print_indentation(depth: usize) {
   for _ in 0..depth * 2 {
      print!(" ");
   }
}

fn print_source_range(lexer: &Lexer, start: usize, end: usize) {
   print!("{}", &lexer.input()[start..end]);
}

fn print_string_range(lexer: &Lexer, start: usize, end: usize) {
   print!("{}", &lexer.string_data()[start..end]);
}

struct State<'l, 'll, 'a, 't> {
   lexer: &'l Lexer<'ll>,
   ast: &'a Ast,
   types: Option<&'t Types>,
}

fn dump_node(s: &State, node: NodeHandle, depth: usize, prefix: Option<Prefix>) {
   let State { lexer, ast, types } = s;
   print_indentation(depth);

   let kind = ast.kind(node);
   let extra = ast.extra(node);

   // Optional prefix.
   if let Some(prefix) = prefix {
      print!("{:?}: ", prefix);
   }

   // Node header: the name, and optionally some source code.
   print!("{:?} ", kind);
   match kind {
      NodeKind::Integer | NodeKind::Float | NodeKind::Atom | NodeKind::Identifier => {
         let (start, end) = (ast.first(node), ast.second(node));
         print_source_range(lexer, start, end);
      }
      NodeKind::String | NodeKind::DocComment => {
         let (start, end) = (ast.first(node), ast.second(node));
         print_string_range(lexer, start, end);
      }
      NodeKind::Character => print!("{:?}", char::from_u32(ast.first(node) as u32)),
      _ => (),
   }
   match extra {
      number
      @
      (NodeData::Uint8(..)
      | NodeData::Uint16(..)
      | NodeData::Uint32(..)
      | NodeData::Uint64(..)
      | NodeData::Int8(..)
      | NodeData::Int16(..)
      | NodeData::Int32(..)
      | NodeData::Int64(..)
      | NodeData::Float32(..)
      | NodeData::Float64(..)) => print!("{:?}", number),
      _ => (),
   }
   if let Some(types) = types {
      let typ = types.node_type(node);
      print!(" : {}", types.name(typ));
   }
   println!();

   match kind {
      | NodeKind::Dot
      | NodeKind::Plus
      | NodeKind::Minus
      | NodeKind::Mul
      | NodeKind::Div
      | NodeKind::Pow
      | NodeKind::Concat
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
      | NodeKind::UpTo
      | NodeKind::UpToInclusive
      | NodeKind::Assign
      | NodeKind::PlusAssign
      | NodeKind::MinusAssign
      | NodeKind::MulAssign
      | NodeKind::DivAssign
      | NodeKind::Push
      | NodeKind::Index
      | NodeKind::IndexAlt => {
         let (left, right) = (ast.first_handle(node), ast.second_handle(node));
         dump_node(s, left, depth + 1, Some(Prefix::L));
         dump_node(s, right, depth + 1, Some(Prefix::R));
      }
      | NodeKind::Check
      | NodeKind::Unwrap
      | NodeKind::Deref
      | NodeKind::Call
      | NodeKind::Not
      | NodeKind::Neg
      | NodeKind::BitNot
      | NodeKind::Member
      | NodeKind::Ref
      | NodeKind::IfBranch => {
         let left = ast.first_handle(node);
         dump_node(
            s,
            left,
            depth + 1,
            Some(match kind {
               NodeKind::Check | NodeKind::Unwrap | NodeKind::Deref => Prefix::L,
               | NodeKind::Not
               | NodeKind::Neg
               | NodeKind::BitNot
               | NodeKind::Member
               | NodeKind::Ref => Prefix::R,
               NodeKind::Call => Prefix::Fun,
               NodeKind::IfBranch => Prefix::Cond,
               _ => unreachable!(),
            }),
         );
      }
      _ => (),
   }

   match extra {
      NodeData::None => (),
      NodeData::NodeList(list) => {
         for &node in list {
            dump_node(s, node, depth + 1, None);
         }
      }
      _ => (),
   }
}

/// Prints the AST to stdout, starting from the given root node.
pub fn dump_ast(lexer: &Lexer, ast: &Ast, types: Option<&Types>, root_node: NodeHandle) {
   dump_node(&State { lexer, ast, types }, root_node, 0, None);
}
