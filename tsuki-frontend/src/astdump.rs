//! AST pretty printer.

use crate::ast::*;
use crate::common::SourceFile;
use crate::types::Types;

#[derive(Debug)]
enum Prefix {
   L,
   R,
   Fun,
   Cond,
   X,
   Name,
   Params,
   Generic,
   Formal,
   Return,
   Type,
}

fn print_indentation(depth: usize) {
   for _ in 0..depth * 2 {
      eprint!(" ");
   }
}

fn print_source_range(file: &SourceFile, start: usize, end: usize) {
   eprint!("{}", &file.source[start..end]);
}

fn print_string_range(file: &SourceFile, start: usize, end: usize) {
   eprint!("{}", &file.source[start..end]);
}

struct State<'s, 'a, 't> {
   file: &'s SourceFile,
   ast: &'a Ast,
   types: Option<&'t Types>,
}

fn dump_node(s: &State, node: NodeId, depth: usize, prefix: Option<Prefix>) {
   let State { file, ast, types } = s;
   print_indentation(depth);

   let kind = ast.kind(node);
   let extra = ast.extra(node);

   // Optional prefix.
   if let Some(prefix) = prefix {
      eprint!("{:?}: ", prefix);
   }

   // Node header: the name, and optionally some source code.
   eprint!("{:?} ", kind);
   match kind {
      NodeKind::Integer | NodeKind::Float | NodeKind::Atom | NodeKind::Identifier => {
         let (start, end) = (ast.first(node), ast.second(node));
         print_source_range(file, start, end);
      }
      NodeKind::String | NodeKind::DocComment => {
         let (start, end) = (ast.first(node), ast.second(node));
         print_string_range(file, start, end);
      }
      NodeKind::Character => eprint!("{:?}", char::from_u32(ast.first(node) as u32)),
      NodeKind::Symbol => eprint!("{:?}", ast.first(node)),
      _ => (),
   }
   match extra {
      number @ (NodeData::Uint8(..)
      | NodeData::Uint16(..)
      | NodeData::Uint32(..)
      | NodeData::Uint64(..)
      | NodeData::Int8(..)
      | NodeData::Int16(..)
      | NodeData::Int32(..)
      | NodeData::Int64(..)
      | NodeData::Float32(..)
      | NodeData::Float64(..)) => eprint!("{:?}", number),
      _ => (),
   }
   if let Some(types) = types {
      let typ = ast.type_id(node);
      eprint!(" : {}", types.name(typ));
   }
   if let Some(scope) = ast.scope(node) {
      eprint!(" +{:?}", scope);
   }
   eprintln!();

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
      | NodeKind::IndexAlt
      | NodeKind::Val
      | NodeKind::Var => {
         let (left, right) = (ast.first_handle(node), ast.second_handle(node));
         dump_node(s, left, depth + 1, Some(Prefix::L));
         dump_node(s, right, depth + 1, Some(Prefix::R));
      }
      NodeKind::Fun => {
         let (left, right) = (ast.first_handle(node), ast.second_handle(node));
         dump_node(s, left, depth + 1, Some(Prefix::Name));
         dump_node(s, right, depth + 1, Some(Prefix::Params));
      }
      NodeKind::Parameters => {
         let (left, right) = (ast.first_handle(node), ast.second_handle(node));
         dump_node(s, left, depth + 1, Some(Prefix::Generic));
         dump_node(s, right, depth + 1, Some(Prefix::Formal));
      }
      | NodeKind::Check
      | NodeKind::Unwrap
      | NodeKind::Deref
      | NodeKind::Call
      | NodeKind::CallFunction
      | NodeKind::Not
      | NodeKind::Neg
      | NodeKind::BitNot
      | NodeKind::Member
      | NodeKind::Ref
      | NodeKind::IfBranch
      | NodeKind::While
      | NodeKind::FormalParameters
      | NodeKind::NamedParameters
      | NodeKind::Variable
      | NodeKind::WidenInt
      | NodeKind::WidenUint
      | NodeKind::WidenFloat => {
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
               NodeKind::Call | NodeKind::CallFunction => Prefix::Fun,
               NodeKind::IfBranch | NodeKind::While => Prefix::Cond,
               NodeKind::FormalParameters => Prefix::Return,
               NodeKind::NamedParameters => Prefix::Type,
               | NodeKind::Variable
               | NodeKind::WidenInt
               | NodeKind::WidenUint
               | NodeKind::WidenFloat => Prefix::X,
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
pub fn dump_ast(file: &SourceFile, ast: &Ast, types: Option<&Types>, root_node: NodeId) {
   dump_node(&State { file, ast, types }, root_node, 0, None);
}
