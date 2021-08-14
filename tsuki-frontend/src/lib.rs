pub mod ast;
pub mod astdump;
pub mod backend;
pub mod common;
pub mod lexer;
pub mod parser;
pub mod sem;
mod sem_literals;
mod sem_types;
pub mod types;

use ast::{Ast, NodeHandle};
use common::{Errors, SourceFile};
use lexer::Lexer;
use sem::AnalyzeOptions;
use types::{DefaultTypes, FloatSize, IntegerSize, Types};

/// The intermediate representation output by the analyzer. This contains the AST and typeinformation.
pub struct Ir {
   pub ast: Ast,
   pub root_node: NodeHandle,
   pub types: Types,
}

/// Parses and analyzes a source file. Returns the fully analyzed, typed IR.
pub fn analyze(file: &SourceFile) -> Result<Ir, Errors> {
   let SourceFile { filename, source } = file;
   let mut lexer = Lexer::new(filename, source);
   let (ast, root_node) = parser::parse(&mut lexer)?;

   println!(":: Source code");
   println!("{}", source);
   println!();

   for handle in ast.node_handles() {
      if ast.span(handle).is_invalid() {
         println!("warning: node with invalid span: {:?}\nAST dump:", handle);
         astdump::dump_ast(&lexer, &ast, None, handle);
      }
   }

   println!(":: AST (pre-sem)");
   astdump::dump_ast(&lexer, &ast, None, root_node);
   println!();

   let (ast, types) = sem::analyze(AnalyzeOptions {
      filename,
      source,
      ast,
      root_node,
      default_types: DefaultTypes {
         int_size: IntegerSize::S32,
         float_size: FloatSize::S32,
         index_size: IntegerSize::U64,
      },
   })?;

   println!(":: AST (post-sem)");
   astdump::dump_ast(&lexer, &ast, Some(&types), root_node);
   println!();

   Ok(Ir {
      ast,
      root_node,
      types,
   })
}
