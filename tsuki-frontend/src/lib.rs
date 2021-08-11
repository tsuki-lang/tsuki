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
use types::{DefaultTypes, FloatSize, IntegerSize};

/// Parses and analyzes a source file. Returns the checked AST and a handle to the root node.
pub fn analyze(file: &SourceFile) -> Result<(Ast, NodeHandle), Errors> {
   let SourceFile { filename, source } = file;
   let mut lexer = Lexer::new(filename, source);
   let (ast, root_node) = parser::parse(&mut lexer)?;

   println!("———");
   println!(":: Source code");
   println!("———");
   println!("{}", source);

   for handle in ast.node_handles() {
      if ast.span(handle).is_invalid() {
         println!("warning: node with invalid span: {:?}\nAST dump:", handle);
         astdump::dump_ast(&lexer, &ast, None, handle);
      }
   }

   println!("———");
   println!(":: AST (pre-sem)");
   println!("———");

   astdump::dump_ast(&lexer, &ast, None, root_node);
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

   println!("———");
   println!(":: AST (post-sem)");
   println!("———");
   astdump::dump_ast(&lexer, &ast, Some(&types), root_node);

   Ok((ast, root_node))
}
