pub mod ast;
pub mod astdump;
pub mod backend;
pub mod common;
pub mod functions;
pub mod lexer;
pub mod parser;
pub mod scope;
pub mod sem;
mod sem_literals;
mod sem_types;
pub mod types;

use common::{Errors, SourceFile};
use lexer::Lexer;
use sem::{AnalyzeOptions, Ir};
use types::{DefaultTypes, FloatSize, IntegerSize};

pub struct DebugOptions {
   pub dump_source: bool,
   pub dump_ast_pre_sem: bool,
   pub dump_ast_post_sem: bool,
}

impl Default for DebugOptions {
   fn default() -> Self {
      Self {
         dump_source: false,
         dump_ast_pre_sem: false,
         dump_ast_post_sem: false,
      }
   }
}

/// Parses and analyzes a source file. Returns the fully analyzed, typed IR.
pub fn analyze(file: &SourceFile, debug: &DebugOptions) -> Result<Ir, Errors> {
   let SourceFile { source, .. } = file;
   let lexer = Lexer::new(file);
   let (ast, root_node) = parser::parse(lexer)?;

   if debug.dump_source {
      eprintln!(":: Source code");
      eprintln!("{}", source);
      eprintln!();
   }

   for handle in ast.node_handles() {
      if ast.span(handle).is_invalid() {
         eprintln!("warning: node with invalid span: {:?}\nAST dump:", handle);
         astdump::dump_ast(file, &ast, None, handle);
      }
   }

   if debug.dump_ast_pre_sem {
      eprintln!(":: AST (pre-sem)");
      astdump::dump_ast(file, &ast, None, root_node);
      eprintln!();
   }

   let ir = sem::analyze(AnalyzeOptions {
      file,
      ast,
      root_node,
      default_types: DefaultTypes {
         int_width: IntegerSize::S32,
         float_width: FloatSize::S32,
         size_width: IntegerSize::U64,
      },
   })?;

   if debug.dump_ast_post_sem {
      eprintln!(":: AST (post-sem)");
      astdump::dump_ast(file, &ir.ast, Some(&ir.types), root_node);
      eprintln!();
   }

   Ok(ir)
}
