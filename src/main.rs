use tsuki_frontend::{astdump, common, lexer, parser, sem};

fn unwrap_errors<T>(r: Result<T, common::Errors>) -> T {
   match r {
      Ok(ok) => ok,
      Err(errors) => {
         errors.iter().for_each(|error| eprintln!("{:#}", error));
         std::process::exit(1)
      }
   }
}

fn main() -> Result<(), common::Error> {
   let filename = "test.tsu";
   let source = r#"
      1 + 2
   "#;
   let mut lexer = lexer::Lexer::new(filename, source);
   let (mut ast, root_node) = unwrap_errors(parser::parse(&mut lexer));

   println!("———");
   println!(":: Source code");
   println!("———");
   println!("{}", source);

   for handle in ast.node_handles() {
      if ast.span(handle).is_invalid() {
         println!("warning: node with invalid span: {:?}\nAST dump:", handle);
         astdump::dump_ast(&lexer, &ast, handle);
      }
   }

   println!("———");
   println!(":: AST (pre-sem)");
   println!("———");

   astdump::dump_ast(&lexer, &ast, root_node);
   ast = unwrap_errors(sem::analyze(filename, ast, root_node));

   println!("———");
   println!(":: AST (post-sem)");
   println!("———");
   astdump::dump_ast(&lexer, &ast, root_node);

   Ok(())
}
