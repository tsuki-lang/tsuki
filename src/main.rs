mod ast;
mod astdump;
mod common;
mod lexer;
mod parser;

fn main() -> Result<(), common::Error> {
   let source = r#"
      a + b + c
        + d
        * e
        ** 2
        + 3
      print("test")
      do
        print("test2")
        print("asdd")
      print("yeah")
   "#;
   let mut lexer = lexer::Lexer::new("test.tsu", source);
   let (ast, root_node) = match parser::parse(&mut lexer) {
      Ok(result) => result,
      Err(errors) => {
         errors.iter().for_each(|error| eprintln!("{:#}", error));
         std::process::exit(1)
      }
   };

   println!("{}", source);

   for handle in ast.node_handles() {
      if ast.span(handle).is_invalid() {
         println!("warning: node with invalid span: {:?}\nAST dump:", handle);
         astdump::dump_ast(&lexer, &ast, handle);
      }
   }
   println!("———");

   astdump::dump_ast(&lexer, &ast, root_node);

   Ok(())
}
