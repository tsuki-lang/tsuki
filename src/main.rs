mod ast;
mod astdump;
mod common;
mod lexer;
mod parser;

fn main() -> Result<(), common::Error> {
   let source = r#"
      2 + 2 * 2
   "#;
   let mut lexer = lexer::Lexer::new("test.tsu", source);
   let (ast, root_node) = match parser::parse(&mut lexer) {
      Ok(result) => result,
      Err(errors) => {
         errors.iter().for_each(|error| eprintln!("{:#}", error));
         std::process::exit(1)
      }
   };
   astdump::dump_ast(&lexer, &ast, root_node);

   Ok(())
}
