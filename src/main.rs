mod common;
mod lexer;

use lexer::{Lexer, TokenKind};

fn main() -> Result<(), common::Error> {
   let source = r#"
      # this is a comment that should be ignored

indentation
  level
    shenanigans

      # literals
      nil true false
      1 123 123_456 0b1010 0o777 0x123abc
      1.0 2.0 3.141592 12.123 1.0e-9 123_456.789_987
      :hello_there
      'a' '\n' '\xff' '\u{1F315}'
      "Welcome\nhome"
      """Long… long…
      maaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaan"""
      ## Here's some documentation for ya.

      ## This should be a separate doc comment.
      ## The lines should be joined together.

      # identifiers
      this_is_an_identifier HelloWorld

      # keywords
      _ and catch derive do for fun if impl in is macro match not object of or pub return try type
      union while val var

      # operators
      . + - * / ** ~ << >> & | ^^ == != < > <= >= ^ ? ! .. ..= = += -= *= /= <-
      1..2 1..=2
      # punctuation
      () [] {} , : :: ;

      # there should be an EOF token here at the end
   "#;
   let mut lexer = Lexer::new("test.tsu", source);

   loop {
      let token = match lexer.next() {
         Ok(token) => token,
         Err(error) => {
            eprintln!("{:#}", error);
            std::process::exit(1);
         }
      };

      print!("{:#} |{}\t{:?}", token.span, token.indent_level, token.kind);
      if let Some(s) = token.kind.get_source(source) {
         print!("\t{:?}", s);
      } else if let TokenKind::Character(ch) = token.kind {
         print!("\t{:?}", char::from_u32(ch));
      } else if let Some(s) = token.kind.get_string(&lexer) {
         print!("\t{:?}", s);
      }
      println!();

      if token.kind == TokenKind::Eof {
         break;
      }
   }

   Ok(())
}
