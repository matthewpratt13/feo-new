use crate::{lexer::Lexer, parser::Parser};

pub(crate) fn get_parser(input: &str) -> Parser {
    let mut lexer = Lexer::new(input);

    let stream = lexer
        .lex()
        .expect(&format!("error tokenizing input: \n{:#?}", lexer.errors()));

    println!("{:#?}", stream.tokens());

    Parser::new(stream)
}
