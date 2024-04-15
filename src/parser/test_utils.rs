use crate::{lexer::Lexer, parser::Parser};

pub(crate) fn get_parser(input: &str, print_tokens: bool) -> Parser {
    let mut lexer = Lexer::new(input);

    let stream = lexer
        .lex()
        .expect(&format!("error tokenizing input: \n{:#?}", lexer.errors()));

    if print_tokens {
        println!("{:#?}", stream.tokens());
    }

    Parser::new(stream)
}
