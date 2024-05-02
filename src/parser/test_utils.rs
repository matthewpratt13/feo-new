use crate::{lexer::Lexer, parser::Parser, token::Token};

/// Utility function that generates a `Parser` instance given some input string (used in testing).
#[allow(dead_code)]
pub fn get_parser(input: &str, print_tokens: bool) -> Parser {
    let mut lexer = Lexer::new(input);

    let stream = lexer
        .lex()
        .expect(&format!("error tokenizing input: \n{:#?}", lexer.errors()));

    if print_tokens {
        println!("{:#?}", stream.tokens());
    }

    Parser::new(stream)
}

pub(crate) fn log_token(parser: &Parser, msg: &str, log_precedence: bool) {
    let token = parser.current_token();
    let precedence = parser.get_precedence(&token.clone().unwrap_or(Token::EOF));

    if log_precedence {
        println!("msg: {msg}");
        println!("current token: {:?}", token);
        println!("token precedence: {:?}\n", precedence);
    } else {
        println!("msg: {msg}");
        println!("current token: {:?}\n", token);
    }
}
