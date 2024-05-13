use crate::{
    lexer::Lexer,
    logger::{LogLevel, LogMsg},
    parser::Parser,
};

/// Utility function that generates a `Parser` instance given some input string (used in testing).
#[allow(dead_code)]
pub fn get_parser(input: &str, log_level: LogLevel, print_tokens: bool) -> Parser {
    let mut lexer = Lexer::new(input);

    let stream = lexer
        .lex()
        .expect(&format!("error tokenizing input: \n{:#?}", lexer.errors()));

    if print_tokens {
        println!("{:#?}", stream.tokens());
    }

    let mut parser = Parser::new(stream, log_level);

    parser
        .logger
        .log(LogLevel::Info, LogMsg::from("instantiated parser"));

    parser
}
