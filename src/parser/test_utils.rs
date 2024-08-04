use crate::{lexer::Lexer, logger::LogLevel, parser::Parser};

/// Utility function that generates a `Parser` instance given some input string (used in testing).
#[allow(dead_code)]
pub fn get_parser(input: &str, log_level: LogLevel, print_tokens: bool) -> Parser {
    let mut lexer = Lexer::new(input);

    let stream = lexer.lex().expect("error tokenizing input");

    if print_tokens {
        println!("{:#?}", stream.tokens());
    }

    let mut parser = Parser::new(stream, log_level);

    // log test parser status
    parser.logger.info("instantiated parser");

    parser
}
