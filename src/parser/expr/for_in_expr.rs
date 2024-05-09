use crate::{
    ast::{BlockExpr, Expression, ForInExpr, IdentifierPatt, Keyword},
    error::ErrorsEmitted,
    parser::{ParseConstruct, ParseControl, Parser, Precedence},
    token::Token,
};

impl ParseControl for ForInExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_for = if let Some(Token::For { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::For)
        } else {
            parser.log_unexpected_token("`for`");
            Err(ErrorsEmitted)
        }?;

        let pattern = match parser.current_token() {
            Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
                IdentifierPatt::parse(parser)
            }
            _ => parser.parse_pattern(),
        }?;

        let kw_in = match parser.current_token() {
            Some(Token::In { .. }) => {
                parser.next_token();
                Ok(Keyword::In)
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`in`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`in`");
                Err(ErrorsEmitted)
            }
        }?;

        let iterable = Box::new(parser.parse_expression(Precedence::Lowest)?);

        let block = match parser.current_token() {
            Some(Token::LBrace { .. }) => Ok(Box::new(BlockExpr::parse(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`{`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let expr = ForInExpr {
            kw_for,
            pattern,
            kw_in,
            iterable,
            block,
        };

        Ok(Expression::ForIn(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_for_in_expr() -> Result<(), ()> {
        let input = r#"
        for x in 0..=5 {
            x += 1;

            let y = 15;

            for z in y {
                print("foo");
            }
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
