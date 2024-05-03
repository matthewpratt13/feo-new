use crate::{
    ast::{BlockExpr, Expression, ForInExpr, Keyword, Pattern},
    error::ErrorsEmitted,
    token::Token,
};

use super::{
    parse::{ParseConstruct, ParseControl},
    Parser, Precedence,
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

        let assignee = match parser.current_token() {
            Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
                parser.get_identifier_patt()
            }
            _ => {
                let expression = parser.parse_expression(Precedence::Lowest)?;
                Pattern::try_from(expression).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })
            }
        }?;

        let kw_in = if let Some(Token::In { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::In)
        } else {
            parser.log_unexpected_token("`in`");
            Err(ErrorsEmitted)
        }?;

        let iterable = parser.parse_expression(Precedence::Lowest)?;

        let block = if let Some(Token::LBrace { .. }) = parser.current_token() {
            Ok(Box::new(BlockExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let expr = ForInExpr {
            kw_for,
            assignee,
            kw_in,
            iterable: Box::new(iterable),
            block,
        };

        Ok(Expression::ForIn(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

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

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
