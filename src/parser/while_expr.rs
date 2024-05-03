use crate::{
    ast::{BlockExpr, Expression, GroupedExpr, Keyword, WhileExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::{
    parse::{ParseConstruct, ParseControl},
    Parser,
};

impl ParseControl for WhileExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_while = if let Some(Token::While { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::While)
        } else {
            parser.log_unexpected_token("`while`");
            Err(ErrorsEmitted)
        }?;

        let condition = if let Some(Token::LParen { .. }) = parser.current_token() {
            Ok(Box::new(GroupedExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token("`(`");
            Err(ErrorsEmitted)
        }?;

        let block = if let Some(Token::LBrace { .. }) = parser.current_token() {
            Ok(Box::new(BlockExpr::parse(parser)?))
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let expr = WhileExpr {
            kw_while,
            condition,
            block,
        };

        Ok(Expression::While(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_while_expr() -> Result<(), ()> {
        let input = r#"
        while (x < 5) {
            x += 1;
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
