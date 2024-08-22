use crate::{
    ast::{GroupedExpr, Keyword, WhileExpr},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, ParseControlExpr, Parser},
    token::Token,
};

use core::fmt;

impl ParseControlExpr for WhileExpr {
    fn parse(parser: &mut Parser) -> Result<WhileExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_while = if let Some(Token::While { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::While)
        } else {
            parser.log_unexpected_token("`while`");
            Err(ErrorsEmitted)
        }?;

        let condition = match parser.current_token() {
            Some(Token::LParen { .. }) => Ok(Box::new(GroupedExpr::parse(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`(`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let block = parser.expect_block()?;

        let span = parser.get_span(&first_token.unwrap().span(), &block.span);

        let expr = WhileExpr {
            kw_while,
            condition,
            block,
            span,
        };

        Ok(expr)
    }
}

impl fmt::Debug for WhileExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WhileExpr")
            .field("condition", &self.condition)
            .field("block", &self.block)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_while_expr() -> Result<(), ()> {
        let input = r#"
        while (x < 5) {
            x += 1;
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
