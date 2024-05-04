use crate::{
    ast::{AssigneeExpr, Delimiter, Expression, IndexExpr},
    error::ErrorsEmitted,
    token::Token,
};

use super::{parse::ParseOperation, Parser, Precedence};

impl ParseOperation for IndexExpr {
    fn parse(parser: &mut Parser, array: Expression) -> Result<Expression, ErrorsEmitted> {
        let array = AssigneeExpr::try_from(array).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let open_bracket = if let Some(Token::LBracket { .. }) = parser.next_token() {
            Ok(Delimiter::LBracket)
        } else {
            parser.log_unexpected_token("`[`");
            Err(ErrorsEmitted)
        }?;

        let index = parser.parse_expression(Precedence::Lowest)?;

        let close_bracket = if let Some(Token::RBracket { .. }) = parser.next_token() {
            Ok(Delimiter::RBracket)
        } else {
            parser.log_missing_token("`]`");
            parser.log_unmatched_delimiter(open_bracket.clone());
            Err(ErrorsEmitted)
        }?;

        let expr = IndexExpr {
            array: Box::new(array),
            open_bracket,
            index: Box::new(index),
            close_bracket,
        };

        Ok(Expression::Index(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_index_expr_uint() -> Result<(), ()> {
        let input = r#"array[0]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_index_expr_identifier() -> Result<(), ()> {
        let input = r#"array[index]"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
