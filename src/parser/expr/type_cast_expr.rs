use crate::{
    ast::{Expression, Keyword, Type, TypeCastExpr, ValueExpr},
    error::ErrorsEmitted,
    parser::{ParseOperation, Parser},
    token::Token,
};

impl ParseOperation for TypeCastExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let value_expr = ValueExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let kw_as = if let Some(Token::As { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::As)
        } else {
            parser.log_unexpected_token("`as`");
            Err(ErrorsEmitted)
        }?;

        let new_type = Box::new(Type::parse(parser)?);

        let expr = TypeCastExpr {
            operand: Box::new(value_expr),
            kw_as,
            new_type,
        };

        Ok(Expression::TypeCast(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_type_cast_expr() -> Result<(), ()> {
        let input = r#"x as u64"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
