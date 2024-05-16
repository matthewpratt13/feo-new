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

        let new_type = match parser.current_token() {
            Some(Token::EOF) | None => {
                parser.log_missing("type", "cast type");
                Err(ErrorsEmitted)
            }
            Some(
                Token::I32Type { .. }
                | Token::I64Type { .. }
                | Token::I128Type { .. }
                | Token::U8Type { .. }
                | Token::U16Type { .. }
                | Token::U32Type { .. }
                | Token::U64Type { .. }
                | Token::U128Type { .. }
                | Token::U256Type { .. }
                | Token::U512Type { .. }
                | Token::ByteType { .. }
                | Token::B2Type { .. }
                | Token::B4Type { .. }
                | Token::B8Type { .. }
                | Token::B16Type { .. }
                | Token::B32Type { .. }
                | Token::H160Type { .. }
                | Token::H256Type { .. }
                | Token::H512Type { .. }
                | Token::CharType { .. }
                | Token::StrType { .. }
                | Token::StringType { .. },
            ) => Ok(Box::new(Type::parse(parser)?)),
            _ => {
                parser.log_unexpected_token("numeric, text or hash cast type");
                Err(ErrorsEmitted)
            }
        }?;

        let expr = TypeCastExpr {
            value: Box::new(value_expr),
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
    fn parse_type_cast_expr_numeric() -> Result<(), ()> {
        let input = r#"x as u64"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[should_panic]
    #[test]
    fn parse_type_cast_expr_identifier() {
        let input = r#"x as foo"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        parser.parse().expect(&format!(
            "unable to parse input. Log output: {:#?}",
            parser.logger.messages()
        ));
    }
}
