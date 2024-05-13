use crate::{
    ast::{Delimiter, Expression, Identifier, OuterAttr, PathExpr, StructExpr, StructField},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{collection, Parser, Precedence},
    token::Token,
};

impl StructExpr {
    pub(crate) fn parse(parser: &mut Parser, path: PathExpr) -> Result<Expression, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `StructExpr::parse()`"),
        );
        parser.log_current_token(false);

        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let fields_opt = collection::get_collection(parser, parse_struct_field, Delimiter::RBrace)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_unmatched_delimiter(&open_brace);
            parser.log_missing_token("`}`");
            Err(ErrorsEmitted)
        }?;

        let expr = StructExpr {
            path,
            open_brace,
            fields_opt,
            close_brace,
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `StructExpr::parse()`"),
        );
        parser.log_current_token(false);

        Ok(Expression::Struct(expr))
    }
}

fn parse_struct_field(parser: &mut Parser) -> Result<StructField, ErrorsEmitted> {
    parser.logger.log(
        LogLevel::Debug,
        LogMsg::from("entering `parse_struct_field()`"),
    );
    parser.log_current_token(false);

    let attributes_opt = collection::get_attributes(parser, OuterAttr::outer_attr);

    let field_name = match parser.current_token() {
        Some(Token::Identifier { name, .. }) => {
            parser.next_token();
            Ok(Identifier(name))
        }
        Some(Token::EOF) | None => {
            parser.log_missing_token("identifier");
            Err(ErrorsEmitted)
        }
        _ => {
            parser.log_unexpected_token("identifier");
            Err(ErrorsEmitted)
        }
    }?;

    match parser.current_token() {
        Some(Token::Colon { .. }) => {
            parser.next_token();

            if let Some(Token::RBrace { .. } | Token::Comma { .. }) = parser.current_token() {
                parser.log_missing("expr", "value for struct field name");
                return Err(ErrorsEmitted);
            }
        }
        Some(Token::EOF) | None => {
            parser.log_unexpected_eoi();
            return Err(ErrorsEmitted);
        }
        _ => {
            parser.log_unexpected_token("`:`");
            return Err(ErrorsEmitted);
        }
    }

    let field_value = parser.parse_expression(Precedence::Lowest)?;

    let struct_field = StructField {
        attributes_opt,
        field_name,
        field_value,
    };

    parser.logger.log(
        LogLevel::Debug,
        LogMsg::from(format!("struct field: {:?}", struct_field)),
    );

    parser.logger.log(
        LogLevel::Debug,
        LogMsg::from("exiting `parse_struct_field()`"),
    );
    parser.log_current_token(false);

    Ok(struct_field)
}

// impl TupleStructExpr {
//     pub(crate) fn parse(parser: &mut Parser, path: PathExpr) -> Result<Expression, ErrorsEmitted> {
//         let open_paren = if let Some(Token::LParen { .. }) = parser.next_token() {
//             Ok(Delimiter::LParen)
//         } else {
//             parser.log_unexpected_token(TokenType::LParen);
//             Err(ErrorsEmitted)
//         }?;

//         let mut elements: Vec<Expression> = Vec::new();

//         loop {
//             if let Some(Token::RParen { .. }) = parser.current_token() {
//                 break;
//             }

//             let element = parser.parse_expression(Precedence::Lowest)?;
//             elements.push(element);

//             match parser.current_token() {
//                 Some(Token::Comma { .. }) => {
//                     parser.next_token();
//                     continue;
//                 }
//                 Some(Token::RParen { .. }) => break,
//                 _ => break,
//             }
//         }

//         let close_paren = parser.expect_delimiter(TokenType::RParen)?;

//         let expr = TupleStructExpr {
//             path,
//             open_paren,
//             elements_opt: {
//                 if elements.is_empty() {
//                     None
//                 } else {
//                     Some(elements)
//                 }
//             },
//             close_paren,
//         };

//         Ok(Expression::TupleStruct(expr))
//     }
// }

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_struct_expr() -> Result<(), ()> {
        let input = r#"
        SomeStruct {
            foo: "bar",
            baz: -10
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
