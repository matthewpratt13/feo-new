use crate::{
    ast::{
        Delimiter, Expression, Identifier, OuterAttr, PathExpr, PathRoot, SelfType, StructExpr,
        StructField,
    },
    error::ErrorsEmitted,
    parser::{collection, ParseConstruct, Parser, Precedence},
    token::Token,
};

impl ParseConstruct for StructExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let root = match parser.current_token() {
            Some(Token::Identifier { name, .. }) => {
                Ok(PathRoot::Identifier(Identifier::from(&name)))
            }
            Some(Token::SelfType { .. }) => Ok(PathRoot::SelfType(SelfType)),
            _ => {
                parser.log_unexpected_token("identifier or `Self`");
                Err(ErrorsEmitted)
            }
        }?;

        let struct_path = PathExpr {
            path_root: root,
            tree_opt: None,
        };

        parser.next_token();

        let open_brace = match parser.current_token() {
            Some(Token::LBrace { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LBrace { position })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let struct_fields_opt =
            collection::get_collection(parser, parse_struct_field, &open_brace)?;

        match parser.current_token() {
            Some(Token::RBrace { .. }) => {
                parser.next_token();
                Ok(Expression::Struct(StructExpr {
                    struct_path,
                    struct_fields_opt,
                }))
            }
            _ => {
                parser.log_unmatched_delimiter(&open_brace);
                Err(ErrorsEmitted)
            }
        }
    }
}

fn parse_struct_field(parser: &mut Parser) -> Result<StructField, ErrorsEmitted> {
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

            if let Some(Token::Comma { .. } | Token::RBrace { .. }) = parser.current_token() {
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
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
