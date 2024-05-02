use crate::{
    ast::{Delimiter, Expression, Identifier, OuterAttr, PathExpr, StructExpr, StructField},
    error::{ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenType},
};

use super::{collection, test_utils::log_token, Parser, Precedence};

impl StructExpr {
    pub(crate) fn parse(parser: &mut Parser, path: PathExpr) -> Result<Expression, ErrorsEmitted> {
        let open_brace = if let Some(Token::LBrace { .. }) = parser.current_token() {
            parser.next_token();
            log_token(parser, "consume token", true);
            Ok(Delimiter::LBrace)
        } else {
            parser.log_unexpected_token(TokenType::LBrace);
            Err(ErrorsEmitted)
        }?;

        let fields = collection::get_collection_braces(parser, parse_struct_field)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Delimiter::RBrace)
        } else {
            parser.log_error(ParserErrorKind::MissingDelimiter {
                delim: TokenType::RBrace,
            });
            Err(ErrorsEmitted)
        }?;

        let expr = StructExpr {
            path,
            open_brace,
            fields_opt: {
                if fields.is_empty() {
                    None
                } else {
                    Some(fields)
                }
            },
            close_brace,
        };

        Ok(Expression::Struct(expr))
    }
}

fn parse_struct_field(parser: &mut Parser) -> Result<StructField, ErrorsEmitted> {
    let mut attributes: Vec<OuterAttr> = Vec::new();

    while let Some(oa) = OuterAttr::outer_attr(parser) {
        attributes.push(oa);
        parser.next_token();
    }

    let field_name = if let Some(Token::Identifier { name, .. }) = parser.current_token() {
        parser.next_token();
        Ok(Identifier(name))
    } else {
        parser.expect_delimiter(TokenType::RBrace)?;
        Err(ErrorsEmitted)
    }?;

    parser.expect_separator(TokenType::Colon)?;

    let field_value = parser.parse_expression(Precedence::Lowest)?;

    let field = StructField {
        attributes_opt: {
            if attributes.is_empty() {
                None
            } else {
                Some(attributes)
            }
        },
        field_name,
        field_value,
    };

    Ok(field)
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
    use crate::parser::test_utils;

    #[test]
    fn parse_struct_expr() -> Result<(), ()> {
        let input = r#"
        SomeStruct {
            foo: "bar",
            baz: -10,
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
