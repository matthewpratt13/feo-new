use crate::{
    ast::{
        BinaryOp, BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Expression, Identifier,
        Separator,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

impl ClosureExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ClosureExpr, ErrorsEmitted> {
        let token = parser.consume_token();

        let params = match token {
            Some(Token::Pipe { .. }) => {
                let mut vec: Vec<ClosureParam> = Vec::new();

                loop {
                    println!("ENTER CLOSURE PARAM LOOP");

                    if let Some(Token::Pipe { .. }) = parser.peek_current() {
                        parser.consume_token();
                        break;
                    }

                    let curr_token = parser.peek_current();

                    let id = match curr_token {
                        Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
                        _ => {
                            parser.log_error(ParserErrorKind::UnexpectedToken {
                                expected: "identifier".to_string(),
                                found: curr_token,
                            });

                            Err(ErrorsEmitted(()))
                        }
                    };

                    println!("CLOSURE PARAM NAME: {:?}", id);
                    println!("CURRENT TOKEN: {:?}", parser.peek_current());

                    parser.consume_token();

                    let ty = if let Some(Token::Colon { .. }) = parser.peek_current() {
                        parser.consume_token();
                        Some(parser.get_type()?)
                    } else {
                        None
                    };

                    let param = ClosureParam { id: id?, ty };
                    vec.push(param);

                    if let Some(Token::Comma { .. }) = parser.peek_current() {
                        parser.consume_token();
                    }
                }

                Ok(ClosureParams::Some(
                    BinaryOp::BitwiseOr,
                    vec,
                    BinaryOp::BitwiseOr,
                ))
            }
            Some(Token::DblPipe { .. }) => Ok(ClosureParams::None(BinaryOp::LogicalOr)),
            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`|` or `||`".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        };

        println!("CLOSURE PARAMS: {:?}\n", params);

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
            parser.consume_token();
            let ty = parser.get_type();
            Some((Separator::ThinArrow, ty?))
        } else {
            None
        };

        println!("RETURN TYPE (OPTIONAL): {:?}\n", return_type_opt);
        println!("CURRENT TOKEN: {:?}\n", parser.peek_current());

        let expression = if return_type_opt.is_some() {
            Expression::Block(BlockExpr::parse(parser)?)
        } else {
            parser.parse_expression(Precedence::Lowest)?
        };

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(ClosureExpr {
            params: params?,
            return_type_opt,
            expression: Box::new(expression),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_closure_expr_without_block() -> Result<(), ()> {
        let input = r#"|| 2 + 2"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_closure_expr_with_block() -> Result<(), ()> {
        let input = r#"|world: String| -> () {
            print("hello {}", world);
        }"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
