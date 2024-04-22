use crate::{
    ast::{BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Expression, Identifier, Separator},
    error::ErrorsEmitted,
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
                    if let Some(Token::Pipe { .. }) = parser.peek_current() {
                        parser.consume_token();
                        break;
                    }

                    let id = match parser.peek_current() {
                        Some(Token::Identifier { name, .. }) => Ok(Identifier(name)),
                        _ => {
                            parser.log_unexpected_token("identifier".to_string());
                            Err(ErrorsEmitted(()))
                        }
                    }?;

                    parser.consume_token();

                    let ty = if let Some(Token::Colon { .. }) = parser.peek_current() {
                        parser.consume_token();
                        Some(parser.get_type()?)
                    } else {
                        None
                    };

                    let param = ClosureParam { id, ty };
                    vec.push(param);

                    if let Some(Token::Comma { .. }) = parser.peek_current() {
                        parser.consume_token();
                    }
                }

                Ok(ClosureParams::Some(Separator::Pipe, vec, Separator::Pipe))
            }
            Some(Token::DblPipe { .. }) => Ok(ClosureParams::None(Separator::DblPipe)),
            _ => {
                parser.log_unexpected_token("`|` or `||`".to_string());
                Err(ErrorsEmitted(()))
            }
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
            parser.consume_token();
            Some((Separator::ThinArrow, parser.get_type()?))
        } else {
            None
        };

        let expression = if return_type_opt.is_some() {
            Expression::Block(BlockExpr::parse(parser)?)
        } else {
            parser.parse_expression(Precedence::Lowest)?
        };

        Ok(ClosureExpr {
            params,
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
        let input = r#"|world: str| -> () {
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
