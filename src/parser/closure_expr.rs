use crate::{
    ast::{BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Expression, Type},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl ClosureExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let token = parser.consume_token();

        let params = match token {
            Some(Token::Pipe { .. }) => {
                let mut vec: Vec<ClosureParam> = Vec::new();

                loop {
                    if let Some(Token::Pipe { .. }) = parser.peek_current() {
                        parser.consume_token();
                        break;
                    }

                    let name = match parser.peek_current() {
                        Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
                            parser.get_identifier_patt()
                        }
                        _ => {
                            parser.log_unexpected_str("identifier");
                            Err(ErrorsEmitted)
                        }
                    }?;

                    let ty = if let Some(Token::Colon { .. }) = parser.peek_current() {
                        parser.consume_token();
                        Some(Type::parse(parser)?)
                    } else {
                        None
                    };

                    let param = ClosureParam {
                        param_name: name,
                        type_ann_opt: ty,
                    };
                    vec.push(param);

                    if let Some(Token::Comma { .. }) = parser.peek_current() {
                        parser.consume_token();
                    }
                }

                Ok(ClosureParams::Some(vec))
            }
            Some(Token::DblPipe { .. }) => Ok(ClosureParams::None),
            _ => {
                parser.log_unexpected_str("`|` or `||`");
                Err(ErrorsEmitted)
            }
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Box::new(Type::parse(parser)?))
        } else {
            None
        };

        let expression = if return_type_opt.is_some() {
            BlockExpr::parse(parser)?
        } else {
            parser.parse_expression(Precedence::Lowest)?
        };

        let expr = ClosureExpr {
            params,
            return_type_opt,
            expression: Box::new(expression),
        };

        Ok(Expression::Closure(expr))
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
