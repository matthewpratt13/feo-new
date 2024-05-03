use crate::{
    ast::{BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Delimiter, Expression, Type},
    error::ErrorsEmitted,
    token::Token,
};

use super::{collection, Parser, Precedence};

impl ClosureExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let params = match parser.current_token() {
            Some(Token::Pipe { .. }) => {
                parser.next_token();

                let vec = collection::get_collection(parser, parse_closure_param, Delimiter::Pipe)?;

                if vec.is_some() {
                    Ok(ClosureParams::Some(vec.unwrap()))
                } else {
                    parser.log_unexpected_str("closure parameters");
                    Err(ErrorsEmitted)
                }
            }
            Some(Token::DblPipe { .. }) => {
                parser.next_token();
                Ok(ClosureParams::None)
            }
            _ => {
                parser.log_unexpected_str("`|` or `||`");
                Err(ErrorsEmitted)
            }
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
            parser.next_token();
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

fn parse_closure_param(parser: &mut Parser) -> Result<ClosureParam, ErrorsEmitted> {
    let name = match parser.current_token() {
        Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
            parser.get_identifier_patt()
        }
        _ => {
            parser.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }
    }?;

    let ty = if let Some(Token::Colon { .. }) = parser.current_token() {
        parser.next_token();
        Some(Type::parse(parser)?)
    } else {
        None
    };

    let param = ClosureParam {
        param_name: name,
        type_ann_opt: ty,
    };

    Ok(param)
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_closure_expr_without_block() -> Result<(), ()> {
        let input = r#"|| 2 + 2"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
