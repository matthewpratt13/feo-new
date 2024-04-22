use crate::{
    ast::{BorrowExpr, DereferenceExpr, NegationExpr, PlaceExpr, UnaryOp},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl NegationExpr {
    pub(crate) fn parse(parser: &mut Parser, op: UnaryOp) -> Result<NegationExpr, ErrorsEmitted> {
        parser.consume_token();

        let expression = parser.parse_expression(Precedence::Unary)?;

        match op {
            _ => Ok(NegationExpr {
                expression: Box::new(expression),
                op,
            }),
        }
    }
}

impl BorrowExpr {
    pub(crate) fn parse(parser: &mut Parser, op: UnaryOp) -> Result<BorrowExpr, ErrorsEmitted> {
        parser.consume_token();

        let expression = parser.parse_expression(Precedence::Unary)?;

        match op {
            _ => Ok(BorrowExpr {
                expression: Box::new(expression),
                op,
            }),
        }
    }
}

impl DereferenceExpr {
    pub(crate) fn parse(parser: &mut Parser, op: UnaryOp) -> Result<DereferenceExpr, ErrorsEmitted> {
        parser.consume_token();

        let expression = PlaceExpr::try_from(parser.parse_expression(Precedence::Unary)?)?;

        match op {
            _ => Ok(DereferenceExpr {
                expression: Box::new(expression),
                op,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_unary_expr_negate() -> Result<(), ()> {
        let input = r#"-x"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    // #[ignore]
    fn parse_unary_expr_reference() -> Result<(), ()> {
        let input = r#"&x"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
