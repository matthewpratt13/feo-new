use crate::{
    ast::{Expression, RangeExpr, RangeOp},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl RangeExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        from: Expression,
        op: RangeOp,
    ) -> Result<RangeExpr, ErrorsEmitted> {
        let to = parser.parse_expression(Precedence::Range);

        if to.is_ok() {
            Ok(RangeExpr {
                from_opt: Some(Box::new(from)),
                op,
                to_opt: Some(Box::new(to?)),
            })
        } else {
            Ok(RangeExpr {
                from_opt: Some(Box::new(from)),
                op,
                to_opt: None,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_range_expr_excl() -> Result<(), ()> {
        let input = r#"1..10"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_range_expr_incl() -> Result<(), ()> {
        let input = r#"..=10"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_range_expr_full() -> Result<(), ()> {
        let input = r#".."#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
