use crate::{
    ast::{AssignmentExpr, AssignmentOp, PlaceExpr},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl AssignmentExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: PlaceExpr,
        op: AssignmentOp,
    ) -> Result<AssignmentExpr, ErrorsEmitted> {
        match op {
            AssignmentOp::Assign => {
                let right_expr = parser.parse_expression(Precedence::Assignment)?;
                Ok(AssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            AssignmentOp::AddAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(AssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            AssignmentOp::SubtractAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(AssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            AssignmentOp::MultiplyAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(AssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            AssignmentOp::DivideAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(AssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
            AssignmentOp::ModulusAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                Ok(AssignmentExpr {
                    lhs: left_expr,
                    op,
                    rhs: Box::new(right_expr),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils;

    #[test]
    fn parse_assign_expr() -> Result<(), ()> {
        let input = r#"x = 5"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
