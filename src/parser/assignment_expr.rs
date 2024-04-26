use crate::{
    ast::{
        AssigneeExpr, AssignmentExpr, AssignmentOp, CompoundAssignmentExpr, CompoundAssignmentOp,
        Expression, ValueExpr,
    },
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

impl AssignmentExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: Expression,
    ) -> Result<AssignmentExpr, ErrorsEmitted> {
        {
            let right_expr = parser.parse_expression(Precedence::Assignment)?;
            let rhs = ValueExpr::try_from(right_expr).map_err(|e| {
                parser.log_error(e);
                ErrorsEmitted
            })?;

            Ok(AssignmentExpr {
                lhs: AssigneeExpr::try_from(left_expr).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?,
                op: AssignmentOp,
                rhs,
            })
        }
    }
}

impl CompoundAssignmentExpr {
    pub(crate) fn parse(
        parser: &mut Parser,
        left_expr: Expression,
        op: CompoundAssignmentOp,
    ) -> Result<CompoundAssignmentExpr, ErrorsEmitted> {
        let lhs = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;
        match op {
            CompoundAssignmentOp::AddAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                let value_expr = ValueExpr::try_from(right_expr).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?;

                Ok(CompoundAssignmentExpr {
                    lhs,
                    op,
                    rhs: value_expr,
                })
            }
            CompoundAssignmentOp::SubtractAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                let value_expr = ValueExpr::try_from(right_expr).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?;

                Ok(CompoundAssignmentExpr {
                    lhs,
                    op,
                    rhs: value_expr,
                })
            }
            CompoundAssignmentOp::MultiplyAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                let value_expr = ValueExpr::try_from(right_expr).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?;

                Ok(CompoundAssignmentExpr {
                    lhs,
                    op,
                    rhs: value_expr,
                })
            }
            CompoundAssignmentOp::DivideAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                let value_expr = ValueExpr::try_from(right_expr).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?;

                Ok(CompoundAssignmentExpr {
                    lhs,
                    op,
                    rhs: value_expr,
                })
            }
            CompoundAssignmentOp::ModulusAssign => {
                let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
                let value_expr = ValueExpr::try_from(right_expr).map_err(|e| {
                    parser.log_error(e);
                    ErrorsEmitted
                })?;

                Ok(CompoundAssignmentExpr {
                    lhs,
                    op,
                    rhs: value_expr,
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
