use crate::{
    ast::{BinaryExpr, BinaryOp, Expression},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

/// Parse a binary operation (e.g., arithmetic, logical and comparison expressions).
/// This method parses the operator and calls `parse_expression()` recursively to handle
/// the right-hand side of the expression.
pub(crate) fn parse_binary_expr(
    parser: &mut Parser,
    left_expr: Expression,
    op: BinaryOp,
) -> Result<BinaryExpr, ErrorsEmitted> {
    println!("ENTER `parse_binary_expr()`");
    println!("CURRENT TOKEN: {:?}", parser.peek_current());

    match op {
        BinaryOp::Add => {
            let right_expr = parser.parse_expression(Precedence::Sum)?;

            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::Subtract => {
            let right_expr = parser.parse_expression(Precedence::Difference)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::Multiply => {
            let right_expr = parser.parse_expression(Precedence::Product)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::Divide => {
            let right_expr = parser.parse_expression(Precedence::Quotient)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::Modulus => {
            let right_expr = parser.parse_expression(Precedence::Remainder)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::Equal => {
            let right_expr = parser.parse_expression(Precedence::Equal)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::NotEqual => {
            let right_expr = parser.parse_expression(Precedence::NotEqual)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::LessThan => {
            let right_expr = parser.parse_expression(Precedence::LessThan)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::LessEqual => {
            let right_expr = parser.parse_expression(Precedence::LessThanOrEqual)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::GreaterThan => {
            let right_expr = parser.parse_expression(Precedence::GreaterThan)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::GreaterEqual => {
            let right_expr = parser.parse_expression(Precedence::GreaterThanOrEqual)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::Assign => {
            let right_expr = parser.parse_expression(Precedence::Assignment)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::AddAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::SubtractAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::MultiplyAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::DivideAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::ModulusAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::LogicalAnd => {
            let right_expr = parser.parse_expression(Precedence::LogicalAnd)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::LogicalOr => {
            let right_expr = parser.parse_expression(Precedence::LogicalOr)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::BitwiseAnd => {
            let right_expr = parser.parse_expression(Precedence::BitwiseAnd)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::BitwiseOr => {
            let right_expr = parser.parse_expression(Precedence::BitwiseOr)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::BitwiseXor => {
            let right_expr = parser.parse_expression(Precedence::BitwiseXor)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::ShiftLeft => {
            let right_expr = parser.parse_expression(Precedence::Shift)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
        BinaryOp::ShiftRight => {
            let right_expr = parser.parse_expression(Precedence::Shift)?;
            Ok(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            })
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::test_utils;

    #[test]
    fn test_binary_expr_add() -> Result<(), ()> {
        let input = r#"x + 2"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
