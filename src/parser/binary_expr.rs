use crate::{
    ast::{expression::BinaryExpr, BinaryOp, Expression},
    error::ErrorsEmitted,
};

use super::{Parser, Precedence};

/// Parse a binary operations (e.g., arithmetic, logical and comparison expressions).
/// This method parses the operator and calls `parse_expression()` recursively to handle
/// the right-hand side of the expression.
pub(crate) fn parse_binary_expression(
    parser: &mut Parser,
    left_expr: Expression,
    op: BinaryOp,
) -> Result<Expression, ErrorsEmitted> {
    match op {
        BinaryOp::Add => {
            let right_expr = parser.parse_expression(Precedence::Sum)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Subtract => {
            let right_expr = parser.parse_expression(Precedence::Difference)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Multiply => {
            let right_expr = parser.parse_expression(Precedence::Product)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Divide => {
            let right_expr = parser.parse_expression(Precedence::Quotient)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Modulus => {
            let right_expr = parser.parse_expression(Precedence::Remainder)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Equal => {
            let right_expr = parser.parse_expression(Precedence::Equal)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::NotEqual => {
            let right_expr = parser.parse_expression(Precedence::NotEqual)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LessThan => {
            let right_expr = parser.parse_expression(Precedence::LessThan)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LessEqual => {
            let right_expr = parser.parse_expression(Precedence::LessThanOrEqual)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::GreaterThan => {
            let right_expr = parser.parse_expression(Precedence::GreaterThan)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::GreaterEqual => {
            let right_expr = parser.parse_expression(Precedence::GreaterThanOrEqual)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Assign => {
            let right_expr = parser.parse_expression(Precedence::Assignment)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::AddAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::SubtractAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::MultiplyAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::DivideAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::ModulusAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LogicalAnd => {
            let right_expr = parser.parse_expression(Precedence::LogicalAnd)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LogicalOr => {
            let right_expr = parser.parse_expression(Precedence::LogicalOr)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::BitwiseAnd => {
            let right_expr = parser.parse_expression(Precedence::BitwiseAnd)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::BitwiseOr => {
            let right_expr = parser.parse_expression(Precedence::BitwiseOr)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::BitwiseXor => {
            let right_expr = parser.parse_expression(Precedence::BitwiseXor)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::ShiftLeft => {
            let right_expr = parser.parse_expression(Precedence::Shift)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::ShiftRight => {
            let right_expr = parser.parse_expression(Precedence::Shift)?;
            Ok(Expression::BinaryOp(BinaryExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
    }
}
