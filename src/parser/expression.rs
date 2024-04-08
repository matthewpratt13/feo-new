use crate::{
    ast::{
        expression::{BinaryOpExpr, BlockExpr, CallExpr, FieldAccessExpr},
        BinaryOp, Delimiter, Expression, Identifier, Separator,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

pub trait ParseExpression {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted>;
}

/// Parse a block expression (i.e., `{ expr1; expr2; ... }`).
impl ParseExpression for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.expect_token(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut expressions: Vec<Expression> = Vec::new();

        // parse expressions until a closing brace
        while !parser.is_expected_token(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        }) {
            expressions.push(parser.parse_expression(Precedence::Lowest)?);
        }

        parser.expect_token(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        Ok(Expression::Block(expressions))
    }
}

/// Parse a field access expression (i.e., `object.field`).
pub(crate) fn parse_field_access_expression(
    parser: &mut Parser,
    object: Expression,
) -> Result<Expression, ErrorsEmitted> {
    parser.expect_token(Token::FullStop {
        punc: '.',
        span: parser.stream.span(),
    })?;

    let token = parser.consume_token();

    if let Ok(Token::Identifier { name, .. }) = token {
        let expr = FieldAccessExpr {
            object: Box::new(object),
            dot: Separator::FullStop,
            field: Identifier(name),
        };

        Ok(Expression::FieldAccess(expr))
    } else {
        parser.log_error(ParserErrorKind::UnexpectedToken {
            expected: "identifier after `.`".to_string(),
            found: token?,
        });
        Err(ErrorsEmitted(()))
    }
}

/// Parse a function call with arguments.
pub(crate) fn parse_call_expression(
    parser: &mut Parser,
    callee: Expression,
) -> Result<Expression, ErrorsEmitted> {
    let mut args: Vec<Expression> = Vec::new(); // store function arguments

    parser.expect_token(Token::LParen {
        delim: '(',
        span: parser.stream.span(),
    })?;

    // parse arguments – separated by commas – until a closing parenthesis
    loop {
        if let Some(Token::RParen { delim: ')', .. }) = parser.peek_current() {
            // end of arguments
            parser.consume_token()?;
            break;
        }

        let arg_expr = parser.parse_expression(Precedence::Call);
        args.push(arg_expr?);

        // error handling
        let token = parser.consume_token();

        match token {
            Ok(Token::Comma { .. }) => continue, // more arguments
            Ok(Token::RParen { .. }) => break,   // end of arguments
            _ => {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`,` or `)`".to_string(),
                    found: token?,
                });

                return Err(ErrorsEmitted(()));
            }
        }
    }

    Ok(Expression::Call(CallExpr {
        open_paren: Delimiter::LParen,
        callee: Box::new(callee),
        args,
        close_paren: Delimiter::RParen,
    }))
}

/// Parse a binary operations (e.g., arithmetic, logical and comparison expressions).
/// This method parses the operator and calls `parse_expression()` recursively to handle
/// the right-hand side of the expression.
pub(crate) fn parse_binary_op_expression(
    parser: &mut Parser,
    left_expr: Expression,
    op: BinaryOp,
) -> Result<Expression, ErrorsEmitted> {
    match op {
        BinaryOp::Add => {
            let right_expr = parser.parse_expression(Precedence::Sum)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Subtract => {
            let right_expr = parser.parse_expression(Precedence::Difference)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Multiply => {
            let right_expr = parser.parse_expression(Precedence::Product)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Divide => {
            let right_expr = parser.parse_expression(Precedence::Quotient)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Modulus => {
            let right_expr = parser.parse_expression(Precedence::Remainder)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Equal => {
            let right_expr = parser.parse_expression(Precedence::Equal)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::NotEqual => {
            let right_expr = parser.parse_expression(Precedence::NotEqual)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LessThan => {
            let right_expr = parser.parse_expression(Precedence::LessThan)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LessEqual => {
            let right_expr = parser.parse_expression(Precedence::LessThanOrEqual)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::GreaterThan => {
            let right_expr = parser.parse_expression(Precedence::GreaterThan)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::GreaterEqual => {
            let right_expr = parser.parse_expression(Precedence::GreaterThanOrEqual)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::Assign => {
            let right_expr = parser.parse_expression(Precedence::Assignment)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::AddAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::SubtractAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::MultiplyAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::DivideAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::ModulusAssign => {
            let right_expr = parser.parse_expression(Precedence::CompoundAssignment)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LogicalAnd => {
            let right_expr = parser.parse_expression(Precedence::LogicalAnd)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::LogicalOr => {
            let right_expr = parser.parse_expression(Precedence::LogicalOr)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::BitwiseAnd => {
            let right_expr = parser.parse_expression(Precedence::BitwiseAnd)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::BitwiseOr => {
            let right_expr = parser.parse_expression(Precedence::BitwiseOr)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::BitwiseXor => {
            let right_expr = parser.parse_expression(Precedence::BitwiseXor)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::ShiftLeft => {
            let right_expr = parser.parse_expression(Precedence::Shift)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
        BinaryOp::ShiftRight => {
            let right_expr = parser.parse_expression(Precedence::Shift)?;
            Ok(Expression::BinaryOp(BinaryOpExpr {
                lhs: Box::new(left_expr),
                op,
                rhs: Box::new(right_expr),
            }))
        }
    }
}
