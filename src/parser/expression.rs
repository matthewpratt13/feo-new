use crate::{
    ast::{
        expression::{
            BinaryOpExpr, BlockExpr, CallExpr, FieldAccessExpr, GroupedExpr, IndexExpr, PathExpr,
            TupleExpr, TypeCastExpr,
        },
        BinaryOp, Delimiter, Expression, Identifier, Keyword, Separator,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

pub trait ParseExpression {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted>;
}

impl ParseExpression for PathExpr {
    fn parse(parser: &mut Parser, prefix: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for TypeCastExpr {
    /// Parse a type cast expression.
    fn parse(parser: &mut Parser, operand: Expression) -> Result<Expression, ErrorsEmitted> {
        let token = parser.expect_token(Token::As {
            name: "as".to_string(),
            span: parser.stream.span(),
        });

        let token = parser.consume_token()?;

        let ty = parser.expect_type(token)?;

        Ok(Expression::TypeCast(TypeCastExpr {
            operand: Box::new(operand),
            kw_as: Keyword::As,
            new_type: ty,
        }))

        // let token = parser.consume_token();

        // if token.clone()?
        //     != (Token::As {
        //         name: "as".to_string(),
        //         span: parser.stream.span(),
        //     })
        // {
        //     parser.log_error(ParserErrorKind::UnexpectedToken {
        //         expected: "`as`".to_string(),
        //         found: token?,
        //     });
        //     return Err(ErrorsEmitted(()));
        // }

        // Ok(Expression::TypeCast(TypeCastExpr {
        //     operand: Box::new(operand),
        //     kw_as: Keyword::As,
        //     new_type: parser.get_type()?,
        // }))
    }
}

impl ParseExpression for GroupedExpr {
    /// Parse a grouped (parenthesized) expression.
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let token = parser.expect_token(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        });

        if let Ok(Token::RParen { .. }) = token {
            Ok(Expression::Grouped(GroupedExpr {
                open_paren: Delimiter::LParen,
                expr: Box::new(expr),
                close_paren: Delimiter::RParen,
            }))
        } else if let Ok(Token::Comma { .. }) = token {
            parser.unconsume(); // go back to the input expression and try to parse a tuple
            TupleExpr::parse(parser, expr)
        } else {
            Err(token.unwrap_err())
        }
    }
}

/// Parse a block expression (i.e., `{ expr1; expr2; ... }`).
impl ParseExpression for BlockExpr {
    fn parse(parser: &mut Parser, first_expr: Expression) -> Result<Expression, ErrorsEmitted> {
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

impl ParseExpression for FieldAccessExpr {
    /// Parse a field access expression (i.e., `object.field`).
    fn parse(parser: &mut Parser, object: Expression) -> Result<Expression, ErrorsEmitted> {
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
            // TODO: check for `(` (method call) or uint (tuple index)
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token?,
            });
            Err(ErrorsEmitted(()))
        }
    }
}

impl ParseExpression for CallExpr {
    /// Parse a function call with arguments.
    fn parse(parser: &mut Parser, callee: Expression) -> Result<Expression, ErrorsEmitted> {
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
}

impl ParseExpression for IndexExpr {
    /// Parse an index expression (i.e., `array[index]`).
    fn parse(parser: &mut Parser, array: Expression) -> Result<Expression, ErrorsEmitted> {
        parser.expect_token(Token::LBracket {
            delim: '[',
            span: parser.stream.span(),
        })?;

        let index = parser.parse_expression(Precedence::Index)?;

        parser.expect_token(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        })?;

        Ok(Expression::Index(IndexExpr {
            array: Box::new(array),
            open_bracket: Delimiter::LBracket,
            index: Box::new(index),
            close_bracket: Delimiter::RBracket,
        }))
    }
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

impl ParseExpression for TupleExpr {
    fn parse(parser: &mut Parser, first_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}
