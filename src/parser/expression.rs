use crate::{
    ast::{
        expression::{
            BinaryOpExpr, BlockExpr, CallExpr, FieldAccessExpr, GroupedExpr, IndexExpr,
            MethodCallExpr, PathExpr, TupleExpr, TupleIndexExpr, TypeCastExpr,
        },
        BinaryOp, Delimiter, Expression, Identifier, Statement,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

///////////////////////////////////////////////////////////////////////////

/// Trait that provides a common interface for parsing different expressions.
pub trait ParseExpression {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted>;
}

///////////////////////////////////////////////////////////////////////////

impl ParseExpression for PathExpr {
    fn parse(parser: &mut Parser, prefix: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for MethodCallExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl ParseExpression for TypeCastExpr {
    fn parse(parser: &mut Parser, operand: Expression) -> Result<Expression, ErrorsEmitted> {
        let kw_as = parser.expect_keyword(Token::As {
            name: "as".to_string(),
            span: parser.stream.span(),
        })?;

        let new_type = parser.get_type()?;

        Ok(Expression::TypeCast(TypeCastExpr {
            operand: Box::new(operand),
            kw_as,
            new_type,
        }))
    }
}

impl ParseExpression for GroupedExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let token = parser.expect_token(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        });

        if let Ok(Token::RParen { .. }) = token {
            Ok(Expression::Grouped(GroupedExpr {
                open_paren: Delimiter::LParen,
                expression: Box::new(expr),
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

impl ParseExpression for BlockExpr {
    fn parse(parser: &mut Parser, first_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut statements: Vec<Statement> = Vec::new();

        // parse expressions until a closing brace
        while !parser.is_expected_token(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        }) {
            statements.push(parser.parse_statement()?);
        }

        let terminal_expression_opt = if let Ok(e) = parser.parse_expression(Precedence::Lowest) {
            Some(Box::new(e))
        } else {
            None
        };

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        Ok(Expression::Block(BlockExpr {
            open_brace,
            statements,
            terminal_expression_opt,
            close_brace,
        }))
    }
}

impl ParseExpression for FieldAccessExpr {
    fn parse(parser: &mut Parser, object: Expression) -> Result<Expression, ErrorsEmitted> {
        let dot = parser.expect_separator(Token::Dot {
            punc: '.',
            span: parser.stream.span(),
        })?;

        let token = parser.consume_token();

        if let Ok(Token::Identifier { name, .. }) = token {
            if let Some(Token::LParen { .. }) = parser.peek_ahead_by(1) {
                parser.unconsume();
                MethodCallExpr::parse(parser, object)
            } else {
                Ok(Expression::FieldAccess(FieldAccessExpr {
                    object: Box::new(object),
                    dot,
                    field: Identifier(name),
                }))
            }
        } else if let Ok(Token::UIntLiteral { .. }) = token {
            parser.unconsume();
            TupleIndexExpr::parse(parser, object)
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token?,
            });
            Err(ErrorsEmitted(()))
        }
    }
}

impl ParseExpression for CallExpr {
    fn parse(parser: &mut Parser, callee: Expression) -> Result<Expression, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new(); // store function arguments

        let open_paren = parser.expect_delimiter(Token::LParen {
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

            let arg_expr = parser.parse_expression(Precedence::Lowest);
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

        if args.is_empty() {
            Ok(Expression::Call(CallExpr {
                open_paren,
                callee: Box::new(callee),
                args_opt: None,
                close_paren: Delimiter::RParen,
            }))
        } else {
            Ok(Expression::Call(CallExpr {
                open_paren,
                callee: Box::new(callee),
                args_opt: Some(args),
                close_paren: Delimiter::RParen,
            }))
        }
    }
}

impl ParseExpression for IndexExpr {
    fn parse(parser: &mut Parser, array: Expression) -> Result<Expression, ErrorsEmitted> {
        let open_bracket = parser.expect_delimiter(Token::LBracket {
            delim: '[',
            span: parser.stream.span(),
        })?;

        let token = parser.consume_token();

        let index = if let Ok(Token::UIntLiteral { value, .. }) = token {
            value
        } else if let Ok(_) = token {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "array index".to_string(),
                found: token?,
            });

            return Err(ErrorsEmitted(()));
        } else {
            parser.log_error(ParserErrorKind::UnexpectedEndOfInput);
            return Err(ErrorsEmitted(()));
        };

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        })?;

        Ok(Expression::Index(IndexExpr {
            array: Box::new(array),
            open_bracket,
            index,
            close_bracket,
        }))
    }
}

impl ParseExpression for TupleIndexExpr {
    fn parse(parser: &mut Parser, expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

///////////////////////////////////////////////////////////////////////////

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
