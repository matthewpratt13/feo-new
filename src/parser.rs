#![allow(dead_code)]

use std::collections::HashMap;

use crate::{
    ast::{
        BinaryOp, Declaration, Definition, Expression, Identifier, Literal, Statement, StructField,
        Type, UnaryOp,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenStream},
};

/// Enum representing the different precedence levels of operators, respectively.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,             // `break`, `return`, closure
    Assignment,         // `=`
    CompoundAssignment, // `+=`, `-=`, `*/`, `/=`, `%=`
    Range,              // `..`, `..=` (requires parentheses)
    LogicalOr,          // `||`
    LogicalAnd,         // `&&`
    Equal,              // `==`
    NotEqual,           // `!=`
    LessThan,           // `<`
    GreaterThan,        // `>`
    LessThanOrEqual,    // `<=`
    GreaterThanOrEqual, // `>=`
    BitwiseOr,          // `|`
    BitwiseXor,         // `^`
    BitwiseAnd,         // `&`
    Shift,              // `«`, `»`
    Sum,                // `+`
    Difference,         // `-`
    Product,            // `*`
    Quotient,           // `/`
    Remainder,          // `%`
    Exponentiation,     // `**`
    Cast,               // "as"
    Unary,              // `-`, `*` `!`, `&`,`&mut`
    Unwrap,             // `?`
    Index,              // `x[0]`
    Call,               // `foo(bar)`
    FieldAccess,        // foo.bar
    MethodCall,         // foo.bar()
    Path,               // `package::module::Item`
}
/// Struct that stores a stream of tokens and contains methods to parse expressions,
/// statements and items, as well as helper methods and error handling capabilities.
#[derive(Debug)]
struct Parser {
    stream: TokenStream,
    current: usize,
    errors: Vec<CompilerError<ParserErrorKind>>, // store compiler errors
    precedences: HashMap<Token, Precedence>,     // record of precedence levels by operator
}

impl Parser {
    /// Create a new `Parser` instance.
    /// Initialize an empty `Vec` to store parser errors and an empty `HashMap`
    /// to store precedences.
    fn new(stream: TokenStream) -> Self {
        Parser {
            stream,
            current: 0,
            errors: Vec::new(),
            precedences: HashMap::new(),
        }
    }

    /// Define precedence levels for operators.
    fn init_precedences(&mut self) {
        self.precedences.insert(
            Token::As {
                name: "as".to_string(),
                span: self.stream.span(),
            },
            Precedence::Cast,
        );
        self.precedences.insert(
            Token::DblAsterisk {
                punc: "**".to_string(),
                span: self.stream.span(),
            },
            Precedence::Exponentiation,
        );
        self.precedences.insert(
            Token::Minus {
                punc: '-',
                span: self.stream.span(),
            },
            Precedence::Unary,
        );
        self.precedences.insert(
            Token::Bang {
                punc: '!',
                span: self.stream.span(),
            },
            Precedence::Unary,
        );
        self.precedences.insert(
            Token::Asterisk {
                punc: '*',
                span: self.stream.span(),
            },
            Precedence::Unary,
        );
        self.precedences.insert(
            Token::Ampersand {
                punc: '&',
                span: self.stream.span(),
            },
            Precedence::Unary,
        );
        self.precedences.insert(
            Token::Percent {
                punc: '%',
                span: self.stream.span(),
            },
            Precedence::Remainder,
        );
        self.precedences.insert(
            Token::Slash {
                punc: '/',
                span: self.stream.span(),
            },
            Precedence::Quotient,
        );
        self.precedences.insert(
            Token::Asterisk {
                punc: '*',
                span: self.stream.span(),
            },
            Precedence::Product,
        );
        self.precedences.insert(
            Token::Minus {
                punc: '-',
                span: self.stream.span(),
            },
            Precedence::Difference,
        );
        self.precedences.insert(
            Token::Plus {
                punc: '+',
                span: self.stream.span(),
            },
            Precedence::Sum,
        );
        self.precedences.insert(
            Token::Pipe {
                punc: '|',
                span: self.stream.span(),
            },
            Precedence::BitwiseOr,
        );
        self.precedences.insert(
            Token::Caret {
                punc: '^',
                span: self.stream.span(),
            },
            Precedence::BitwiseXor,
        );
        self.precedences.insert(
            Token::Ampersand {
                punc: '&',
                span: self.stream.span(),
            },
            Precedence::BitwiseAnd,
        );
        self.precedences.insert(
            Token::DblLessThan {
                punc: "<<".to_string(),
                span: self.stream.span(),
            },
            Precedence::Shift,
        );
        self.precedences.insert(
            Token::DblGreaterThan {
                punc: ">>".to_string(),
                span: self.stream.span(),
            },
            Precedence::Shift,
        );
        self.precedences.insert(
            Token::GreaterThanEquals {
                punc: ">=".to_string(),
                span: self.stream.span(),
            },
            Precedence::GreaterThanOrEqual,
        );
        self.precedences.insert(
            Token::GreaterThan {
                punc: '>',
                span: self.stream.span(),
            },
            Precedence::GreaterThan,
        );
        self.precedences.insert(
            Token::LessThanEquals {
                punc: "<=".to_string(),
                span: self.stream.span(),
            },
            Precedence::LessThanOrEqual,
        );
        self.precedences.insert(
            Token::LessThan {
                punc: '<',
                span: self.stream.span(),
            },
            Precedence::LessThan,
        );
        self.precedences.insert(
            Token::BangEquals {
                punc: "!=".to_string(),
                span: self.stream.span(),
            },
            Precedence::NotEqual,
        );
        self.precedences.insert(
            Token::DblEquals {
                punc: "==".to_string(),
                span: self.stream.span(),
            },
            Precedence::Equal,
        );
        self.precedences.insert(
            Token::DblAmpersand {
                punc: "&&".to_string(),
                span: self.stream.span(),
            },
            Precedence::LogicalAnd,
        );
        self.precedences.insert(
            Token::DblPipe {
                punc: "||".to_string(),
                span: self.stream.span(),
            },
            Precedence::LogicalOr,
        );
        self.precedences.insert(
            Token::PercentEquals {
                punc: "%=".to_string(),
                span: self.stream.span(),
            },
            Precedence::CompoundAssignment,
        );
        self.precedences.insert(
            Token::SlashEquals {
                punc: "/=".to_string(),
                span: self.stream.span(),
            },
            Precedence::CompoundAssignment,
        );
        self.precedences.insert(
            Token::AsteriskEquals {
                punc: "*=".to_string(),
                span: self.stream.span(),
            },
            Precedence::CompoundAssignment,
        );
        self.precedences.insert(
            Token::MinusEquals {
                punc: "-=".to_string(),
                span: self.stream.span(),
            },
            Precedence::CompoundAssignment,
        );
        self.precedences.insert(
            Token::PlusEquals {
                punc: "+=".to_string(),
                span: self.stream.span(),
            },
            Precedence::CompoundAssignment,
        );
        self.precedences.insert(
            Token::Equals {
                punc: '=',
                span: self.stream.span(),
            },
            Precedence::Assignment,
        );
        self.precedences.insert(
            Token::QuestionMark {
                punc: '?',
                span: self.stream.span(),
            },
            Precedence::Unwrap,
        );
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Main parsing function that returns a `Vec<Statement>`.
    fn parse(&mut self) -> Result<Vec<Statement>, ErrorsEmitted> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    ///////////////////////////////////////////////////////////////////////////
    /// EXPRESSIONS
    ///////////////////////////////////////////////////////////////////////////

    /// Recursively parse an expression based on operator precedence.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ErrorsEmitted> {
        let mut left_expr = self.parse_prefix()?;

        let current_precedence = &self.precedence(&self.peek_current().expect("token not found"));

        while let Some(cp) = current_precedence {
            if precedence < *cp
                && !self.is_expected_token(Token::Semicolon {
                    punc: ';',
                    span: self.stream.span(),
                })
            {
                left_expr = self.parse_infix(left_expr)?;
            } else {
                break;
            }
        }

        Ok(left_expr)
    }

    /// Parse primary expressions (e.g., grouped expressions, identifiers and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Ok(
                Token::Identifier { name, .. }
                | Token::SelfKeyword { name, .. }
                | Token::Underscore { name, .. },
            ) => Ok(Expression::Identifier(Identifier(name))),
            Ok(Token::IntLiteral { value, .. }) => Ok(Expression::Literal(Literal::Int(value))),
            Ok(Token::UIntLiteral { value, .. }) => Ok(Expression::Literal(Literal::UInt(value))),
            Ok(Token::BigUIntLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::BigUInt(value)))
            }
            Ok(Token::ByteLiteral { value, .. }) => Ok(Expression::Literal(Literal::Byte(value))),
            Ok(Token::BytesLiteral { value, .. }) => Ok(Expression::Literal(Literal::Bytes(value))),
            Ok(Token::HashLiteral { value, .. }) => Ok(Expression::Literal(Literal::Hash(value))),
            Ok(Token::StringLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::String(value)))
            }
            Ok(Token::CharLiteral { value, .. }) => Ok(Expression::Literal(Literal::Char(value))),
            Ok(Token::BoolLiteral { value, .. }) => Ok(Expression::Literal(Literal::Bool(value))),
            Ok(Token::LParen { .. }) => self.parse_grouped_expression(),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier, `Self`, `_`, literal or `(`".to_string(),
                    found: token?,
                });

                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Parse prefix expressions (e.g., integer literals, identifiers and parentheses),
    /// where the respective token type appears at the beginning of an expression.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Ok(Token::IntLiteral { .. }) => self.parse_primary(),
            Ok(Token::UIntLiteral { .. }) => self.parse_primary(),
            Ok(Token::BigUIntLiteral { .. }) => self.parse_primary(),
            Ok(Token::HashLiteral { .. }) => self.parse_primary(),
            Ok(Token::ByteLiteral { .. }) => self.parse_primary(),
            Ok(Token::BytesLiteral { .. }) => self.parse_primary(),
            Ok(Token::StringLiteral { .. }) => self.parse_primary(),
            Ok(Token::CharLiteral { .. }) => self.parse_primary(),
            Ok(Token::BoolLiteral { .. }) => self.parse_primary(),
            Ok(Token::Identifier { .. }) => self.parse_primary(),
            Ok(Token::SelfKeyword { .. }) => match self.peek_current() {
                Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) => {
                    self.parse_path_expression()
                }

                _ => self.parse_primary(),
            },
            Ok(Token::Minus { .. }) => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Negate, Box::new(expr)))
            }
            Ok(Token::Bang { .. }) => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Not, Box::new(expr)))
            }
            Ok(Token::Ampersand { .. }) => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Reference, Box::new(expr)))
            }
            Ok(Token::Asterisk { .. }) => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Dereference, Box::new(expr)))
            }
            Ok(Token::LParen { .. }) => {
                let expr = if let Ok(Token::Comma { .. }) =
                    self.peek_ahead_by(2)
                        .ok_or(ParserErrorKind::TokenIndexOutOfBounds {
                            len: self.stream.tokens().len(),
                            i: self.current + 2,
                        }) {
                    self.parse_tuple_expression()?
                } else {
                    self.parse_grouped_expression()?
                };

                Ok(expr)
            }
            Ok(Token::LBracket { .. }) => self.parse_array_expression(),
            Ok(Token::Pipe { .. } | Token::DblPipe { .. }) => {
                let expr = if let Ok(c) = self.parse_closure_with_block() {
                    c
                } else {
                    self.parse_closure_without_block()?
                };

                match expr {
                    Expression::ClosureWithBlock(a, b) => {
                        return Ok(Expression::ClosureWithBlock(a, b));
                    }
                    Expression::ClosureWithoutBlock(a) => {
                        return Ok(Expression::ClosureWithoutBlock(a));
                    }

                    _ => (),
                }

                Ok(expr)
            }

            Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) => {
                let expr = self.parse_range_expression()?;

                match self.peek_current() {
                    Some(
                        Token::Identifier { .. }
                        | Token::IntLiteral { .. }
                        | Token::UIntLiteral { .. }
                        | Token::BigUIntLiteral { .. },
                    ) => Ok(expr),
                    Some(t) => {
                        self.log_error(ParserErrorKind::UnexpectedToken {
                            expected: "identifier or number".to_string(),
                            found: t,
                        });

                        Err(ErrorsEmitted(()))
                    }
                    None => {
                        self.log_error(ParserErrorKind::UnexpectedEndOfInput);
                        Err(ErrorsEmitted(()))
                    }
                }
            }

            Ok(Token::Super { .. } | Token::Package { .. }) => self.parse_path_expression(),

            Ok(Token::Return { .. }) => self.parse_return_expression(),

            Ok(Token::Break { .. } | Token::Continue { .. }) => {
                self.parse_break_or_continue_expression()
            }

            Ok(Token::Underscore { .. }) => self.parse_primary(),

            _ => {
                self.log_error(ParserErrorKind::InvalidToken { token: token? });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Parse infix expressions (e.g., binary operators), where the respective token type
    /// appears in the middle of an expression.
    fn parse_infix(&mut self, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Ok(Token::Plus { .. }) => self.parse_binary_expression(left_expr, BinaryOp::Add),
            Ok(Token::Minus { .. }) => self.parse_binary_expression(left_expr, BinaryOp::Subtract),
            Ok(Token::Asterisk { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::Multiply)
            }
            Ok(Token::Slash { .. }) => self.parse_binary_expression(left_expr, BinaryOp::Divide),
            Ok(Token::Percent { .. }) => self.parse_binary_expression(left_expr, BinaryOp::Modulus),
            Ok(Token::DblEquals { .. }) => self.parse_binary_expression(left_expr, BinaryOp::Equal),
            Ok(Token::BangEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::NotEqual)
            }
            Ok(Token::LessThan { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::LessThan)
            }
            Ok(Token::LessThanEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::LessEqual)
            }
            Ok(Token::GreaterThan { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::GreaterThan)
            }
            Ok(Token::GreaterThanEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::GreaterEqual)
            }
            Ok(Token::Equals { .. }) => self.parse_binary_expression(left_expr, BinaryOp::Assign),
            Ok(Token::PlusEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::AddAssign)
            }
            Ok(Token::MinusEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::SubtractAssign)
            }
            Ok(Token::AsteriskEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::MultiplyAssign)
            }
            Ok(Token::SlashEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::DivideAssign)
            }
            Ok(Token::PercentEquals { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::ModulusAssign)
            }
            Ok(Token::DblAmpersand { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::LogicalAnd)
            }
            Ok(Token::DblPipe { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::LogicalOr)
            }
            Ok(Token::Ampersand { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::BitwiseAnd)
            }
            Ok(Token::Pipe { .. }) => self.parse_binary_expression(left_expr, BinaryOp::BitwiseOr),
            Ok(Token::Caret { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::BitwiseXor)
            }
            Ok(Token::DblLessThan { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::ShiftLeft)
            }
            Ok(Token::DblGreaterThan { .. }) => {
                self.parse_binary_expression(left_expr, BinaryOp::ShiftRight)
            }
            Ok(Token::QuestionMark { .. }) => self.parse_unwrap_expression(),
            Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) => self.parse_range_expression(),
            Ok(Token::As { .. }) => self.parse_cast_expression(),
            Ok(Token::LParen { .. }) => self.parse_call_expression(left_expr), // TODO: or tuple struct
            Ok(Token::LBrace { .. }) => self.parse_struct_expression(),
            Ok(Token::LBracket { .. }) => self.parse_index_expression(left_expr), // TODO: check
            Ok(Token::FullStop { .. }) => match self.peek_current() {
                Some(Token::Identifier { .. } | Token::SelfKeyword { .. }) => {
                    let token = self.peek_ahead_by(1).ok_or({
                        self.log_error(ParserErrorKind::TokenIndexOutOfBounds {
                            len: self.stream.tokens().len(),
                            i: self.current + 1,
                        })
                    });

                    match token {
                        Ok(Token::LParen { .. }) => return self.parse_method_call_expression(),
                        _ => self.parse_field_access_expression(left_expr),
                    }
                }
                Some(Token::UIntLiteral { .. }) => self.parse_tuple_index_expression(),
                Some(_) => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or tuple index".to_string(),
                        found: token?,
                    });
                    Err(ErrorsEmitted(()))
                }
                None => {
                    self.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    Err(ErrorsEmitted(()))
                }
            },

            Ok(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) => {
                self.parse_path_expression()
            }

            _ => {
                self.log_error(ParserErrorKind::InvalidToken { token: token? });
                Err(ErrorsEmitted(()))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////

    fn parse_path_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_method_call_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    /// Parse a field access expression (i.e., `object.field`).
    fn parse_field_access_expression(
        &mut self,
        object_expr: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        self.expect_token(Token::FullStop {
            punc: '.',
            span: self.stream.span(),
        })?;

        let token = self.consume_token();

        if let Ok(Token::Identifier { name, .. }) = token {
            Ok(Expression::FieldAccess(
                Box::new(object_expr),
                Identifier(name),
            ))
        } else {
            self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token?,
            });
            Err(ErrorsEmitted(()))
        }
    }

    /// Parse a function call with arguments.
    fn parse_call_expression(&mut self, callee: Expression) -> Result<Expression, ErrorsEmitted> {
        let mut args: Vec<Expression> = Vec::new(); // store function arguments

        self.expect_token(Token::LParen {
            delim: '(',
            span: self.stream.span(),
        })?;

        // parse arguments – separated by commas – until a closing parenthesis
        loop {
            if let Some(Token::RParen { delim: ')', .. }) = self.peek_current() {
                // end of arguments
                self.consume_token()?;
                break;
            }

            let arg_expr = self.parse_expression(Precedence::Call);
            args.push(arg_expr?);

            // error handling
            let token = self.consume_token();

            match token {
                Ok(Token::Comma { .. }) => continue, // more arguments
                Ok(Token::RParen { .. }) => break,   // end of arguments
                _ => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: token?,
                    });

                    return Err(ErrorsEmitted(()));
                }
            }
        }

        Ok(Expression::Call(Box::new(callee), args))
    }

    /// Parse an index expression (i.e., `array[index]`).
    fn parse_index_expression(
        &mut self,
        array_expr: Expression,
    ) -> Result<Expression, ErrorsEmitted> {
        self.expect_token(Token::LBracket {
            delim: '[',
            span: self.stream.span(),
        })?;

        let index_expr = self.parse_expression(Precedence::Index)?;

        self.expect_token(Token::RBracket {
            delim: ']',
            span: self.stream.span(),
        })?;

        Ok(Expression::Index(
            Box::new(array_expr),
            Box::new(index_expr),
        ))
    }

    fn parse_tuple_index_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_unwrap_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    /// Parse a type cast expression.
    fn parse_cast_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        let expr = self.parse_expression(Precedence::Cast)?;

        let token = self.consume_token();

        if token.clone()?
            != (Token::As {
                name: "as".to_string(),
                span: self.stream.span(),
            })
        {
            self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`as`".to_string(),
                found: token?,
            });
            return Err(ErrorsEmitted(()));
        }

        Ok(Expression::Cast(Box::new(expr), self.get_type()?))
    }

    /// Parse a binary expressions (e.g., arithmetic, logical and comparison expressions).
    /// This method parses the operator and calls `parse_expression()` recursively to handle
    /// the right-hand side of the expression.
    fn parse_binary_expression(
        &mut self,
        left_expr: Expression,
        op: BinaryOp,
    ) -> Result<Expression, ErrorsEmitted> {
        match op {
            BinaryOp::Add => {
                let right_expr = self.parse_expression(Precedence::Sum)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::Subtract => {
                let right_expr = self.parse_expression(Precedence::Difference)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::Multiply => {
                let right_expr = self.parse_expression(Precedence::Product)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::Divide => {
                let right_expr = self.parse_expression(Precedence::Quotient)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::Modulus => {
                let right_expr = self.parse_expression(Precedence::Remainder)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::Equal => {
                let right_expr = self.parse_expression(Precedence::Equal)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::NotEqual => {
                let right_expr = self.parse_expression(Precedence::NotEqual)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::LessThan => {
                let right_expr = self.parse_expression(Precedence::LessThan)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::LessEqual => {
                let right_expr = self.parse_expression(Precedence::LessThanOrEqual)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::GreaterThan => {
                let right_expr = self.parse_expression(Precedence::GreaterThan)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::GreaterEqual => {
                let right_expr = self.parse_expression(Precedence::GreaterThanOrEqual)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::Assign => {
                let right_expr = self.parse_expression(Precedence::Assignment)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::AddAssign => {
                let right_expr = self.parse_expression(Precedence::CompoundAssignment)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::SubtractAssign => {
                let right_expr = self.parse_expression(Precedence::CompoundAssignment)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::MultiplyAssign => {
                let right_expr = self.parse_expression(Precedence::CompoundAssignment)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::DivideAssign => {
                let right_expr = self.parse_expression(Precedence::CompoundAssignment)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::ModulusAssign => {
                let right_expr = self.parse_expression(Precedence::CompoundAssignment)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::LogicalAnd => {
                let right_expr = self.parse_expression(Precedence::LogicalAnd)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::LogicalOr => {
                let right_expr = self.parse_expression(Precedence::LogicalOr)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::BitwiseAnd => {
                let right_expr = self.parse_expression(Precedence::BitwiseAnd)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::BitwiseOr => {
                let right_expr = self.parse_expression(Precedence::BitwiseOr)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::BitwiseXor => {
                let right_expr = self.parse_expression(Precedence::BitwiseXor)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::ShiftLeft => {
                let right_expr = self.parse_expression(Precedence::Shift)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            BinaryOp::ShiftRight => {
                let right_expr = self.parse_expression(Precedence::Shift)?;
                Ok(Expression::BinaryOp(
                    op,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
        }
    }

    fn parse_range_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_return_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_break_or_continue_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_closure_with_block(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_closure_without_block(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    /// Parse a grouped (parenthesized) expression.
    fn parse_grouped_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        self.expect_token(Token::LParen {
            delim: '(',
            span: self.stream.span(),
        })?;

        // self.consume_token(); // consume `(``

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_token(Token::RParen {
            delim: ')',
            span: self.stream.span(),
        })?;

        Ok(expr)
    }

    /// Parse a block expression (i.e., `{ expr1; expr2; ... }`).
    fn parse_block_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        self.expect_token(Token::LBrace {
            delim: '{',
            span: self.stream.span(),
        })?;

        let mut expressions: Vec<Expression> = Vec::new();

        // parse expressions until a closing brace
        while !self.is_expected_token(Token::RBrace {
            delim: '}',
            span: self.stream.span(),
        }) {
            expressions.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_token(Token::RBrace {
            delim: '}',
            span: self.stream.span(),
        })?;

        Ok(Expression::Block(expressions))
    }

    /// Parse an array expression (i.e., `[element1, element2, element3, etc.]`).
    fn parse_array_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        self.expect_token(Token::LBracket {
            delim: '[',
            span: self.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        while !self.is_expected_token(Token::RBracket {
            delim: ']',
            span: self.stream.span(),
        }) {
            elements.push(self.parse_expression(Precedence::Lowest)?);

            if !self.tokens_match(Token::Comma {
                punc: ',',
                span: self.stream.span(),
            }) {
                break;
            }
        }

        self.expect_token(Token::RBracket {
            delim: ']',
            span: self.stream.span(),
        })?;

        Ok(Expression::Array(elements))
    }

    /// Parse a tuple expression (i.e., `(element1, element2, element3)`).
    fn parse_tuple_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        self.expect_token(Token::LParen {
            delim: '(',
            span: self.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        while !self.is_expected_token(Token::RParen {
            delim: ')',
            span: self.stream.span(),
        }) {
            elements.push(self.parse_expression(Precedence::Lowest)?);

            if !self.tokens_match(Token::Comma {
                punc: ',',
                span: self.stream.span(),
            }) {
                break;
            }
        }

        self.expect_token(Token::RParen {
            delim: ')',
            span: self.stream.span(),
        })?;

        Ok(Expression::Tuple(elements))
    }

    /// Parse a struct with fields.
    fn parse_struct_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        let mut fields: Vec<StructField> = Vec::new(); // stores struct fields

        self.expect_token(Token::LBrace {
            delim: '{',
            span: self.stream.span(),
        })?;

        // parse struct fields – separated by commas – until a closing brace
        loop {
            // get the field name
            let token = self.consume_token();

            let field_name = match token {
                Ok(Token::Identifier { name, .. }) => name,
                Ok(Token::RBrace { .. }) => break, // end of struct
                _ => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or `)`".to_string(),
                        found: token?,
                    });
                    return Err(ErrorsEmitted(()));
                }
            };

            self.expect_token(Token::Colon {
                punc: ':',
                span: self.stream.span(),
            })?;

            // parse field value
            let field_value = self.parse_expression(Precedence::Lowest)?;

            // push field to list of fields
            fields.push(StructField {
                name: Identifier(field_name),
                value: field_value,
            });

            // error handling
            let token = self.consume_token();

            match token {
                Ok(Token::Comma { .. }) => continue, // more fields
                Ok(Token::RBrace { .. }) => break,   // end of struct
                _ => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `}`".to_string(),
                        found: token?,
                    });
                    return Err(ErrorsEmitted(()));
                }
            }
        }

        Ok(Expression::Struct(fields))
    }

    ///////////////////////////////////////////////////////////////////////////
    /// STATEMENTS
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a `Statement`.
    fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        match self.consume_token() {
            Ok(Token::Let { .. }) => self.parse_let_statement(),
            Ok(Token::If { .. }) => self.parse_if_statement(),
            Ok(Token::Match { .. }) => self.parse_match_statement(),
            Ok(Token::For { .. }) => self.parse_for_in_statement(),
            Ok(Token::While { .. }) => self.parse_while_statement(),
            Ok(Token::Import { .. }) => {
                Ok(Statement::Declaration(self.parse_import_declaration()?))
            }
            Ok(Token::Alias { .. }) => Ok(Statement::Declaration(self.parse_alias_declaration()?)),
            Ok(Token::Const { .. }) => Ok(Statement::Declaration(self.parse_const_declaration()?)),
            Ok(Token::Static { .. }) => {
                Ok(Statement::Declaration(self.parse_static_var_declaration()?))
            }
            Ok(Token::Module { .. }) => Ok(Statement::Definition(self.parse_module()?)),
            Ok(Token::Trait { .. }) => Ok(Statement::Definition(self.parse_trait()?)),
            Ok(Token::Enum { .. }) => Ok(Statement::Definition(self.parse_enum()?)),
            Ok(Token::Struct { .. }) => Ok(Statement::Definition(self.parse_struct()?)),
            Ok(Token::Impl { .. }) => Ok(Statement::Definition(self.parse_impl()?)),
            Ok(Token::Func { .. }) => Ok(Statement::Definition(self.parse_function()?)),

            _ => {
                self.unconsume();
                self.parse_expression_statement()
            }
        }
    }

    /// Parse an identifier and the assignment operation that binds the variable to a value.
    fn parse_let_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        let identifier = self.consume_identifier();

        self.expect_token(Token::Equals {
            punc: '=',
            span: self.stream.span(),
        })?;

        let value = self.parse_expression(Precedence::Assignment);

        self.expect_token(Token::Semicolon {
            punc: ';',
            span: self.stream.span(),
        })?;

        Ok(Statement::Let(identifier?, value?))
    }

    /// Parse an expression into a statement, separated by a semicolon.
    fn parse_expression_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        let expression = self.parse_expression(Precedence::Lowest);

        self.expect_token(Token::Semicolon {
            punc: ';',
            span: self.stream.span(),
        })?;

        Ok(Statement::Expression(expression?))
    }

    // TODO: what about `else-if` branches ?
    /// Parse an `if` expression (i.e., `if (condition) { true block } else { false block }`).
    fn parse_if_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        self.expect_token(Token::If {
            name: "if".to_string(),
            span: self.stream.span(),
        })?;

        self.expect_token(Token::LParen {
            delim: '(',
            span: self.stream.span(),
        })?;

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_token(Token::RParen {
            delim: ')',
            span: self.stream.span(),
        })?;

        let true_branch = Box::new(self.parse_expression(Precedence::Lowest)?);

        let false_branch = if self.tokens_match(Token::Else {
            name: "else".to_string(),
            span: self.stream.span(),
        }) {
            Some(Box::new(self.parse_expression(Precedence::Lowest)?))
        } else {
            None
        };

        Ok(Statement::If(condition, true_branch, false_branch))
    }

    fn parse_match_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        todo!()
    }

    /// Parse a `for-in` expression (i.e., `for var in iterable { execution logic }`).
    fn parse_for_in_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        self.expect_token(Token::For {
            name: "for".to_string(),
            span: self.stream.span(),
        })?;

        let variable = Box::new(Expression::Identifier(self.consume_identifier()?));

        self.expect_token(Token::In {
            name: "in".to_string(),
            span: self.stream.span(),
        })?;

        let iterable = Box::new(self.parse_expression(Precedence::Lowest)?);

        let body = Box::new(self.parse_expression(Precedence::Lowest)?);

        Ok(Statement::ForIn(variable, iterable, body))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        todo!()
    }

    ///////////////////////////////////////////////////////////////////////////
    /// MODULE ITEMS
    ///////////////////////////////////////////////////////////////////////////

    fn parse_import_declaration(&mut self) -> Result<Declaration, ErrorsEmitted> {
        todo!()
    }

    fn parse_alias_declaration(&mut self) -> Result<Declaration, ErrorsEmitted> {
        todo!()
    }

    fn parse_const_declaration(&mut self) -> Result<Declaration, ErrorsEmitted> {
        todo!()
    }

    fn parse_static_var_declaration(&mut self) -> Result<Declaration, ErrorsEmitted> {
        todo!()
    }

    fn parse_module(&mut self) -> Result<Definition, ErrorsEmitted> {
        todo!()
    }

    fn parse_trait(&mut self) -> Result<Definition, ErrorsEmitted> {
        todo!()
    }

    fn parse_enum(&mut self) -> Result<Definition, ErrorsEmitted> {
        todo!()
    }

    fn parse_struct(&mut self) -> Result<Definition, ErrorsEmitted> {
        todo!()
    }

    fn parse_impl(&mut self) -> Result<Definition, ErrorsEmitted> {
        todo!()
    }

    fn parse_function(&mut self) -> Result<Definition, ErrorsEmitted> {
        todo!()
    }

    ///////////////////////////////////////////////////////////////////////////
    /// HELPERS
    ///////////////////////////////////////////////////////////////////////////

    /// Return an identifier with the appropriate error handling.
    fn consume_identifier(&mut self) -> Result<Identifier, ErrorsEmitted> {
        let token = self.peek_current().ok_or({
            self.log_error(ParserErrorKind::TokenNotFound {
                expected: "identifier".to_string(),
            })
        });

        match self.consume_token() {
            Ok(Token::Identifier { name, .. }) => Ok(Identifier(name)),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier".to_string(),
                    found: token.unwrap_or(Token::EOF),
                });

                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Advance the parser by one if the expected token matches the current one
    /// and return whether or not this is the case.
    fn tokens_match(&mut self, expected: Token) -> bool {
        if self.is_expected_token(expected.clone()) {
            self.current += 1;
            true
        } else {
            false
        }
    }

    /// Verify that the current token is the expected one.
    fn is_expected_token(&self, expected: Token) -> bool {
        if self.current < self.stream.tokens().len() {
            self.stream.tokens()[self.current] == expected
        } else {
            false
        }
    }

    /// Consume and check tokens, to ensure that the expected tokens are encountered
    /// during parsing. Return the relevant `ParserErrorKind` where applicable.
    fn expect_token(&mut self, expected: Token) -> Result<(), ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            _ if token == Ok(expected) => Ok(()),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`{:#?}`".to_string(),
                    found: token?,
                });

                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    fn get_type(&mut self) -> Result<Type, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Ok(Token::I32Type { .. } | Token::I64Type { .. } | Token::I128Type { .. }) => {
                Ok(Type::Int)
            }

            Ok(
                Token::U8Type { .. }
                | Token::U16Type { .. }
                | Token::U32Type { .. }
                | Token::U64Type { .. }
                | Token::U128Type { .. },
            ) => Ok(Type::UInt),
            Ok(Token::U256Type { .. }) => Ok(Type::BigUInt),
            Ok(Token::U512Type { .. }) => Ok(Type::BigUInt),

            Ok(Token::ByteType { .. }) => Ok(Type::Byte),

            Ok(
                Token::B2Type { .. }
                | Token::B3Type { .. }
                | Token::B4Type { .. }
                | Token::B5Type { .. }
                | Token::B6Type { .. }
                | Token::B7Type { .. }
                | Token::B8Type { .. }
                | Token::B9Type { .. }
                | Token::B10Type { .. }
                | Token::B11Type { .. }
                | Token::B12Type { .. }
                | Token::B13Type { .. }
                | Token::B14Type { .. }
                | Token::B15Type { .. }
                | Token::B16Type { .. }
                | Token::B17Type { .. }
                | Token::B18Type { .. }
                | Token::B19Type { .. }
                | Token::B20Type { .. }
                | Token::B21Type { .. }
                | Token::B22Type { .. }
                | Token::B23Type { .. }
                | Token::B24Type { .. }
                | Token::B25Type { .. }
                | Token::B26Type { .. }
                | Token::B27Type { .. }
                | Token::B28Type { .. }
                | Token::B29Type { .. }
                | Token::B30Type { .. }
                | Token::B31Type { .. }
                | Token::B32Type { .. },
            ) => Ok(Type::Bytes),
            Ok(Token::H160Type { .. }) => Ok(Type::Hash),
            Ok(Token::H256Type { .. }) => Ok(Type::Hash),
            Ok(Token::H512Type { .. }) => Ok(Type::Hash),
            Ok(Token::StringType { .. }) => Ok(Type::String),
            Ok(Token::CharType { .. }) => Ok(Type::Char),
            Ok(Token::BoolType { .. }) => Ok(Type::Bool),
            Ok(Token::CustomType { .. }) => Ok(Type::UserDefined),
            Ok(Token::SelfType { .. }) => Ok(Type::SelfType),
            Ok(Token::LParen { .. }) => Ok(Type::Tuple),
            Ok(Token::LBracket { .. }) => Ok(Type::Array),
            Ok(Token::Func { .. }) => Ok(Type::Function),
            Ok(Token::Ampersand { .. }) => Ok(Type::Reference),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "type annotation".to_string(),
                    found: token?,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Retrieve the respective precedence level for an operator.
    fn precedence(&self, token: &Token) -> Option<Precedence> {
        self.precedences.get(token).cloned()
    }

    /// Peek at the token at the current position.
    fn peek_current(&self) -> Option<Token> {
        self.stream.tokens().get(self.current).cloned()
    }

    /// Peek at the token `num_tokens` ahead of the token at the current position.
    fn peek_ahead_by(&self, num_tokens: usize) -> Option<Token> {
        self.stream.tokens().get(self.current + num_tokens).cloned()
    }

    /// Advance the parser and return the current token.
    fn consume_token(&mut self) -> Result<Token, ErrorsEmitted> {
        if let Some(t) = self.peek_current() {
            self.current += 1;
            Ok(t)
        } else {
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        }
    }

    /// Step the parser back by one.
    fn unconsume(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    /// Returns whether or not the position is the last index in the token stream.
    fn is_at_end(&self) -> bool {
        self.current >= self.stream.tokens().len()
    }

    /// Log information about an error that occurred during lexing.
    fn log_error(&mut self, error_kind: ParserErrorKind) {
        let error = CompilerError::new(error_kind, self.stream.span().substring(), self.current);

        self.errors.push(error);
    }

    pub fn errors(&self) -> &[CompilerError<ParserErrorKind>] {
        &self.errors
    }
}
