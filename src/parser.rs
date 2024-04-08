#![allow(dead_code)]

use std::collections::HashMap;

use crate::{
    ast::{
        BinaryOp, Declaration, Expression, Identifier, Item, Literal, Statement, StructField, Type,
        UnaryOp,
    },
    error::{CompilerError, ErrorEmitted, ParserErrorKind},
    token::{Token, TokenStream},
};

/// Enum representing the different precedence levels of operators, respectively.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Unwrap,             // `?`
    Assignment,         // `=`
    CompoundAssignment, // `+=`, `-=`, `*/`, `/=`, `%=`
    LogicalOr,          // `||`
    LogicalAnd,         // `&&`
    Equal,              // `==`
    NotEqual,           // `!=`
    LessThan,           // `<`
    LessThanOrEqual,    // `<=`
    GreaterThan,        // `>`
    GreaterThanOrEqual, // `>=`
    Shift,              // `«`, `»`
    BitwiseAnd,         // `&`
    BitwiseXor,         // `^`
    BitwiseOr,          // `|`
    Sum,                // `+`
    Difference,         // `-`
    Product,            // `*`
    Quotient,           // `/`
    Remainder,          // `%`
    Unary,              // `-`, `!`, `&`, `*`
    Exponentiation,     // `**`
    Cast,               // "as"
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
    fn parse(&mut self) -> Result<Vec<Statement>, ErrorEmitted> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Parse a `Statement`.
    fn parse_statement(&mut self) -> Result<Statement, ErrorEmitted> {
        match self.consume_token()? {
            Token::Let { .. } => self.parse_let_statement(),
            Token::Import { .. } => Ok(Statement::Declaration(self.parse_import_declaration()?)),
            Token::Alias { .. } => Ok(Statement::Declaration(self.parse_alias_declaration()?)),
            Token::Const { .. } => Ok(Statement::Declaration(self.parse_const_declaration()?)),
            Token::Static { .. } => {
                Ok(Statement::Declaration(self.parse_static_var_declaration()?))
            }
            Token::Module { .. } => Ok(Statement::Item(self.parse_module()?)),
            Token::Struct { .. } => Ok(Statement::Item(self.parse_struct()?)),
            Token::Enum { .. } => Ok(Statement::Item(self.parse_enum()?)),
            Token::Trait { .. } => Ok(Statement::Item(self.parse_trait()?)),
            Token::Impl { .. } => Ok(Statement::Item(self.parse_impl()?)),
            Token::Func { .. } => Ok(Statement::Item(self.parse_function()?)),

            _ => {
                self.unconsume();
                self.parse_expression_statement()
            }
        }
    }

    /// Parse an identifier and the assignment operation that binds the variable to a value.
    fn parse_let_statement(&mut self) -> Result<Statement, ErrorEmitted> {
        let identifier = self.consume_identifier()?;

        self.expect_token(Token::Equals {
            punc: '=',
            span: self.stream.span(),
        })?;

        let value = self.parse_expression(Precedence::Assignment)?;

        self.expect_token(Token::Semicolon {
            punc: ';',
            span: self.stream.span(),
        })?;

        Ok(Statement::Let(identifier, value))
    }

    /// Parse an expression into a statement, separated by a semicolon.
    fn parse_expression_statement(&mut self) -> Result<Statement, ErrorEmitted> {
        let expression = self.parse_expression(Precedence::Lowest)?;

        self.expect_token(Token::Semicolon {
            punc: ';',
            span: self.stream.span(),
        })?;

        Ok(Statement::Expression(expression))
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Recursively parse an expression based on operator precedence.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ErrorEmitted> {
        let mut left_expr = self.parse_prefix()?;

        let current_precedence = &self.precedence(&self.peek_current().expect("token not found"));

        while let Some(cp) = current_precedence {
            if precedence < *cp {
                left_expr = self.parse_infix(left_expr)?;
            } else {
                break;
            }
        }

        Ok(left_expr)
    }

    /// Parse primary expressions (e.g., grouped expressions, identifiers and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::Identifier { name, .. }
            | Token::SelfKeyword { name, .. }
            | Token::Underscore { name, .. } => {
                Ok(Expression::Identifier(Identifier(name.to_string())))
            }
            Token::IntLiteral { value, .. } => Ok(Expression::Literal(Literal::Int(value))),
            Token::UIntLiteral { value, .. } => Ok(Expression::Literal(Literal::UInt(value))),
            Token::U256Literal { value, .. } => Ok(Expression::Literal(Literal::U256(value))),
            Token::H256Literal { value, .. } => Ok(Expression::Literal(Literal::H256(value))),
            Token::ByteLiteral { value, .. } => Ok(Expression::Literal(Literal::Byte(value))),
            Token::BytesLiteral { value, .. } => Ok(Expression::Literal(Literal::Bytes(value))),
            Token::H160Literal { value, .. } => Ok(Expression::Literal(Literal::H160(value))),
            Token::StringLiteral { value, .. } => Ok(Expression::Literal(Literal::String(value))),
            Token::CharLiteral { value, .. } => Ok(Expression::Literal(Literal::Char(value))),
            Token::BoolLiteral { value, .. } => Ok(Expression::Literal(Literal::Bool(value))),
            Token::LParen { .. } => self.parse_grouped_expression(),
            _ => Err(self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier, `Self`, `_`, literal or `(`".to_string(),
                found: token,
            })),
        }
    }

    /// Parse prefix expressions (e.g., integer literals, identifiers and parentheses),
    /// where the respective token type appears at the beginning of an expression.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::IntLiteral { .. } => self.parse_primary(),
            Token::UIntLiteral { .. } => self.parse_primary(),
            Token::U256Literal { .. } => self.parse_primary(),
            Token::H160Literal { .. } => self.parse_primary(),
            Token::H256Literal { .. } => self.parse_primary(),
            Token::ByteLiteral { .. } => self.parse_primary(),
            Token::BytesLiteral { .. } => self.parse_primary(),
            Token::StringLiteral { .. } => self.parse_primary(),
            Token::CharLiteral { .. } => self.parse_primary(),
            Token::BoolLiteral { .. } => self.parse_primary(),
            Token::Identifier { .. } => self.parse_primary(),
            Token::SelfKeyword { .. } => match self.peek_current() {
                Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) => {
                    self.parse_path_expression()
                }

                _ => self.parse_primary(),
            },
            Token::Minus { .. } => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Negate, Box::new(expr)))
            }
            Token::Bang { .. } => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Not, Box::new(expr)))
            }
            Token::Ampersand { .. } => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Reference, Box::new(expr)))
            }
            Token::Asterisk { .. } => {
                let expr = self.parse_expression(Precedence::Unary)?;
                Ok(Expression::UnaryOp(UnaryOp::Dereference, Box::new(expr)))
            }
            Token::LParen { .. } => {
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
            Token::LBracket { .. } => self.parse_array_expression(),
            Token::Pipe { .. } | Token::DblPipe { .. } => {
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

            Token::DblDot { .. } | Token::DotDotEquals { .. } => {
                let expr = self.parse_range_expression()?;

                match self.peek_current() {
                    Some(
                        Token::Identifier { .. }
                        | Token::UIntLiteral { .. }
                        | Token::IntLiteral { .. }
                        | Token::U256Literal { .. },
                    ) => Ok(expr),
                    Some(t) => Err(self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or number".to_string(),
                        found: t,
                    })),
                    None => Err(self.log_error(ParserErrorKind::UnexpectedEndOfInput)),
                }
            }

            Token::Return { .. } => self.parse_return_expression(),

            Token::Match { .. } => self.parse_match_expression(),

            Token::Super { .. } | Token::Package { .. } => self.parse_path_expression(),

            Token::Break { .. } | Token::Continue { .. } => {
                self.parse_break_or_continue_expression()
            }

            Token::Underscore { .. } => self.parse_primary(),

            _ => Err(self.log_error(ParserErrorKind::InvalidToken { token })),
        }
    }

    /// Parse infix expressions (e.g., binary operators), where the respective token type
    /// appears in the middle of an expression.
    fn parse_infix(&mut self, left_expr: Expression) -> Result<Expression, ErrorEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::Plus { .. } => self.parse_binary_expression(left_expr, BinaryOp::Add),
            Token::Minus { .. } => self.parse_binary_expression(left_expr, BinaryOp::Subtract),
            Token::Asterisk { .. } => self.parse_binary_expression(left_expr, BinaryOp::Multiply),
            Token::Slash { .. } => self.parse_binary_expression(left_expr, BinaryOp::Divide),
            Token::Percent { .. } => self.parse_binary_expression(left_expr, BinaryOp::Modulus),
            Token::DblEquals { .. } => self.parse_binary_expression(left_expr, BinaryOp::Equal),
            Token::BangEquals { .. } => self.parse_binary_expression(left_expr, BinaryOp::NotEqual),
            Token::LessThan { .. } => self.parse_binary_expression(left_expr, BinaryOp::LessThan),
            Token::LessThanEquals { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::LessEqual)
            }
            Token::GreaterThan { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::GreaterThan)
            }
            Token::GreaterThanEquals { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::GreaterEqual)
            }
            Token::Equals { .. } => self.parse_binary_expression(left_expr, BinaryOp::Assign),
            Token::PlusEquals { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::AddAssign)
            }
            Token::MinusEquals { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::SubtractAssign)
            }
            Token::AsteriskEquals { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::MultiplyAssign)
            }
            Token::SlashEquals { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::DivideAssign)
            }
            Token::PercentEquals { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::ModulusAssign)
            }
            Token::DblAmpersand { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::LogicalAnd)
            }
            Token::DblPipe { .. } => self.parse_binary_expression(left_expr, BinaryOp::LogicalOr),
            Token::Ampersand { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::BitwiseAnd)
            }
            Token::Pipe { .. } => self.parse_binary_expression(left_expr, BinaryOp::BitwiseOr),
            Token::Caret { .. } => self.parse_binary_expression(left_expr, BinaryOp::BitwiseXor),
            Token::DblLessThan { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::ShiftLeft)
            }
            Token::DblGreaterThan { .. } => {
                self.parse_binary_expression(left_expr, BinaryOp::ShiftRight)
            }
            Token::QuestionMark { .. } => self.parse_unwrap_expression(),
            Token::DblDot { .. } | Token::DotDotEquals { .. } => self.parse_range_expression(),
            Token::As { .. } => self.parse_cast_expression(),
            Token::LParen { .. } => self.parse_call_expression(left_expr), // TODO: or tuple struct
            Token::LBrace { .. } => self.parse_struct_expression(),
            Token::LBracket { .. } => self.parse_index_expression(left_expr), // TODO: check
            Token::FullStop { .. } => match self.peek_current() {
                Some(Token::Identifier { .. } | Token::SelfKeyword { .. }) => {
                    let token = self.peek_ahead_by(1).ok_or(self.log_error(
                        ParserErrorKind::TokenIndexOutOfBounds {
                            len: self.stream.tokens().len(),
                            i: self.current + 1,
                        },
                    ))?;

                    match token {
                        Token::LParen { .. } => self.parse_method_call_expression(),
                        _ => self.parse_field_access_expression(left_expr),
                    }
                }
                Some(Token::UIntLiteral { .. }) => self.parse_tuple_index_expression(),
                Some(_) => Err(self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier or tuple index".to_string(),
                    found: token,
                })),
                None => Err(self.log_error(ParserErrorKind::UnexpectedEndOfInput)),
            },

            Token::DblColon { .. } | Token::ColonColonAsterisk { .. } => {
                self.parse_path_expression()
            }

            _ => Err(self.log_error(ParserErrorKind::InvalidToken { token })),
        }
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Parse a grouped (parenthesized) expression.
    fn parse_grouped_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        self.expect_token(Token::LParen {
            delim: '(',
            span: self.stream.span(),
        })?;

        self.consume_token()?; // consume `(``

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_token(Token::RParen {
            delim: ')',
            span: self.stream.span(),
        })?;

        Ok(expr)
    }

    /// Parse a binary expressions (e.g., arithmetic, logical and comparison expressions).
    /// This method parses the operator and calls `parse_expression()` recursively to handle
    /// the right-hand side of the expression.
    fn parse_binary_expression(
        &mut self,
        left_expr: Expression,
        op: BinaryOp,
    ) -> Result<Expression, ErrorEmitted> {
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

    /// Parse a function call with arguments.
    fn parse_call_expression(&mut self, callee: Expression) -> Result<Expression, ErrorEmitted> {
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

            let arg_expr = self.parse_expression(Precedence::Lowest)?;
            args.push(arg_expr);

            // error handling
            let token = self.consume_token()?;

            match token {
                Token::Comma { .. } => continue, // more arguments
                Token::RParen { .. } => break,   // end of arguments
                _ => {
                    return Err(self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: token,
                    }))
                }
            }
        }

        Ok(Expression::Call(Box::new(callee), args))
    }

    /// Parse an index expression (i.e., `array[index]`).
    fn parse_index_expression(
        &mut self,
        array_expr: Expression,
    ) -> Result<Expression, ErrorEmitted> {
        self.expect_token(Token::LBracket {
            delim: '[',
            span: self.stream.span(),
        })?;

        let index_expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_token(Token::RBracket {
            delim: ']',
            span: self.stream.span(),
        })?;

        Ok(Expression::Index(
            Box::new(array_expr),
            Box::new(index_expr),
        ))
    }

    /// Parse a field access expression (i.e., `object.field`).
    fn parse_field_access_expression(
        &mut self,
        object_expr: Expression,
    ) -> Result<Expression, ErrorEmitted> {
        self.expect_token(Token::FullStop {
            punc: '.',
            span: self.stream.span(),
        })?;

        let token = self.consume_token()?;

        if let Token::Identifier { name, .. } = token {
            Ok(Expression::FieldAccess(
                Box::new(object_expr),
                Identifier(name),
            ))
        } else {
            Err(self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: token,
            }))
        }
    }

    // TODO: what about `else-if` branches ?
    /// Parse an `if` expression (i.e., `if (condition) { true block } else { false block }`).
    fn parse_if_expression(&mut self) -> Result<Expression, ErrorEmitted> {
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

        Ok(Expression::If(condition, true_branch, false_branch))
    }

    /// Parse a `for-in` expression (i.e., `for var in iterable { execution logic }`).
    fn parse_for_in_expression(&mut self) -> Result<Expression, ErrorEmitted> {
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

        Ok(Expression::ForIn(variable, iterable, body))
    }

    /// Parse a block expression (i.e., `{ expr1; expr2; ... }`).
    fn parse_block_expression(&mut self) -> Result<Expression, ErrorEmitted> {
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
    fn parse_array_expression(&mut self) -> Result<Expression, ErrorEmitted> {
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
    fn parse_tuple_expression(&mut self) -> Result<Expression, ErrorEmitted> {
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

    /// Parse a type cast expression.
    fn parse_cast_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        let expr = self.parse_expression(Precedence::Cast)?;

        let token = self.consume_token()?;

        if token
            != (Token::As {
                name: "as".to_string(),
                span: self.stream.span(),
            })
        {
            return Err(self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`as`".to_string(),
                found: token,
            }));
        }

        Ok(Expression::Cast(Box::new(expr), self.get_type()?))
    }

    /// Parse a struct with fields.
    fn parse_struct_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        let mut fields: Vec<StructField> = Vec::new(); // stores struct fields

        self.expect_token(Token::LBrace {
            delim: '{',
            span: self.stream.span(),
        })?;

        // parse struct fields – separated by commas – until a closing brace
        loop {
            // get the field name
            let token = self.consume_token()?;

            let field_name = match token {
                Token::Identifier { name, .. } => name,
                Token::RBrace { .. } => break, // end of struct
                _ => {
                    return Err(self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or `)`".to_string(),
                        found: token,
                    }))?
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
            let token = self.consume_token()?;

            match token {
                Token::Comma { .. } => continue, // more fields
                Token::RBrace { .. } => break,   // end of struct
                _ => {
                    return Err(self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `}`".to_string(),
                        found: token,
                    }))
                }
            }
        }

        Ok(Expression::Struct(fields))
    }

    fn parse_closure_with_block(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_closure_without_block(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_tuple_index_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_method_call_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_path_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_unwrap_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_range_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_return_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_match_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_break_or_continue_expression(&mut self) -> Result<Expression, ErrorEmitted> {
        todo!()
    }

    fn parse_import_declaration(&mut self) -> Result<Declaration, ErrorEmitted> {
        todo!()
    }

    fn parse_alias_declaration(&mut self) -> Result<Declaration, ErrorEmitted> {
        todo!()
    }

    fn parse_const_declaration(&mut self) -> Result<Declaration, ErrorEmitted> {
        todo!()
    }

    fn parse_static_var_declaration(&mut self) -> Result<Declaration, ErrorEmitted> {
        todo!()
    }

    fn parse_module(&mut self) -> Result<Item, ErrorEmitted> {
        todo!()
    }

    fn parse_struct(&mut self) -> Result<Item, ErrorEmitted> {
        todo!()
    }

    fn parse_enum(&mut self) -> Result<Item, ErrorEmitted> {
        todo!()
    }

    fn parse_trait(&mut self) -> Result<Item, ErrorEmitted> {
        todo!()
    }

    fn parse_impl(&mut self) -> Result<Item, ErrorEmitted> {
        todo!()
    }

    fn parse_function(&mut self) -> Result<Item, ErrorEmitted> {
        todo!()
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Return an identifier with the appropriate error handling.
    fn consume_identifier(&mut self) -> Result<Identifier, ErrorEmitted> {
        let token = self
            .peek_current()
            .ok_or(self.log_error(ParserErrorKind::TokenNotFound {
                expected: "identifier".to_string(),
            }))?;

        match self.consume_token()? {
            Token::Identifier { name, .. } => Ok(Identifier(name)),
            _ => Err(self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            })),
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
    fn expect_token(&mut self, expected: Token) -> Result<(), ErrorEmitted> {
        let token = self.consume_token()?;

        match token {
            _ if token == expected => Ok(()),
            _ => Err(self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`{:#?}`".to_string(),
                found: token,
            })),
        }
    }

    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    fn get_type(&mut self) -> Result<Type, ErrorEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::I32Type { .. } | Token::I64Type { .. } | Token::I128Type { .. } => Ok(Type::Int),

            Token::U8Type { .. }
            | Token::U16Type { .. }
            | Token::U32Type { .. }
            | Token::U64Type { .. }
            | Token::U128Type { .. } => Ok(Type::UInt),
            Token::U256Type { .. } => Ok(Type::U256),
            Token::H160Type { .. } => Ok(Type::H160),
            Token::H256Type { .. } => Ok(Type::H256),
            Token::ByteType { .. } => Ok(Type::Byte),

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
            | Token::B32Type { .. } => Ok(Type::Bytes),
            Token::StringType { .. } => Ok(Type::String),
            Token::CharType { .. } => Ok(Type::Char),
            Token::BoolType { .. } => Ok(Type::Bool),
            Token::CustomType { .. } => Ok(Type::UserDefined),
            Token::SelfType { .. } => Ok(Type::SelfType),
            Token::LParen { .. } => Ok(Type::Tuple),
            Token::LBracket { .. } => Ok(Type::Array),
            Token::Func { .. } => Ok(Type::Function),
            Token::Ampersand { .. } => Ok(Type::Reference),
            _ => Err(self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "type annotation".to_string(),
                found: token,
            })),
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
    fn consume_token(&mut self) -> Result<Token, ErrorEmitted> {
        if let Some(t) = self.peek_current() {
            self.current += 1;
            Ok(t)
        } else {
            Err(self.log_error(ParserErrorKind::UnexpectedEndOfInput))
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

    /// Log and store information about an error that occurred during lexing.
    /// Return `ErrorEmitted` just to confirm that the action happened.
    fn log_error(&mut self, error_kind: ParserErrorKind) -> ErrorEmitted {
        let error = CompilerError::new(error_kind, self.stream.span().substring(), self.current);

        self.errors.push(error);
        ErrorEmitted(())
    }
}
