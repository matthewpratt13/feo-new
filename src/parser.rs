#![allow(dead_code)]
#![allow(unused_variables)]

mod expression;
mod item;
mod statement;

use std::collections::HashMap;

use crate::{
    ast::{
        expression::{
            CallExpr, FieldAccessExpr, GroupedExpr, IndexExpr, PathExpr, TupleExpr, TypeCastExpr,
        },
        BinaryOp, Declaration, Definition, Delimiter, Expression, Identifier, Keyword, Literal,
        Statement, StructField, Type, UnaryOp,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenStream},
};

use self::expression::ParseExpression;

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
            Token::AmpersandMut {
                punc: "&mut".to_string(),
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
            Token::DblDot {
                punc: "..".to_string(),
                span: self.stream.span(),
            },
            Precedence::Range,
        );
        self.precedences.insert(
            Token::DotDotEquals {
                punc: "..=".to_string(),
                span: self.stream.span(),
            },
            Precedence::Range,
        );
        self.precedences.insert(
            Token::QuestionMark {
                punc: '?',
                span: self.stream.span(),
            },
            Precedence::Unwrap,
        );
        self.precedences.insert(
            Token::FullStop {
                punc: '.',
                span: self.stream.span(),
            },
            Precedence::MethodCall,
        );
        self.precedences.insert(
            Token::FullStop {
                punc: '.',
                span: self.stream.span(),
            },
            Precedence::FieldAccess,
        );
        self.precedences.insert(
            Token::DblColon {
                punc: "::".to_string(),
                span: self.stream.span(),
            },
            Precedence::Path,
        );
        self.precedences.insert(
            Token::ColonColonAsterisk {
                punc: "::*".to_string(),
                span: self.stream.span(),
            },
            Precedence::Path,
        );
        self.precedences.insert(
            Token::LBracket {
                delim: '(',
                span: self.stream.span(),
            },
            Precedence::Call,
        );
        self.precedences.insert(
            Token::LBracket {
                delim: '[',
                span: self.stream.span(),
            },
            Precedence::Index,
        );
        self.precedences.insert(
            Token::Break {
                name: "break".to_string(),
                span: self.stream.span(),
            },
            Precedence::Lowest,
        );

        self.precedences.insert(
            Token::Continue {
                name: "continue".to_string(),
                span: self.stream.span(),
            },
            Precedence::Lowest,
        );

        self.precedences.insert(
            Token::Return {
                name: "return".to_string(),
                span: self.stream.span(),
            },
            Precedence::Lowest,
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
            Ok(Token::LParen { .. }) => {
                self.expect_token(Token::LParen {
                    delim: '(',
                    span: self.stream.span(),
                })?;

                let expr = self.parse_expression(Precedence::Lowest)?;

                self.expect_token(Token::RParen {
                    delim: ')',
                    span: self.stream.span(),
                })?;

                Ok(Expression::Grouped(GroupedExpr {
                    open_paren: Delimiter::LParen,
                    expr: Box::new(expr),
                    close_paren: Delimiter::RParen,
                }))
            }
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
        let token = self.peek_current().ok_or({
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

        match token {
            Token::IntLiteral { .. }
            | Token::UIntLiteral { .. }
            | Token::BigUIntLiteral { .. }
            | Token::HashLiteral { .. }
            | Token::ByteLiteral { .. }
            | Token::BytesLiteral { .. }
            | Token::StringLiteral { .. }
            | Token::CharLiteral { .. }
            | Token::BoolLiteral { .. }
            | Token::Identifier { .. }
            | Token::SelfKeyword { .. } => self.parse_primary(),
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
                if let Ok(Token::Comma { .. }) =
                    self.peek_ahead_by(2)
                        .ok_or(ParserErrorKind::TokenIndexOutOfBounds {
                            len: self.stream.tokens().len(),
                            i: self.current + 2,
                        })
                {
                    self.consume_token()?;
                    let expr = self.parse_expression(Precedence::Lowest)?;
                    TupleExpr::parse(self, expr)
                } else {
                    self.consume_token()?;
                    let expr = self.parse_expression(Precedence::Lowest)?;
                    GroupedExpr::parse(self, expr)
                }
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
            Token::Super { .. } | Token::Package { .. } => {
                let expr = self.parse_expression(Precedence::Path)?;
                PathExpr::parse(self, expr)
            }
            Token::Return { .. } => self.parse_return_expression(),

            Token::Break { .. } | Token::Continue { .. } => {
                self.parse_break_or_continue_expression()
            }
            Token::Underscore { .. } => self.parse_primary(),

            _ => {
                self.log_error(ParserErrorKind::InvalidToken { token });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Parse infix expressions (e.g., binary operators), where the respective token type
    /// appears in the middle of an expression.
    fn parse_infix(&mut self, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Ok(Token::Plus { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::Add)
            }
            Ok(Token::Minus { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::Subtract)
            }
            Ok(Token::Asterisk { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::Multiply)
            }
            Ok(Token::Slash { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::Divide)
            }
            Ok(Token::Percent { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::Modulus)
            }
            Ok(Token::DblEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::Equal)
            }
            Ok(Token::BangEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::NotEqual)
            }
            Ok(Token::LessThan { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::LessThan)
            }
            Ok(Token::LessThanEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::LessEqual)
            }
            Ok(Token::GreaterThan { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::GreaterThan)
            }
            Ok(Token::GreaterThanEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::GreaterEqual)
            }
            Ok(Token::Equals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::Assign)
            }
            Ok(Token::PlusEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::AddAssign)
            }
            Ok(Token::MinusEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::SubtractAssign)
            }
            Ok(Token::AsteriskEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::MultiplyAssign)
            }
            Ok(Token::SlashEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::DivideAssign)
            }
            Ok(Token::PercentEquals { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::ModulusAssign)
            }
            Ok(Token::DblAmpersand { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::LogicalAnd)
            }
            Ok(Token::DblPipe { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::LogicalOr)
            }
            Ok(Token::Ampersand { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::BitwiseAnd)
            }
            Ok(Token::Pipe { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::BitwiseOr)
            }
            Ok(Token::Caret { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::BitwiseXor)
            }
            Ok(Token::DblLessThan { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::ShiftLeft)
            }
            Ok(Token::DblGreaterThan { .. }) => {
                expression::parse_binary_op_expression(self, left_expr, BinaryOp::ShiftRight)
            }
            Ok(Token::QuestionMark { .. }) => self.parse_unwrap_expression(), // TODO: or ternary
            Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) => self.parse_range_expression(),
            Ok(Token::As { .. }) => TypeCastExpr::parse(self, left_expr),
            Ok(Token::LParen { .. }) => CallExpr::parse(self, left_expr), // TODO: or tuple struct
            Ok(Token::LBrace { .. }) => self.parse_struct_expression(), // TODO: or match statement
            Ok(Token::LBracket { .. }) => IndexExpr::parse(self, left_expr),
            Ok(Token::FullStop { .. }) => match self.unconsume() {
                Some(Token::Identifier { .. } | Token::SelfKeyword { .. }) => {
                    let token = self.peek_ahead_by(2).ok_or({
                        self.log_error(ParserErrorKind::TokenIndexOutOfBounds {
                            len: self.stream.tokens().len(),
                            i: self.current + 2,
                        })
                    });

                    // consume the `Identifier` or `SelfKeyword` (i.e., move back to the `.`)
                    self.consume_token()?;

                    match token {
                        Ok(Token::LParen { .. }) => {
                            let expr = self.parse_expression(Precedence::MethodCall)?;
                            self.parse_method_call_expression()
                        }
                        Ok(Token::UIntLiteral { .. }) => {
                            let expr = self.parse_expression(Precedence::Index)?;
                            self.parse_tuple_index_expression()
                        }
                        _ => {
                            let expr = self.parse_expression(Precedence::FieldAccess)?;
                            FieldAccessExpr::parse(self, expr)
                        }
                    }
                }
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
                PathExpr::parse(self, left_expr)
            }

            _ => {
                self.log_error(ParserErrorKind::InvalidToken { token: token? });
                Err(ErrorsEmitted(()))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////

    fn parse_method_call_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_tuple_index_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }

    fn parse_unwrap_expression(&mut self) -> Result<Expression, ErrorsEmitted> {
        todo!()
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
    fn expect_token(&mut self, expected: Token) -> Result<Token, ErrorsEmitted> {
        let token = self.consume_token()?;

        match token {
            _ if token == expected => Ok(token),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
                    found: token,
                });

                Err(ErrorsEmitted(()))
            }
        }
    }

    fn expect_keyword(&mut self, expected: Token) -> Result<Keyword, ErrorsEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::Import { .. } => Ok(Keyword::Import),
            Token::Module { .. } => Ok(Keyword::Module),
            Token::Package { .. } => Ok(Keyword::Package),
            Token::SelfKeyword { .. } => Ok(Keyword::KwSelf),
            Token::SelfType { .. } => Ok(Keyword::SelfType),
            Token::Super { .. } => Ok(Keyword::Super),
            Token::Pub { .. } => Ok(Keyword::Pub),
            Token::As { .. } => Ok(Keyword::As),
            Token::Const { .. } => Ok(Keyword::Const),
            Token::Static { .. } => Ok(Keyword::Static),
            Token::Func { .. } => Ok(Keyword::Func),
            Token::Struct { .. } => Ok(Keyword::Struct),
            Token::Enum { .. } => Ok(Keyword::Enum),
            Token::Trait { .. } => Ok(Keyword::Trait),
            Token::Impl { .. } => Ok(Keyword::Impl),
            Token::If { .. } => Ok(Keyword::If),
            Token::Else { .. } => Ok(Keyword::Else),
            Token::Match { .. } => Ok(Keyword::Match),
            Token::Loop { .. } => Ok(Keyword::Loop),
            Token::For { .. } => Ok(Keyword::For),
            Token::In { .. } => Ok(Keyword::In),
            Token::While { .. } => Ok(Keyword::While),
            Token::Break { .. } => Ok(Keyword::Break),
            Token::Continue { .. } => Ok(Keyword::Continue),
            Token::Return { .. } => Ok(Keyword::Return),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    fn get_type(&mut self) -> Result<Type, ErrorsEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::I32Type { .. } | Token::I64Type { .. } | Token::I128Type { .. } => Ok(Type::Int),
            Token::U8Type { .. }
            | Token::U16Type { .. }
            | Token::U32Type { .. }
            | Token::U64Type { .. }
            | Token::U128Type { .. } => Ok(Type::UInt),
            Token::U256Type { .. } => Ok(Type::BigUInt),
            Token::U512Type { .. } => Ok(Type::BigUInt),
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
            Token::H160Type { .. } | Token::H256Type { .. } | Token::H512Type { .. } => {
                Ok(Type::Hash)
            }
            Token::StringType { .. } => Ok(Type::String),
            Token::CharType { .. } => Ok(Type::Char),
            Token::BoolType { .. } => Ok(Type::Bool),
            Token::CustomType { .. } => Ok(Type::UserDefined),
            Token::SelfType { .. } => Ok(Type::SelfType),
            Token::LParen { .. } => Ok(Type::Tuple),
            Token::LBracket { .. } => Ok(Type::Array),
            Token::Func { .. } => Ok(Type::Function),
            Token::Ampersand { .. } => Ok(Type::Reference),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "type annotation".to_string(),
                    found: token,
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
    fn unconsume(&mut self) -> Option<Token> {
        if self.current > 0 {
            self.current -= 1;
        }
        self.peek_current()
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
