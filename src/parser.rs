#![allow(dead_code)]
#![allow(unused_variables)]

mod binary_expr;
mod expression;
mod expression_collection;
mod item;
mod precedence;
mod statement;

use std::collections::HashMap;

use crate::{
    ast::{
        expression::{
            ArrayExpr, BreakExpr, CallExpr, ClosureExpr, ContinueExpr, ExpressionStmt,
            FieldAccessExpr, ForInStmt, GroupedExpr, IfStmt, IndexExpr, LetStmt, MatchStmt,
            MethodCallExpr, PathExpr, RangeExpr, ReturnExpr, StructExpr, TupleExpr, TupleIndexExpr,
            TypeCastExpr, UnderscoreExpr, UnwrapExpr, WhileStmt,
        },
        BinaryOp, Declaration, Definition, Delimiter, Expression, Identifier, Keyword, Literal,
        Separator, Statement, Type, UnaryOp,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenStream},
};

pub use self::precedence::Precedence;
use self::{
    binary_expr::parse_binary_expression, expression::ParseExpression,
    expression_collection::ParseExpressionCollection, statement::ParseStatement,
};

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
            Token::Dot {
                punc: '.',
                span: self.stream.span(),
            },
            Precedence::MethodCall,
        );
        self.precedences.insert(
            Token::Dot {
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
                let open_paren = self.expect_delimiter(Token::LParen {
                    delim: '(',
                    span: self.stream.span(),
                })?;

                let expr = self.parse_expression(Precedence::Lowest)?;

                let close_paren = self.expect_delimiter(Token::RParen {
                    delim: ')',
                    span: self.stream.span(),
                })?;

                Ok(Expression::Grouped(GroupedExpr {
                    open_paren,
                    expression: Box::new(expr),
                    close_paren,
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
            | Token::BoolLiteral { .. } => self.parse_primary(),

            Token::Identifier { name, .. } => {
                if &name == "_" {
                    Ok(Expression::Underscore(UnderscoreExpr {
                        underscore: Separator::Underscore,
                    }))
                } else {
                    self.parse_expression(Precedence::Path)
                }
            }

            Token::SelfKeyword { .. }
            | Token::SelfType { .. }
            | Token::Package { .. }
            | Token::Super { .. } => {
                let expr = self.parse_expression(Precedence::Path)?;
                Ok(Expression::Path(PathExpr::parse(self, expr)?))
            }

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
                    Ok(Expression::Tuple(TupleExpr::parse(self)?))
                } else {
                    Ok(Expression::Grouped(GroupedExpr::parse(self)?))
                }
            }
            Token::LBracket { .. } => Ok(Expression::Array(ArrayExpr::parse(self)?)),
            Token::Pipe { .. } | Token::DblPipe { .. } => {
                if let Some(Token::Identifier { .. }) = self.peek_ahead_by(1) {
                    self.consume_token()?;
                    let expr = self.parse_expression(Precedence::Lowest)?;
                    Ok(Expression::Closure(ClosureExpr::parse(self, expr)?))
                } else if let Ok(e) = self.parse_expression(Precedence::Lowest) {
                    Ok(Expression::Closure(ClosureExpr::parse(self, e)?))
                } else {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "closure".to_string(),
                        found: token,
                    });
                    Err(ErrorsEmitted(()))
                }
            }
            Token::DblDot { .. } | Token::DotDotEquals { .. } => {
                let expr = self.parse_expression(Precedence::Range)?;
                Ok(Expression::Range(RangeExpr::parse(self, expr)?))
                // match self.peek_current() {
                //     Some(
                //         Token::Identifier { .. }
                //         | Token::IntLiteral { .. }
                //         | Token::UIntLiteral { .. }
                //         | Token::BigUIntLiteral { .. },
                //     ) => Ok(expr),
                //     Some(t) => {
                //         self.log_error(ParserErrorKind::UnexpectedToken {
                //             expected: "identifier or number".to_string(),
                //             found: t,
                //         });

                //         Err(ErrorsEmitted(()))
                //     }
                //     None => {
                //         self.log_error(ParserErrorKind::UnexpectedEndOfInput);
                //         Err(ErrorsEmitted(()))
                //     }
                // }
            }
            Token::Return { .. } => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                Ok(Expression::Return(ReturnExpr::parse(self, expr)?))
            }

            Token::Break { name, span } => {
                let kw_break = self.expect_keyword(Token::Break { name, span })?;
                Ok(Expression::Break(BreakExpr { kw_break }))
            }

            Token::Continue { name, span } => {
                let kw_continue = self.expect_keyword(Token::Continue { name, span })?;
                Ok(Expression::Continue(ContinueExpr { kw_continue }))
            }

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
            Ok(Token::Plus { .. }) => parse_binary_expression(self, left_expr, BinaryOp::Add),
            Ok(Token::Minus { .. }) => parse_binary_expression(self, left_expr, BinaryOp::Subtract),
            Ok(Token::Asterisk { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::Multiply)
            }
            Ok(Token::Slash { .. }) => parse_binary_expression(self, left_expr, BinaryOp::Divide),
            Ok(Token::Percent { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::Modulus)
            }
            Ok(Token::DblEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::Equal)
            }
            Ok(Token::BangEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::NotEqual)
            }
            Ok(Token::LessThan { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::LessThan)
            }
            Ok(Token::LessThanEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::LessEqual)
            }
            Ok(Token::GreaterThan { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::GreaterThan)
            }
            Ok(Token::GreaterThanEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::GreaterEqual)
            }
            Ok(Token::Equals { .. }) => parse_binary_expression(self, left_expr, BinaryOp::Assign),
            Ok(Token::PlusEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::AddAssign)
            }
            Ok(Token::MinusEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::SubtractAssign)
            }
            Ok(Token::AsteriskEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::MultiplyAssign)
            }
            Ok(Token::SlashEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::DivideAssign)
            }
            Ok(Token::PercentEquals { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::ModulusAssign)
            }
            Ok(Token::DblAmpersand { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::LogicalAnd)
            }
            Ok(Token::DblPipe { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::LogicalOr)
            }
            Ok(Token::Ampersand { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::BitwiseAnd)
            }
            Ok(Token::Pipe { .. }) => parse_binary_expression(self, left_expr, BinaryOp::BitwiseOr),
            Ok(Token::Caret { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::BitwiseXor)
            }
            Ok(Token::DblLessThan { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::ShiftLeft)
            }
            Ok(Token::DblGreaterThan { .. }) => {
                parse_binary_expression(self, left_expr, BinaryOp::ShiftRight)
            }
            Ok(Token::QuestionMark { .. }) => {
                Ok(Expression::Unwrap(UnwrapExpr::parse(self, left_expr)?))
            }
            Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) => {
                Ok(Expression::Range(RangeExpr::parse(self, left_expr)?))
            }
            Ok(Token::As { .. }) => Ok(Expression::TypeCast(TypeCastExpr::parse(self, left_expr)?)),
            Ok(Token::LParen { .. }) => {
                // TODO: use symbol table to check whether this could be a `TupleStructExpr`
                Ok(Expression::Call(CallExpr::parse(self, left_expr)?))
            }
            Ok(Token::LBrace { .. }) => Ok(Expression::Struct(StructExpr::parse(self)?)),
            Ok(Token::LBracket { .. }) => Ok(Expression::Index(IndexExpr::parse(self, left_expr)?)),
            Ok(Token::Dot { .. }) => match self.unconsume() {
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
                            Ok(Expression::MethodCall(MethodCallExpr::parse(self, expr)?))
                        }
                        Ok(Token::UIntLiteral { .. }) => {
                            let expr = self.parse_expression(Precedence::Index)?;
                            Ok(Expression::TupleIndex(TupleIndexExpr::parse(self, expr)?))
                        }
                        _ => {
                            let expr = self.parse_expression(Precedence::FieldAccess)?;
                            Ok(Expression::FieldAccess(FieldAccessExpr::parse(self, expr)?))
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
                Ok(Expression::Path(PathExpr::parse(self, left_expr)?))
            }

            _ => {
                self.log_error(ParserErrorKind::InvalidToken { token: token? });
                Err(ErrorsEmitted(()))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// STATEMENTS
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a `Statement`.
    fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        match self.consume_token() {
            Ok(Token::Let { .. }) => Ok(Statement::Let(LetStmt::parse(self)?)),
            Ok(Token::If { .. }) => Ok(Statement::If(IfStmt::parse(self)?)),
            Ok(Token::Match { .. }) => Ok(Statement::Match(MatchStmt::parse(self)?)),
            Ok(Token::For { .. }) => Ok(Statement::ForIn(ForInStmt::parse(self)?)),
            Ok(Token::While { .. }) => Ok(Statement::While(WhileStmt::parse(self)?)),
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
                Ok(Statement::Expression(ExpressionStmt::parse(self)?))
            }
        }
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

    fn expect_delimiter(&mut self, expected: Token) -> Result<Delimiter, ErrorsEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::LParen { .. } => Ok(Delimiter::LParen),
            Token::RParen { .. } => Ok(Delimiter::RParen),
            Token::LBracket { .. } => Ok(Delimiter::LBracket),
            Token::RBracket { .. } => Ok(Delimiter::RBracket),
            Token::LBrace { .. } => Ok(Delimiter::LBrace),
            Token::RBrace { .. } => Ok(Delimiter::RBrace),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    fn expect_binary_op(&mut self, expected: Token) -> Result<BinaryOp, ErrorsEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::Equals { .. } => Ok(BinaryOp::Assign),

            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    fn expect_separator(&mut self, expected: Token) -> Result<Separator, ErrorsEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::Colon { .. } => Ok(Separator::Colon),
            Token::Semicolon { .. } => Ok(Separator::Semicolon),
            Token::Comma { .. } => Ok(Separator::Comma),
            Token::Dot { .. } => Ok(Separator::Dot),
            Token::DblColon { .. } => Ok(Separator::DblColon),
            Token::ColonColonAsterisk { .. } => Ok(Separator::ColonColonAsterisk),
            Token::ThinArrow { .. } => Ok(Separator::ThinArrow),
            Token::FatArrow { .. } => Ok(Separator::FatArrow),

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
