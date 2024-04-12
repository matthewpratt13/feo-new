#![allow(dead_code)]
#![allow(unused_variables)]

mod binary_expr;
mod compound_expr;
mod expression;
mod item;
mod precedence;
mod statement;
mod unary_expr;

use std::collections::HashMap;

use crate::{
    ast::{
        AliasDecl, ArrayExpr, BinaryOp, BlockExpr, BreakExpr, CallExpr, ClosureExpr, ConstantDecl,
        ContinueExpr, Declaration, Definition, Delimiter, EnumDef, Expression, ExpressionStmt,
        FieldAccessExpr, ForInStmt, FunctionDef, GroupedExpr, Identifier, IfStmt, ImportDecl,
        IndexExpr, InherentImplDef, Keyword, LetStmt, Literal, MatchStmt, MethodCallExpr,
        ModuleDef, PathExpr, PathPrefix, RangeExpr, RangeOp, ReturnExpr, Separator, Statement,
        StaticItemDecl, StructDef, StructExpr, TraitDef, TraitImplDef, TupleExpr, TupleIndexExpr,
        Type, TypeCastExpr, UnaryOp, UnderscoreExpr, UnwrapExpr, WhileStmt,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenStream},
};

pub use self::precedence::Precedence;
use self::{
    binary_expr::parse_binary_expr,
    compound_expr::ParseCompoundExpr,
    expression::ParseExpression,
    item::{ParseDeclaration, ParseDefinition},
    statement::ParseStatement,
    unary_expr::parse_unary_expr,
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
            Precedence::TypeCast,
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
        while self.current < self.stream.tokens().len() {
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

        let curr_token = self.peek_current().ok_or({
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

        while let Some(curr_precedence) = self.precedence(&curr_token) {
            if precedence < curr_precedence {
                left_expr = self.parse_infix(left_expr)?;
            } else {
                break;
            }
        }

        Ok(left_expr)
    }

    /// Parse primary expressions (e.g., grouped expressions, identifiers and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        let token = self.consume_token()?;

        match token {
            Token::Identifier { name, .. } => Ok(Expression::Path(PathExpr {
                root: PathPrefix::Identifier(Identifier(name)),
                tree_opt: None,
            })),
            Token::IntLiteral { value, .. } => Ok(Expression::Literal(Literal::Int(value))),
            Token::UIntLiteral { value, .. } => Ok(Expression::Literal(Literal::UInt(value))),
            Token::BigUIntLiteral { value, .. } => Ok(Expression::Literal(Literal::BigUInt(value))),
            Token::ByteLiteral { value, .. } => Ok(Expression::Literal(Literal::Byte(value))),
            Token::BytesLiteral { value, .. } => Ok(Expression::Literal(Literal::Bytes(value))),
            Token::HashLiteral { value, .. } => Ok(Expression::Literal(Literal::Hash(value))),
            Token::StringLiteral { value, .. } => Ok(Expression::Literal(Literal::String(value))),
            Token::CharLiteral { value, .. } => Ok(Expression::Literal(Literal::Char(value))),
            Token::BoolLiteral { value, .. } => Ok(Expression::Literal(Literal::Bool(value))),
            Token::LParen { .. } => Ok(Expression::Grouped(GroupedExpr::parse(self)?)),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier, `_`, literal or `(`".to_string(),
                    found: token,
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
            | Token::HashLiteral { .. } => {
                if let Ok(Token::As { .. }) = self.peek_ahead_by(1) {
                    let expr = self.parse_expression(Precedence::TypeCast)?;
                    Ok(Expression::TypeCast(TypeCastExpr::parse(self, expr)?))
                } else if let Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    let expr = self.parse_expression(Precedence::Range)?;
                    Ok(Expression::Range(RangeExpr::parse(self, expr)?))
                } else {
                    self.parse_primary()
                }
            }

            Token::ByteLiteral { .. }
            | Token::BytesLiteral { .. }
            | Token::StringLiteral { .. }
            | Token::CharLiteral { .. }
            | Token::BoolLiteral { .. } => self.parse_primary(),

            Token::Identifier { name, .. } => {
                if &name == "_" {
                    Ok(Expression::Underscore(UnderscoreExpr {
                        underscore: Separator::Underscore,
                    }))
                } else if let Ok(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_ahead_by(1)
                {
                    let expr = self.parse_expression(Precedence::Path)?;
                    Ok(Expression::Path(PathExpr::parse(self, expr)?))
                } else if let Ok(Token::Dot { .. }) = self.peek_ahead_by(1) {
                    match self.peek_ahead_by(2) {
                        Ok(Token::LParen { .. }) => {
                            let expr = self.parse_expression(Precedence::MethodCall)?;
                            Ok(Expression::MethodCall(MethodCallExpr::parse(self, expr)?))
                        }
                        Ok(Token::UIntLiteral { .. }) => {
                            let expr = self.parse_expression(Precedence::FieldAccess)?;
                            Ok(Expression::TupleIndex(TupleIndexExpr::parse(self, expr)?))
                        }
                        _ => {
                            let expr = self.parse_expression(Precedence::FieldAccess)?;
                            Ok(Expression::FieldAccess(FieldAccessExpr::parse(self, expr)?))
                        }
                    }
                } else if let Ok(Token::LParen { .. }) = self.peek_ahead_by(1) {
                    // TODO: use symbol table to check whether this could be a `TupleStructExpr`
                    let expr = self.parse_expression(Precedence::Call)?;
                    Ok(Expression::Call(CallExpr::parse(self, expr)?))
                } else if let Ok(Token::LBracket { .. }) = self.peek_ahead_by(1) {
                    let expr = self.parse_expression(Precedence::Index)?;
                    Ok(Expression::Index(IndexExpr::parse(self, expr)?))
                } else if let Ok(Token::QuestionMark { .. }) = self.peek_ahead_by(1) {
                    let expr = self.parse_expression(Precedence::Unwrap)?;
                    Ok(Expression::Unwrap(UnwrapExpr::parse(self, expr)?))
                } else if let Ok(Token::As { .. }) = self.peek_ahead_by(1) {
                    let expr = self.parse_expression(Precedence::TypeCast)?;
                    Ok(Expression::TypeCast(TypeCastExpr::parse(self, expr)?))
                } else if let Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    let expr = self.parse_expression(Precedence::Range)?;
                    Ok(Expression::Range(RangeExpr::parse(self, expr)?))
                } else if let Ok(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    let expr = self.parse_expression(Precedence::Lowest)?;
                    match expr {
                        Expression::Path(_) => {
                            Ok(Expression::Struct(StructExpr::parse(self, expr)?))
                        }
                        _ => {
                            let token = self.peek_current().ok_or({
                                self.log_error(ParserErrorKind::UnexpectedEndOfInput);
                                ErrorsEmitted(())
                            })?;
                            self.log_error(ParserErrorKind::UnexpectedToken {
                                expected: "path expression".to_string(),
                                found: token,
                            });
                            Err(ErrorsEmitted(()))
                        }
                    }
                } else {
                    self.parse_primary()
                }
            }

            Token::SelfType { .. } => {
                if let Ok(Token::DblColon { .. }) = self.peek_ahead_by(1) {
                    if let Ok(Token::Identifier { .. }) = self.peek_ahead_by(2) {
                        let expr = self.parse_expression(Precedence::Call)?;
                        Ok(Expression::Call(CallExpr::parse(self, expr)?))
                    } else {
                        let expr = self.parse_expression(Precedence::Path)?;
                        Ok(Expression::Path(PathExpr::parse(self, expr)?))
                    }
                } else {
                    self.consume_token()?;
                    Ok(Expression::Path(PathExpr {
                        root: PathPrefix::SelfType,
                        tree_opt: None,
                    }))
                }
            }

            Token::SelfKeyword { .. } => {
                if let Ok(Token::DblColon { .. }) = self.peek_ahead_by(1) {
                    if let Ok(Token::Identifier { .. }) = self.peek_ahead_by(2) {
                        let expr = self.parse_expression(Precedence::Call)?;
                        Ok(Expression::Call(CallExpr::parse(self, expr)?))
                    } else {
                        let expr = self.parse_expression(Precedence::Path)?;
                        Ok(Expression::Path(PathExpr::parse(self, expr)?))
                    }
                } else if let Ok(Token::Dot { .. }) = self.peek_ahead_by(1) {
                    if let Ok(Token::Identifier { .. }) = self.peek_ahead_by(2) {
                        if let Ok(Token::LParen { .. }) = self.peek_ahead_by(3) {
                            let expr = self.parse_expression(Precedence::MethodCall)?;
                            Ok(Expression::Path(PathExpr::parse(self, expr)?))
                        } else {
                            let expr = self.parse_expression(Precedence::FieldAccess)?;
                            Ok(Expression::FieldAccess(FieldAccessExpr::parse(self, expr)?))
                        }
                    } else {
                        self.consume_token()?;
                        Ok(Expression::Path(PathExpr {
                            root: PathPrefix::SelfKw,
                            tree_opt: None,
                        }))
                    }
                } else {
                    self.consume_token()?;
                    Ok(Expression::Path(PathExpr {
                        root: PathPrefix::SelfKw,
                        tree_opt: None,
                    }))
                }
            }

            Token::Package { .. } => {
                if let Ok(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_ahead_by(1)
                {
                    let expr = self.parse_expression(Precedence::Path)?;
                    Ok(Expression::Path(PathExpr::parse(self, expr)?))
                } else {
                    self.consume_token()?;
                    Ok(Expression::Path(PathExpr {
                        root: PathPrefix::Package,
                        tree_opt: None,
                    }))
                }
            }

            Token::Super { .. } => {
                if let Ok(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_ahead_by(1)
                {
                    let expr = self.parse_expression(Precedence::Path)?;
                    Ok(Expression::Path(PathExpr::parse(self, expr)?))
                } else {
                    self.consume_token()?;
                    Ok(Expression::Path(PathExpr {
                        root: PathPrefix::Super,
                        tree_opt: None,
                    }))
                }
            }

            Token::Minus { .. } => Ok(Expression::Unary(parse_unary_expr(self, UnaryOp::Negate)?)),
            Token::Bang { .. } => Ok(Expression::Unary(parse_unary_expr(self, UnaryOp::Not)?)),
            Token::Ampersand { .. } | Token::AmpersandMut { .. } => Ok(Expression::Unary(
                parse_unary_expr(self, UnaryOp::Reference)?,
            )),
            Token::Asterisk { .. } => Ok(Expression::Unary(parse_unary_expr(
                self,
                UnaryOp::Dereference,
            )?)),
            Token::LParen { .. } => {
                if let Ok(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    Ok(Expression::Tuple(TupleExpr::parse(self)?))
                } else {
                    self.parse_primary()
                }
            }
            Token::LBracket { .. } => Ok(Expression::Array(ArrayExpr::parse(self)?)),
            Token::LBrace { .. } => Ok(Expression::Block(BlockExpr::parse(self)?)),
            Token::Pipe { .. } | Token::DblPipe { .. } => {
                Ok(Expression::Closure(ClosureExpr::parse(self)?))
            }
            Token::DblDot { .. } => {
                if let Ok(
                    Token::Identifier { .. }
                    | Token::IntLiteral { .. }
                    | Token::UIntLiteral { .. }
                    | Token::BigUIntLiteral { .. }
                    | Token::HashLiteral { .. },
                ) = self.consume_token()
                {
                    let expr = self.parse_expression(Precedence::Range)?;

                    Ok(Expression::Range(RangeExpr {
                        from_opt: None,
                        op: RangeOp::RangeExclusive,
                        to: Some(Box::new(expr)),
                    }))
                } else {
                    Ok(Expression::Range(RangeExpr {
                        from_opt: None,
                        op: RangeOp::RangeExclusive,
                        to: None,
                    }))
                }
            }
            Token::DotDotEquals { .. } => {
                self.consume_token()?;
                let expr = self.parse_expression(Precedence::Range)?;

                Ok(Expression::Range(RangeExpr {
                    from_opt: None,
                    op: RangeOp::RangeInclusive,
                    to: Some(Box::new(expr)),
                }))
            }
            Token::Return { .. } => {
                self.consume_token()?;
                let expr = self.parse_expression(Precedence::Lowest)?;

                Ok(Expression::Return(ReturnExpr {
                    kw_return: Keyword::Return,
                    expression: Box::new(expr),
                }))
            }

            Token::Break { name, .. } => {
                self.consume_token()?;

                Ok(Expression::Break(BreakExpr {
                    kw_break: Keyword::Break,
                }))
            }

            Token::Continue { name, .. } => {
                self.consume_token()?;
                Ok(Expression::Continue(ContinueExpr {
                    kw_continue: Keyword::Continue,
                }))
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
        let token = self.consume_token()?;

        match token {
            Token::Plus { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Add,
            )?)),
            Token::Minus { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Subtract,
            )?)),
            Token::Asterisk { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Multiply,
            )?)),
            Token::Slash { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Divide,
            )?)),
            Token::Percent { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Modulus,
            )?)),
            Token::DblEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Equal,
            )?)),
            Token::BangEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::NotEqual,
            )?)),
            Token::LessThan { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::LessThan,
            )?)),
            Token::LessThanEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::LessEqual,
            )?)),
            Token::GreaterThan { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::GreaterThan,
            )?)),
            Token::GreaterThanEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::GreaterEqual,
            )?)),
            Token::Equals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Assign,
            )?)),
            Token::PlusEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::AddAssign,
            )?)),
            Token::MinusEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::SubtractAssign,
            )?)),
            Token::AsteriskEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::MultiplyAssign,
            )?)),
            Token::SlashEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::DivideAssign,
            )?)),
            Token::PercentEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::ModulusAssign,
            )?)),
            Token::DblAmpersand { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::LogicalAnd,
            )?)),
            Token::DblPipe { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::LogicalOr,
            )?)),
            Token::Ampersand { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::BitwiseAnd,
            )?)),
            Token::Pipe { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::BitwiseOr,
            )?)),
            Token::Caret { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::BitwiseXor,
            )?)),
            Token::DblLessThan { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::ShiftLeft,
            )?)),
            Token::DblGreaterThan { .. } => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::ShiftRight,
            )?)),
            Token::QuestionMark { .. } => {
                Ok(Expression::Unwrap(UnwrapExpr::parse(self, left_expr)?))
            }

            _ => {
                self.log_error(ParserErrorKind::InvalidToken { token });
                Err(ErrorsEmitted(()))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// STATEMENT
    ///////////////////////////////////////////////////////////////////////////

    fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        let token = self.peek_current().ok_or({
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

        match token {
            Token::Let { .. } => Ok(Statement::Let(LetStmt::parse(self)?)),
            Token::If { .. } => Ok(Statement::If(IfStmt::parse(self)?)),
            Token::Match { .. } => Ok(Statement::Match(MatchStmt::parse(self)?)),
            Token::For { .. } => Ok(Statement::ForIn(ForInStmt::parse(self)?)),
            Token::While { .. } => Ok(Statement::While(WhileStmt::parse(self)?)),

            Token::Import { .. }
            | Token::Alias { .. }
            | Token::Const { .. }
            | Token::Static { .. } => Ok(Statement::Declaration(self.parse_declaration()?)),

            Token::Module { .. }
            | Token::Trait { .. }
            | Token::Enum { .. }
            | Token::Struct { .. }
            | Token::Impl { .. } => Ok(Statement::Definition(self.parse_definition()?)),

            _ => Ok(Statement::Expression(ExpressionStmt::parse(self)?)),
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// ITEMS
    ///////////////////////////////////////////////////////////////////////////

    fn parse_declaration(&mut self) -> Result<Declaration, ErrorsEmitted> {
        let token = self.peek_current().ok_or({
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

        match token {
            Token::Import { .. } => Ok(Declaration::Import(ImportDecl::parse(self)?)),
            Token::Alias { .. } => Ok(Declaration::Alias(AliasDecl::parse(self)?)),
            Token::Const { .. } => Ok(Declaration::Constant(ConstantDecl::parse(self)?)),
            Token::Static { .. } => Ok(Declaration::StaticItem(StaticItemDecl::parse(self)?)),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "declaration item".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    fn parse_definition(&mut self) -> Result<Definition, ErrorsEmitted> {
        let token = self.peek_current().ok_or({
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

        match token {
            Token::Module { .. } => Ok(Definition::Module(ModuleDef::parse(self)?)),
            Token::Trait { .. } => Ok(Definition::Trait(TraitDef::parse(self)?)),
            Token::Enum { .. } => Ok(Definition::Enum(EnumDef::parse(self)?)),
            Token::Struct { .. } => Ok(Definition::Struct(StructDef::parse(self)?)),
            Token::Impl { .. } => {
                if let Ok(Token::For { .. }) = self.peek_ahead_by(2) {
                    Ok(Definition::TraitImpl(TraitImplDef::parse(self)?))
                } else {
                    Ok(Definition::InherentImpl(InherentImplDef::parse(self)?))
                }
            }
            Token::Func { .. } => Ok(Definition::Function(FunctionDef::parse(self)?)),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "definition item".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// HELPERS
    ///////////////////////////////////////////////////////////////////////////

    /// Peek at the token at the current position.
    fn peek_current(&self) -> Option<Token> {
        self.stream.tokens().get(self.current).cloned()
    }

    /// Peek at the token `num_tokens` ahead of the token at the current position.
    fn peek_ahead_by(&mut self, num_tokens: usize) -> Result<Token, ErrorsEmitted> {
        let i = self.current + num_tokens;

        let tokens = self.stream.tokens();

        tokens.get(i).cloned().ok_or({
            self.log_error(ParserErrorKind::TokenIndexOutOfBounds {
                len: tokens.len(),
                i,
            });
            ErrorsEmitted(())
        })
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
    fn unconsume_token(&mut self) -> Option<Token> {
        if self.current > 0 {
            self.current -= 1;
        }

        self.peek_current()
    }

    /// Consume and check tokens, to ensure that the expected tokens are encountered
    /// during parsing. Return the relevant `ParserErrorKind` where applicable.
    fn expect_token(&mut self, expected: Token) -> Result<Token, ErrorsEmitted> {
        let token = self.consume_token()?;

        if token == expected {
            Ok(token)
        } else {
            self.log_error(ParserErrorKind::UnexpectedToken {
                expected: format!("`{:#?}`", expected),
                found: token,
            });
            Err(ErrorsEmitted(()))
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

    fn expect_identifier(&mut self, expected: Token) -> Result<Identifier, ErrorsEmitted> {
        let token = self.consume_token()?;

        if let Token::Identifier { name, .. } = expected {
            Ok(Identifier(name))
        } else {
            self.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
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

    /// Advance the parser by one if the expected token matches the current one
    /// and return whether or not this is the case.
    fn tokens_match(&mut self, expected: Token) -> bool {
        if self.is_expected_token(&expected) {
            self.current += 1;
            true
        } else {
            false
        }
    }

    /// Verify that the current token is the expected one.
    fn is_expected_token(&self, expected: &Token) -> bool {
        if self.current < self.stream.tokens().len() {
            &self.stream.tokens()[self.current] == expected
        } else {
            false
        }
    }

    /// Retrieve the respective precedence level for an operator.
    fn precedence(&self, token: &Token) -> Option<Precedence> {
        self.precedences.get(token).cloned()
    }

    /// Log information about an error that occurred during lexing.
    fn log_error(&mut self, error_kind: ParserErrorKind) {
        let error = CompilerError::new(error_kind, self.stream.span().substring(), self.current);
        self.errors.push(error);
    }

    pub fn errors(&self) -> &[CompilerError<ParserErrorKind>] {
        &self.errors
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
}
