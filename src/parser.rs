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
    // compound_expr::ParseCompoundExpr,
    // expression::ParseExpression,
    // item::{ParseDeclaration, ParseDefinition},
    // statement::ParseStatement,
    // unary_expr::parse_unary_expr,
};

/// Struct that stores a stream of tokens and contains methods to parse expressions,
/// statements and items, as well as helper methods and error handling capabilities.
#[derive(Debug)]
pub(crate) struct Parser {
    stream: TokenStream,
    current: usize,
    errors: Vec<CompilerError<ParserErrorKind>>,
    precedences: HashMap<Token, Precedence>,
}

impl Parser {
    /// Create a new `Parser` instance.
    /// Initialize an empty `Vec` to store parser errors and an empty `HashMap`
    /// to store precedences.
    pub(crate) fn new(stream: TokenStream) -> Self {
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
    fn parse(&mut self) -> Result<Vec<Expression>, ErrorsEmitted> {
        let mut expressions: Vec<Expression> = Vec::new();
        while self.current < self.stream.tokens().len() {
            expressions.push(self.parse_expression(Precedence::Lowest)?);
            println!("foo");
        }
        Ok(expressions)
    }

    ///////////////////////////////////////////////////////////////////////////
    /// EXPRESSIONS
    ///////////////////////////////////////////////////////////////////////////

    /// Recursively parse an expression based on operator precedence.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_expression()`");
        println!("INPUT PRECEDENCE: {:?}", precedence);

        let mut left_expr = self.parse_prefix()?;

        println!("PREFIX EXPRESSION: {:?}", left_expr.clone());
        println!("CURRENT TOKEN: {:?}", self.peek_current());

        while let Some(t) = self.peek_current() {
            let curr_precedence = self.precedence(&t);

            if precedence < curr_precedence {
                left_expr = self.parse_infix(left_expr)?;
            }
        }

        println!("RETURNED EXPRESSION: {:?}", left_expr);
        println!("EXIT `parse_expression()`");
        println!("CURRENT TOKEN: {:?}", self.peek_current());

        Ok(left_expr)
    }

    /// Parse primary expressions (e.g., grouped expressions, identifiers and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_primary()`");
        println!("CURRENT TOKEN: {:?}", self.peek_current());

        let token = self.consume_token();

        match token {
            // Token::Identifier { name, .. } => Ok(Expression::Path(PathExpr {
            //     root: PathPrefix::Identifier(Identifier(name)),
            //     tree_opt: None,
            // })),
            // Token::IntLiteral { value, .. } => Ok(Expression::Literal(Literal::Int(value))),
            Some(Token::UIntLiteral { value, .. }) => Ok(Expression::Literal(Literal::UInt(value))),
            // Token::BigUIntLiteral { value, .. } => Ok(Expression::Literal(Literal::BigUInt(value))),
            // Token::ByteLiteral { value, .. } => Ok(Expression::Literal(Literal::Byte(value))),
            // Token::BytesLiteral { value, .. } => Ok(Expression::Literal(Literal::Bytes(value))),
            // Token::HashLiteral { value, .. } => Ok(Expression::Literal(Literal::Hash(value))),
            // Token::StringLiteral { value, .. } => Ok(Expression::Literal(Literal::String(value))),
            // Token::CharLiteral { value, .. } => Ok(Expression::Literal(Literal::Char(value))),
            // Token::BoolLiteral { value, .. } => Ok(Expression::Literal(Literal::Bool(value))),
            // Token::LParen { .. } => Ok(Expression::Grouped(GroupedExpr::parse(self)?)),
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier, `_`, literal or `(`".to_string(),
                    found: token.unwrap(),
                });

                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Parse prefix expressions (e.g., integer literals, identifiers and parentheses),
    /// where the respective token type appears at the beginning of an expression.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_prefix()`");
        println!("CURRENT TOKEN: {:?}", self.peek_current());

        // let token = self.consume_token();
        let token = self.peek_current();

        // let token = self.peek_current().ok_or({
        //     self.log_error(ParserErrorKind::UnexpectedEndOfInput);
        //     ErrorsEmitted(())
        // })?;

        match token {
            Some(
                Token::IntLiteral { .. }
                | Token::UIntLiteral { .. }
                | Token::BigUIntLiteral { .. }
                | Token::HashLiteral { .. },
            ) => {
                // if let Ok(Token::As { .. }) = self.peek_ahead_by(1) {
                //     let expr = self.parse_expression(Precedence::TypeCast)?;
                //     Ok(Expression::TypeCast(TypeCastExpr::parse(self, expr)?))
                // } else if let Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                //     self.peek_ahead_by(1)
                // {
                //     let expr = self.parse_expression(Precedence::Range)?;
                //     Ok(Expression::Range(RangeExpr::parse(self, expr)?))
                // } else {
                self.parse_primary()
                // }
            }

            // Token::ByteLiteral { .. }
            // | Token::BytesLiteral { .. }
            // | Token::StringLiteral { .. }
            // | Token::CharLiteral { .. }
            // | Token::BoolLiteral { .. } => self.parse_primary(),

            // Token::Identifier { name, .. } => {
            //     if &name == "_" {
            //         Ok(Expression::Underscore(UnderscoreExpr {
            //             underscore: Separator::Underscore,
            //         }))
            //     } else if let Ok(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
            //         self.peek_ahead_by(1)
            //     {
            //         let expr = self.parse_expression(Precedence::Path)?;
            //         Ok(Expression::Path(PathExpr::parse(self, expr)?))
            //     } else if let Ok(Token::Dot { .. }) = self.peek_ahead_by(1) {
            //         match self.peek_ahead_by(2) {
            //             Ok(Token::LParen { .. }) => {
            //                 let expr = self.parse_expression(Precedence::MethodCall)?;
            //                 Ok(Expression::MethodCall(MethodCallExpr::parse(self, expr)?))
            //             }
            //             Ok(Token::UIntLiteral { .. }) => {
            //                 let expr = self.parse_expression(Precedence::FieldAccess)?;
            //                 Ok(Expression::TupleIndex(TupleIndexExpr::parse(self, expr)?))
            //             }
            //             _ => {
            //                 let expr = self.parse_expression(Precedence::FieldAccess)?;
            //                 Ok(Expression::FieldAccess(FieldAccessExpr::parse(self, expr)?))
            //             }
            //         }
            //     } else if let Ok(Token::LParen { .. }) = self.peek_ahead_by(1) {
            //         // TODO: use symbol table to check whether this could be a `TupleStructExpr`
            //         let expr = self.parse_expression(Precedence::Call)?;
            //         Ok(Expression::Call(CallExpr::parse(self, expr)?))
            //     } else if let Ok(Token::LBracket { .. }) = self.peek_ahead_by(1) {
            //         let expr = self.parse_expression(Precedence::Index)?;
            //         Ok(Expression::Index(IndexExpr::parse(self, expr)?))
            //     } else if let Ok(Token::QuestionMark { .. }) = self.peek_ahead_by(1) {
            //         let expr = self.parse_expression(Precedence::Unwrap)?;
            //         Ok(Expression::Unwrap(UnwrapExpr::parse(self, expr)?))
            //     } else if let Ok(Token::As { .. }) = self.peek_ahead_by(1) {
            //         let expr = self.parse_expression(Precedence::TypeCast)?;
            //         Ok(Expression::TypeCast(TypeCastExpr::parse(self, expr)?))
            //     } else if let Ok(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
            //         self.peek_ahead_by(1)
            //     {
            //         let expr = self.parse_expression(Precedence::Range)?;
            //         Ok(Expression::Range(RangeExpr::parse(self, expr)?))
            //     } else if let Ok(Token::LBrace { .. }) = self.peek_ahead_by(1) {
            //         let expr = self.parse_expression(Precedence::Lowest)?;
            //         match expr {
            //             Expression::Path(_) => {
            //                 Ok(Expression::Struct(StructExpr::parse(self, expr)?))
            //             }
            //             _ => {
            //                 let token = self.peek_current().ok_or({
            //                     self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            //                     ErrorsEmitted(())
            //                 })?;
            //                 self.log_error(ParserErrorKind::UnexpectedToken {
            //                     expected: "path expression".to_string(),
            //                     found: token,
            //                 });
            //                 Err(ErrorsEmitted(()))
            //             }
            //         }
            //     } else {
            //         self.parse_primary()
            //     }
            // }

            // Token::SelfType { .. } => {
            //     if let Ok(Token::DblColon { .. }) = self.peek_ahead_by(1) {
            //         if let Ok(Token::Identifier { .. }) = self.peek_ahead_by(2) {
            //             let expr = self.parse_expression(Precedence::Call)?;
            //             Ok(Expression::Call(CallExpr::parse(self, expr)?))
            //         } else {
            //             let expr = self.parse_expression(Precedence::Path)?;
            //             Ok(Expression::Path(PathExpr::parse(self, expr)?))
            //         }
            //     } else {
            //         self.consume_token()?;
            //         Ok(Expression::Path(PathExpr {
            //             root: PathPrefix::SelfType,
            //             tree_opt: None,
            //         }))
            //     }
            // }

            // Token::SelfKeyword { .. } => {
            //     if let Ok(Token::DblColon { .. }) = self.peek_ahead_by(1) {
            //         if let Ok(Token::Identifier { .. }) = self.peek_ahead_by(2) {
            //             let expr = self.parse_expression(Precedence::Call)?;
            //             Ok(Expression::Call(CallExpr::parse(self, expr)?))
            //         } else {
            //             let expr = self.parse_expression(Precedence::Path)?;
            //             Ok(Expression::Path(PathExpr::parse(self, expr)?))
            //         }
            //     } else if let Ok(Token::Dot { .. }) = self.peek_ahead_by(1) {
            //         if let Ok(Token::Identifier { .. }) = self.peek_ahead_by(2) {
            //             if let Ok(Token::LParen { .. }) = self.peek_ahead_by(3) {
            //                 let expr = self.parse_expression(Precedence::MethodCall)?;
            //                 Ok(Expression::Path(PathExpr::parse(self, expr)?))
            //             } else {
            //                 let expr = self.parse_expression(Precedence::FieldAccess)?;
            //                 Ok(Expression::FieldAccess(FieldAccessExpr::parse(self, expr)?))
            //             }
            //         } else {
            //             self.consume_token()?;
            //             Ok(Expression::Path(PathExpr {
            //                 root: PathPrefix::SelfKw,
            //                 tree_opt: None,
            //             }))
            //         }
            //     } else {
            //         self.consume_token()?;
            //         Ok(Expression::Path(PathExpr {
            //             root: PathPrefix::SelfKw,
            //             tree_opt: None,
            //         }))
            //     }
            // }

            // Token::Package { .. } => {
            //     if let Ok(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
            //         self.peek_ahead_by(1)
            //     {
            //         let expr = self.parse_expression(Precedence::Path)?;
            //         Ok(Expression::Path(PathExpr::parse(self, expr)?))
            //     } else {
            //         self.consume_token()?;
            //         Ok(Expression::Path(PathExpr {
            //             root: PathPrefix::Package,
            //             tree_opt: None,
            //         }))
            //     }
            // }

            // Token::Super { .. } => {
            //     if let Ok(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
            //         self.peek_ahead_by(1)
            //     {
            //         let expr = self.parse_expression(Precedence::Path)?;
            //         Ok(Expression::Path(PathExpr::parse(self, expr)?))
            //     } else {
            //         self.consume_token()?;
            //         Ok(Expression::Path(PathExpr {
            //             root: PathPrefix::Super,
            //             tree_opt: None,
            //         }))
            //     }
            // }

            // Token::Minus { .. } => Ok(Expression::Unary(parse_unary_expr(self, UnaryOp::Negate)?)),
            // Token::Bang { .. } => Ok(Expression::Unary(parse_unary_expr(self, UnaryOp::Not)?)),
            // Token::Ampersand { .. } | Token::AmpersandMut { .. } => Ok(Expression::Unary(
            //     parse_unary_expr(self, UnaryOp::Reference)?,
            // )),
            // Token::Asterisk { .. } => Ok(Expression::Unary(parse_unary_expr(
            //     self,
            //     UnaryOp::Dereference,
            // )?)),
            // Token::LParen { .. } => {
            //     if let Ok(Token::Comma { .. }) = self.peek_ahead_by(2) {
            //         Ok(Expression::Tuple(TupleExpr::parse(self)?))
            //     } else {
            //         self.parse_primary()
            //     }
            // }
            // Token::LBracket { .. } => Ok(Expression::Array(ArrayExpr::parse(self)?)),
            // Token::LBrace { .. } => Ok(Expression::Block(BlockExpr::parse(self)?)),
            // Token::Pipe { .. } | Token::DblPipe { .. } => {
            //     Ok(Expression::Closure(ClosureExpr::parse(self)?))
            // }
            // Token::DblDot { .. } => {
            //     if let Ok(
            //         Token::Identifier { .. }
            //         | Token::IntLiteral { .. }
            //         | Token::UIntLiteral { .. }
            //         | Token::BigUIntLiteral { .. }
            //         | Token::HashLiteral { .. },
            //     ) = self.consume_token()
            //     {
            //         let expr = self.parse_expression(Precedence::Range)?;

            //         Ok(Expression::Range(RangeExpr {
            //             from_opt: None,
            //             op: RangeOp::RangeExclusive,
            //             to_opt: Some(Box::new(expr)),
            //         }))
            //     } else {
            //         Ok(Expression::Range(RangeExpr {
            //             from_opt: None,
            //             op: RangeOp::RangeExclusive,
            //             to_opt: None,
            //         }))
            //     }
            // }
            // Token::DotDotEquals { .. } => {
            //     self.consume_token()?;
            //     let expr = self.parse_expression(Precedence::Range)?;

            //     Ok(Expression::Range(RangeExpr {
            //         from_opt: None,
            //         op: RangeOp::RangeInclusive,
            //         to_opt: Some(Box::new(expr)),
            //     }))
            // }
            // Token::Return { .. } => {
            //     self.consume_token()?;
            //     let expr = self.parse_expression(Precedence::Lowest)?;

            //     Ok(Expression::Return(ReturnExpr {
            //         kw_return: Keyword::Return,
            //         expression: Box::new(expr),
            //     }))
            // }

            // Token::Break { name, .. } => {
            //     self.consume_token()?;

            //     Ok(Expression::Break(BreakExpr {
            //         kw_break: Keyword::Break,
            //     }))
            // }

            // Token::Continue { name, .. } => {
            //     self.consume_token()?;
            //     Ok(Expression::Continue(ContinueExpr {
            //         kw_continue: Keyword::Continue,
            //     }))
            // }
            _ => {
                self.log_error(ParserErrorKind::InvalidToken {
                    token: token.unwrap(),
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Parse infix expressions (e.g., binary operators), where the respective token type
    /// appears in the middle of an expression.
    fn parse_infix(&mut self, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_infix()`");
        println!("CURRENT TOKEN: {:?}", self.peek_current());

        let token = self.consume_token();

        match token {
            Some(Token::Plus { .. }) => Ok(Expression::Binary(parse_binary_expr(
                self,
                left_expr,
                BinaryOp::Add,
            )?)),
            // Token::Minus { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::Subtract,
            // )?)),
            // Token::Asterisk { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::Multiply,
            // )?)),
            // Token::Slash { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::Divide,
            // )?)),
            // Token::Percent { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::Modulus,
            // )?)),
            // Token::DblEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::Equal,
            // )?)),
            // Token::BangEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::NotEqual,
            // )?)),
            // Token::LessThan { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::LessThan,
            // )?)),
            // Token::LessThanEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::LessEqual,
            // )?)),
            // Token::GreaterThan { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::GreaterThan,
            // )?)),
            // Token::GreaterThanEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::GreaterEqual,
            // )?)),
            // Token::Equals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::Assign,
            // )?)),
            // Token::PlusEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::AddAssign,
            // )?)),
            // Token::MinusEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::SubtractAssign,
            // )?)),
            // Token::AsteriskEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::MultiplyAssign,
            // )?)),
            // Token::SlashEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::DivideAssign,
            // )?)),
            // Token::PercentEquals { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::ModulusAssign,
            // )?)),
            // Token::DblAmpersand { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::LogicalAnd,
            // )?)),
            // Token::DblPipe { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::LogicalOr,
            // )?)),
            // Token::Ampersand { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::BitwiseAnd,
            // )?)),
            // Token::Pipe { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::BitwiseOr,
            // )?)),
            // Token::Caret { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::BitwiseXor,
            // )?)),
            // Token::DblLessThan { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::ShiftLeft,
            // )?)),
            // Token::DblGreaterThan { .. } => Ok(Expression::Binary(parse_binary_expr(
            //     self,
            //     left_expr,
            //     BinaryOp::ShiftRight,
            // )?)),
            // Token::QuestionMark { .. } => {
            //     Ok(Expression::Unwrap(UnwrapExpr::parse(self, left_expr)?))
            // }
            Some(_) => {
                self.log_error(ParserErrorKind::InvalidToken {
                    token: token.unwrap(),
                });
                Err(ErrorsEmitted(()))
            }

            None => panic!("EOF"),
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// STATEMENT
    ///////////////////////////////////////////////////////////////////////////

    // fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
    //     let token = self.peek_current().ok_or({
    //         self.log_error(ParserErrorKind::UnexpectedEndOfInput);
    //         ErrorsEmitted(())
    //     })?;

    //     match token {
    //         Token::Let { .. } => Ok(Statement::Let(LetStmt::parse(self)?)),
    //         Token::If { .. } => Ok(Statement::If(IfStmt::parse(self)?)),
    //         Token::Match { .. } => Ok(Statement::Match(MatchStmt::parse(self)?)),
    //         Token::For { .. } => Ok(Statement::ForIn(ForInStmt::parse(self)?)),
    //         Token::While { .. } => Ok(Statement::While(WhileStmt::parse(self)?)),

    //         Token::Import { .. }
    //         | Token::Alias { .. }
    //         | Token::Const { .. }
    //         | Token::Static { .. } => Ok(Statement::Declaration(self.parse_declaration()?)),

    //         Token::Module { .. }
    //         | Token::Trait { .. }
    //         | Token::Enum { .. }
    //         | Token::Struct { .. }
    //         | Token::Impl { .. } => Ok(Statement::Definition(self.parse_definition()?)),

    //         _ => Ok(Statement::Expression(ExpressionStmt::parse(self)?)),
    //     }
    // }

    ///////////////////////////////////////////////////////////////////////////
    /// ITEMS
    ///////////////////////////////////////////////////////////////////////////

    // fn parse_declaration(&mut self) -> Result<Declaration, ErrorsEmitted> {
    //     let token = self.peek_current().ok_or({
    //         self.log_error(ParserErrorKind::UnexpectedEndOfInput);
    //         ErrorsEmitted(())
    //     })?;

    //     match token {
    //         Token::Import { .. } => Ok(Declaration::Import(ImportDecl::parse(self)?)),
    //         Token::Alias { .. } => Ok(Declaration::Alias(AliasDecl::parse(self)?)),
    //         Token::Const { .. } => Ok(Declaration::Constant(ConstantDecl::parse(self)?)),
    //         Token::Static { .. } => Ok(Declaration::StaticItem(StaticItemDecl::parse(self)?)),
    //         _ => {
    //             self.log_error(ParserErrorKind::UnexpectedToken {
    //                 expected: "declaration item".to_string(),
    //                 found: token,
    //             });
    //             Err(ErrorsEmitted(()))
    //         }
    //     }
    // }

    // fn parse_definition(&mut self) -> Result<Definition, ErrorsEmitted> {
    //     let token = self.peek_current().ok_or({
    //         self.log_error(ParserErrorKind::UnexpectedEndOfInput);
    //         ErrorsEmitted(())
    //     })?;

    //     match token {
    //         Token::Module { .. } => Ok(Definition::Module(ModuleDef::parse(self)?)),
    //         Token::Trait { .. } => Ok(Definition::Trait(TraitDef::parse(self)?)),
    //         Token::Enum { .. } => Ok(Definition::Enum(EnumDef::parse(self)?)),
    //         Token::Struct { .. } => Ok(Definition::Struct(StructDef::parse(self)?)),
    //         Token::Impl { .. } => {
    //             if let Ok(Token::For { .. }) = self.peek_ahead_by(2) {
    //                 Ok(Definition::TraitImpl(TraitImplDef::parse(self)?))
    //             } else {
    //                 Ok(Definition::InherentImpl(InherentImplDef::parse(self)?))
    //             }
    //         }
    //         Token::Func { .. } => Ok(Definition::Function(FunctionDef::parse(self)?)),
    //         _ => {
    //             self.log_error(ParserErrorKind::UnexpectedToken {
    //                 expected: "definition item".to_string(),
    //                 found: token,
    //             });
    //             Err(ErrorsEmitted(()))
    //         }
    //     }
    // }

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
    fn consume_token(&mut self) -> Option<Token> {
        if let Some(t) = self.peek_current() {
            self.current += 1;
            Some(t)
        } else {
            None
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
    // fn expect_token(&mut self, expected: Token) -> Result<Token, ErrorsEmitted> {
    //     let token = self.consume_token()?;

    //     if token == expected {
    //         Ok(token)
    //     } else {
    //         self.log_error(ParserErrorKind::UnexpectedToken {
    //             expected: format!("`{:#?}`", expected),
    //             found: token,
    //         });
    //         Err(ErrorsEmitted(()))
    //     }
    // }

    // fn expect_keyword(&mut self, expected: Token) -> Result<Keyword, ErrorsEmitted> {
    //     let token = self.consume_token()?;

    //     match token {
    //         Token::Import { .. } => Ok(Keyword::Import),
    //         Token::Module { .. } => Ok(Keyword::Module),
    //         Token::Package { .. } => Ok(Keyword::Package),
    //         Token::SelfKeyword { .. } => Ok(Keyword::KwSelf),
    //         Token::SelfType { .. } => Ok(Keyword::SelfType),
    //         Token::Super { .. } => Ok(Keyword::Super),
    //         Token::Pub { .. } => Ok(Keyword::Pub),
    //         Token::As { .. } => Ok(Keyword::As),
    //         Token::Const { .. } => Ok(Keyword::Const),
    //         Token::Static { .. } => Ok(Keyword::Static),
    //         Token::Func { .. } => Ok(Keyword::Func),
    //         Token::Struct { .. } => Ok(Keyword::Struct),
    //         Token::Enum { .. } => Ok(Keyword::Enum),
    //         Token::Trait { .. } => Ok(Keyword::Trait),
    //         Token::Impl { .. } => Ok(Keyword::Impl),
    //         Token::If { .. } => Ok(Keyword::If),
    //         Token::Else { .. } => Ok(Keyword::Else),
    //         Token::Match { .. } => Ok(Keyword::Match),
    //         Token::Loop { .. } => Ok(Keyword::Loop),
    //         Token::For { .. } => Ok(Keyword::For),
    //         Token::In { .. } => Ok(Keyword::In),
    //         Token::While { .. } => Ok(Keyword::While),
    //         Token::Break { .. } => Ok(Keyword::Break),
    //         Token::Continue { .. } => Ok(Keyword::Continue),
    //         Token::Return { .. } => Ok(Keyword::Return),
    //         _ => {
    //             self.log_error(ParserErrorKind::UnexpectedToken {
    //                 expected: format!("`{:#?}`", expected),
    //                 found: token,
    //             });
    //             Err(ErrorsEmitted(()))
    //         }
    //     }
    // }

    // fn expect_delimiter(&mut self, expected: Token) -> Result<Delimiter, ErrorsEmitted> {
    //     let token = self.consume_token()?;

    //     match token {
    //         Token::LParen { .. } => Ok(Delimiter::LParen),
    //         Token::RParen { .. } => Ok(Delimiter::RParen),
    //         Token::LBracket { .. } => Ok(Delimiter::LBracket),
    //         Token::RBracket { .. } => Ok(Delimiter::RBracket),
    //         Token::LBrace { .. } => Ok(Delimiter::LBrace),
    //         Token::RBrace { .. } => Ok(Delimiter::RBrace),
    //         _ => {
    //             self.log_error(ParserErrorKind::UnexpectedToken {
    //                 expected: format!("`{:#?}`", expected),
    //                 found: token,
    //             });
    //             Err(ErrorsEmitted(()))
    //         }
    //     }
    // }

    // fn expect_binary_op(&mut self, expected: Token) -> Result<BinaryOp, ErrorsEmitted> {
    //     let token = self.consume_token()?;

    //     match token {
    //         Token::Plus { .. } => Ok(BinaryOp::Add),
    //         Token::Minus { .. } => Ok(BinaryOp::Subtract),
    //         Token::Asterisk { .. } => Ok(BinaryOp::Multiply),
    //         Token::Slash { .. } => Ok(BinaryOp::Divide),
    //         Token::Percent { .. } => Ok(BinaryOp::Modulus),
    //         Token::DblEquals { .. } => Ok(BinaryOp::Equal),
    //         Token::BangEquals { .. } => Ok(BinaryOp::NotEqual),
    //         Token::LessThan { .. } => Ok(BinaryOp::LessThan),
    //         Token::LessThanEquals { .. } => Ok(BinaryOp::LessEqual),
    //         Token::GreaterThan { .. } => Ok(BinaryOp::GreaterThan),
    //         Token::GreaterThanEquals { .. } => Ok(BinaryOp::GreaterEqual),
    //         Token::Equals { .. } => Ok(BinaryOp::Assign),
    //         Token::PlusEquals { .. } => Ok(BinaryOp::AddAssign),
    //         Token::MinusEquals { .. } => Ok(BinaryOp::SubtractAssign),
    //         Token::SlashEquals { .. } => Ok(BinaryOp::DivideAssign),
    //         Token::PercentEquals { .. } => Ok(BinaryOp::ModulusAssign),
    //         Token::DblAmpersand { .. } => Ok(BinaryOp::LogicalAnd),
    //         Token::DblPipe { .. } => Ok(BinaryOp::LogicalOr),
    //         Token::Ampersand { .. } => Ok(BinaryOp::BitwiseAnd),
    //         Token::Pipe { .. } => Ok(BinaryOp::BitwiseOr),
    //         Token::Caret { .. } => Ok(BinaryOp::BitwiseXor),
    //         Token::DblLessThan { .. } => Ok(BinaryOp::ShiftLeft),
    //         Token::DblGreaterThan { .. } => Ok(BinaryOp::ShiftRight),
    //         _ => {
    //             self.log_error(ParserErrorKind::UnexpectedToken {
    //                 expected: format!("`{:#?}`", expected),
    //                 found: token,
    //             });
    //             Err(ErrorsEmitted(()))
    //         }
    //     }
    // }

    // fn expect_separator(&mut self, expected: Token) -> Result<Separator, ErrorsEmitted> {
    //     let token = self.consume_token()?;

    //     match token {
    //         Token::Colon { .. } => Ok(Separator::Colon),
    //         Token::Semicolon { .. } => Ok(Separator::Semicolon),
    //         Token::Comma { .. } => Ok(Separator::Comma),
    //         Token::Dot { .. } => Ok(Separator::Dot),
    //         Token::DblColon { .. } => Ok(Separator::DblColon),
    //         Token::ColonColonAsterisk { .. } => Ok(Separator::ColonColonAsterisk),
    //         Token::ThinArrow { .. } => Ok(Separator::ThinArrow),
    //         Token::FatArrow { .. } => Ok(Separator::FatArrow),

    //         _ => {
    //             self.log_error(ParserErrorKind::UnexpectedToken {
    //                 expected: format!("`{:#?}`", expected),
    //                 found: token,
    //             });
    //             Err(ErrorsEmitted(()))
    //         }
    //     }
    // }

    /// Advance the parser by one if the expected token matches the current one
    /// and return whether or not this is the case.
    // fn tokens_match(&mut self, expected: Token) -> bool {
    //     if self.is_expected_token(&expected) {
    //         self.current += 1;
    //         true
    //     } else {
    //         false
    //     }
    // }

    // /// Verify that the current token is the expected one.
    // fn is_expected_token(&self, expected: &Token) -> bool {
    //     if self.current < self.stream.tokens().len() {
    //         &self.stream.tokens()[self.current] == expected
    //     } else {
    //         false
    //     }
    // }

    /// Retrieve the respective precedence level for an operator.
    fn precedence(&self, token: &Token) -> Precedence {
        match token {
            // Token::Identifier { name, span } => todo!(),
            // Token::IntLiteral { value, span } => todo!(),
            Token::UIntLiteral { value, span } => Precedence::Lowest,
            // Token::BigUIntLiteral { value, span } => todo!(),
            // Token::ByteLiteral { value, span } => todo!(),
            // Token::BytesLiteral { value, span } => todo!(),
            // Token::HashLiteral { value, span } => todo!(),
            // Token::StringLiteral { value, span } => todo!(),
            // Token::CharLiteral { value, span } => todo!(),
            // Token::BoolLiteral { value, span } => todo!(),
            // Token::Let { name, span } => todo!(),
            // Token::Mut { name, span } => todo!(),
            // Token::Ref { name, span } => todo!(),
            // Token::Pub { name, span } => todo!(),
            // Token::Func { name, span } => todo!(),
            // Token::Contract { name, span } => todo!(),
            // Token::Library { name, span } => todo!(),
            // Token::Interface { name, span } => todo!(),
            // Token::Script { name, span } => todo!(),
            // Token::Constructor { name, span } => todo!(),
            // Token::Modifier { name, span } => todo!(),
            // Token::Test { name, span } => todo!(),
            // Token::Event { name, span } => todo!(),
            // Token::Error { name, span } => todo!(),
            // Token::Abstract { name, span } => todo!(),
            // Token::Payable { name, span } => todo!(),
            // Token::Storage { name, span } => todo!(),
            // Token::View { name, span } => todo!(),
            // Token::Topic { name, span } => todo!(),
            // Token::Calldata { name, span } => todo!(),
            // Token::Return { name, span } => todo!(),
            // Token::Struct { name, span } => todo!(),
            // Token::Enum { name, span } => todo!(),
            // Token::Trait { name, span } => todo!(),
            // Token::Impl { name, span } => todo!(),
            // Token::Module { name, span } => todo!(),
            // Token::Extern { name, span } => todo!(),
            // Token::Import { name, span } => todo!(),
            // Token::Package { name, span } => todo!(),
            // Token::Super { name, span } => todo!(),
            // Token::SelfKeyword { name, span } => todo!(),
            // Token::Const { name, span } => todo!(),
            // Token::Static { name, span } => todo!(),
            // Token::Unsafe { name, span } => todo!(),
            // Token::Alias { name, span } => todo!(),
            // Token::As { name, span } => todo!(),
            // Token::If { name, span } => todo!(),
            // Token::Else { name, span } => todo!(),
            // Token::Match { name, span } => todo!(),
            // Token::For { name, span } => todo!(),
            // Token::In { name, span } => todo!(),
            // Token::Loop { name, span } => todo!(),
            // Token::While { name, span } => todo!(),
            // Token::Break { name, span } => todo!(),
            // Token::Continue { name, span } => todo!(),
            // Token::I32Type { name, span } => todo!(),
            // Token::I64Type { name, span } => todo!(),
            // Token::I128Type { name, span } => todo!(),
            // Token::U8Type { name, span } => todo!(),
            // Token::U16Type { name, span } => todo!(),
            // Token::U32Type { name, span } => todo!(),
            // Token::U64Type { name, span } => todo!(),
            // Token::U128Type { name, span } => todo!(),
            // Token::U256Type { name, span } => todo!(),
            // Token::U512Type { name, span } => todo!(),
            // Token::ByteType { name, span } => todo!(),
            // Token::B2Type { name, span } => todo!(),
            // Token::B3Type { name, span } => todo!(),
            // Token::B4Type { name, span } => todo!(),
            // Token::B5Type { name, span } => todo!(),
            // Token::B6Type { name, span } => todo!(),
            // Token::B7Type { name, span } => todo!(),
            // Token::B8Type { name, span } => todo!(),
            // Token::B9Type { name, span } => todo!(),
            // Token::B10Type { name, span } => todo!(),
            // Token::B11Type { name, span } => todo!(),
            // Token::B12Type { name, span } => todo!(),
            // Token::B13Type { name, span } => todo!(),
            // Token::B14Type { name, span } => todo!(),
            // Token::B15Type { name, span } => todo!(),
            // Token::B16Type { name, span } => todo!(),
            // Token::B17Type { name, span } => todo!(),
            // Token::B18Type { name, span } => todo!(),
            // Token::B19Type { name, span } => todo!(),
            // Token::B20Type { name, span } => todo!(),
            // Token::B21Type { name, span } => todo!(),
            // Token::B22Type { name, span } => todo!(),
            // Token::B23Type { name, span } => todo!(),
            // Token::B24Type { name, span } => todo!(),
            // Token::B25Type { name, span } => todo!(),
            // Token::B26Type { name, span } => todo!(),
            // Token::B27Type { name, span } => todo!(),
            // Token::B28Type { name, span } => todo!(),
            // Token::B29Type { name, span } => todo!(),
            // Token::B30Type { name, span } => todo!(),
            // Token::B31Type { name, span } => todo!(),
            // Token::B32Type { name, span } => todo!(),
            // Token::H160Type { name, span } => todo!(),
            // Token::H256Type { name, span } => todo!(),
            // Token::H512Type { name, span } => todo!(),
            // Token::StringType { name, span } => todo!(),
            // Token::CharType { name, span } => todo!(),
            // Token::BoolType { name, span } => todo!(),
            // Token::SelfType { name, span } => todo!(),
            // Token::CustomType { name, span } => todo!(),
            // Token::LParen { delim, span } => todo!(),
            // Token::RParen { delim, span } => todo!(),
            // Token::LBrace { delim, span } => todo!(),
            // Token::RBrace { delim, span } => todo!(),
            // Token::LBracket { delim, span } => todo!(),
            // Token::RBracket { delim, span } => todo!(),
            // Token::Colon { punc, span } => todo!(),
            // Token::Semicolon { punc, span } => todo!(),
            // Token::Comma { punc, span } => todo!(),
            // Token::Dot { punc, span } => todo!(),
            // Token::DblColon { punc, span } => todo!(),
            // Token::ColonColonAsterisk { punc, span } => todo!(),
            // Token::HashSign { punc, span } => todo!(),
            // Token::HashBang { punc, span } => todo!(),
            // Token::ThinArrow { punc, span } => todo!(),
            // Token::FatArrow { punc, span } => todo!(),
            // Token::Bang { punc, span } => todo!(),
            // Token::DollarSign { punc, span } => todo!(),
            // Token::Percent { punc, span } => todo!(),
            // Token::Ampersand { punc, span } => todo!(),
            // Token::Asterisk { punc, span } => todo!(),
            Token::Plus { punc, span } => Precedence::Sum,
            // Token::Minus { punc, span } => todo!(),
            // Token::Slash { punc, span } => todo!(),
            // Token::LessThan { punc, span } => todo!(),
            // Token::Equals { punc, span } => todo!(),
            // Token::GreaterThan { punc, span } => todo!(),
            // Token::QuestionMark { punc, span } => todo!(),
            // Token::AtSign { punc, span } => todo!(),
            // Token::Backslash { punc, span } => todo!(),
            // Token::Caret { punc, span } => todo!(),
            // Token::Backtick { punc, span } => todo!(),
            // Token::Pipe { punc, span } => todo!(),
            // Token::DblDot { punc, span } => todo!(),
            // Token::DotDotEquals { punc, span } => todo!(),
            // Token::BangEquals { punc, span } => todo!(),
            // Token::PercentEquals { punc, span } => todo!(),
            // Token::DblAsterisk { punc, span } => todo!(),
            // Token::AsteriskEquals { punc, span } => todo!(),
            // Token::DblAmpersand { punc, span } => todo!(),
            // Token::AmpersandMut { punc, span } => todo!(),
            // Token::PlusEquals { punc, span } => todo!(),
            // Token::MinusEquals { punc, span } => todo!(),
            // Token::SlashEquals { punc, span } => todo!(),
            // Token::DblLessThan { punc, span } => todo!(),
            // Token::LessThanEquals { punc, span } => todo!(),
            // Token::DblEquals { punc, span } => todo!(),
            // Token::DblGreaterThan { punc, span } => todo!(),
            // Token::GreaterThanEquals { punc, span } => todo!(),
            // Token::DblPipe { punc, span } => todo!(),
            // Token::LineComment { span } => todo!(),
            // Token::BlockComment { span } => todo!(),
            // Token::DocComment { comment, span } => todo!(),
            // Token::EOF => todo!(),
            _ => panic!(),
        }
    }

    /// Log information about an error that occurred during lexing.
    fn log_error(&mut self, error_kind: ParserErrorKind) {
        let error = CompilerError::new(error_kind, self.stream.span().substring(), self.current);
        self.errors.push(error);
    }

    pub fn errors(&self) -> &[CompilerError<ParserErrorKind>] {
        &self.errors
    }

    // / Match a `Token` to a `Type` and return the `Type` or emit an error.
    // fn get_type(&mut self) -> Result<Type, ErrorsEmitted> {
    //     let token = self.consume_token()?;

    //     match token {
    //         Token::I32Type { .. } | Token::I64Type { .. } | Token::I128Type { .. } => Ok(Type::Int),
    //         Token::U8Type { .. }
    //         | Token::U16Type { .. }
    //         | Token::U32Type { .. }
    //         | Token::U64Type { .. }
    //         | Token::U128Type { .. } => Ok(Type::UInt),
    //         Token::U256Type { .. } => Ok(Type::BigUInt),
    //         Token::U512Type { .. } => Ok(Type::BigUInt),
    //         Token::ByteType { .. } => Ok(Type::Byte),
    //         Token::B2Type { .. }
    //         | Token::B3Type { .. }
    //         | Token::B4Type { .. }
    //         | Token::B5Type { .. }
    //         | Token::B6Type { .. }
    //         | Token::B7Type { .. }
    //         | Token::B8Type { .. }
    //         | Token::B9Type { .. }
    //         | Token::B10Type { .. }
    //         | Token::B11Type { .. }
    //         | Token::B12Type { .. }
    //         | Token::B13Type { .. }
    //         | Token::B14Type { .. }
    //         | Token::B15Type { .. }
    //         | Token::B16Type { .. }
    //         | Token::B17Type { .. }
    //         | Token::B18Type { .. }
    //         | Token::B19Type { .. }
    //         | Token::B20Type { .. }
    //         | Token::B21Type { .. }
    //         | Token::B22Type { .. }
    //         | Token::B23Type { .. }
    //         | Token::B24Type { .. }
    //         | Token::B25Type { .. }
    //         | Token::B26Type { .. }
    //         | Token::B27Type { .. }
    //         | Token::B28Type { .. }
    //         | Token::B29Type { .. }
    //         | Token::B30Type { .. }
    //         | Token::B31Type { .. }
    //         | Token::B32Type { .. } => Ok(Type::Bytes),
    //         Token::H160Type { .. } | Token::H256Type { .. } | Token::H512Type { .. } => {
    //             Ok(Type::Hash)
    //         }
    //         Token::StringType { .. } => Ok(Type::String),
    //         Token::CharType { .. } => Ok(Type::Char),
    //         Token::BoolType { .. } => Ok(Type::Bool),
    //         Token::CustomType { .. } => Ok(Type::UserDefined),
    //         Token::SelfType { .. } => Ok(Type::SelfType),
    //         Token::LParen { .. } => {
    //             if let Ok(Token::RParen { .. }) = self.peek_ahead_by(1) {
    //                 Ok(Type::UnitType)
    //             } else {
    //                 Ok(Type::Tuple)
    //             }
    //         }
    //         Token::LBracket { .. } => Ok(Type::Array),
    //         Token::Func { .. } => Ok(Type::Function),
    //         Token::Ampersand { .. } => Ok(Type::Reference),
    //         _ => {
    //             self.log_error(ParserErrorKind::UnexpectedToken {
    //                 expected: "type annotation".to_string(),
    //                 found: token,
    //             });
    //             Err(ErrorsEmitted(()))
    //         }
    //     }
    // }
}
