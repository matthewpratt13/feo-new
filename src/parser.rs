#![allow(dead_code)]
#![allow(unused_variables)]

mod array_expr;
mod binary_expr;
mod block_expr;
mod call_expr;
mod closure_expr;
mod field_access_expr;
mod grouped_expr;
mod index_expr;
mod item;
mod method_call_expr;
mod path_expr;
mod precedence;
mod range_expr;
mod statement;
mod struct_expr;
mod test_utils;
mod tuple_expr;
mod unary_expr;

use crate::{
    ast::{
        AliasDecl, ArrayExpr, BinaryExpr, BinaryOp, BlockExpr, BreakExpr, CallExpr, ClosureExpr,
        ConstantDecl, ContinueExpr, Declaration, Definition, Delimiter, EnumDef, Expression,
        ExpressionStmt, FieldAccessExpr, ForInStmt, FunctionDef, GroupedExpr, Identifier, IfStmt,
        ImportDecl, IndexExpr, InherentImplDef, Keyword, LetStmt, Literal, MatchStmt,
        MethodCallExpr, ModuleDef, PathExpr, PathPrefix, RangeExpr, RangeOp, ReturnExpr, Separator,
        Statement, StaticItemDecl, StructDef, StructExpr, TraitDef, TraitImplDef, TupleExpr,
        TupleIndexExpr, Type, TypeCastExpr, UnaryExpr, UnaryOp, UnderscoreExpr, UnwrapExpr,
        UnwrapOp, WhileStmt,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenStream},
};

pub use self::precedence::Precedence;
use self::{
    item::{ParseDeclaration, ParseDefinition},
    statement::ParseStatement,
};

/// Struct that stores a stream of tokens and contains methods to parse expressions,
/// statements and items, as well as helper methods and error handling capabilities.
#[derive(Debug)]
pub(crate) struct Parser {
    stream: TokenStream,
    current: usize,
    errors: Vec<CompilerError<ParserErrorKind>>,
}

impl Parser {
    /// Create a new `Parser` instance.
    /// Initialize an empty `Vec` to store parser errors.
    pub(crate) fn new(stream: TokenStream) -> Self {
        Parser {
            stream,
            current: 0,
            errors: Vec::new(),
        }
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Main parsing function that returns a `Vec<Statement>`.
    fn parse(&mut self) -> Result<Vec<Expression>, ErrorsEmitted> {
        let mut expressions: Vec<Expression> = Vec::new();
        while self.current < self.stream.tokens().len() {
            expressions.push(self.parse_expression(Precedence::Lowest)?);
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

            println!("CURRENT PRECEDENCE: {:?}", curr_precedence);
            println!("CURRENT TOKEN: {:?}", self.peek_current());

            if precedence < curr_precedence {
                left_expr = self.parse_infix(left_expr)?;
            } else {
                break;
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
            Some(Token::Identifier { name, .. }) => Ok(Expression::Path(PathExpr {
                root: PathPrefix::Identifier(Identifier(name)),
                tree_opt: None,
            })),
            Some(Token::IntLiteral { value, .. }) => Ok(Expression::Literal(Literal::Int(value))),
            Some(Token::UIntLiteral { value, .. }) => Ok(Expression::Literal(Literal::UInt(value))),
            Some(Token::BigUIntLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::BigUInt(value)))
            }
            Some(Token::ByteLiteral { value, .. }) => Ok(Expression::Literal(Literal::Byte(value))),
            Some(Token::BytesLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::Bytes(value)))
            }
            Some(Token::HashLiteral { value, .. }) => Ok(Expression::Literal(Literal::Hash(value))),
            Some(Token::StringLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::String(value)))
            }
            Some(Token::CharLiteral { value, .. }) => Ok(Expression::Literal(Literal::Char(value))),
            Some(Token::BoolLiteral { value, .. }) => Ok(Expression::Literal(Literal::Bool(value))),
            Some(Token::LParen { .. }) => Ok(Expression::Grouped(GroupedExpr::parse(self)?)),
            Some(t) => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier, `_`, literal or `(`".to_string(),
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

    /// Parse prefix expressions (e.g., integer literals, identifiers and parentheses),
    /// where the respective token type appears at the beginning of an expression.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_prefix()`");
        println!("CURRENT TOKEN: {:?}", self.peek_current());

        let token = self.peek_current();

        match token {
            Some(
                Token::IntLiteral { .. }
                | Token::UIntLiteral { .. }
                | Token::BigUIntLiteral { .. }
                | Token::HashLiteral { .. },
            ) => self.parse_primary(),
            Some(
                Token::ByteLiteral { .. }
                | Token::BytesLiteral { .. }
                | Token::StringLiteral { .. }
                | Token::CharLiteral { .. }
                | Token::BoolLiteral { .. },
            ) => self.parse_primary(),
            Some(Token::Identifier { name, .. }) => {
                if &name == "_" {
                    Ok(Expression::Underscore(UnderscoreExpr {
                        underscore: Separator::Underscore,
                    }))
                } else {
                    let root = PathPrefix::Identifier(Identifier(name));
                    let expr = PathExpr::parse(self, root)?;
                    Ok(Expression::Path(expr))
                }
            }
            Some(Token::SelfType { .. }) => {
                self.consume_token();
                Ok(Expression::Path(PathExpr {
                    root: PathPrefix::SelfType,
                    tree_opt: None,
                }))
            }
            Some(Token::SelfKeyword { .. }) => {
                self.consume_token();
                Ok(Expression::Path(PathExpr {
                    root: PathPrefix::SelfKw,
                    tree_opt: None,
                }))
            }
            Some(Token::Package { .. }) => {
                self.consume_token();
                Ok(Expression::Path(PathExpr::parse(
                    self,
                    PathPrefix::Package,
                )?))
            }
            Some(Token::Super { .. }) => {
                self.consume_token();
                Ok(Expression::Path(PathExpr::parse(self, PathPrefix::Super)?))
            }
            Some(Token::Minus { .. }) => {
                Ok(Expression::Unary(UnaryExpr::parse(self, UnaryOp::Negate)?))
            }
            Some(Token::Bang { .. }) => {
                Ok(Expression::Unary(UnaryExpr::parse(self, UnaryOp::Not)?))
            }
            Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) => Ok(Expression::Unary(
                UnaryExpr::parse(self, UnaryOp::Reference)?,
            )),
            Some(Token::Asterisk { .. }) => Ok(Expression::Unary(UnaryExpr::parse(
                self,
                UnaryOp::Dereference,
            )?)),
            Some(Token::LParen { .. }) => {
                if let Some(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    Ok(Expression::Tuple(TupleExpr::parse(self)?))
                } else {
                    self.parse_primary()
                }
            }
            Some(Token::LBracket { .. }) => Ok(Expression::Array(ArrayExpr::parse(self)?)),
            Some(Token::LBrace { .. }) => Ok(Expression::Block(BlockExpr::parse(self)?)),
            Some(Token::Pipe { .. } | Token::DblPipe { .. }) => {
                Ok(Expression::Closure(ClosureExpr::parse(self)?))
            }
            Some(Token::Dot { .. }) => {
                let next_token = self.peek_ahead_by(1);

                match next_token {
                    Some(Token::Identifier { .. } | Token::UIntLiteral { .. }) => {
                        self.consume_token();
                        Err(ErrorsEmitted(()))
                    }
                    Some(t) => {
                        self.log_error(ParserErrorKind::UnexpectedToken {
                            expected: "identifier or tuple index after `.`".to_string(),
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
            Some(Token::QuestionMark { .. }) => match self.peek_behind_by(1) {
                Some(Token::Identifier { .. }) => {
                    self.consume_token();
                    Err(ErrorsEmitted(()))
                }
                Some(t) => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "expression before `?`".to_string(),
                        found: t,
                    });
                    Err(ErrorsEmitted(()))
                }
                None => {
                    self.log_error(ParserErrorKind::TokenIndexOutOfBounds {
                        len: self.stream.tokens().len(),
                        i: self.current,
                    });
                    Err(ErrorsEmitted(()))
                }
            },
            Some(Token::DblDot { .. }) => {
                if let Some(
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
                        to_opt: Some(Box::new(expr)),
                    }))
                } else {
                    Ok(Expression::Range(RangeExpr {
                        from_opt: None,
                        op: RangeOp::RangeExclusive,
                        to_opt: None,
                    }))
                }
            }
            Some(Token::DotDotEquals { .. }) => {
                self.consume_token();
                let expr = self.parse_expression(Precedence::Range)?;

                Ok(Expression::Range(RangeExpr {
                    from_opt: None,
                    op: RangeOp::RangeInclusive,
                    to_opt: Some(Box::new(expr)),
                }))
            }
            Some(Token::Return { .. }) => {
                self.consume_token();
                let expr = self.parse_expression(Precedence::Lowest)?;

                Ok(Expression::Return(ReturnExpr {
                    kw_return: Keyword::Return,
                    expression: Box::new(expr),
                }))
            }
            Some(Token::Break { name, .. }) => {
                self.consume_token();

                Ok(Expression::Break(BreakExpr {
                    kw_break: Keyword::Break,
                }))
            }
            Some(Token::Continue { name, .. }) => {
                self.consume_token();
                Ok(Expression::Continue(ContinueExpr {
                    kw_continue: Keyword::Continue,
                }))
            }
            Some(t) => {
                self.log_error(ParserErrorKind::InvalidToken { token: t });
                Err(ErrorsEmitted(()))
            }

            None => {
                self.log_error(ParserErrorKind::UnexpectedEndOfInput);
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
            Some(Token::Plus { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Add,
            )?)),
            Some(Token::Minus { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Subtract,
            )?)),
            Some(Token::Asterisk { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Multiply,
            )?)),
            Some(Token::Slash { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Divide,
            )?)),
            Some(Token::Percent { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Modulus,
            )?)),
            Some(Token::DblEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Equal,
            )?)),
            Some(Token::BangEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::NotEqual,
            )?)),
            Some(Token::LessThan { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::LessThan,
            )?)),
            Some(Token::LessThanEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::LessEqual,
            )?)),
            Some(Token::GreaterThan { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::GreaterThan,
            )?)),
            Some(Token::GreaterThanEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::GreaterEqual,
            )?)),
            Some(Token::Equals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Assign,
            )?)),
            Some(Token::PlusEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::AddAssign,
            )?)),
            Some(Token::MinusEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::SubtractAssign,
            )?)),
            Some(Token::AsteriskEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::MultiplyAssign,
            )?)),
            Some(Token::SlashEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::DivideAssign,
            )?)),
            Some(Token::PercentEquals { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::ModulusAssign,
            )?)),
            Some(Token::DblAmpersand { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::LogicalAnd,
            )?)),
            Some(Token::DblPipe { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::LogicalOr,
            )?)),
            Some(Token::Ampersand { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::BitwiseAnd,
            )?)),
            Some(Token::Pipe { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::BitwiseOr,
            )?)),
            Some(Token::Caret { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::BitwiseXor,
            )?)),
            Some(Token::DblLessThan { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::ShiftLeft,
            )?)),
            Some(Token::DblGreaterThan { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::ShiftRight,
            )?)),
            Some(Token::LParen { .. }) => {
                let expr = CallExpr::parse(self, left_expr)?;

                Ok(Expression::Call(expr))
            }
            Some(Token::LBracket { .. }) => {
                let expr = IndexExpr::parse(self, left_expr)?;

                Ok(Expression::Index(expr))
            }
            Some(Token::LBrace { .. }) => {
                let expr = StructExpr::parse(self, left_expr)?;

                Ok(Expression::Struct(expr))
            }
            Some(Token::As { .. }) => {
                let new_type = self.get_type()?;

                let expr = TypeCastExpr {
                    operand: Box::new(left_expr),
                    kw_as: Keyword::As,
                    new_type,
                };

                Ok(Expression::TypeCast(expr))
            }
            Some(Token::QuestionMark { .. }) => Ok(Expression::Unwrap(UnwrapExpr {
                expression: Box::new(left_expr),
                op: UnwrapOp(()),
            })),
            Some(Token::Dot { .. }) => match self.peek_current() {
                Some(Token::Identifier { .. }) => match self.peek_ahead_by(1) {
                    Some(Token::LParen { .. }) => Ok(Expression::MethodCall(
                        MethodCallExpr::parse(self, left_expr)?,
                    )),
                    _ => Ok(Expression::FieldAccess(FieldAccessExpr::parse(
                        self, left_expr,
                    )?)),
                },
                Some(Token::UIntLiteral { .. }) => Ok(Expression::TupleIndex(
                    TupleIndexExpr::parse(self, left_expr)?,
                )),
                Some(t) => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or index".to_string(),
                        found: t,
                    });

                    Err(ErrorsEmitted(()))
                }
                None => {
                    self.log_error(ParserErrorKind::UnexpectedEndOfInput);
                    Err(ErrorsEmitted(()))
                }
            },
            Some(Token::DblDot { .. }) => {
                let expr = RangeExpr::parse(self, left_expr, RangeOp::RangeExclusive)?;

                Ok(Expression::Range(expr))
            }
            Some(Token::DotDotEquals { .. }) => {
                let expr = RangeExpr::parse(self, left_expr, RangeOp::RangeInclusive)?;

                Ok(Expression::Range(expr))
            }
            Some(t) => {
                self.log_error(ParserErrorKind::InvalidToken { token: t });
                Err(ErrorsEmitted(()))
            }
            None => {
                self.log_error(ParserErrorKind::UnexpectedEndOfInput);
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

            Token::Mod { .. }
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
            Token::Mod { .. } => Ok(Definition::Module(ModuleDef::parse(self)?)),
            Token::Trait { .. } => Ok(Definition::Trait(TraitDef::parse(self)?)),
            Token::Enum { .. } => Ok(Definition::Enum(EnumDef::parse(self)?)),
            Token::Struct { .. } => Ok(Definition::Struct(StructDef::parse(self)?)),
            Token::Impl { .. } => {
                if let Some(Token::For { .. }) = self.peek_ahead_by(2) {
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
    fn peek_ahead_by(&mut self, num_tokens: usize) -> Option<Token> {
        let i = self.current + num_tokens;
        self.stream.tokens().get(i).cloned()
    }

    /// Peek at the token `num_tokens` behind the token at the current position.
    fn peek_behind_by(&mut self, num_tokens: usize) -> Option<Token> {
        let i = self.current - num_tokens;
        self.stream.tokens().get(i).cloned()
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
    fn expect_token(&mut self, expected: Token) -> Result<Token, ErrorsEmitted> {
        let token = self.consume_token().ok_or({
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            ErrorsEmitted(())
        })?;

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
        let token = self.consume_token();

        match token {
            Some(Token::Import { .. }) => Ok(Keyword::Import),
            Some(Token::Mod { .. }) => Ok(Keyword::Module),
            Some(Token::Package { .. }) => Ok(Keyword::Package),
            Some(Token::SelfKeyword { .. }) => Ok(Keyword::KwSelf),
            Some(Token::SelfType { .. }) => Ok(Keyword::SelfType),
            Some(Token::Super { .. }) => Ok(Keyword::Super),
            Some(Token::Pub { .. }) => Ok(Keyword::Pub),
            Some(Token::As { .. }) => Ok(Keyword::As),
            Some(Token::Const { .. }) => Ok(Keyword::Const),
            Some(Token::Static { .. }) => Ok(Keyword::Static),
            Some(Token::Func { .. }) => Ok(Keyword::Func),
            Some(Token::Struct { .. }) => Ok(Keyword::Struct),
            Some(Token::Enum { .. }) => Ok(Keyword::Enum),
            Some(Token::Trait { .. }) => Ok(Keyword::Trait),
            Some(Token::Impl { .. }) => Ok(Keyword::Impl),
            Some(Token::If { .. }) => Ok(Keyword::If),
            Some(Token::Else { .. }) => Ok(Keyword::Else),
            Some(Token::Match { .. }) => Ok(Keyword::Match),
            Some(Token::Loop { .. }) => Ok(Keyword::Loop),
            Some(Token::For { .. }) => Ok(Keyword::For),
            Some(Token::In { .. }) => Ok(Keyword::In),
            Some(Token::While { .. }) => Ok(Keyword::While),
            Some(Token::Break { .. }) => Ok(Keyword::Break),
            Some(Token::Continue { .. }) => Ok(Keyword::Continue),
            Some(Token::Return { .. }) => Ok(Keyword::Return),
            Some(t) => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
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

    fn expect_delimiter(&mut self, expected: Token) -> Result<Delimiter, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Some(Token::LParen { .. }) => Ok(Delimiter::LParen),
            Some(Token::RParen { .. }) => Ok(Delimiter::RParen),
            Some(Token::LBracket { .. }) => Ok(Delimiter::LBracket),
            Some(Token::RBracket { .. }) => Ok(Delimiter::RBracket),
            Some(Token::LBrace { .. }) => Ok(Delimiter::LBrace),
            Some(Token::RBrace { .. }) => Ok(Delimiter::RBrace),
            Some(t) => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
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

    fn expect_binary_op(&mut self, expected: Token) -> Result<BinaryOp, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Some(Token::Plus { .. }) => Ok(BinaryOp::Add),
            Some(Token::Minus { .. }) => Ok(BinaryOp::Subtract),
            Some(Token::Asterisk { .. }) => Ok(BinaryOp::Multiply),
            Some(Token::Slash { .. }) => Ok(BinaryOp::Divide),
            Some(Token::Percent { .. }) => Ok(BinaryOp::Modulus),
            Some(Token::DblEquals { .. }) => Ok(BinaryOp::Equal),
            Some(Token::BangEquals { .. }) => Ok(BinaryOp::NotEqual),
            Some(Token::LessThan { .. }) => Ok(BinaryOp::LessThan),
            Some(Token::LessThanEquals { .. }) => Ok(BinaryOp::LessEqual),
            Some(Token::GreaterThan { .. }) => Ok(BinaryOp::GreaterThan),
            Some(Token::GreaterThanEquals { .. }) => Ok(BinaryOp::GreaterEqual),
            Some(Token::Equals { .. }) => Ok(BinaryOp::Assign),
            Some(Token::PlusEquals { .. }) => Ok(BinaryOp::AddAssign),
            Some(Token::MinusEquals { .. }) => Ok(BinaryOp::SubtractAssign),
            Some(Token::SlashEquals { .. }) => Ok(BinaryOp::DivideAssign),
            Some(Token::PercentEquals { .. }) => Ok(BinaryOp::ModulusAssign),
            Some(Token::DblAmpersand { .. }) => Ok(BinaryOp::LogicalAnd),
            Some(Token::DblPipe { .. }) => Ok(BinaryOp::LogicalOr),
            Some(Token::Ampersand { .. }) => Ok(BinaryOp::BitwiseAnd),
            Some(Token::Pipe { .. }) => Ok(BinaryOp::BitwiseOr),
            Some(Token::Caret { .. }) => Ok(BinaryOp::BitwiseXor),
            Some(Token::DblLessThan { .. }) => Ok(BinaryOp::ShiftLeft),
            Some(Token::DblGreaterThan { .. }) => Ok(BinaryOp::ShiftRight),
            Some(t) => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
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

    fn expect_separator(&mut self, expected: Token) -> Result<Separator, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Some(Token::Colon { .. }) => Ok(Separator::Colon),
            Some(Token::Semicolon { .. }) => Ok(Separator::Semicolon),
            Some(Token::Comma { .. }) => Ok(Separator::Comma),
            Some(Token::Dot { .. }) => Ok(Separator::Dot),
            Some(Token::DblColon { .. }) => Ok(Separator::DblColon),
            Some(Token::ColonColonAsterisk { .. }) => Ok(Separator::ColonColonAsterisk),
            Some(Token::ThinArrow { .. }) => Ok(Separator::ThinArrow),
            Some(Token::FatArrow { .. }) => Ok(Separator::FatArrow),
            Some(t) => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
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
    fn precedence(&mut self, token: &Token) -> Precedence {
        match token {
            Token::As { .. } => Precedence::TypeCast,
            Token::LParen { .. } => Precedence::Call,
            Token::LBracket { .. } => Precedence::Index,
            Token::Dot { .. } => match self.peek_ahead_by(1) {
                Some(Token::Identifier { .. }) => match self.peek_ahead_by(2) {
                    Some(Token::LParen { .. }) => Precedence::MethodCall,
                    _ => Precedence::FieldAccess,
                },
                Some(Token::UIntLiteral { .. }) => Precedence::Index,
                _ => Precedence::Lowest,
            },
            Token::DblColon { .. } => Precedence::Path,
            Token::ColonColonAsterisk { .. } => Precedence::Path,
            Token::Bang { .. } => Precedence::Unary,
            Token::Percent { .. } => Precedence::Remainder,
            Token::Ampersand { .. } => {
                if self.peek_behind_by(1).is_some() {
                    Precedence::BitwiseAnd
                } else {
                    Precedence::Unary
                }
            }
            Token::Asterisk { .. } => {
                if self.peek_behind_by(1).is_some() {
                    Precedence::Product
                } else {
                    Precedence::Unary
                }
            }
            Token::Plus { .. } => Precedence::Sum,
            Token::Minus { .. } => {
                if self.peek_behind_by(1).is_some() {
                    Precedence::Difference
                } else {
                    Precedence::Unary
                }
            }
            Token::Slash { .. } => Precedence::Quotient,
            Token::LessThan { .. } => Precedence::LessThan,
            Token::Equals { .. } => Precedence::Assignment,
            Token::GreaterThan { .. } => Precedence::GreaterThan,
            Token::QuestionMark { .. } => Precedence::Unwrap,
            Token::Caret { .. } => Precedence::BitwiseXor,
            Token::Pipe { .. } => {
                if self.peek_behind_by(1).is_some() {
                    Precedence::Lowest // closure with arguments
                } else {
                    Precedence::BitwiseOr
                }
            }
            Token::DblDot { .. } => Precedence::Range,
            Token::DotDotEquals { .. } => Precedence::Range,
            Token::BangEquals { .. } => Precedence::NotEqual,
            Token::PercentEquals { .. } => Precedence::CompoundAssignment,
            Token::DblAsterisk { .. } => Precedence::Exponentiation,
            Token::AsteriskEquals { .. } => Precedence::CompoundAssignment,
            Token::DblAmpersand { .. } => Precedence::LogicalAnd,
            Token::AmpersandMut { .. } => Precedence::Unary,
            Token::PlusEquals { .. } => Precedence::CompoundAssignment,
            Token::MinusEquals { .. } => Precedence::CompoundAssignment,
            Token::SlashEquals { .. } => Precedence::CompoundAssignment,
            Token::DblLessThan { .. } => Precedence::Shift,
            Token::LessThanEquals { .. } => Precedence::LessThanOrEqual,
            Token::DblEquals { .. } => Precedence::Equal,
            Token::DblGreaterThan { .. } => Precedence::Shift,
            Token::GreaterThanEquals { .. } => Precedence::GreaterThanOrEqual,
            Token::DblPipe { .. } => {
                if self.peek_behind_by(1).is_some() {
                    Precedence::Lowest // closure without arguments
                } else {
                    Precedence::LogicalOr
                }
            }
            _ => Precedence::Lowest,
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

    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    fn get_type(&mut self) -> Result<Type, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Some(Token::I32Type { .. }) => Ok(Type::I32),
            Some(Token::I64Type { .. }) => Ok(Type::I64),
            Some(Token::I128Type { .. }) => Ok(Type::I128),
            Some(Token::U8Type { .. }) => Ok(Type::U8),
            Some(Token::U16Type { .. }) => Ok(Type::U16),
            Some(Token::U32Type { .. }) => Ok(Type::U32),
            Some(Token::U64Type { .. }) => Ok(Type::U64),
            Some(Token::U128Type { .. }) => Ok(Type::U128),
            Some(Token::U256Type { .. }) => Ok(Type::U256),
            Some(Token::U512Type { .. }) => Ok(Type::U512),
            Some(Token::ByteType { .. }) => Ok(Type::Byte),
            Some(
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
            Some(Token::H160Type { .. }) => Ok(Type::H160),
            Some(Token::H256Type { .. }) => Ok(Type::H256),
            Some(Token::H512Type { .. }) => Ok(Type::H512),
            Some(Token::StringType { .. }) => Ok(Type::String),
            Some(Token::CharType { .. }) => Ok(Type::Char),
            Some(Token::BoolType { .. }) => Ok(Type::Bool),
            Some(Token::CustomType { .. }) => Ok(Type::UserDefined),
            Some(Token::SelfType { .. }) => Ok(Type::SelfType),
            Some(Token::LParen { .. }) => {
                if let Some(Token::RParen { .. }) = self.peek_ahead_by(1) {
                    Ok(Type::UnitType)
                } else {
                    Ok(Type::Tuple)
                }
            }
            Some(Token::LBracket { .. }) => Ok(Type::Array),
            Some(Token::Func { .. }) => Ok(Type::Function),
            Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) => Ok(Type::Reference),
            Some(Token::Identifier { .. }) => Ok(Type::UserDefined),
            Some(t) => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "type annotation".to_string(),
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
}

#[cfg(test)]
mod tests {
    use super::test_utils;

    #[test]
    fn test_unwrap_expr() -> Result<(), ()> {
        let input = r#"(x + 2)?"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn test_type_cast_expr() -> Result<(), ()> {
        let input = r#"x as u32"#;

        let mut parser = test_utils::get_parser(input);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
