#![allow(dead_code)]
#![allow(unused_variables)]

mod alias_decl;
mod array_expr;
mod binary_expr;
mod block_expr;
mod call_expr;
mod closure_expr;
mod constant_decl;
mod enum_def;
mod field_access_expr;
mod for_in_expr;
mod function_def;
mod grouped_expr;
mod if_expr;
mod impl_def;
mod import_decl;
mod index_expr;
mod item;
mod match_expr;
mod method_call_expr;
mod module_def;
mod path_expr;
mod precedence;
mod range_expr;
mod result_expr;
mod return_expr;
mod some_expr;
mod statement;
mod static_item_decl;
mod struct_def;
mod struct_expr;
mod test_utils;
mod trait_def;
mod tuple_expr;
mod unary_expr;
mod while_expr;

use crate::{
    ast::{
        AliasDecl, ArrayExpr, BinaryExpr, BinaryOp, BlockExpr, BreakExpr, CallExpr, ClosureExpr,
        ConstantDecl, ContinueExpr, Delimiter, EnumDef, Expression, ExpressionStmt,
        FieldAccessExpr, ForInExpr, FunctionDef, FunctionOrMethodParam, GroupedExpr, Identifier,
        IfExpr, ImportDecl, IndexExpr, InherentImplDef, InnerAttr, Item, Keyword, LetStmt, Literal,
        MatchExpr, MethodCallExpr, ModuleDef, NoneExpr, OuterAttr, PathExpr, PathPrefix,
        PubPackageVis, RangeExpr, RangeOp, ResultExpr, ReturnExpr, Separator, SomeExpr, Statement,
        StaticItemDecl, StructDef, StructExpr, TraitDef, TraitImplDef, TupleExpr, TupleIndexExpr,
        Type, TypeCastExpr, UnaryExpr, UnaryOp, UnderscoreExpr, UnwrapExpr, UnwrapOp, Visibility,
        WhileExpr,
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
    fn parse(&mut self) -> Result<Vec<Statement>, ErrorsEmitted> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.current < self.stream.tokens().len() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        Ok(statements)
    }

    ///////////////////////////////////////////////////////////////////////////
    /// EXPRESSIONS
    ///////////////////////////////////////////////////////////////////////////

    /// Recursively parse an expression based on operator precedence.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_expression()`");
        println!("INPUT PRECEDENCE: {:?}\n", precedence);

        let mut left_expr = self.parse_prefix()?;

        println!("PREFIX EXPRESSION: {:?}", left_expr.clone());
        println!("CURRENT TOKEN: {:?}\n", self.peek_current());

        while let Some(t) = self.peek_current() {
            let curr_precedence = self.precedence(&t);

            println!("CURRENT PRECEDENCE: {:?}", curr_precedence);

            if precedence < curr_precedence {
                left_expr = self.parse_infix(left_expr)?;
                println!("INFIX EXPRESSION: {:?}", left_expr.clone());
                println!("CURRENT TOKEN: {:?}\n", self.peek_current());
            } else {
                break;
            }
        }

        println!("RETURNED EXPRESSION: {:?}", left_expr);
        println!("EXIT `parse_expression()`");
        println!("CURRENT TOKEN: {:?}\n", self.peek_current());

        Ok(left_expr)
    }

    /// Parse primary expressions (e.g., grouped expressions, identifiers and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_primary()`");
        println!("CURRENT TOKEN: {:?}\n", self.peek_current());

        let token = self.peek_current();

        match token {
            Some(Token::Identifier { name, .. }) => Ok(Expression::Path(PathExpr {
                root: PathPrefix::Identifier(Identifier(name)),
                tree_opt: None,
                wildcard_opt: None,
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
            Some(Token::StrLiteral { value, .. }) => Ok(Expression::Literal(Literal::Str(value))),
            Some(Token::CharLiteral { value, .. }) => Ok(Expression::Literal(Literal::Char(value))),
            Some(Token::BoolLiteral { value, .. }) => Ok(Expression::Literal(Literal::Bool(value))),
            Some(Token::LParen { .. }) => {
                self.consume_token();
                Ok(Expression::Grouped(GroupedExpr::parse(self)?))
            }
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
        println!("ENTER `parse_prefix()`");
        println!("CURRENT TOKEN: {:?}\n", self.peek_current());

        let token = self.peek_current();

        match token {
            Some(
                Token::IntLiteral { .. }
                | Token::UIntLiteral { .. }
                | Token::BigUIntLiteral { .. }
                | Token::HashLiteral { .. }
                | Token::ByteLiteral { .. }
                | Token::BytesLiteral { .. }
                | Token::StrLiteral { .. }
                | Token::CharLiteral { .. }
                | Token::BoolLiteral { .. },
            ) => {
                let lit = self.parse_primary();
                self.consume_token();
                lit
            }

            Some(Token::Identifier { name, .. }) => {
                if &name == "_" {
                    self.consume_token();
                    Ok(Expression::Underscore(UnderscoreExpr {
                        underscore: Separator::Underscore,
                    }))
                } else if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    self.consume_token();

                    match self.peek_ahead_by(2) {
                        Some(Token::Colon { .. }) => {
                            let path = PathExpr {
                                root: PathPrefix::Identifier(Identifier(name)),
                                tree_opt: None,
                                wildcard_opt: None,
                            };

                            Ok(Expression::Struct(StructExpr::parse(self, path)?))
                        }
                        _ => Ok(Expression::Path(PathExpr::parse(
                            self,
                            PathPrefix::Identifier(Identifier(name)),
                        )?)),
                    }
                } else if let Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_current()
                {
                    Ok(Expression::Path(PathExpr::parse(
                        self,
                        PathPrefix::Identifier(Identifier(name)),
                    )?))
                } else {
                    let id = self.parse_primary();
                    self.consume_token();
                    id
                }
            }
            Some(Token::SelfType { .. }) => {
                if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    self.consume_token();

                    let path = PathExpr::parse(self, PathPrefix::SelfType)?;

                    Ok(Expression::Struct(StructExpr::parse(self, path)?))
                } else {
                    Ok(Expression::Path(PathExpr {
                        root: PathPrefix::SelfType,
                        tree_opt: None,
                        wildcard_opt: None,
                    }))
                }
            }
            Some(Token::SelfKeyword { .. }) => {
                self.consume_token();
                Ok(Expression::Path(PathExpr::parse(
                    self,
                    PathPrefix::SelfKeyword,
                )?))
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
            Some(Token::Unsafe { .. }) => Ok(Expression::Block(BlockExpr::parse(self)?)),

            Some(Token::LParen { .. }) => {
                if let Some(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    Ok(Expression::Tuple(TupleExpr::parse(self)?))
                } else {
                    self.parse_primary()
                }
            }
            Some(Token::LBrace { .. }) => Ok(Expression::Block(BlockExpr::parse(self)?)),

            Some(Token::LBracket { .. }) => Ok(Expression::Array(ArrayExpr::parse(self)?)),

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
                    _ => {
                        let token = self.peek_ahead_by(1);

                        self.log_error(ParserErrorKind::UnexpectedToken {
                            expected: "identifier or unsigned integer after `.`".to_string(),
                            found: token,
                        });
                        Err(ErrorsEmitted(()))
                    }
                }
            }
            Some(Token::QuestionMark { .. }) => match self.peek_behind_by(1) {
                Some(Token::Identifier { .. }) => {
                    self.consume_token();
                    Err(ErrorsEmitted(()))
                }
                _ => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "expression before `?`".to_string(),
                        found: token,
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
            Some(Token::If { .. }) => Ok(Expression::If(IfExpr::parse(self)?)),

            // TODO: distinguish from `StructExpr` (i.e., after the `match` keyword)
            Some(Token::Match { .. }) => Ok(Expression::Match(MatchExpr::parse(self)?)),

            Some(Token::For { .. }) => Ok(Expression::ForIn(ForInExpr::parse(self)?)),

            Some(Token::While { .. }) => Ok(Expression::While(WhileExpr::parse(self)?)),

            Some(Token::Some { .. }) => Ok(Expression::SomeExpr(SomeExpr::parse(self)?)),

            Some(Token::None { .. }) => {
                self.consume_token();

                Ok(Expression::NoneExpr(NoneExpr {
                    kw_none: Keyword::None,
                }))
            }

            Some(Token::Ok { .. } | Token::Err { .. }) => {
                Ok(Expression::ResultExpr(ResultExpr::parse(self)?))
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

            Some(Token::Return { .. }) => Ok(Expression::Return(ReturnExpr::parse(self)?)),

            Some(t) => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "expression prefix".to_string(),
                    found: Some(t),
                });
                Err(ErrorsEmitted(()))
            }
            _ => {
                self.log_error(ParserErrorKind::UnexpectedEndOfInput);
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Parse infix expressions (e.g., binary operators), where the respective token type
    /// appears in the middle of an expression.
    fn parse_infix(&mut self, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        println!("ENTER `parse_infix()`");
        println!("CURRENT TOKEN: {:?}\n", self.peek_current());

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
            Some(Token::DblAsterisk { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
                self,
                left_expr,
                BinaryOp::Exponentiation,
            )?)),
            Some(Token::LParen { .. }) => {
                let expr = CallExpr::parse(self, left_expr)?;
                Ok(Expression::Call(expr))
            }
            Some(Token::LBracket { .. }) => {
                let expr = IndexExpr::parse(self, left_expr)?;
                Ok(Expression::Index(expr))
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
                _ => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or index".to_string(),
                        found: token,
                    });
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
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "expression infix".to_string(),
                    found: Some(t),
                });
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
        let token = self.peek_current();

        match token {
            Some(
                Token::Import { .. }
                | Token::Alias { .. }
                | Token::Const { .. }
                | Token::Static { .. }
                | Token::Mod { .. }
                | Token::Trait { .. }
                | Token::Enum { .. }
                | Token::Struct { .. }
                | Token::Impl { .. }
                | Token::Contract { .. }
                | Token::Library { .. }
                | Token::Script { .. }
                | Token::Interface { .. }
                | Token::Constructor { .. }
                | Token::Modifier { .. }
                | Token::Test { .. }
                | Token::View { .. }
                | Token::Extern { .. }
                | Token::Payable { .. }
                | Token::Event { .. }
                | Token::Error { .. }
                | Token::Storage { .. }
                | Token::Topic { .. }
                | Token::Calldata { .. }
                | Token::Pub { .. },
            ) => self.get_item(),

            Some(Token::Let { .. }) => Ok(Statement::Let(LetStmt::parse(self)?)),

            _ => Ok(Statement::Expression(ExpressionStmt::parse(self)?)),
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

        if i < self.stream.tokens().len() {
            self.stream.tokens().get(i).cloned()
        } else {
            None
        }
    }

    /// Peek at the token `num_tokens` behind the token at the current position.
    fn peek_behind_by(&mut self, num_tokens: usize) -> Option<Token> {
        if self.current >= num_tokens {
            self.stream.tokens().get(self.current - num_tokens).cloned()
        } else {
            None
        }
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

    fn expect_keyword(&mut self, expected: Token) -> Result<Keyword, ErrorsEmitted> {
        let token = self.consume_token();

        match token {
            Some(Token::Import { .. }) => Ok(Keyword::Import),
            Some(Token::Mod { .. }) => Ok(Keyword::Mod),
            Some(Token::Package { .. }) => Ok(Keyword::Package),
            Some(Token::SelfKeyword { .. }) => Ok(Keyword::SelfKeyword),
            Some(Token::SelfType { .. }) => Ok(Keyword::SelfType),
            Some(Token::Super { .. }) => Ok(Keyword::Super),
            Some(Token::Pub { .. }) => Ok(Keyword::Pub),
            Some(Token::As { .. }) => Ok(Keyword::As),
            Some(Token::Const { .. }) => Ok(Keyword::Const),
            Some(Token::Static { .. }) => Ok(Keyword::Static),
            Some(Token::Alias { .. }) => Ok(Keyword::Alias),
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
            Some(Token::Let { .. }) => Ok(Keyword::Let),
            Some(Token::Mut { .. }) => Ok(Keyword::Mut),
            Some(Token::Some { .. }) => Ok(Keyword::Some),
            Some(Token::None { .. }) => Ok(Keyword::None),
            Some(Token::Ok { .. }) => Ok(Keyword::Ok),
            Some(Token::Err { .. }) => Ok(Keyword::Err),
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
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: format!("`{:#?}`", expected),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
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

            Token::Some { .. } | Token::None { .. } | Token::Ok { .. } | Token::Err { .. } => {
                Precedence::Unary
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

    /// Log information about an error that occurred during parsing.
    fn log_error(&mut self, error_kind: ParserErrorKind) {
        let error = CompilerError::new(
            error_kind,
            &self.stream.span().input(),
            self.stream.tokens()[self.current - 1].span().start(),
        );
        self.errors.push(error);
    }

    fn log_unexpected_token(&mut self, expected: String) {
        self.log_error(ParserErrorKind::UnexpectedToken {
            expected,
            found: self.peek_current(),
        });

        self.consume_token();
    }

    fn log_missing_delimiter(&mut self, delim: char) {
        match delim {
            '(' | ')' | '[' | ']' | '{' | '}' => {
                self.consume_token();
                self.log_error(ParserErrorKind::MissingDelimiter { delim });
            }

            _ => self.log_unexpected_token(format!("`{}`", delim)),
        }
    }

    pub fn errors(&self) -> &[CompilerError<ParserErrorKind>] {
        &self.errors
    }

    /// Match a `Token` to a `Type` and return the `Type` or emit an error.
    fn get_type(&mut self) -> Result<Type, ErrorsEmitted> {
        println!("ENTER `get_type()`");
        println!("CURRENT TOKEN: {:?}", self.peek_current());
        let token = self.consume_token();

        match token {
            Some(Token::I32Type { name, .. }) => Ok(Type::I32(name)),
            Some(Token::I64Type { name, .. }) => Ok(Type::I64(name)),
            Some(Token::I128Type { name, .. }) => Ok(Type::I128(name)),
            Some(Token::U8Type { name, .. }) => Ok(Type::U8(name)),
            Some(Token::U16Type { name, .. }) => Ok(Type::U16(name)),
            Some(Token::U32Type { name, .. }) => Ok(Type::U32(name)),
            Some(Token::U64Type { name, .. }) => Ok(Type::U64(name)),
            Some(Token::U128Type { name, .. }) => Ok(Type::U128(name)),
            Some(Token::U256Type { name, .. }) => Ok(Type::U256(name)),
            Some(Token::U512Type { name, .. }) => Ok(Type::U512(name)),
            Some(Token::ByteType { name, .. }) => Ok(Type::Byte(name)),
            Some(Token::B2Type { name, .. }) => Ok(Type::B2(name)),
            Some(Token::B4Type { name, .. }) => Ok(Type::B4(name)),
            Some(Token::B8Type { name, .. }) => Ok(Type::B8(name)),
            Some(Token::B16Type { name, .. }) => Ok(Type::B16(name)),
            Some(Token::B32Type { name, .. }) => Ok(Type::B32(name)),
            Some(Token::H160Type { name, .. }) => Ok(Type::H160(name)),
            Some(Token::H256Type { name, .. }) => Ok(Type::H256(name)),
            Some(Token::H512Type { name, .. }) => Ok(Type::H512(name)),
            Some(Token::StrType { name, .. }) => Ok(Type::Str(name)),
            Some(Token::CharType { name, .. }) => Ok(Type::Char(name)),
            Some(Token::BoolType { name, .. }) => Ok(Type::Bool(name)),
            Some(Token::SelfType { name, .. }) => Ok(Type::SelfType(name)),
            Some(Token::LParen { .. }) => {
                if let Some(Token::RParen { .. }) = self.peek_current() {
                    self.consume_token();
                    Ok(Type::UnitType)
                } else {
                    let mut types: Vec<Type> = Vec::new();

                    loop {
                        if let Some(Token::Comma { .. }) = self.peek_current() {
                            self.consume_token();
                        }

                        if let Some(Token::RParen { .. }) = self.peek_current() {
                            break;
                        }

                        let ty = self.get_type()?;
                        types.push(ty);

                        let token = self.peek_current();

                        match token {
                            Some(Token::Comma { .. }) => {
                                self.consume_token();
                                continue;
                            }
                            Some(Token::RParen { .. }) => break,
                            Some(t) => self.log_error(ParserErrorKind::UnexpectedToken {
                                expected: "`,` or `)`".to_string(),
                                found: Some(t),
                            }),
                            None => {
                                self.log_missing_delimiter(')');
                            }
                        }
                    }

                    if let Some(Token::RParen { .. }) = self.peek_current() {
                        self.consume_token();
                    } else {
                        self.log_missing_delimiter(')');
                    }

                    Ok(Type::Tuple(types))
                }
            }
            Some(Token::LBracket { .. }) => {
                let ty = self.get_type()?;

                // if let Some(Token::LBracket { .. }) = self.peek_current() {
                //     self.consume_token();
                // } else {
                //     self.log_unexpected_token("`[`".to_string());
                // }

                if let Some(Token::Semicolon { .. }) = self.peek_current() {
                    self.consume_token();
                } else {
                    self.log_unexpected_token("`;`".to_string());
                }

                let num_elements =
                    if let Some(Token::UIntLiteral { value, .. }) = self.consume_token() {
                        Ok(value)
                    } else {
                        self.log_unexpected_token("uint literal".to_string());
                        Err(ErrorsEmitted(()))
                    }?;

                if let Some(Token::RBracket { .. }) = self.peek_current() {
                    self.consume_token();
                } else {
                    self.log_missing_delimiter(']');
                }

                Ok(Type::Array {
                    element_type: Box::new(ty),
                    num_elements,
                })
            }
            Some(Token::Func { .. }) => {
                let mut params: Vec<FunctionOrMethodParam> = Vec::new();

                let function_name = if let Some(Token::Identifier { name, .. }) = token {
                    Ok(Identifier(name))
                } else {
                    self.log_unexpected_token("identifier".to_string());
                    Err(ErrorsEmitted(()))
                }?;

                if let Some(Token::LBrace { .. }) = self.peek_current() {
                    self.consume_token();
                } else {
                    self.log_unexpected_token("`{`".to_string());
                };

                if let Some(Token::LParen { .. }) = self.peek_current() {
                    self.consume_token();
                } else {
                    self.log_unexpected_token("`(`".to_string());
                }

                // `&self` and `&mut self` can only occur as the first parameter in a method
                if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) =
                    self.peek_current()
                {
                    let param = FunctionOrMethodParam::parse(self)?;
                    params.push(param);
                }

                loop {
                    if let Some(Token::Comma { .. }) = self.peek_current() {
                        self.consume_token();
                    }

                    if let Some(Token::RParen { .. }) = self.peek_current() {
                        break;
                    }

                    let param = FunctionOrMethodParam::parse(self)?;
                    params.push(param);

                    let token = self.peek_current();

                    match token {
                        Some(Token::Comma { .. }) => {
                            self.consume_token();
                            continue;
                        }
                        Some(Token::RParen { .. }) => break,
                        Some(t) => self.log_unexpected_token("`,` or `)`".to_string()),
                        None => {
                            self.log_missing_delimiter(')');
                        }
                    }
                }

                if let Some(Token::RParen { .. }) = self.peek_current() {
                    self.consume_token();
                } else {
                    self.log_missing_delimiter(')');
                }

                let return_type_opt = if let Some(Token::ThinArrow { .. }) = self.peek_current() {
                    self.consume_token();
                    Some(Box::new(self.get_type()?))
                } else {
                    None
                };

                Ok(Type::Function {
                    function_name,
                    return_type_opt,
                })
            }
            Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) => {
                let inner_type = Box::new(self.get_type()?);
                Ok(Type::Reference(inner_type))
            }

            Some(Token::VecType { .. }) => {
                let _ = self.expect_binary_op(Token::LessThan {
                    punc: '<',
                    span: self.stream.span(),
                })?;

                let ty = self.get_type()?;

                let _ = self.expect_binary_op(Token::GreaterThan {
                    punc: '>',
                    span: self.stream.span(),
                })?;

                Ok(Type::Vec(Box::new(ty)))
            }

            Some(Token::MappingType { .. }) => {
                let _ = self.expect_binary_op(Token::LessThan {
                    punc: '<',
                    span: self.stream.span(),
                })?;

                let key_type = Box::new(self.get_type()?);

                let _ = self.expect_separator(Token::Comma {
                    punc: '.',
                    span: self.stream.span(),
                })?;

                let value_type = Box::new(self.get_type()?);

                let _ = self.expect_binary_op(Token::GreaterThan {
                    punc: '>',
                    span: self.stream.span(),
                })?;

                Ok(Type::Mapping {
                    key_type,
                    value_type,
                })
            }

            Some(Token::OptionType { .. }) => {
                let _ = self.expect_binary_op(Token::LessThan {
                    punc: '<',
                    span: self.stream.span(),
                })?;

                let ty = self.get_type()?;

                let _ = self.expect_binary_op(Token::GreaterThan {
                    punc: '>',
                    span: self.stream.span(),
                })?;

                Ok(Type::Option(Box::new(ty)))
            }

            Some(Token::ResultType { .. }) => {
                let _ = self.expect_binary_op(Token::LessThan {
                    punc: '<',
                    span: self.stream.span(),
                })?;

                let ok = Box::new(self.get_type()?);

                let _ = self.expect_separator(Token::Comma {
                    punc: '.',
                    span: self.stream.span(),
                })?;

                let err = Box::new(self.get_type()?);

                let _ = self.expect_binary_op(Token::GreaterThan {
                    punc: '>',
                    span: self.stream.span(),
                })?;

                Ok(Type::Result { ok, err })
            }

            Some(Token::Identifier { name, .. }) => Ok(Type::UserDefined(name)),

            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "type annotation".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    fn get_outer_attr(&self) -> Option<OuterAttr> {
        let token = self.peek_current();

        match token {
            Some(Token::Calldata { .. }) => Some(OuterAttr::Calldata),
            Some(Token::Constructor { .. }) => Some(OuterAttr::Constructor),
            Some(Token::Error { .. }) => Some(OuterAttr::Error),
            Some(Token::Event { .. }) => Some(OuterAttr::Event),
            Some(Token::Extern { .. }) => Some(OuterAttr::Extern),
            Some(Token::Modifier { .. }) => Some(OuterAttr::Modifier),
            Some(Token::Payable { .. }) => Some(OuterAttr::Payable),
            Some(Token::Storage { .. }) => Some(OuterAttr::Storage),
            Some(Token::Test { .. }) => Some(OuterAttr::Test),
            Some(Token::Topic { .. }) => Some(OuterAttr::Topic),
            Some(Token::View { .. }) => Some(OuterAttr::View),
            _ => None,
        }
    }

    fn get_inner_attr(&self) -> Option<InnerAttr> {
        let token = self.peek_current();

        match token {
            Some(Token::Contract { .. }) => Some(InnerAttr::Contract),
            Some(Token::Interface { .. }) => Some(InnerAttr::Interface),
            Some(Token::Library { .. }) => Some(InnerAttr::Library),
            Some(Token::Script { .. }) => Some(InnerAttr::Script),
            _ => None,
        }
    }

    fn get_visibility(&mut self) -> Result<Visibility, ErrorsEmitted> {
        let token = self.peek_current();

        match token {
            Some(Token::Pub { .. }) => {
                self.consume_token();

                match self.peek_current() {
                    Some(Token::LParen { .. }) => {
                        self.consume_token();

                        let kw_package = self.expect_keyword(Token::Package {
                            name: "package".to_string(),
                            span: self.stream.span(),
                        })?;

                        let close_paren = if let Some(Token::RParen { .. }) = self.consume_token() {
                            Ok(Delimiter::RParen)
                        } else {
                            self.log_missing_delimiter(')');
                            Err(ErrorsEmitted(()))
                        }?;

                        let pub_package = PubPackageVis {
                            kw_pub: Keyword::Pub,
                            open_paren: Delimiter::LParen,
                            kw_package,
                            close_paren,
                        };

                        Ok(Visibility::PubPackage(pub_package))
                    }
                    _ => Ok(Visibility::Pub),
                }
            }
            _ => Ok(Visibility::Private),
        }
    }

    fn get_item(&mut self) -> Result<Statement, ErrorsEmitted> {
        let mut outer_attributes: Vec<OuterAttr> = Vec::new();
        let mut inner_attributes: Vec<InnerAttr> = Vec::new();

        while let Some(oa) = self.get_outer_attr() {
            outer_attributes.push(oa);
            self.consume_token();
        }

        while let Some(ia) = self.get_inner_attr() {
            inner_attributes.push(ia);
            self.consume_token();
        }

        let visibility = self.get_visibility()?;

        let token = self.peek_current();

        match token {
            Some(Token::Import { .. }) => Ok(Statement::Item(Item::ImportDecl(ImportDecl::parse(
                self,
                outer_attributes,
                visibility,
            )?))),
            Some(Token::Alias { .. }) => Ok(Statement::Item(Item::AliasDecl(AliasDecl::parse(
                self,
                outer_attributes,
                visibility,
            )?))),
            Some(Token::Const { .. }) => Ok(Statement::Item(Item::ConstantDecl(
                ConstantDecl::parse(self, outer_attributes, visibility)?,
            ))),
            Some(Token::Static { .. }) => Ok(Statement::Item(Item::StaticItemDecl(
                StaticItemDecl::parse(self, outer_attributes, visibility)?,
            ))),
            Some(Token::Mod { .. }) => Ok(Statement::Item(Item::ModuleDef(Box::new(
                ModuleDef::parse(self, inner_attributes, visibility)?,
            )))),
            Some(Token::Trait { .. }) => Ok(Statement::Item(Item::TraitDef(TraitDef::parse(
                self,
                outer_attributes,
                visibility,
            )?))),
            Some(Token::Enum { .. }) => Ok(Statement::Item(Item::EnumDef(EnumDef::parse(
                self,
                outer_attributes,
                visibility,
            )?))),
            Some(Token::Struct { .. }) => Ok(Statement::Item(Item::StructDef(StructDef::parse(
                self,
                outer_attributes,
                visibility,
            )?))),

            Some(Token::Func { .. }) => Ok(Statement::Item(Item::FunctionDef(FunctionDef::parse(
                self,
                outer_attributes,
                visibility,
            )?))),
            Some(Token::Impl { .. }) => match self.peek_ahead_by(2) {
                Some(Token::For { .. }) => Ok(Statement::Item(Item::TraitImplDef(
                    TraitImplDef::parse(self, outer_attributes, visibility)?,
                ))),
                Some(Token::LBrace { .. }) => Ok(Statement::Item(Item::InherentImplDef(
                    InherentImplDef::parse(self, outer_attributes, visibility)?,
                ))),
                _ => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "`for` or `{`".to_string(),
                        found: token,
                    });
                    Err(ErrorsEmitted(()))
                }
            },
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "declaration or definition item".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::test_utils;

    #[test]
    fn parse_unwrap_expr() -> Result<(), ()> {
        let input = r#"(x + 2)?"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_type_cast_expr() -> Result<(), ()> {
        let input = r#"x as u64"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_break_expr() -> Result<(), ()> {
        let input = r#"break"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_continue_expr() -> Result<(), ()> {
        let input = r#"continue"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_underscore_expr() -> Result<(), ()> {
        let input = r#"_"#;

        let mut parser = test_utils::get_parser(input, false);

        let expressions = parser.parse();

        match expressions {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
