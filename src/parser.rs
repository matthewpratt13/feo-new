//! # Parser
//!
//! Implements a combination of the recursive descent algorithm and Pratt parsing
//! (also known as top-down operator precedence parsing), where each operator is associated
//! with a precedence level, and the parsing functions recursively parse expressions based
//! on the precedence of the next token.
//!
//! Here are the benefits of Pratt parsing:
//!
//! - **Simplicity and Readability**: Pratt parsing is relatively simple to implement compared to
//! other parsing techniques like recursive descent or LR parsing. Its structure closely mirrors
//! the grammar rules and operator precedence hierarchy, making the parser code intuitive and easy
//! to understand.
//!
//! - **Efficiency**: Pratt parsing is typically more efficient than traditional recursive descent
//! parsing for expression parsing because it avoids the overhead of recursive function calls.
//! Instead, it uses a loop to iteratively parse the input tokens, resulting in faster parsing
//! times, especially for complex expressions.
//!
//! - **Modularity**: Pratt parsing encourages modular code design. Each operator is associated with
//! a parsing function, allowing easy addition or modification of operators without affecting other
//! parts of the parser. This modularity facilitates maintainability and extensibility of the parser.
//!
//! - **Customization**: Pratt parsing allows fine-grained control over operator precedence and
//! associativity. Parser developers can easily define custom precedence levels and
//! associativity rules for different operators, providing flexibility to support a wide range
//! of grammars and language constructs.
//!
//! - **Error Reporting**: Pratt parsing naturally supports good error reporting. Since it parses
//! expressions incrementally and detects syntax errors as soon as they occur, it can provide
//! precise error messages pinpointing the location of errors in the input expression.
//!
//! - **Ease of Debugging**: Pratt parsing simplifies debugging due to its clear separation of
//! parsing functions for different operators. Developers can easily trace the parsing process
//! and identify any issues by inspecting the individual parsing functions.

mod attribute;
mod collection;
mod expr;
mod item;
mod let_statement;
mod parse;
mod patt;
mod precedence;
mod test_utils;
mod ty;
mod visibility;

use std::collections::HashMap;

use crate::{
    ast::{
        ArrayExpr, AssignmentExpr, BinaryExpr, BlockExpr, BreakExpr, CallExpr, ClosureExpr,
        ComparisonExpr, CompoundAssignmentExpr, ContinueExpr, Delimiter, DereferenceExpr,
        DereferenceOp, Expression, FieldAccessExpr, ForInExpr, GroupedExpr, GroupedPatt,
        Identifier, IdentifierPatt, IfExpr, IndexExpr, Item, Keyword, LetStmt, Literal,
        MappingExpr, MatchExpr, MethodCallExpr, NoneExpr, NonePatt, PathExpr, PathPatt, PathPrefix,
        Pattern, RangeExpr, RangeOp, RangePatt, ReferenceExpr, ReferenceOp, ReferencePatt,
        RestPatt, ResultExpr, ResultPatt, ReturnExpr, SelfType, SomeExpr, SomePatt, Statement,
        StructExpr, StructPatt, TupleExpr, TupleIndexExpr, TuplePatt, TypeCastExpr, UnaryExpr,
        UnaryOp, UnderscoreExpr, UnwrapExpr, WhileExpr, WildcardPatt,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    logger::{LogLevel, LogMsg, Logger},
    token::{Token, TokenStream, TokenType},
};

pub(crate) use self::parse::{ParseConstruct, ParseControl, ParseOperation, ParseStatement};
pub(crate) use self::precedence::Precedence;

/// Enum representing the different parsing contexts in which tokens can be interpreted.
/// This context can influence the precedence of specific tokens according to their role
/// in the current expression or statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParserContext {
    Default,
    Closure,     // `|` or `||`
    LogicalOr,   // `||`
    BitwiseOr,   // `|`
    Unary,       // `&` `*`, `-`
    TupleIndex,  // `.`
    FieldAccess, // `.`
    MethodCall,  // `.`
}

/// Parser struct that stores a stream of tokens and contains methods to parse expressions,
/// statements and items, as well as helper methods and error handling functionality.
#[derive(Debug)]
pub struct Parser {
    stream: TokenStream,
    current: usize,
    precedences: HashMap<Token, Precedence>, // map tokens to corresponding precedence levels
    context: ParserContext,                  // keep track of the current parsing context
    errors: Vec<CompilerError<ParserErrorKind>>, // store parser errors
    logger: Logger,
}

impl Parser {
    /// Create a new `Parser` instance.
    /// Initialize an empty `Vec` to store potentials errors that occur during parsing.
    fn new(stream: TokenStream, log_level: LogLevel) -> Self {
        let mut parser = Parser {
            stream: stream.clone(),
            current: 0,
            precedences: HashMap::new(),
            context: ParserContext::Default,
            errors: Vec::new(),
            logger: Logger::new(log_level),
        };

        parser.init_precedences(stream.tokens());
        parser
    }

    /// Define and initialize token precedence levels.
    fn init_precedences(&mut self, tokens: &[Token]) {
        self.logger
            .log(LogLevel::Debug, LogMsg::from("initializing precedences"));

        for t in tokens.to_vec() {
            match t.token_type() {
                TokenType::DblColon | TokenType::ColonColonAsterisk => {
                    self.precedences.insert(t, Precedence::Path)
                }
                TokenType::Dot => self.precedences.insert(t, Precedence::FieldAccess), // default
                TokenType::LParen => self.precedences.insert(t, Precedence::Call),
                TokenType::LBracket => self.precedences.insert(t, Precedence::Index),
                TokenType::QuestionMark => self.precedences.insert(t, Precedence::Unwrap),
                TokenType::Ampersand => self.precedences.insert(t, Precedence::BitwiseAnd), // default,
                TokenType::AmpersandMut => self.precedences.insert(t, Precedence::Unary),
                TokenType::Minus => self.precedences.insert(t, Precedence::Difference), // default
                TokenType::Asterisk => self.precedences.insert(t, Precedence::Product), // default
                TokenType::Bang => self.precedences.insert(t, Precedence::Unary),
                TokenType::Some | TokenType::None | TokenType::Ok | TokenType::Err => {
                    self.precedences.insert(t, Precedence::Unary)
                }
                TokenType::As => self.precedences.insert(t, Precedence::TypeCast),
                TokenType::DblAsterisk => self.precedences.insert(t, Precedence::Exponentiation),
                TokenType::Plus => self.precedences.insert(t, Precedence::Sum),
                TokenType::Slash => self.precedences.insert(t, Precedence::Quotient),
                TokenType::Percent => self.precedences.insert(t, Precedence::Remainder),
                TokenType::DblLessThan | TokenType::DblGreaterThan => {
                    self.precedences.insert(t, Precedence::Shift)
                }
                TokenType::Caret => self.precedences.insert(t, Precedence::BitwiseXor),
                TokenType::Pipe => self.precedences.insert(t, Precedence::BitwiseOr), // default
                TokenType::LessThan => self.precedences.insert(t, Precedence::LessThan),
                TokenType::GreaterThan => self.precedences.insert(t, Precedence::GreaterThan),
                TokenType::LessThanEquals => {
                    self.precedences.insert(t, Precedence::LessThanOrEqual)
                }
                TokenType::GreaterThanEquals => {
                    self.precedences.insert(t, Precedence::GreaterThanOrEqual)
                }
                TokenType::DblEquals => self.precedences.insert(t, Precedence::Equal),
                TokenType::BangEquals => self.precedences.insert(t, Precedence::NotEqual),
                TokenType::DblAmpersand => self.precedences.insert(t, Precedence::LogicalAnd),
                TokenType::DblPipe => self.precedences.insert(t, Precedence::LogicalOr), // default
                TokenType::DblDot | TokenType::DotDotEquals => {
                    self.precedences.insert(t, Precedence::Range)
                }
                TokenType::PlusEquals
                | TokenType::MinusEquals
                | TokenType::AsteriskEquals
                | TokenType::SlashEquals
                | TokenType::PercentEquals => {
                    self.precedences.insert(t, Precedence::CompoundAssignment)
                }
                TokenType::Equals => self.precedences.insert(t, Precedence::Assignment),
                // TokenType::Colon => self.precedences.insert(t, Precedence::Assignment),
                // TokenType::FatArrow => self.precedences.insert(t, Precedence::Assignment),
                _ => self.precedences.insert(t, Precedence::Lowest),
            };
        }
    }

    /// Set the parser's context based on the current expression or statement being parsed.
    /// This allows the parser to adjust the precedence of tokens based on the surrounding context.
    /// E.g., setting the context to `ParserContext::FieldAccess` in expressions involving
    /// struct instances.
    fn set_context(&mut self, context: ParserContext) {
        self.context = context;
        self.logger.log(
            LogLevel::Debug,
            LogMsg::from(format!("set context: {:?}", context)),
        );
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Main parsing function that returns the parsed tokens as a `Vec<Statement>`.
    #[allow(dead_code)]
    fn parse(&mut self) -> Result<Vec<Statement>, ErrorsEmitted> {
        let mut statements: Vec<Statement> = Vec::new();

        self.logger.clear_logs();

        self.logger
            .log(LogLevel::Info, LogMsg::from("starting to parse tokens"));

        while self.current < self.stream.tokens().len() {
            let statement = self.parse_statement()?;
            self.logger.log(
                LogLevel::Info,
                LogMsg::from(format!("parsed statement: {:?}", statement)),
            );
            statements.push(statement);
        }

        self.logger
            .log(LogLevel::Info, LogMsg::from("reached end of file"));
        Ok(statements)
    }

    ///////////////////////////////////////////////////////////////////////////
    // EXPRESSION PARSING
    ///////////////////////////////////////////////////////////////////////////

    /// Recursively parse an expression based on the next token's operator precedence.
    /// The input `precedence` argument is used to determine when to stop parsing infix expressions
    /// based on the current precedence level.
    /// Use `parse_infix()` to look up the appropriate parsing function based on the current token
    /// precedence and parser context.
    /// If an infix parsing function is found, it is called with the left expression to produce
    /// the next expression in the parse tree.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ErrorsEmitted> {
        self.logger.log(
            LogLevel::Debug,
            LogMsg::from(format!(
                "entering `parse_expression()` with precedence: {:?}",
                precedence
            )),
        );

        self.log_current_token(true);

        let mut left_expr = self.parse_prefix()?; // start with prefix expression
        self.logger
            .log(LogLevel::Debug, LogMsg::from("exited `parse_prefix()`"));
        self.log_current_token(true);

        // repeatedly call `parse_infix()` while the precedence of the current token is higher
        // than the input precedence
        while precedence < self.peek_precedence() {
            self.log_current_token(true);
            self.logger.log(
                LogLevel::Debug,
                LogMsg::from("current precedence >= input precedence"),
            );

            if let Some(infix_parser) = self.parse_infix() {
                left_expr = infix_parser(self, left_expr)?; // parse infix expressions
                self.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("exited infix parsing function"),
                );
                self.log_current_token(true);
            } else {
                break;
            }
        }

        self.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `parse_expression()`"),
        );
        self.log_current_token(true);

        Ok(left_expr)
    }

    /// Parse the basic building blocks of expressions (e.g., grouped expressions, identifiers
    /// and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_primary()`"));
        self.log_current_token(true);

        match self.current_token() {
            Some(Token::Identifier { name, .. }) => {
                let expr = PathExpr::parse(self, PathPrefix::Identifier(Identifier(name)));
                expr
            }
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
                let expr = GroupedExpr::parse(self);
                self.next_token();
                expr
            }
            Some(_) => {
                self.log_unexpected_token("literal, identifier or grouped expression");
                Err(ErrorsEmitted)
            }
            None => {
                self.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
        }
    }

    /// Parse prefix expressions (e.g., unary operators, literals, identifiers and parentheses),
    /// where the respective token appears at the beginning of an expression.
    /// Where applicable, check the current token and set the parser context based on
    /// surrounding tokens.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorsEmitted> {
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_prefix()`"));
        self.log_current_token(true);

        match self.current_token() {
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
                let expr = self.parse_primary();
                self.next_token();
                expr
            }

            Some(Token::Identifier { name, .. }) => {
                if &name == "_" {
                    self.next_token();
                    Ok(Expression::Underscore(UnderscoreExpr {
                        underscore: Identifier(name),
                    }))
                } else if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    {
                        let expr = self.parse_primary();
                        self.next_token();

                        match self.peek_ahead_by(2) {
                            Some(Token::Colon { .. }) => {
                                let path = PathExpr {
                                    root: PathPrefix::Identifier(Identifier(name)),
                                    tree_opt: None,
                                    wildcard_opt: None,
                                };
                                StructExpr::parse(self, path)
                            }
                            Some(Token::FatArrow { .. } | Token::If { .. }) => {
                                if let Some(Token::LBrace { .. }) = self.current_token() {
                                    expr
                                } else {
                                    self.log_unexpected_token("`{`");
                                    Err(ErrorsEmitted)
                                }
                            }
                            _ => expr,
                        }
                    }
                } else if let Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_ahead_by(1)
                {
                    let expr = PathExpr::parse(self, PathPrefix::Identifier(Identifier(name)));
                    self.next_token();
                    expr
                } else {
                    let expr = self.parse_primary();
                    self.next_token();
                    expr
                }
            }
            Some(Token::SelfType { .. }) => {
                if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    let path = PathExpr {
                        root: PathPrefix::SelfType(SelfType),
                        tree_opt: None,
                        wildcard_opt: None,
                    };

                    self.next_token();
                    StructExpr::parse(self, path)
                } else {
                    self.next_token();
                    PathExpr::parse(self, PathPrefix::SelfType(SelfType))
                }
            }
            Some(Token::SelfKeyword { .. }) => {
                self.next_token();
                PathExpr::parse(self, PathPrefix::SelfKeyword)
            }
            Some(Token::Package { .. }) => {
                self.next_token();
                PathExpr::parse(self, PathPrefix::Package)
            }
            Some(Token::Super { .. }) => {
                self.next_token();
                PathExpr::parse(self, PathPrefix::Super)
            }
            Some(Token::Minus { .. }) => {
                if self.context == ParserContext::Unary {
                    UnaryExpr::parse(self, UnaryOp::Negate)
                } else {
                    self.set_context(ParserContext::Unary);
                    self.parse_expression(Precedence::Difference)
                }
            }

            Some(Token::Bang { .. }) => UnaryExpr::parse(self, UnaryOp::Not),

            Some(Token::Ampersand { .. }) => {
                if self.context == ParserContext::Unary {
                    ReferenceExpr::parse(self, ReferenceOp::Borrow)
                } else {
                    self.set_context(ParserContext::Unary);
                    self.parse_expression(Precedence::Unary)
                }
            }

            Some(Token::AmpersandMut { .. }) => {
                ReferenceExpr::parse(self, ReferenceOp::MutableBorrow)
            }

            Some(Token::Asterisk { .. }) => {
                if self.context == ParserContext::Unary {
                    DereferenceExpr::parse(self, DereferenceOp)
                } else {
                    self.set_context(ParserContext::Unary);
                    self.parse_expression(Precedence::Product)
                }
            }

            Some(Token::Unsafe { .. }) => BlockExpr::parse(self),

            Some(Token::LParen { .. }) => {
                if let Some(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    TupleExpr::parse(self)
                } else {
                    self.parse_primary()
                }
            }

            Some(Token::LBrace { .. }) => match self.peek_ahead_by(2) {
                Some(Token::Colon { .. }) => MappingExpr::parse(self),
                Some(Token::FatArrow { .. }) => MatchExpr::parse(self),
                _ => match self.peek_ahead_by(1) {
                    Some(Token::RBrace { .. }) => MappingExpr::parse(self),
                    _ => BlockExpr::parse(self),
                },
            },

            Some(Token::LBracket { .. }) => ArrayExpr::parse(self),

            Some(Token::Pipe { .. }) => {
                if self.is_closure_with_params() {
                    self.set_context(ParserContext::Closure);
                    ClosureExpr::parse(self)
                } else if self.is_bitwise_or() {
                    self.set_context(ParserContext::BitwiseOr);
                    self.next_token();
                    let left = self.parse_prefix()?;
                    BinaryExpr::parse(self, left)
                } else {
                    self.log_error(ParserErrorKind::InvalidTokenContext {
                        token: self.current_token(),
                    });
                    Err(ErrorsEmitted)
                }
            }

            Some(Token::DblPipe { .. }) => {
                if self.is_closure_without_params() {
                    self.set_context(ParserContext::Closure);
                    ClosureExpr::parse(self)
                } else if self.is_logical_or() {
                    self.set_context(ParserContext::LogicalOr);
                    self.next_token();
                    let left = self.parse_prefix()?;
                    BinaryExpr::parse(self, left)
                } else {
                    self.log_error(ParserErrorKind::InvalidTokenContext {
                        token: self.current_token(),
                    });
                    Err(ErrorsEmitted)
                }
            }

            Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) => {
                if self.peek_ahead_by(1).is_none() {
                    let expr = RangeExpr {
                        from_opt: None,
                        range_op: RangeOp::RangeExclusive,
                        to_opt: None,
                    };
                    self.next_token();
                    Ok(Expression::Range(expr))
                } else {
                    RangeExpr::parse_prefix(self)
                }
            }

            Some(Token::If { .. }) => IfExpr::parse(self),

            Some(Token::Match { .. }) => MatchExpr::parse(self),

            Some(Token::For { .. }) => ForInExpr::parse(self),

            Some(Token::While { .. }) => WhileExpr::parse(self),

            Some(Token::Some { .. }) => SomeExpr::parse(self),

            Some(Token::None { .. }) => {
                self.next_token();

                Ok(Expression::NoneExpr(NoneExpr {
                    kw_none: Keyword::None,
                }))
            }

            Some(Token::Ok { .. } | Token::Err { .. }) => ResultExpr::parse(self),

            Some(Token::Break { .. }) => {
                self.next_token();
                Ok(Expression::Break(BreakExpr {
                    kw_break: Keyword::Break,
                }))
            }

            Some(Token::Continue { .. }) => {
                self.next_token();
                Ok(Expression::Continue(ContinueExpr {
                    kw_continue: Keyword::Continue,
                }))
            }

            Some(Token::Return { .. }) => ReturnExpr::parse(self),

            Some(Token::EOF) | None => {
                self.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                self.log_error(ParserErrorKind::InvalidTokenContext {
                    token: self.current_token(),
                });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Map the current token to the appropriate infix parsing function.
    /// Return this function, which is based on the current context and the token's precedence.
    /// The function should take a `&mut Parser` and a left `Expression` as input,
    /// and should combine the left expression with the operator and a right expression.
    fn parse_infix(
        &mut self,
    ) -> Option<fn(&mut Self, Expression) -> Result<Expression, ErrorsEmitted>> {
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_infix()`"));
        self.log_current_token(true);

        match &self.current_token() {
            Some(Token::Dot { .. }) => {
                if self.is_tuple_index() {
                    self.set_context(ParserContext::TupleIndex);
                } else if self.is_method_call() {
                    self.set_context(ParserContext::MethodCall);
                } else if self.is_field_access() {
                    self.set_context(ParserContext::FieldAccess);
                } else {
                    self.set_context(ParserContext::Default)
                }

                self.next_token();

                match self.context {
                    ParserContext::FieldAccess => Some(FieldAccessExpr::parse),
                    ParserContext::MethodCall => Some(MethodCallExpr::parse),
                    ParserContext::TupleIndex => Some(TupleIndexExpr::parse),
                    _ => None, // default to no infix parser
                }
            }

            Some(Token::LParen { .. }) => Some(CallExpr::parse),

            Some(Token::LBracket { .. }) => Some(IndexExpr::parse),

            Some(Token::QuestionMark { .. }) => Some(UnwrapExpr::parse),

            Some(Token::Ampersand { .. }) => {
                if self.context == ParserContext::Unary {
                    None
                } else {
                    Some(BinaryExpr::parse)
                }
            }

            Some(Token::Minus { .. }) => {
                if self.context == ParserContext::Unary {
                    None
                } else {
                    Some(BinaryExpr::parse)
                }
            }

            Some(Token::Asterisk { .. }) => {
                if self.context == ParserContext::Unary {
                    None
                } else {
                    Some(BinaryExpr::parse)
                }
            }

            Some(Token::As { .. }) => Some(TypeCastExpr::parse),

            Some(
                Token::Plus { .. }
                | Token::Slash { .. }
                | Token::Percent { .. }
                | Token::DblAsterisk { .. },
            ) => Some(BinaryExpr::parse),

            Some(Token::DblLessThan { .. } | Token::DblGreaterThan { .. }) => {
                Some(BinaryExpr::parse)
            }

            Some(Token::Caret { .. }) => Some(BinaryExpr::parse),

            Some(Token::Pipe { .. }) => {
                if self.context == ParserContext::Closure {
                    None
                } else {
                    Some(BinaryExpr::parse)
                }
            }

            Some(
                Token::DblEquals { .. }
                | Token::BangEquals { .. }
                | Token::LessThan { .. }
                | Token::GreaterThan { .. }
                | Token::LessThanEquals { .. }
                | Token::GreaterThanEquals { .. },
            ) => Some(ComparisonExpr::parse),

            Some(Token::DblAmpersand { .. }) => Some(BinaryExpr::parse),

            Some(Token::DblPipe { .. }) => {
                if self.context == ParserContext::Closure {
                    None
                } else {
                    Some(BinaryExpr::parse)
                }
            }

            Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) => Some(RangeExpr::parse),

            Some(
                Token::PlusEquals { .. }
                | Token::MinusEquals { .. }
                | Token::AsteriskEquals { .. }
                | Token::SlashEquals { .. }
                | Token::PercentEquals { .. },
            ) => Some(CompoundAssignmentExpr::parse),

            Some(Token::Equals { .. }) => Some(AssignmentExpr::parse),

            Some(_) => {
                self.logger.log(
                    LogLevel::Debug,
                    LogMsg::from(
                        ParserErrorKind::InvalidTokenContext {
                            token: self.current_token(),
                        }
                        .to_string(),
                    ),
                );
                self.log_current_token(true);
                None
            }

            None => {
                self.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("no infix parsing function found"),
                );
                self.log_current_token(true);
                None
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // STATEMENT PARSING
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a statement (i.e., let statement, item or expression).
    fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        self.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `parse_statement()`"),
        );
        self.log_current_token(true);

        let token = self.current_token();

        match token {
            Some(Token::Let { .. }) => LetStmt::parse_statement(self),

            Some(
                Token::Import { .. }
                | Token::Alias { .. }
                | Token::Const { .. }
                | Token::Static { .. }
                | Token::Module { .. }
                | Token::Trait { .. }
                | Token::Enum { .. }
                | Token::Struct { .. }
                | Token::Impl { .. }
                | Token::Contract { .. }
                | Token::Library { .. }
                | Token::Script { .. }
                | Token::Interface { .. }
                | Token::Abstract { .. }
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
            ) => Item::parse_statement(self),

            _ => {
                let statement = Ok(Statement::Expression(
                    self.parse_expression(Precedence::Lowest)?,
                ));

                statement
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // PATTERN PARSING
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a `Pattern` – used in match expressions, function call expression and elsewhere.
    fn parse_pattern(&mut self) -> Result<Pattern, ErrorsEmitted> {
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_pattern()`"));
        self.log_current_token(true);

        match self.current_token() {
            Some(Token::IntLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::Int(value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    self.next_token();
                    RangePatt::parse(self, patt)
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::UIntLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::UInt(value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    self.next_token();
                    RangePatt::parse(self, patt)
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }

            Some(Token::BigUIntLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::BigUInt(value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    self.next_token();
                    RangePatt::parse(self, patt)
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::ByteLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::Byte(value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    self.next_token();
                    RangePatt::parse(self, patt)
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::BytesLiteral { value, .. }) => {
                self.next_token();

                Ok(Pattern::Literal(Literal::Bytes(value)))
            }
            Some(Token::HashLiteral { value, .. }) => {
                self.next_token();

                Ok(Pattern::Literal(Literal::Hash(value)))
            }
            Some(Token::StrLiteral { value, .. }) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Str(value)))
            }

            Some(Token::CharLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::Char(value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    self.next_token();
                    RangePatt::parse(self, patt)
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::BoolLiteral { value, .. }) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Bool(value)))
            }
            Some(Token::LParen { .. }) => {
                if let Some(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    TuplePatt::parse(self)
                } else {
                    let patt = GroupedPatt::parse(self);
                    self.next_token();
                    patt
                }
            }
            Some(Token::Identifier { name, .. }) => {
                if &name == "_" {
                    self.next_token();
                    Ok(Pattern::WildcardPatt(WildcardPatt {
                        underscore: Identifier(name),
                    }))
                } else if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    let patt =
                        PathPatt::parse(self, PathPrefix::Identifier(Identifier(name.clone())));
                    self.next_token();

                    if let Some(Token::Colon { .. }) = self.peek_ahead_by(2) {
                        let path = PathPatt {
                            root: PathPrefix::Identifier(Identifier(name)),
                            tree_opt: None,
                        };
                        StructPatt::parse(self, path)
                    } else {
                        patt
                    }
                } else if let Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_ahead_by(1)
                {
                    let patt = PathPatt::parse(self, PathPrefix::Identifier(Identifier(name)));
                    self.next_token();
                    patt
                } else if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    let patt = PathPatt::parse(self, PathPrefix::Identifier(Identifier(name)))?;
                    self.next_token();
                    RangePatt::parse(self, patt)
                } else {
                    IdentifierPatt::parse(self)
                }
            }

            Some(Token::Ref { .. } | Token::Mut { .. }) => IdentifierPatt::parse(self),

            Some(Token::SelfType { .. }) => {
                if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    let path = PathPatt {
                        root: PathPrefix::SelfType(SelfType),
                        tree_opt: None,
                    };

                    self.next_token();
                    StructPatt::parse(self, path)
                } else {
                    self.next_token();
                    PathPatt::parse(self, PathPrefix::SelfType(SelfType))
                }
            }
            Some(Token::SelfKeyword { .. }) => {
                self.next_token();
                PathPatt::parse(self, PathPrefix::SelfKeyword)
            }
            Some(Token::Package { .. }) => {
                self.next_token();
                PathPatt::parse(self, PathPrefix::Package)
            }
            Some(Token::Super { .. }) => {
                self.next_token();
                PathPatt::parse(self, PathPrefix::Super)
            }

            Some(Token::Ampersand { .. }) => ReferencePatt::parse(self, ReferenceOp::Borrow),
            Some(Token::AmpersandMut { .. }) => {
                ReferencePatt::parse(self, ReferenceOp::MutableBorrow)
            }

            Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) => {
                if self.peek_ahead_by(1).is_none() {
                    let patt = RestPatt {
                        dbl_dot: RangeOp::RangeExclusive,
                    };
                    self.next_token();
                    Ok(Pattern::RestPatt(patt))
                } else {
                    RangePatt::parse_prefix(self)
                }
            }

            Some(Token::Some { .. }) => SomePatt::parse(self),

            Some(Token::None { .. }) => {
                self.next_token();

                Ok(Pattern::NonePatt(NonePatt {
                    kw_none: Keyword::None,
                }))
            }

            Some(Token::Ok { .. } | Token::Err { .. }) => ResultPatt::parse(self),

            Some(Token::EOF) | None => {
                self.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                self.log_error(ParserErrorKind::InvalidTokenContext {
                    token: self.current_token(),
                });
                Err(ErrorsEmitted)
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // TOKEN RETRIEVAL
    ///////////////////////////////////////////////////////////////////////////

    /// Advance the parser to the next token (returns current token).
    fn next_token(&mut self) -> Option<Token> {
        if self.current < self.stream.tokens().len() {
            self.current += 1;

            self.logger
                .log(LogLevel::Debug, LogMsg::from("consumed token"));
        } else {
            self.logger.log(
                LogLevel::Warning,
                LogMsg::from("WARNING: reached end of tokens"),
            );
        }

        self.log_current_token(true);

        self.current_token()
    }

    /// Get the token at the current index in the `TokenStream`.
    fn current_token(&self) -> Option<Token> {
        if self.current < self.stream.tokens().len() {
            self.stream.tokens().get(self.current).cloned()
        } else {
            Some(Token::EOF)
        }
    }

    /// Peek at the token `num_tokens` ahead of the token at the current index in the `TokenStream`.
    fn peek_ahead_by(&self, num_tokens: usize) -> Option<Token> {
        let i = self.current + num_tokens;

        if i < self.stream.tokens().len() {
            self.stream.tokens().get(i).cloned()
        } else {
            None
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // ERROR HANDLING
    ///////////////////////////////////////////////////////////////////////////

    /// Log information about an error that occurred during parsing, by pushing the error
    /// to the `errors` vector and providing information about error kind and position.
    fn log_error(&mut self, error_kind: ParserErrorKind) {
        let i = if self.current < self.stream.tokens().len() {
            self.current
        } else if self.current == self.stream.tokens().len() && self.stream.tokens().len() > 0 {
            self.current - 1
        } else {
            0
        };

        let error = CompilerError::new(
            error_kind,
            &self.stream.span().input(),
            self.stream.tokens()[i].span().start() + 1,
        );

        self.logger
            .log(LogLevel::Error, LogMsg::from(error.to_string()));
        self.errors.push(error);
    }

    /// Utility function that is used to report the current token and its precedence for debugging.
    fn log_current_token(&mut self, log_precedence: bool) {
        let token = self.current_token();
        let precedence = self.get_precedence(&token.clone().unwrap());

        self.logger.log(
            LogLevel::Debug,
            LogMsg::from(format!("current token: {:?}", token)),
        );

        if log_precedence {
            self.logger.log(
                LogLevel::Debug,
                LogMsg::from(format!("current precedence: {:?}", precedence)),
            );
        }
    }

    /// Log error information on encountering an unexpected token by providing the expected token.
    fn log_unexpected_token(&mut self, expected: &str) {
        self.log_error(ParserErrorKind::UnexpectedToken {
            expected: expected.to_string(),
            found: self.current_token(),
        });

        self.next_token();
    }

    /// Log error information when an expected token is missing.
    fn log_missing_token(&mut self, expected: &str) {
        self.log_error(ParserErrorKind::MissingToken {
            expected: expected.to_string(),
        });

        self.next_token();
    }

    /// Log error information about an unmatched delimiter.
    fn log_unmatched_delimiter(&mut self, expected: &Delimiter) {
        self.log_error(ParserErrorKind::UnmatchedDelimiter {
            delim: format!("{}", *expected),
        });

        self.next_token();
    }

    /// Log error information on encountering an unexpected pattern by naming the expected pattern
    /// and providing what was found, without advancing the parser.
    fn log_unexpected_patt(&mut self, expected: &str, patt: Pattern) {
        self.log_error(ParserErrorKind::UnexpectedPattern {
            expected: expected.to_string(),
            found: format!("{}", patt),
        });
    }

    /// Log error information when an expected node is missing.
    fn log_missing(&mut self, ty: &str, expected: &str) {
        match ty {
            "expr" => self.log_error(ParserErrorKind::MissingExpression {
                expected: expected.to_string(),
            }),
            "item" => self.log_error(ParserErrorKind::MissingItem {
                expected: expected.to_string(),
            }),
            "type" => self.log_error(ParserErrorKind::MissingType {
                expected: expected.to_string(),
            }),
            _ => self.logger.log(
                LogLevel::Error,
                LogMsg::from(format!("{ty} not found. Expected {expected}, found none")),
            ),
        }
    }

    fn log_unexpected_eoi(&mut self) {
        self.log_error(ParserErrorKind::UnexpectedEndOfInput)
    }

    /// Retrieve a list of any errors that occurred during parsing.
    #[allow(dead_code)]
    pub(crate) fn errors(&self) -> &[CompilerError<ParserErrorKind>] {
        &self.errors
    }

    ///////////////////////////////////////////////////////////////////////////
    // PRECEDENCE RETRIEVAL
    ///////////////////////////////////////////////////////////////////////////

    /// Retrieve the precedence for a given token (operator), considering the current context.
    /// If the current context is `ParserContent::FieldAccess`, the precedence for `Token::Dot`
    /// is `Precedence::FieldAccess`; if the current context is `ParserContext::MethodCall`,
    /// the precedence for `Token::Dot` is `Precedence::MethodCall`, etc.
    /// Return `Precedence::Lowest` if the input token has no assigned precedence.
    fn get_precedence(&self, token: &Token) -> Precedence {
        match token {
            Token::Dot { .. } => match self.context {
                ParserContext::FieldAccess => Precedence::FieldAccess,
                ParserContext::MethodCall => Precedence::MethodCall,
                ParserContext::TupleIndex => Precedence::TupleIndex,
                _ => Precedence::FieldAccess,
            },
            Token::Ampersand { .. } => {
                if self.context == ParserContext::Unary {
                    Precedence::Unary
                } else {
                    Precedence::BitwiseAnd
                }
            }
            Token::Asterisk { .. } => {
                if self.context == ParserContext::Unary {
                    Precedence::Unary
                } else {
                    Precedence::Product
                }
            }
            Token::Minus { .. } => {
                if self.context == ParserContext::Unary {
                    Precedence::Unary
                } else {
                    Precedence::Difference
                }
            }
            Token::Pipe { .. } => {
                if self.context == ParserContext::Closure {
                    Precedence::Lowest
                } else {
                    Precedence::BitwiseOr
                }
            }
            Token::DblPipe { .. } => {
                if self.context == ParserContext::Closure {
                    Precedence::Lowest
                } else {
                    Precedence::LogicalOr
                }
            }
            Token::DblColon { .. } | Token::ColonColonAsterisk { .. } => Precedence::Path,
            Token::LParen { .. } => Precedence::Call,
            Token::LBracket { .. } => Precedence::Index,
            Token::QuestionMark { .. } => Precedence::Unwrap,
            Token::AmpersandMut { .. } => Precedence::Unary,
            Token::Bang { .. } => Precedence::Unary,
            Token::Some { .. } | Token::None { .. } | Token::Ok { .. } | Token::Err { .. } => {
                Precedence::Unary
            }
            Token::As { .. } => Precedence::TypeCast,
            Token::DblAsterisk { .. } => Precedence::Exponentiation,
            Token::Plus { .. } => Precedence::Sum,
            Token::Slash { .. } => Precedence::Quotient,
            Token::Percent { .. } => Precedence::Remainder,
            Token::DblLessThan { .. } | Token::DblGreaterThan { .. } => Precedence::Shift,
            Token::Caret { .. } => Precedence::BitwiseXor,
            Token::LessThan { .. } => Precedence::LessThan,
            Token::GreaterThan { .. } => Precedence::GreaterThan,
            Token::LessThanEquals { .. } => Precedence::LessThanOrEqual,
            Token::GreaterThanEquals { .. } => Precedence::GreaterThanOrEqual,
            Token::DblEquals { .. } => Precedence::Equal,
            Token::BangEquals { .. } => Precedence::NotEqual,
            Token::DblAmpersand { .. } => Precedence::LogicalAnd,
            Token::DblDot { .. } | Token::DotDotEquals { .. } => Precedence::Range,
            Token::PlusEquals { .. }
            | Token::MinusEquals { .. }
            | Token::AsteriskEquals { .. }
            | Token::SlashEquals { .. }
            | Token::PercentEquals { .. } => Precedence::CompoundAssignment,
            Token::Equals { .. } => Precedence::Assignment,
            // Token::Colon { .. } => Precedence::Assignment,
            // Token::FatArrow { .. } => Precedence::Assignment,
            _ => *self.precedences.get(token).unwrap_or(&Precedence::Lowest),
        }
    }

    /// Get the precedence of the next token
    fn peek_precedence(&mut self) -> Precedence {
        self.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `peek_precedence()`"),
        );

        let precedence = self.get_precedence(&self.current_token().unwrap());

        self.logger.log(
            LogLevel::Debug,
            LogMsg::from(format!("peeked precedence: {:?}", precedence)),
        );

        precedence
    }

    ///////////////////////////////////////////////////////////////////////////
    // ADDITIONAL HELPERS
    ///////////////////////////////////////////////////////////////////////////

    /// Determine if `Token::Dot` token indicates a tuple index operator (followed by a digit).
    fn is_tuple_index(&self) -> bool {
        match self.peek_ahead_by(1) {
            Some(Token::UIntLiteral { .. }) => true,
            _ => false,
        }
    }

    /// Determine if `Token::Dot` indicates a method call.
    fn is_method_call(&self) -> bool {
        match (
            self.current_token(),
            self.peek_ahead_by(1),
            self.peek_ahead_by(2),
        ) {
            (
                Some(Token::Dot { .. }),
                Some(Token::Identifier { .. }),
                Some(Token::LParen { .. }),
            ) => true, // `receiver.method()`
            _ => false,
        }
    }

    /// Determine if `Token::Dot` indicates field access.
    fn is_field_access(&self) -> bool {
        match (self.current_token(), self.peek_ahead_by(1)) {
            (Some(Token::Dot { .. }), Some(Token::Identifier { .. })) => true, // `object.field`
            _ => false,
        }
    }

    /// Determine if `Token::Pipe` indicates a closure parameter delimiter.
    fn is_closure_with_params(&self) -> bool {
        match (self.current_token(), self.peek_ahead_by(1)) {
            (Some(Token::Pipe { .. }), Some(Token::Identifier { .. })) => true,
            _ => false,
        }
    }

    /// Determine if `Token::Pipe` indicates the bitwise OR operator.
    fn is_bitwise_or(&self) -> bool {
        match (self.current_token(), self.peek_ahead_by(1)) {
            (Some(Token::Pipe { .. }), Some(Token::Identifier { .. })) => false,
            _ => true,
        }
    }

    /// Determine if `Token::DblPipe` indicates an empty closure parameter list.
    fn is_closure_without_params(&self) -> bool {
        match (self.current_token(), self.peek_ahead_by(1)) {
            (Some(Token::DblPipe { .. }), Some(Token::Identifier { .. })) => true,
            _ => false,
        }
    }

    /// Determine if `Token::DblPipe` indicates the logical OR operator.
    fn is_logical_or(&self) -> bool {
        match (self.current_token(), self.peek_ahead_by(1)) {
            (Some(Token::DblPipe { .. }), Some(Token::Identifier { .. })) => false,
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::logger::LogLevel;

    use super::test_utils;

    #[test]
    fn parse_break_expr() -> Result<(), ()> {
        let input = r#"break"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_continue_expr() -> Result<(), ()> {
        let input = r#"continue"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_underscore_expr() -> Result<(), ()> {
        let input = r#"_"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
