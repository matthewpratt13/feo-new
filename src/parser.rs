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
mod path_type;
mod patt;
mod precedence;
mod test_utils;
mod ty;
mod visibility;

use std::collections::HashMap;

use crate::{
    ast::{
        AliasDecl, ArrayExpr, AssigneeExpr, AssignmentExpr, BinaryExpr, BlockExpr, BreakExpr,
        CallExpr, ClosureExpr, ComparisonExpr, CompoundAssignmentExpr, ConstantDecl, ContinueExpr,
        Delimiter, DereferenceExpr, EnumDef, Expression, FieldAccessExpr, ForInExpr, FunctionItem,
        GroupedExpr, GroupedPatt, Identifier, IdentifierPatt, IfExpr, ImportDecl, IndexExpr,
        InherentImplDef, Item, Keyword, LetStmt, Literal, MappingExpr, MatchExpr, MethodCallExpr,
        ModuleItem, NoneExpr, NonePatt, OuterAttr, PathExpr, PathPatt, Pattern, RangeExpr, RangeOp,
        RangePatt, ReferenceExpr, ReferencePatt, RestPatt, ResultExpr, ResultPatt, ReturnExpr,
        SomeExpr, SomePatt, Statement, StaticVarDecl, StructDef, StructExpr, StructPatt, TraitDef,
        TraitImplDef, TupleExpr, TupleIndexExpr, TuplePatt, TupleStructDef, TupleStructPatt,
        TypeCastExpr, UnaryExpr, UnderscoreExpr, UnwrapExpr, ValueExpr, Visibility, WhileExpr,
        WildcardPatt,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    logger::{LogLevel, LogMsg, Logger},
    span::Position,
    token::{Token, TokenStream, TokenType},
};

use self::item::{ParseDeclItem, ParseDefItem};
pub(crate) use self::parse::{
    ParseConstructExpr, ParseControlExpr, ParseOperatorExpr, ParsePattern, ParseSimpleExpr,
    ParseStatement,
};
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
    Unary,       // `&`, `*`, `-`
    TupleIndex,  // `.`
    FieldAccess, // `.`
    MethodCall,  // `.`
}

#[allow(dead_code)]
struct Module {
    items: Vec<Item>,
}

/// Parser struct that stores a stream of tokens and contains methods to parse expressions,
/// statements and items, as well as helper methods and error handling functionality.
pub(crate) struct Parser {
    stream: TokenStream,
    current: usize,
    precedences: HashMap<Token, Precedence>, // map tokens to corresponding precedence levels
    context: ParserContext,                  // keep track of the current parsing context
    errors: Vec<CompilerError<ParserErrorKind>>, // store parser errors
    logger: Logger,                          // log events and errors for easy debugging
}

impl Parser {
    /// Create a new `Parser` instance.
    /// Initialize an empty `Vec` to store potential errors that occur during parsing.
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
        for t in tokens.to_vec() {
            match &t.token_type() {
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
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Main parsing function that returns the parsed tokens as a `Module`.
    #[allow(dead_code)]
    fn parse_module(&mut self) -> Result<Module, ErrorsEmitted> {
        let mut items: Vec<Item> = Vec::new();

        // clear log messages, then log status info
        self.logger.clear_messages();
        self.logger
            .log(LogLevel::Info, LogMsg::from("starting to parse tokens"));

        while self.current < self.stream.tokens().len() {
            let item = self.parse_item()?;

            // log status info
            self.logger.log(
                LogLevel::Info,
                LogMsg::from(format!("parsed item: {:?}", &item)),
            );

            items.push(item);
        }

        // log status info
        self.logger
            .log(LogLevel::Info, LogMsg::from("reached end of file"));

        Ok(Module { items })
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
        ////////////////////////////////////////////////////////////////////////////////
        self.logger.log(
            LogLevel::Debug,
            LogMsg::from(format!(
                "entering `parse_expression()` with precedence: {:?}",
                &precedence
            )),
        );
        self.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

        let mut left_expr = self.parse_prefix()?; // start with prefix expression

        ////////////////////////////////////////////////////////////////////////////////
        self.logger
            .log(LogLevel::Debug, LogMsg::from("exited `parse_prefix()`"));
        self.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

        // repeatedly call `parse_infix()` while the precedence of the current token is higher
        // than the input precedence
        while precedence < self.peek_precedence() {
            if let Some(infix_parser) = self.parse_infix()? {
                left_expr = infix_parser(self, left_expr)?; // parse infix expressions
            }
        }

        ////////////////////////////////////////////////////////////////////////////////
        self.logger.log(
            LogLevel::Debug,
            LogMsg::from("exiting `parse_expression()`"),
        );
        self.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

        // return parsed expression
        Ok(left_expr)
    }

    /// Parse the basic building blocks of expressions (e.g., grouped expressions, identifiers
    /// and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_primary()`"));
        self.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

        match self.current_token() {
            Some(Token::Identifier { .. }) => Ok(Expression::Path(PathExpr::parse(self)?)),
            Some(Token::IntLiteral { value, .. }) => Ok(Expression::Literal(Literal::Int(*value))),
            Some(Token::UIntLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::UInt(*value)))
            }
            Some(Token::BigUIntLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::BigUInt(*value)))
            }
            Some(Token::ByteLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::Byte(*value)))
            }
            Some(Token::BytesLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::Bytes(*value)))
            }
            Some(Token::HashLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::Hash(*value)))
            }
            Some(Token::StrLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::Str(value.clone())))
            }
            Some(Token::CharLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::Char(*value)))
            }
            Some(Token::BoolLiteral { value, .. }) => {
                Ok(Expression::Literal(Literal::Bool(*value)))
            }
            Some(Token::LParen { .. }) => {
                let expr = GroupedExpr::parse(self)?;
                self.next_token();
                Ok(Expression::Grouped(expr))
            }
            _ => {
                // log the error and advance the parser, then return `Err(ErrorsEmitted)`
                self.log_unexpected_token("literal, identifier or grouped expression");
                Err(ErrorsEmitted)
            }
        }
    }

    /// Parse prefix expressions (e.g., unary operators, literals, identifiers and parentheses),
    /// where the respective token appears at the beginning of an expression.
    /// Where applicable, check the current token and set the parser context based on
    /// surrounding tokens.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_prefix()`"));
        self.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

        let token = self.current_token().cloned();

        match &token {
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
                if name == "_" {
                    self.next_token();
                    Ok(Expression::Underscore(UnderscoreExpr {
                        underscore: Identifier::from(name),
                    }))
                } else {
                    match self.peek_ahead_by(1) {
                        Some(Token::LBrace { .. }) => match self.peek_behind_by(1) {
                            Some(
                                Token::Equals { .. }
                                | Token::LParen { .. }
                                | Token::LBracket { .. }
                                | Token::LBrace { .. }
                                | Token::Comma { .. }
                                | Token::RBrace { .. }
                                | Token::Return { .. }
                                | Token::Semicolon { .. },
                            )
                            | None => Ok(Expression::Struct(StructExpr::parse(self)?)),

                            _ => self.parse_primary(),
                        },
                        Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) => {
                            Ok(Expression::Path(PathExpr::parse(self)?))
                        }
                        _ => self.parse_primary(),
                    }
                }
            }
            Some(Token::SelfType { .. }) => {
                if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    Ok(Expression::Struct(StructExpr::parse(self)?))
                } else {
                    Ok(Expression::Path(PathExpr::parse(self)?))
                }
            }
            Some(Token::SelfKeyword { .. } | Token::Package { .. } | Token::Super { .. }) => {
                Ok(Expression::Path(PathExpr::parse(self)?))
            }
            Some(Token::Minus { .. }) => {
                if self.context == ParserContext::Unary {
                    Ok(Expression::Unary(UnaryExpr::parse(self)?))
                } else {
                    self.set_context(ParserContext::Unary);
                    self.parse_expression(Precedence::Difference)
                }
            }

            Some(Token::Bang { .. }) => Ok(Expression::Unary(UnaryExpr::parse(self)?)),

            Some(Token::Ampersand { .. }) => {
                if self.context == ParserContext::Unary {
                    Ok(Expression::Reference(ReferenceExpr::parse(self)?))
                } else {
                    self.set_context(ParserContext::Unary);
                    self.parse_expression(Precedence::Unary)
                }
            }

            Some(Token::AmpersandMut { .. }) => {
                Ok(Expression::Reference(ReferenceExpr::parse(self)?))
            }

            Some(Token::Asterisk { .. }) => {
                if self.context == ParserContext::Unary {
                    Ok(Expression::Dereference(DereferenceExpr::parse(self)?))
                } else {
                    self.set_context(ParserContext::Unary);
                    self.parse_expression(Precedence::Product)
                }
            }

            Some(Token::Unsafe { .. }) => Ok(Expression::Block(BlockExpr::parse(self)?)),

            Some(Token::LParen { .. }) => {
                if let Some(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    Ok(Expression::Tuple(TupleExpr::parse(self)?))
                } else {
                    self.parse_primary()
                }
            }

            Some(Token::LBrace { .. }) => match self.peek_ahead_by(2) {
                Some(Token::Colon { .. }) => Ok(Expression::Mapping(MappingExpr::parse(self)?)),
                _ => match self.peek_ahead_by(1) {
                    Some(Token::RBrace { .. }) => {
                        Ok(Expression::Mapping(MappingExpr::parse(self)?))
                    }
                    _ => Ok(Expression::Block(BlockExpr::parse(self)?)),
                },
            },

            Some(Token::LBracket { .. }) => Ok(Expression::Array(ArrayExpr::parse(self)?)),

            Some(Token::Pipe { .. }) => {
                if self.is_closure_with_params() {
                    self.set_context(ParserContext::Closure);
                    Ok(Expression::Closure(ClosureExpr::parse(self)?))
                } else if self.is_bitwise_or() {
                    self.set_context(ParserContext::BitwiseOr);
                    self.next_token();
                    let left = self.parse_prefix()?;
                    BinaryExpr::parse(self, left)
                } else {
                    self.log_error(ParserErrorKind::InvalidTokenContext { token });
                    Err(ErrorsEmitted)
                }
            }

            Some(Token::DblPipe { .. }) => {
                if self.is_closure_without_params() {
                    self.set_context(ParserContext::Closure);
                    Ok(Expression::Closure(ClosureExpr::parse(self)?))
                } else if self.is_logical_or() {
                    self.set_context(ParserContext::LogicalOr);
                    self.next_token();
                    let left = self.parse_prefix()?;
                    BinaryExpr::parse(self, left)
                } else {
                    self.log_error(ParserErrorKind::InvalidTokenContext { token });
                    Err(ErrorsEmitted)
                }
            }

            Some(Token::DblDot { .. }) => match (self.peek_behind_by(1), self.peek_ahead_by(1)) {
                (None, Some(Token::Semicolon { .. } | Token::EOF) | None) => {
                    let expr = RangeExpr {
                        from_expr_opt: None,
                        range_op: RangeOp::RangeExclusive,
                        to_expr_opt: None,
                    };
                    self.next_token();
                    Ok(Expression::Range(expr))
                }

                _ => Ok(Expression::Range(RangeExpr::parse_prefix(self)?)),
            },

            Some(Token::DotDotEquals { .. }) => {
                Ok(Expression::Range(RangeExpr::parse_prefix(self)?))
            }

            Some(Token::If { .. }) => Ok(Expression::If(IfExpr::parse(self)?)),

            Some(Token::Match { .. }) => Ok(Expression::Match(MatchExpr::parse(self)?)),

            Some(Token::For { .. }) => Ok(Expression::ForIn(ForInExpr::parse(self)?)),

            Some(Token::While { .. }) => Ok(Expression::While(WhileExpr::parse(self)?)),

            Some(Token::Some { .. }) => Ok(Expression::SomeExpr(SomeExpr::parse(self)?)),

            Some(Token::None { .. }) => {
                self.next_token();
                Ok(Expression::NoneExpr(NoneExpr {
                    kw_none: Keyword::None,
                }))
            }

            Some(Token::Ok { .. } | Token::Err { .. }) => {
                Ok(Expression::ResultExpr(ResultExpr::parse(self)?))
            }

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

            Some(Token::Return { .. }) => Ok(Expression::Return(ReturnExpr::parse(self)?)),

            Some(Token::EOF) | None => {
                // log the error, then return `Err(ErrorsEmitted)`
                self.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                // log the error and advance the parser, then return `Err(ErrorsEmitted)`
                self.log_error(ParserErrorKind::InvalidTokenContext { token });
                self.next_token();
                Err(ErrorsEmitted)
            }
        }
    }

    /// Map the current token to the appropriate infix parsing function.
    /// Return the function, which is based on the current context and the token's precedence.
    /// The function should take a `&mut Parser` and a left `Expression` as input,
    /// and should combine the left expression with the operator and a right expression.
    fn parse_infix(
        &mut self,
    ) -> Result<Option<fn(&mut Self, Expression) -> Result<Expression, ErrorsEmitted>>, ErrorsEmitted>
    {
        ////////////////////////////////////////////////////////////////////////////////
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_infix()`"));
        self.log_current_token(true);
        ////////////////////////////////////////////////////////////////////////////////

        let token = self.current_token().cloned();

        match &token {
            Some(Token::Dot { .. }) => {
                if self.is_tuple_index() {
                    self.set_context(ParserContext::TupleIndex);
                } else if self.is_method_call() {
                    self.set_context(ParserContext::MethodCall);
                } else if self.is_field_access() {
                    self.set_context(ParserContext::FieldAccess);
                } else {
                    self.log_error(ParserErrorKind::InvalidTokenContext { token });
                    self.set_context(ParserContext::Default)
                }

                self.next_token();

                match self.current_token() {
                    Some(Token::EOF) | None => {
                        self.log_unexpected_eoi();
                        return Err(ErrorsEmitted);
                    }

                    Some(Token::Identifier { .. } | Token::UIntLiteral { .. }) => {
                        match self.context {
                            ParserContext::FieldAccess => Ok(Some(FieldAccessExpr::parse)),
                            ParserContext::MethodCall => Ok(Some(MethodCallExpr::parse)),
                            ParserContext::TupleIndex => Ok(Some(TupleIndexExpr::parse)),
                            _ => Ok(None), // default to no infix parser
                        }
                    }

                    _ => {
                        self.log_unexpected_token(
                            "identifier or tuple index (unsigned decimal integer)",
                        );
                        Err(ErrorsEmitted)
                    }
                }
            }

            Some(Token::LParen { .. }) => Ok(Some(CallExpr::parse)),

            Some(Token::LBracket { .. }) => Ok(Some(IndexExpr::parse)),

            Some(Token::QuestionMark { .. }) => Ok(Some(UnwrapExpr::parse)),

            Some(Token::Ampersand { .. }) => {
                if self.context == ParserContext::Unary {
                    Ok(None)
                } else {
                    Ok(Some(BinaryExpr::parse))
                }
            }

            Some(Token::Minus { .. }) => {
                if self.context == ParserContext::Unary {
                    Ok(None)
                } else {
                    Ok(Some(BinaryExpr::parse))
                }
            }

            Some(Token::Asterisk { .. }) => {
                if self.context == ParserContext::Unary {
                    Ok(None)
                } else {
                    Ok(Some(BinaryExpr::parse))
                }
            }

            Some(Token::As { .. }) => Ok(Some(TypeCastExpr::parse)),

            Some(
                Token::Plus { .. }
                | Token::Slash { .. }
                | Token::Percent { .. }
                | Token::DblAsterisk { .. },
            ) => Ok(Some(BinaryExpr::parse)),

            Some(Token::DblLessThan { .. } | Token::DblGreaterThan { .. }) => {
                Ok(Some(BinaryExpr::parse))
            }

            Some(Token::Caret { .. }) => Ok(Some(BinaryExpr::parse)),

            Some(Token::Pipe { .. }) => {
                if self.context == ParserContext::Closure {
                    Ok(None)
                } else {
                    Ok(Some(BinaryExpr::parse))
                }
            }

            Some(
                Token::DblEquals { .. }
                | Token::BangEquals { .. }
                | Token::LessThan { .. }
                | Token::GreaterThan { .. }
                | Token::LessThanEquals { .. }
                | Token::GreaterThanEquals { .. },
            ) => Ok(Some(ComparisonExpr::parse)),

            Some(Token::DblAmpersand { .. }) => Ok(Some(BinaryExpr::parse)),

            Some(Token::DblPipe { .. }) => {
                if self.context == ParserContext::Closure {
                    Ok(None)
                } else {
                    Ok(Some(BinaryExpr::parse))
                }
            }

            Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) => Ok(Some(RangeExpr::parse)),

            Some(
                Token::PlusEquals { .. }
                | Token::MinusEquals { .. }
                | Token::AsteriskEquals { .. }
                | Token::SlashEquals { .. }
                | Token::PercentEquals { .. },
            ) => Ok(Some(CompoundAssignmentExpr::parse)),

            Some(Token::Equals { .. }) => Ok(Some(AssignmentExpr::parse)),

            Some(Token::EOF) | None => {
                ////////////////////////////////////////////////////////////////////////////////
                self.logger.log(
                    LogLevel::Debug,
                    LogMsg::from("no infix parsing function found"),
                );
                ////////////////////////////////////////////////////////////////////////////////

                Ok(None)
            }

            _ => {
                self.log_error(ParserErrorKind::InvalidTokenContext { token });
                self.next_token();

                ////////////////////////////////////////////////////////////////////////////////
                self.log_current_token(true);
                ////////////////////////////////////////////////////////////////////////////////

                Err(ErrorsEmitted)
            }
        }
    }

    /// Parse an expression and attempt to convert it to a value expression.
    fn parse_value_expr(&mut self, precedence: Precedence) -> Result<ValueExpr, ErrorsEmitted> {
        self.parse_expression(precedence)?.try_into().map_err(|e| {
            self.log_error(e);
            ErrorsEmitted
        })
    }

    /// Parse an expression and attempt to convert it to an assignee expression.
    fn parse_assignee_expr(
        &mut self,
        precedence: Precedence,
    ) -> Result<AssigneeExpr, ErrorsEmitted> {
        self.parse_expression(precedence)?.try_into().map_err(|e| {
            self.log_error(e);
            ErrorsEmitted
        })
    }

    ///////////////////////////////////////////////////////////////////////////
    // ITEM PARSING
    ///////////////////////////////////////////////////////////////////////////

    /// Parse an item (e.g., import declaration, function definition and struct definition).
    fn parse_item(&mut self) -> Result<Item, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_item()`"));
        self.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        let attributes_opt = collection::get_attributes(self, OuterAttr::outer_attr);

        let visibility = Visibility::visibility(self)?;

        match self.current_token() {
            Some(Token::Import { .. }) => {
                let import_decl = ImportDecl::parse(self, attributes_opt, visibility)?;
                Ok(Item::ImportDecl(import_decl))
            }
            Some(Token::Alias { .. }) => {
                let alias_decl = AliasDecl::parse(self, attributes_opt, visibility)?;
                Ok(Item::AliasDecl(alias_decl))
            }
            Some(Token::Const { .. }) => {
                let constant_decl = ConstantDecl::parse(self, attributes_opt, visibility)?;
                Ok(Item::ConstantDecl(constant_decl))
            }
            Some(Token::Static { .. }) => {
                let static_var_decl = StaticVarDecl::parse(self, attributes_opt, visibility)?;
                Ok(Item::StaticVarDecl(static_var_decl))
            }
            Some(Token::Module { .. }) => {
                let module_item = ModuleItem::parse(self, attributes_opt, visibility)?;
                Ok(Item::ModuleItem(module_item))
            }
            Some(Token::Trait { .. }) => {
                let trait_def = TraitDef::parse(self, attributes_opt, visibility)?;
                Ok(Item::TraitDef(trait_def))
            }
            Some(Token::Enum { .. }) => {
                let enum_def = EnumDef::parse(self, attributes_opt, visibility)?;
                Ok(Item::EnumDef(enum_def))
            }
            Some(Token::Struct { .. }) => match self.peek_ahead_by(2) {
                Some(Token::LBrace { .. }) => {
                    let struct_def = StructDef::parse(self, attributes_opt, visibility)?;
                    Ok(Item::StructDef(struct_def))
                }
                Some(Token::LParen { .. }) => {
                    let tuple_struct_def = TupleStructDef::parse(self, attributes_opt, visibility)?;
                    Ok(Item::TupleStructDef(tuple_struct_def))
                }
                _ => {
                    self.log_unexpected_token("`{` or `(`");
                    Err(ErrorsEmitted)
                }
            },
            Some(Token::Impl { .. }) => {
                if let Some(Token::For { .. }) = self.peek_ahead_by(2) {
                    Ok(Item::TraitImplDef(TraitImplDef::parse(
                        self,
                        attributes_opt,
                        visibility,
                    )?))
                } else {
                    Ok(Item::InherentImplDef(InherentImplDef::parse(
                        self,
                        attributes_opt,
                        visibility,
                    )?))
                }
            }
            Some(Token::Func { .. }) => Ok(Item::FunctionItem(FunctionItem::parse(
                self,
                attributes_opt,
                visibility,
            )?)),
            _ => {
                self.log_unexpected_token("item declaration or definition");
                Err(ErrorsEmitted)
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // STATEMENT PARSING
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a statement (i.e., let statement, item declaration / definition or expression).
    fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        self.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `parse_statement()`"),
        );
        self.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        match self.current_token() {
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

            Some(
                Token::If { .. } | Token::Match { .. } | Token::For { .. } | Token::While { .. },
            ) => Ok(Statement::Expression(
                self.parse_expression(Precedence::Lowest)?,
            )),

            _ => {
                let statement = Ok(Statement::Expression(
                    self.parse_expression(Precedence::Lowest)?,
                ));

                match self.current_token() {
                    Some(Token::Semicolon { .. }) => {
                        self.next_token();
                    }
                    Some(Token::RBrace { .. } | Token::EOF) | None => (),

                    _ => {
                        self.log_unexpected_token("`;` or `}`");
                        return Err(ErrorsEmitted);
                    }
                }

                statement
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // PATTERN PARSING
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a `Pattern` – used in match expressions, function definitions and elsewhere.
    fn parse_pattern(&mut self) -> Result<Pattern, ErrorsEmitted> {
        ////////////////////////////////////////////////////////////////////////////////
        self.logger
            .log(LogLevel::Debug, LogMsg::from("entering `parse_pattern()`"));
        self.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        let token = self.current_token().cloned();

        match &token {
            Some(Token::IntLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::Int(*value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    Ok(Pattern::RangePatt(RangePatt::parse_patt(self)?))
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::UIntLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::UInt(*value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    Ok(Pattern::RangePatt(RangePatt::parse_patt(self)?))
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }

            Some(Token::BigUIntLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::BigUInt(*value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    Ok(Pattern::RangePatt(RangePatt::parse_patt(self)?))
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::ByteLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::Byte(*value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    Ok(Pattern::RangePatt(RangePatt::parse_patt(self)?))
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::BytesLiteral { value, .. }) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Bytes(*value)))
            }
            Some(Token::HashLiteral { value, .. }) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Hash(*value)))
            }
            Some(Token::StrLiteral { value, .. }) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Str(value.clone())))
            }

            Some(Token::CharLiteral { value, .. }) => {
                let patt = Pattern::Literal(Literal::Char(*value));

                if let Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) =
                    self.peek_ahead_by(1)
                {
                    Ok(Pattern::RangePatt(RangePatt::parse_patt(self)?))
                } else {
                    self.next_token();
                    Ok(patt)
                }
            }
            Some(Token::BoolLiteral { value, .. }) => {
                self.next_token();
                Ok(Pattern::Literal(Literal::Bool(*value)))
            }
            Some(Token::LParen { .. }) => {
                if let Some(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    Ok(Pattern::TuplePatt(TuplePatt::parse_patt(self)?))
                } else {
                    let patt = GroupedPatt::parse_patt(self)?;
                    self.next_token();
                    Ok(Pattern::GroupedPatt(patt))
                }
            }
            Some(Token::Identifier { name, .. }) => {
                if name == "_" {
                    self.next_token();
                    Ok(Pattern::WildcardPatt(WildcardPatt {
                        underscore: Identifier::from(name),
                    }))
                } else {
                    match self.peek_ahead_by(1) {
                        Some(Token::LBrace { .. }) => {
                            Ok(Pattern::StructPatt(StructPatt::parse_patt(self)?))
                        }
                        Some(Token::LParen { .. }) => {
                            Ok(Pattern::TupleStructPatt(TupleStructPatt::parse_patt(self)?))
                        }
                        Some(Token::DblColon { .. }) => {
                            Ok(Pattern::PathPatt(PathPatt::parse_patt(self)?))
                        }
                        Some(Token::DblDot { .. } | Token::DotDotEquals { .. }) => {
                            Ok(Pattern::RangePatt(RangePatt::parse_patt(self)?))
                        }
                        _ => Ok(Pattern::IdentifierPatt(IdentifierPatt::parse_patt(self)?)),
                    }
                }
            }

            Some(Token::Ref { .. } | Token::Mut { .. }) => {
                Ok(Pattern::IdentifierPatt(IdentifierPatt::parse_patt(self)?))
            }

            Some(Token::SelfType { .. }) => match self.peek_ahead_by(1) {
                Some(Token::LBrace { .. }) => {
                    Ok(Pattern::StructPatt(StructPatt::parse_patt(self)?))
                }
                Some(Token::LParen { .. }) => {
                    Ok(Pattern::TupleStructPatt(TupleStructPatt::parse_patt(self)?))
                }
                _ => Ok(Pattern::PathPatt(PathPatt::parse_patt(self)?)),
            },
            Some(Token::SelfKeyword { .. } | Token::Package { .. } | Token::Super { .. }) => {
                Ok(Pattern::PathPatt(PathPatt::parse_patt(self)?))
            }
            Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) => {
                Ok(Pattern::ReferencePatt(ReferencePatt::parse_patt(self)?))
            }

            Some(Token::DblDot { .. }) => {
                self.next_token();
                Ok(Pattern::RestPatt(RestPatt {
                    dbl_dot: RangeOp::RangeExclusive,
                }))
            }

            Some(Token::DotDotEquals { .. }) => {
                Ok(Pattern::RangePatt(RangePatt::parse_patt(self)?))
            }

            Some(Token::Some { .. }) => Ok(Pattern::SomePatt(SomePatt::parse_patt(self)?)),

            Some(Token::None { .. }) => {
                self.next_token();
                Ok(Pattern::NonePatt(NonePatt {
                    kw_none: Keyword::None,
                }))
            }

            Some(Token::Ok { .. } | Token::Err { .. }) => {
                Ok(Pattern::ResultPatt(ResultPatt::parse_patt(self)?))
            }

            Some(Token::EOF) | None => {
                // log the error, then return `Err(ErrorsEmitted)`
                self.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => {
                // log the error and advance the parser, then return `Err(ErrorsEmitted)`
                self.log_error(ParserErrorKind::InvalidTokenContext { token });
                self.next_token();

                Err(ErrorsEmitted)
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // TOKEN RETRIEVAL
    ///////////////////////////////////////////////////////////////////////////

    /// Advance the parser to the next token (returns the current token).
    fn next_token(&mut self) -> Option<Token> {
        let token = self.current_token().cloned();

        if self.current < self.stream.tokens().len() {
            self.current += 1;

            ////////////////////////////////////////////////////////////////////////////////
            self.logger
                .log(LogLevel::Debug, LogMsg::from("consumed token"));
            ////////////////////////////////////////////////////////////////////////////////
        } else {
            // log warning
            self.logger.log(
                LogLevel::Warning,
                LogMsg::from("WARNING: reached end of tokens"),
            );
        }

        ////////////////////////////////////////////////////////////////////////////////
        self.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        token
    }

    /// Get the token at the current index in the `TokenStream`.
    fn current_token(&self) -> Option<&Token> {
        if self.current < self.stream.tokens().len() {
            self.stream.tokens().get(self.current)
        } else {
            // return `Token::EOF` instead of `None` to prevent unwrap errors
            Some(&Token::EOF)
        }
    }

    /// Peek at the token `num_tokens` ahead of the token at the current index in the `TokenStream`.
    fn peek_ahead_by(&self, num_tokens: usize) -> Option<&Token> {
        let i = self.current + num_tokens;

        if i < self.stream.tokens().len() {
            self.stream.tokens().get(i)
        } else {
            None
        }
    }

    /// Peek at the token `num_tokens` behind without consuming it.
    fn peek_behind_by(&self, num_tokens: usize) -> Option<&Token> {
        if self.current < num_tokens {
            None
        } else {
            Some(&self.stream.tokens()[self.current - num_tokens])
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // ERROR HANDLING
    ///////////////////////////////////////////////////////////////////////////

    /// Log information about an error that occurred during parsing, by pushing the error
    /// to the `errors` vector and providing information about error kind and position.
    pub(crate) fn log_error(&mut self, error_kind: ParserErrorKind) {
        let current = self.current;
        let tokens = self.stream.tokens();

        let pos = match current {
            _ if current < tokens.len() => tokens[current].span().start(),
            _ if current == tokens.len() && tokens.len() > 0 => tokens[current - 1].span().end(),
            _ => 0,
        };

        // create a new `CompilerError` and push it to the `errors` vector
        let error = CompilerError::new(error_kind, pos, &self.stream.span().input());
        self.errors.push(error.clone());

        // log the error as a message
        self.logger
            .log(LogLevel::Error, LogMsg::from(error.to_string()));
    }

    /// Utility function that is used to report the current token and its precedence for debugging.
    fn log_current_token(&mut self, log_precedence: bool) {
        let token = self.current_token().unwrap();
        let precedence = self.get_precedence(token);

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
            found: self.current_token().cloned(),
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
            position: expected.position(),
        });

        self.next_token();
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
            "patt" => self.log_error(ParserErrorKind::MissingPattern {
                expected: expected.to_string(),
            }),
            _ => self.logger.log(
                LogLevel::Error,
                LogMsg::from(format!("{ty} not found. Expected {expected}, found none")),
            ),
        }

        self.next_token();
    }

    /// Log error information when the source code has to an unexpected end.
    fn log_unexpected_eoi(&mut self) {
        self.log_error(ParserErrorKind::UnexpectedEndOfInput)
    }

    ///////////////////////////////////////////////////////////////////////////
    // PRECEDENCE AND POSITION GETTERS
    ///////////////////////////////////////////////////////////////////////////

    /// Retrieve the precedence for a given token (operator), considering the current context.
    /// If the current context is `ParserContent::FieldAccess`, the precedence for `Token::Dot`
    /// is `Precedence::FieldAccess`; if the current context is `ParserContext::MethodCall`,
    /// the precedence for `Token::Dot` is `Precedence::MethodCall`, etc.
    /// Return `Precedence::Lowest` if the input token has no assigned precedence.
    fn get_precedence(&self, token: &Token) -> Precedence {
        match token {
            Token::Dot { .. } => match &self.context {
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
            _ => *self.precedences.get(token).unwrap_or(&Precedence::Lowest),
        }
    }

    /// Get the precedence of the current token to be compared and consumed.
    fn peek_precedence(&self) -> Precedence {
        self.get_precedence(self.current_token().unwrap())
    }

    /// Get the current token's position in the token stream, formatted to include line
    /// and column data, and a snippet of the source code leading up to the current token.
    fn current_position(&self) -> Position {
        Position::new(
            self.current_token().unwrap().span().start(),
            &self.stream.span().input(),
        )
    }

    ///////////////////////////////////////////////////////////////////////////
    // ADDITIONAL HELPERS
    ///////////////////////////////////////////////////////////////////////////

    /// Determine if `Token::Dot` token indicates a tuple index operator (i.e., if it is
    /// followed by a digit).
    fn is_tuple_index(&self) -> bool {
        match (self.current_token(), self.peek_ahead_by(1)) {
            (Some(Token::Dot { .. }), Some(Token::UIntLiteral { .. })) => true,
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
            (Some(Token::Dot { .. }), Some(Token::Identifier { .. })) => {
                !self.is_method_call() && !self.is_tuple_index()
            }

            _ => false,
        }
    }

    /// Determine if `Token::Pipe` indicates a closure parameter delimiter.
    fn is_closure_with_params(&self) -> bool {
        match (
            self.peek_behind_by(1),
            self.current_token(),
            self.peek_ahead_by(1),
        ) {
            (
                Some(
                    Token::Equals { .. }
                    | Token::LParen { .. }
                    | Token::LBracket { .. }
                    | Token::LBrace { .. }
                    | Token::Comma { .. }
                    | Token::RBrace { .. }
                    | Token::Return { .. }
                    | Token::Semicolon { .. },
                )
                | None,
                Some(Token::Pipe { .. }),
                Some(
                    Token::Identifier { .. }
                    | Token::Ref { .. }
                    | Token::Mut { .. }
                    | Token::Pipe { .. },
                ),
            ) => true,
            _ => false,
        }
    }

    /// Determine if `Token::Pipe` indicates the bitwise `OR` operator.
    fn is_bitwise_or(&self) -> bool {
        !self.is_closure_with_params()
    }

    /// Determine if `Token::DblPipe` indicates an empty closure parameter list.
    fn is_closure_without_params(&self) -> bool {
        match (
            self.peek_behind_by(1),
            self.current_token(),
            self.peek_ahead_by(1),
        ) {
            (
                Some(
                    Token::Equals { .. }
                    | Token::LParen { .. }
                    | Token::LBracket { .. }
                    | Token::LBrace { .. }
                    | Token::Comma { .. }
                    | Token::RBrace { .. }
                    | Token::Return { .. }
                    | Token::Semicolon { .. },
                )
                | None,
                Some(Token::DblPipe { .. }),
                Some(
                    Token::Identifier { .. }
                    | Token::IntLiteral { .. }
                    | Token::UIntLiteral { .. }
                    | Token::BigUIntLiteral { .. }
                    | Token::HashLiteral { .. }
                    | Token::ByteLiteral { .. }
                    | Token::BytesLiteral { .. }
                    | Token::CharLiteral { .. }
                    | Token::StrLiteral { .. }
                    | Token::BoolLiteral { .. }
                    | Token::Bang { .. }
                    | Token::Minus { .. }
                    | Token::Ampersand { .. }
                    | Token::Asterisk { .. }
                    | Token::ThinArrow { .. }
                    | Token::Some { .. }
                    | Token::None { .. }
                    | Token::Ok { .. }
                    | Token::Err { .. }
                    | Token::LParen { .. }
                    | Token::LBracket { .. }
                    | Token::LBrace { .. },
                ),
            ) => true,
            _ => false,
        }
    }

    /// Determine if `Token::DblPipe` indicates the logical `OR` operator.
    fn is_logical_or(&self) -> bool {
        !self.is_closure_without_params()
    }
}

#[cfg(test)]
mod tests {
    use crate::logger::LogLevel;

    use super::{test_utils, Precedence};

    #[test]
    fn parse_break_expr() -> Result<(), ()> {
        let input = r#"break"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_continue_expr() -> Result<(), ()> {
        let input = r#"continue"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_underscore_expr() -> Result<(), ()> {
        let input = r#"_"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
