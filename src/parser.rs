//! Feo's parser module.
//! The parser implements a combination of the recursive descent algorithm and Pratt parsing
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

mod alias_decl;
mod array_expr;
mod assignment_expr;
mod binary_expr;
mod block_expr;
mod call_expr;
mod closure_expr;
mod constant_decl;
mod enum_def;
mod field_access_expr;
mod for_in_expr;
mod function_item;
mod grouped_expr;
mod if_expr;
mod impl_def;
mod import_decl;
mod index_expr;
mod item;
mod let_statement;
mod match_expr;
mod method_call_expr;
mod module_item;
mod path_expr;
mod precedence;
mod range_expr;
mod result_expr;
mod return_expr;
mod some_expr;
mod static_item_decl;
mod struct_def;
mod struct_expr;
mod test_utils;
mod trait_def;
mod tuple_expr;
mod ty;
mod type_cast_expr;
mod unary_expr;
mod unwrap_expr;
mod while_expr;

use std::collections::HashMap;

use crate::{
    ast::{
        AliasDecl, ArrayExpr, AssignmentExpr, BinaryExpr, BlockExpr, BorrowExpr, BreakExpr,
        CallExpr, ClosureExpr, ComparisonExpr, CompoundAssignmentExpr, ConstantDecl, ContinueExpr,
        Delimiter, DereferenceExpr, DereferenceOp, EnumDef, Expression, FieldAccessExpr, ForInExpr,
        FunctionItem, GroupedExpr, Identifier, IfExpr, ImportDecl, IndexExpr, InherentImplDef,
        InnerAttr, Item, Keyword, LetStmt, Literal, MatchArm, MatchExpr, MethodCallExpr,
        ModuleItem, NoneExpr, OuterAttr, PathExpr, PathPrefix, Pattern, PubPackageVis, RangeExpr,
        RangeOp, ReferenceOp, ResultExpr, ReturnExpr, SelfType, Separator, SomeExpr, Statement,
        StaticItemDecl, StructDef, StructExpr, TraitDef, TraitImplDef, TupleExpr, TupleIndexExpr,
        TupleStructDef, TypeCastExpr, UnaryExpr, UnaryOp, UnderscoreExpr, UnwrapExpr, Visibility,
        WhileExpr,
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenStream, TokenType},
};

pub use self::precedence::Precedence;
use self::{
    item::{ParseDeclaration, ParseDefinition},
    test_utils::log_token,
};

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
    MatchArm,
}

/// Parser struct that stores a stream of tokens and contains methods to parse expressions,
/// statements and items, as well as helper methods and error handling functionality.
#[derive(Debug)]
pub(crate) struct Parser {
    stream: TokenStream,
    current: usize,
    errors: Vec<CompilerError<ParserErrorKind>>, // store parser errors
    precedences: HashMap<Token, Precedence>,     // map tokens to corresponding precedence levels
    context: ParserContext,                      // keep track of the current parsing context
}

impl Parser {
    /// Create a new `Parser` instance.
    /// Initialize an empty `Vec` to store potentials errors that occur during parsing.
    pub(crate) fn new(stream: TokenStream) -> Self {
        let tokens = stream.tokens();
        let mut parser = Parser {
            stream,
            current: 0,
            errors: Vec::new(),
            precedences: HashMap::new(),
            context: ParserContext::Default,
        };

        parser.init_precedences(tokens);
        parser
    }

    /// Define and initialize token precedence levels.
    fn init_precedences(&mut self, tokens: Vec<Token>) {
        for t in tokens {
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
                TokenType::FatArrow => self.precedences.insert(t, Precedence::Assignment),

                _ => self.precedences.insert(t, Precedence::Lowest),
            };
        }
    }

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
            Token::LParen { .. } => Precedence::Call, // TODO: what about tuple structs?
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

    /// Set the parser's context based on the current expression or statement being parsed.
    /// This allows the parser to adjust the precedence of tokens based on the surrounding context.
    /// E.g., setting the context to `ParserContext::FieldAccess` in expressions involving
    /// struct instances.
    fn set_context(&mut self, context: ParserContext) {
        self.context = context;
    }

    ///////////////////////////////////////////////////////////////////////////

    /// Main parsing function that returns the parsed tokens as a `Vec<Statement>`
    #[allow(dead_code)]
    fn parse(&mut self) -> Result<Vec<Statement>, ErrorsEmitted> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.current < self.stream.tokens().len() {
            let statement = self.parse_statement()?;
            log_token(self, "exit `parse_statement()`", true);
            statements.push(statement);
        }

        log_token(self, "end of file", false);
        Ok(statements)
    }

    ///////////////////////////////////////////////////////////////////////////
    // EXPRESSIONS
    ///////////////////////////////////////////////////////////////////////////

    /// Recursively parse an expression based on the next token's operator precedence.
    /// The input `precedence` argument is used to determine when to stop parsing infix expressions
    /// based on the current precedence level.
    /// Use `parse_infix()` to look up the appropriate parsing function based on the current token
    /// precedence and parser context.
    /// If an infix parsing function is found, it is called with the left expression to produce
    /// the next expression in the parse tree.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ErrorsEmitted> {
        log_token(self, "enter `parse_expression()`", false);
        println!("input precedence: {:?}\n", precedence);

        let mut left_expr = self.parse_prefix()?; // start with prefix expression
        log_token(self, "exit `parse_prefix()`", true);

        // repeatedly call `parse_infix()` while the precedence of the current token is higher
        // than the input precedence
        while precedence < self.peek_precedence() {
            log_token(self, "current precedence > input precedence", true);

            if let Some(infix_parser) = self.parse_infix() {
                left_expr = infix_parser(self, left_expr)?; // parse infix expressions
                log_token(self, "exit infix parsing function", true);
            } else {
                break;
            }
        }

        log_token(self, "exit `parse_expression()`", true);

        Ok(left_expr)
    }

    /// Parse the basic building blocks of expressions (e.g., grouped expressions, identifiers
    /// and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        log_token(self, "enter `parse_primary()`", true);

        let token = self.peek_current();

        match token {
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
                self.consume_token();
                expr
            }
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier, `_`, literal or `(`".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Parse prefix expressions (e.g., unary operators, literals, identifiers and parentheses),
    /// where the respective token appears at the beginning of an expression.
    /// Where applicable, check the current token and set the parser context based on
    /// surrounding tokens.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorsEmitted> {
        log_token(self, "enter `parse_prefix()`", true);

        match self.peek_current() {
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
                self.consume_token();
                log_token(self, "consume token", true);

                expr
            }

            Some(Token::Identifier { name, .. }) => {
                if &name == "_" {
                    self.consume_token();
                    Ok(Expression::Underscore(UnderscoreExpr {
                        underscore: Separator::Underscore,
                    }))
                } else if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    {
                        let expr = self.parse_primary();
                        self.consume_token();

                        if let Some(Token::Colon { .. }) = self.peek_ahead_by(2) {
                            let path = PathExpr {
                                root: PathPrefix::Identifier(Identifier(name)),
                                tree_opt: None,
                                wildcard_opt: None,
                            };
                            StructExpr::parse(self, path)
                        } else if let Some(Token::FatArrow { .. } | Token::If { .. }) =
                            self.peek_ahead_by(2)
                        {
                            if let Some(Token::LBrace { .. }) = self.peek_current() {
                                self.set_context(ParserContext::MatchArm);
                                expr
                            } else {
                                self.log_unexpected_token(TokenType::LBrace);
                                Err(ErrorsEmitted)
                            }
                        } else {
                            expr
                        }
                    }
                } else if let Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_ahead_by(1)
                {
                    let expr = PathExpr::parse(self, PathPrefix::Identifier(Identifier(name)));
                    self.consume_token();
                    expr
                } else {
                    let expr = self.parse_primary();
                    self.consume_token();
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

                    self.consume_token();
                    StructExpr::parse(self, path)
                } else {
                    self.consume_token();
                    PathExpr::parse(self, PathPrefix::SelfType(SelfType))
                }
            }
            Some(Token::SelfKeyword { .. }) => {
                self.consume_token();
                PathExpr::parse(self, PathPrefix::SelfKeyword)
            }
            Some(Token::Package { .. }) => {
                self.consume_token();
                PathExpr::parse(self, PathPrefix::Package)
            }
            Some(Token::Super { .. }) => {
                self.consume_token();
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
                    BorrowExpr::parse(self, ReferenceOp::Borrow)
                } else {
                    self.set_context(ParserContext::Unary);
                    self.parse_expression(Precedence::Unary)
                }
            }

            Some(Token::AmpersandMut { .. }) => BorrowExpr::parse(self, ReferenceOp::MutableBorrow),

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

            Some(Token::LBrace { .. }) => {
                if self.is_match_expr() {
                    self.set_context(ParserContext::MatchArm);
                    let expr = self.parse_prefix()?;
                    MatchArm::parse(self, expr)
                } else {
                    let expr = BlockExpr::parse(self);
                    expr
                }
            }

            Some(Token::LBracket { .. }) => ArrayExpr::parse(self),

            Some(Token::Pipe { .. }) => {
                if self.is_closure_with_params() {
                    self.set_context(ParserContext::Closure);
                    ClosureExpr::parse(self)
                } else if self.is_bitwise_or() {
                    self.set_context(ParserContext::BitwiseOr);
                    self.consume_token(); // Consume the pipe
                    let left = self.parse_prefix()?;
                    BinaryExpr::parse(self, left)
                } else {
                    self.log_error(ParserErrorKind::InvalidTokenContext {
                        token: TokenType::Pipe,
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
                    self.consume_token(); // Consume the pipe
                    let left = self.parse_prefix()?;
                    BinaryExpr::parse(self, left)
                } else {
                    self.log_error(ParserErrorKind::InvalidTokenContext {
                        token: TokenType::Pipe,
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
                    self.consume_token();
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
                self.consume_token();

                Ok(Expression::NoneExpr(NoneExpr {
                    kw_none: Keyword::None,
                }))
            }

            Some(Token::Ok { .. } | Token::Err { .. }) => ResultExpr::parse(self),

            Some(Token::Break { .. }) => {
                self.consume_token();
                Ok(Expression::Break(BreakExpr {
                    kw_break: Keyword::Break,
                }))
            }

            Some(Token::Continue { .. }) => {
                self.consume_token();
                Ok(Expression::Continue(ContinueExpr {
                    kw_continue: Keyword::Continue,
                }))
            }

            Some(Token::Return { .. }) => ReturnExpr::parse(self),

            _ => {
                log_token(self, "unexpected token", true);
                self.consume_token(); // skip token

                log_token(self, "consume token", true);

                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "prefix expression".to_string(),
                    found: self.peek_current(),
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
        log_token(self, "enter `parse_infix()`", true);

        match &self.peek_current() {
            Some(Token::Dot { .. }) => {
                if self.is_tuple_index() {
                    self.set_context(ParserContext::TupleIndex);
                } else if self.is_method_call() {
                    self.set_context(ParserContext::MethodCall);
                } else if self.is_field_access() {
                    self.set_context(ParserContext::FieldAccess);
                } else {
                    self.set_context(ParserContext::Default);
                }

                self.consume_token();

                match self.context {
                    ParserContext::FieldAccess => Some(FieldAccessExpr::parse),
                    ParserContext::MethodCall => Some(MethodCallExpr::parse),
                    ParserContext::TupleIndex => Some(TupleIndexExpr::parse),
                    _ => None, // Default to no infix parser
                }
            }

            Some(Token::LParen { .. }) => {
                // TODO: resolve similarity between `CallExpr` and `TupleStructExpr` (symbol table)
                Some(CallExpr::parse)
            }

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

            Some(Token::FatArrow { .. }) => {
                if self.context == ParserContext::MatchArm {
                    Some(MatchArm::parse)
                } else {
                    None
                }
            }

            _ => None,
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // STATEMENT
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a statement (i.e., let statement, item or expression).
    fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
        log_token(self, "enter `parse_statement()`", true);

        let token = self.peek_current();

        match token {
            Some(Token::Let { .. }) => Ok(Statement::Let(LetStmt::parse(self)?)),

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

            _ => {
                let statement = Ok(Statement::Expression(
                    self.parse_expression(Precedence::Lowest)?,
                ));

                if let Some(Token::Semicolon { .. }) = self.peek_current() {
                    log_token(self, "encounter `;`", false);
                    self.consume_token();
                    log_token(self, "consume token", true);
                }

                statement
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    // HELPERS
    ///////////////////////////////////////////////////////////////////////////

    /// Peek at the token at the current index in the `TokenStream`.
    fn peek_current(&self) -> Option<Token> {
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

    /// Peek at the token `num_tokens` behind the token at the current index in the `TokenStream`.
    fn peek_behind_by(&self, num_tokens: usize) -> Option<Token> {
        if self.current >= num_tokens {
            self.stream.tokens().get(self.current - num_tokens).cloned()
        } else {
            None
        }
    }

    /// Get the precedence of the next token
    fn peek_precedence(&self) -> Precedence {
        log_token(self, "enter `peek_precedence()`", true);

        let precedence = self.get_precedence(&self.peek_current().unwrap_or(Token::EOF));

        precedence
    }

    /// Advance the parser to the next token (returns current token).
    fn consume_token(&mut self) -> Option<Token> {
        if let Some(t) = self.peek_current() {
            self.current += 1;
            Some(t)
        } else {
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
            None
        }
    }

    /// Consume and check the current token, returning its respective `Keyword` or `ErrorsEmitted`.
    fn expect_keyword(&mut self, expected: TokenType) -> Result<Keyword, ErrorsEmitted> {
        log_token(self, "enter `expect_keyword()`", false);

        let token = self.consume_token().unwrap_or(Token::EOF);

        log_token(self, "consume token", false);

        if token.token_type() == expected {
            match token.token_type() {
                TokenType::Import { .. } => Ok(Keyword::Import),
                TokenType::Module { .. } => Ok(Keyword::Module),
                TokenType::Package { .. } => Ok(Keyword::Package),
                TokenType::SelfKeyword { .. } => Ok(Keyword::SelfKeyword),
                TokenType::SelfType { .. } => Ok(Keyword::SelfType),
                TokenType::Super { .. } => Ok(Keyword::Super),
                TokenType::Pub { .. } => Ok(Keyword::Pub),
                TokenType::As { .. } => Ok(Keyword::As),
                TokenType::Const { .. } => Ok(Keyword::Const),
                TokenType::Static { .. } => Ok(Keyword::Static),
                TokenType::Alias { .. } => Ok(Keyword::Alias),
                TokenType::Func { .. } => Ok(Keyword::Func),
                TokenType::Struct { .. } => Ok(Keyword::Struct),
                TokenType::Enum { .. } => Ok(Keyword::Enum),
                TokenType::Trait { .. } => Ok(Keyword::Trait),
                TokenType::Impl { .. } => Ok(Keyword::Impl),
                TokenType::If { .. } => Ok(Keyword::If),
                TokenType::Else { .. } => Ok(Keyword::Else),
                TokenType::Match { .. } => Ok(Keyword::Match),
                TokenType::Loop { .. } => Ok(Keyword::Loop),
                TokenType::For { .. } => Ok(Keyword::For),
                TokenType::In { .. } => Ok(Keyword::In),
                TokenType::While { .. } => Ok(Keyword::While),
                TokenType::Break { .. } => Ok(Keyword::Break),
                TokenType::Continue { .. } => Ok(Keyword::Continue),
                TokenType::Return { .. } => Ok(Keyword::Return),
                TokenType::Let { .. } => Ok(Keyword::Let),
                TokenType::Mut { .. } => Ok(Keyword::Mut),
                TokenType::Some { .. } => Ok(Keyword::Some),
                TokenType::None { .. } => Ok(Keyword::None),
                TokenType::Ok { .. } => Ok(Keyword::Ok),
                TokenType::Err { .. } => Ok(Keyword::Err),
                _ => {
                    self.log_unexpected_str("keyword");
                    Err(ErrorsEmitted)
                }
            }
        } else {
            self.log_unexpected_token(expected);
            Err(ErrorsEmitted)
        }
    }

    /// Consume and check the current token, returning its respective `Delimiter`
    /// or `ErrorsEmitted`.
    fn expect_delimiter(&mut self, expected: TokenType) -> Result<Delimiter, ErrorsEmitted> {
        log_token(self, "enter `expect_delimiter()`", false);

        let token = self.consume_token().unwrap_or(Token::EOF);

        log_token(self, "consume token", false);

        if token.token_type() == expected {
            match token.token_type() {
                TokenType::LParen => Ok(Delimiter::LParen),
                TokenType::RParen => Ok(Delimiter::RParen),
                TokenType::LBrace => Ok(Delimiter::LBrace),
                TokenType::RBrace => Ok(Delimiter::RBrace),
                TokenType::LBracket => Ok(Delimiter::LBracket),
                TokenType::RBracket => Ok(Delimiter::RBracket),
                _ => {
                    self.log_unexpected_str("delimiter");
                    Err(ErrorsEmitted)
                }
            }
        } else {
            self.log_unexpected_token(expected);
            Err(ErrorsEmitted)
        }
    }

    /// Consume and check the current token, returning its respective `Separator`
    /// or `ErrorsEmitted`.
    fn expect_separator(&mut self, expected: TokenType) -> Result<Separator, ErrorsEmitted> {
        log_token(self, "enter `expect_separator()`", false);

        let token = self.consume_token().unwrap_or(Token::EOF);

        log_token(self, "consume token", false);

        if token.token_type() == expected {
            match token.token_type() {
                TokenType::Colon => Ok(Separator::Colon),
                TokenType::Semicolon => Ok(Separator::Semicolon),
                TokenType::Comma => Ok(Separator::Comma),
                TokenType::ThinArrow => Ok(Separator::ThinArrow),
                TokenType::FatArrow => Ok(Separator::FatArrow),
                TokenType::LessThan => Ok(Separator::LeftAngledBracket),
                TokenType::GreaterThan => Ok(Separator::RightAngledBracket),
                TokenType::Pipe => Ok(Separator::Pipe),
                TokenType::DblPipe => Ok(Separator::DblPipe),
                _ => {
                    self.log_unexpected_str("separator");
                    Err(ErrorsEmitted)
                }
            }
        } else {
            self.log_unexpected_token(expected);
            Err(ErrorsEmitted)
        }
    }

    /// Log information about an error that occurred during parsing, by pushing the error
    /// to the `errors` vector and providing information about error kind and position.
    fn log_error(&mut self, error_kind: ParserErrorKind) {
        let i = if self.current <= self.stream.tokens().len() && self.current > 0 {
            self.current - 1 // one index behind, as the parser should have already advanced
        } else {
            0
        };

        let error = CompilerError::new(
            error_kind,
            &self.stream.span().input(),
            self.stream.tokens()[i].span().start(),
        );
        self.errors.push(error);
    }

    /// Log error information on encountering an unexpected token, with the expected behaviour
    /// described as a `&str`.
    fn log_unexpected_str(&mut self, expected: &str) {
        self.log_error(ParserErrorKind::UnexpectedToken {
            expected: expected.to_string(),
            found: self.peek_current(),
        });

        self.consume_token();
        log_token(self, "consume token", false);
    }

    /// Log error information on encountering an unexpected token by providing the expected token.
    fn log_unexpected_token(&mut self, expected: TokenType) {
        self.log_error(ParserErrorKind::UnexpectedToken {
            expected: expected.to_string(),
            found: self.peek_current(),
        });

        self.consume_token();
        log_token(self, "consume token", false);
    }

    /// Retrieve any errors that occurred during parsing.
    #[allow(dead_code)]
    pub fn errors(&self) -> &[CompilerError<ParserErrorKind>] {
        &self.errors
    }

    fn get_identifier_patt(&mut self) -> Result<Pattern, ErrorsEmitted> {
        log_token(self, "enter get_identifier_patt()`", true);

        let kw_ref_opt = if let Some(Token::Ref { .. }) = self.peek_current() {
            self.consume_token();
            log_token(self, "consume token", false);
            Some(Keyword::Ref)
        } else {
            None
        };

        let kw_mut_opt = if let Some(Token::Mut { .. }) = self.peek_current() {
            self.consume_token();
            log_token(self, "consume token", false);
            Some(Keyword::Mut)
        } else {
            None
        };

        let name = if let Some(Token::Identifier { name, .. }) = self.consume_token() {
            Ok(Identifier(name))
        } else {
            self.log_unexpected_str("identifier");
            Err(ErrorsEmitted)
        }?;

        log_token(self, "exit `get_identifier_patt()`", true);

        Ok(Pattern::IdentifierPatt {
            kw_ref_opt,
            kw_mut_opt,
            name,
        })
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
            Some(Token::Library { .. }) => Some(InnerAttr::Library),
            Some(Token::Interface { .. }) => Some(InnerAttr::Interface),
            Some(Token::Script { .. }) => Some(InnerAttr::Script),
            Some(Token::Unsafe { .. }) => Some(InnerAttr::Unsafe),
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

                        let kw_package = self.expect_keyword(TokenType::Package)?;

                        let close_paren = if let Some(Token::RParen { .. }) = self.consume_token() {
                            Ok(Delimiter::RParen)
                        } else {
                            self.expect_delimiter(TokenType::RParen)?;
                            Err(ErrorsEmitted)
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

        while let Some(oa) = self.get_outer_attr() {
            outer_attributes.push(oa);
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
            Some(Token::Module { .. }) => Ok(Statement::Item(Item::ModuleItem(Box::new(
                ModuleItem::parse(self, outer_attributes, visibility)?,
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
            Some(Token::Struct { .. }) => match self.peek_ahead_by(2) {
                Some(Token::LBrace { .. }) => Ok(Statement::Item(Item::StructDef(
                    StructDef::parse(self, outer_attributes, visibility)?,
                ))),

                Some(Token::LParen { .. }) => Ok(Statement::Item(Item::TupleStructDef(
                    TupleStructDef::parse(self, outer_attributes, visibility)?,
                ))),

                _ => {
                    self.log_unexpected_str("`{` or `(`");
                    Err(ErrorsEmitted)
                }
            },

            Some(Token::Func { .. }) => Ok(Statement::Item(Item::FunctionItem(
                FunctionItem::parse(self, outer_attributes, visibility)?,
            ))),
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
                    Err(ErrorsEmitted)
                }
            },
            _ => {
                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "declaration or definition item".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Determine if `Token::Dot` token indicates a tuple index operator (followed by a digit).
    fn is_tuple_index(&self) -> bool {
        match self.peek_ahead_by(1) {
            Some(Token::UIntLiteral { .. }) => true,
            _ => false,
        }
    }

    /// Determine if `Token::Dot` indicates a method call.
    fn is_method_call(&self) -> bool {
        if self.peek_ahead_by(2).is_some() {
            match (
                self.peek_current(),
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
        } else {
            false
        }
    }

    /// Determine if `Token::Dot` indicates field access.
    fn is_field_access(&self) -> bool {
        if self.peek_ahead_by(1).is_some() {
            match (self.peek_current(), self.peek_ahead_by(1)) {
                (Some(Token::Dot { .. }), Some(Token::Identifier { .. })) => true, // `object.field`
                _ => false,
            }
        } else {
            false
        }
    }

    /// Determine if `Token::Pipe` indicates a closure parameter delimiter.
    fn is_closure_with_params(&self) -> bool {
        match (self.peek_current(), self.peek_ahead_by(1)) {
            (Some(Token::Pipe { .. }), Some(Token::Identifier { .. })) => true,
            _ => false,
        }
    }

    /// Determine if `Token::Pipe` indicates the bitwise OR operator.
    fn is_bitwise_or(&self) -> bool {
        match (self.peek_current(), self.peek_ahead_by(1)) {
            (Some(Token::Pipe { .. }), Some(Token::Identifier { .. })) => false,
            _ => true,
        }
    }

    /// Determine if `Token::DblPipe` indicates an empty closure parameter list.
    fn is_closure_without_params(&self) -> bool {
        match (self.peek_current(), self.peek_ahead_by(1)) {
            (Some(Token::DblPipe { .. }), Some(Token::Identifier { .. })) => true,
            _ => false,
        }
    }

    /// Determine if `Token::DblPipe` indicates the logical OR operator.
    fn is_logical_or(&self) -> bool {
        match (self.peek_current(), self.peek_ahead_by(1)) {
            (Some(Token::DblPipe { .. }), Some(Token::Identifier { .. })) => false,
            _ => true,
        }
    }

    /// Determine if `Token::LBrace` indicates the opening of a match expression body.
    fn is_match_expr(&self) -> bool {
        match (self.peek_behind_by(2), self.peek_ahead_by(2)) {
            (Some(Token::Match { .. }), Some(Token::FatArrow { .. } | Token::If { .. })) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::test_utils;

    #[test]
    fn parse_break_expr() -> Result<(), ()> {
        let input = r#"break"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_continue_expr() -> Result<(), ()> {
        let input = r#"continue"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }

    #[test]
    fn parse_underscore_expr() -> Result<(), ()> {
        let input = r#"_"#;

        let mut parser = test_utils::get_parser(input, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.errors())),
        }
    }
}
