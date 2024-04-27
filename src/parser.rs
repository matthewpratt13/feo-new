#![allow(dead_code)]
#![allow(unused_variables)]

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
mod unary_expr;
mod while_expr;

use std::collections::HashMap;

use crate::{
    ast::{
        AliasDecl, ArrayExpr, AssignmentExpr, BinaryExpr, BlockExpr, BorrowExpr, BreakExpr, CallExpr, ClosureExpr, ConstantDecl, ContinueExpr, Delimiter, DereferenceExpr, EnumDef, Expression, FieldAccessExpr, ForInExpr, FunctionItem, GroupedExpr, Identifier, IfExpr, ImportDecl, IndexExpr, InherentImplDef, InnerAttr, Item, Keyword, LetStmt, Literal, MatchExpr, MethodCallExpr, ModuleItem, NoneExpr, OuterAttr, PathExpr, PathPrefix, Pattern, PubPackageVis, RangeExpr, RangeOp, ReferenceOp, ResultExpr, ReturnExpr, Separator, SomeExpr, Statement, StaticItemDecl, StructDef, StructExpr, TraitDef, TraitImplDef, TupleExpr, TupleIndexExpr, TupleStructDef, UnaryExpr, UnaryOp, UnderscoreExpr, Visibility, WhileExpr
    },
    error::{CompilerError, ErrorsEmitted, ParserErrorKind},
    token::{Token, TokenStream, TokenType},
};

use self::item::{ParseDeclaration, ParseDefinition};
pub use self::precedence::Precedence;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParserContext {
    Default,
    Closure,     // `|` or `||`
    LogicalOr,   // `||`
    BitwiseOr,   // `|`
    BitwiseAnd,  // `&`
    Difference,  // `-`
    Product,     // `*`
    Unary,       // `&` `*`, `-`
    TupleIndex,  // `.`
    FieldAccess, // `.`
    MethodCall,  // `.`
}

/// Struct that stores a stream of tokens and contains methods to parse expressions,
/// statements and items, as well as helper methods and error handling functionality.
#[derive(Debug)]
pub(crate) struct Parser {
    stream: TokenStream,
    current: usize,
    errors: Vec<CompilerError<ParserErrorKind>>,
    precedences: HashMap<Token, Precedence>,
    context: ParserContext,
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

    // Initialize operator precedences
    fn init_precedences(&mut self, tokens: Vec<Token>) {
        for t in tokens {
            match t.token_type() {
                TokenType::As => self.precedences.insert(t, Precedence::TypeCast),
                TokenType::LParen => self.precedences.insert(t, Precedence::Call),
                TokenType::LBracket => self.precedences.insert(t, Precedence::Index),
                TokenType::Dot => self.precedences.insert(t, Precedence::FieldAccess), // default
                TokenType::DblColon | TokenType::ColonColonAsterisk => {
                    self.precedences.insert(t, Precedence::Path)
                }
                TokenType::QuestionMark => self.precedences.insert(t, Precedence::Unwrap),
                TokenType::Some | TokenType::None | TokenType::Ok | TokenType::Err => {
                    self.precedences.insert(t, Precedence::Unary)
                }
                TokenType::Bang => self.precedences.insert(t, Precedence::Unary),
                TokenType::Percent => self.precedences.insert(t, Precedence::Remainder),
                TokenType::Ampersand => self.precedences.insert(t, Precedence::BitwiseAnd), // default,
                TokenType::Asterisk => self.precedences.insert(t, Precedence::Product), // default
                TokenType::Plus => self.precedences.insert(t, Precedence::Sum),
                TokenType::Minus => self.precedences.insert(t, Precedence::Difference), // default
                TokenType::Slash => self.precedences.insert(t, Precedence::Quotient),
                TokenType::LessThan => self.precedences.insert(t, Precedence::LessThan),
                TokenType::Equals => self.precedences.insert(t, Precedence::Assignment),
                TokenType::GreaterThan => self.precedences.insert(t, Precedence::GreaterThan),
                TokenType::Caret => self.precedences.insert(t, Precedence::BitwiseXor),
                TokenType::Pipe => self.precedences.insert(t, Precedence::BitwiseOr),
                TokenType::DblDot | TokenType::DotDotEquals => {
                    self.precedences.insert(t, Precedence::Range)
                }
                TokenType::BangEquals => self.precedences.insert(t, Precedence::NotEqual),
                TokenType::PlusEquals
                | TokenType::MinusEquals
                | TokenType::AsteriskEquals
                | TokenType::SlashEquals
                | TokenType::PercentEquals => {
                    self.precedences.insert(t, Precedence::CompoundAssignment)
                }
                TokenType::DblAsterisk => self.precedences.insert(t, Precedence::Exponentiation),
                TokenType::DblAmpersand => self.precedences.insert(t, Precedence::LogicalAnd),
                TokenType::AmpersandMut => self.precedences.insert(t, Precedence::Unary),
                TokenType::DblLessThan | TokenType::DblGreaterThan => {
                    self.precedences.insert(t, Precedence::Shift)
                }
                TokenType::LessThanEquals => {
                    self.precedences.insert(t, Precedence::LessThanOrEqual)
                }
                TokenType::DblEquals => self.precedences.insert(t, Precedence::Equal),

                TokenType::GreaterThanEquals => {
                    self.precedences.insert(t, Precedence::GreaterThanOrEqual)
                }
                TokenType::DblPipe => self.precedences.insert(t, Precedence::LogicalOr),
                _ => self.precedences.insert(t, Precedence::Lowest),
            };
        }
    }

    // Get the precedence for a given token, considering context
    fn get_precedence(&self, token: &Token) -> Precedence {
        match token {
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
            Token::Dot { .. } => match self.context {
                ParserContext::FieldAccess => Precedence::FieldAccess,
                ParserContext::MethodCall => Precedence::MethodCall,
                ParserContext::TupleIndex => Precedence::TupleIndex,
                _ => Precedence::Lowest,
            },
            Token::As { .. } => Precedence::TypeCast,
            Token::LParen { .. } => Precedence::Call, // TODO: what about tuple structs?
            Token::LBracket { .. } => Precedence::Index,
            Token::DblColon { .. } | Token::ColonColonAsterisk { .. } => Precedence::Path,
            Token::Bang { .. } => Precedence::Unary,
            Token::Percent { .. } => Precedence::Remainder,
            Token::Plus { .. } => Precedence::Sum,
            Token::Some { .. } | Token::None { .. } | Token::Ok { .. } | Token::Err { .. } => {
                Precedence::Unary
            }
            Token::Slash { .. } => Precedence::Quotient,
            Token::LessThan { .. } => Precedence::LessThan,
            Token::Equals { .. } => Precedence::Assignment,
            Token::GreaterThan { .. } => Precedence::GreaterThan,
            Token::QuestionMark { .. } => Precedence::Unwrap,
            Token::Caret { .. } => Precedence::BitwiseXor,
            Token::DblDot { .. } | Token::DotDotEquals { .. } => Precedence::Range,
            Token::BangEquals { .. } => Precedence::NotEqual,
            Token::PlusEquals { .. }
            | Token::MinusEquals { .. }
            | Token::AsteriskEquals { .. }
            | Token::SlashEquals { .. }
            | Token::PercentEquals { .. } => Precedence::CompoundAssignment,
            Token::DblAsterisk { .. } => Precedence::Exponentiation,
            Token::DblAmpersand { .. } => Precedence::LogicalAnd,
            Token::AmpersandMut { .. } => Precedence::Unary,
            Token::DblLessThan { .. } | Token::DblGreaterThan { .. } => Precedence::Shift,
            Token::LessThanEquals { .. } => Precedence::LessThanOrEqual,
            Token::DblEquals { .. } => Precedence::Equal,
            Token::GreaterThanEquals { .. } => Precedence::GreaterThanOrEqual,
            _ => Precedence::Lowest, // Default precedence for other tokens
        }
    }

    // Set the context
    fn set_context(&mut self, context: ParserContext) {
        self.context = context;
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

    /// Recursively parse an expression based on the next token's operator precedence.
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ErrorsEmitted> {
        println!("enter `parse_expression()`");
        println!("current token: `{:?}`", self.peek_current());
        println!("input precedence: {:?}\n", precedence);

        let mut left_expr = self.parse_prefix()?;
        println!("exit `parse_prefix()`");
        println!("current token: `{:?}`", self.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
        );

        while precedence < self.peek_precedence() {
            self.consume_token(); // Advance to the next token
            println!("consume token`");
            println!("current token: `{:?}`", self.peek_current());
            println!(
                "token precedence: `{:?}`\n",
                self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
            );

            if let Some(infix_parser) = self.parse_infix() {
                left_expr = infix_parser(self, left_expr)?; // Parse infix expressions

                println!("exit infix parsing function: `{:?}`", infix_parser);
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );
            } else {
                println!("no infix parsing function found");
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );
                break; // Exit if no infix parser is found
            }

            // self.consume_token(); // Advance to the next token
            // println!("consume token`");
            // println!("current token: `{:?}`", self.peek_current());
            // println!(
            //     "token precedence: `{:?}`\n",
            //     self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
            // );
        }

        // while let Some(t) = self.peek_current() {
        //     let token_precedence = self.precedence(&t);

        //     println!("CURRENT PRECEDENCE: {:?}", token_precedence);

        //     if token_precedence > precedence {
        //         left_expr = self.parse_infix(left_expr)?;
        //         println!("INFIX EXPRESSION: {:?}", left_expr.clone());
        //         println!("CURRENT TOKEN: {:?}\n", self.peek_current());
        //     } else {
        //         break;
        //     }
        // }

        Ok(left_expr)
    }

    /// Parse primary expressions (e.g., grouped expressions, identifiers and literals).
    fn parse_primary(&mut self) -> Result<Expression, ErrorsEmitted> {
        println!("enter `parse_primary()`");
        println!("current token: `{:?}`", self.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
        );

        let token = self.peek_current();

        match token {
            Some(Token::Identifier { name, .. }) => {
                println!("enter `Some(Token::Identifier)` match arm");
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );

                Ok(Expression::Path(PathExpr {
                    root: PathPrefix::Identifier(Identifier(name)),
                    tree_opt: None,
                    wildcard_opt: None,
                }))
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
                self.consume_token();
                GroupedExpr::parse(self)
            }
            _ => {
                println!("enter default match arm (unexpected token or none)");
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );

                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "identifier, `_`, literal or `(`".to_string(),
                    found: token,
                });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Parse prefix expressions (e.g., unary operators, literals, identifiers and parentheses),
    /// where the respective token type appears at the beginning of an expression.
    fn parse_prefix(&mut self) -> Result<Expression, ErrorsEmitted> {
        println!("enter `parse_prefix()`");
        println!("current token: `{:?}`", self.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
        );

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
                println!("enter `Some(Token::Identifier {{ name, ..}})` match arm");
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );

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

                            StructExpr::parse(self, Expression::Path(path))
                        }
                        _ => PathExpr::parse(self, PathPrefix::Identifier(Identifier(name))),
                    }
                } else if let Some(Token::DblColon { .. } | Token::ColonColonAsterisk { .. }) =
                    self.peek_current()
                {
                    PathExpr::parse(self, PathPrefix::Identifier(Identifier(name)))
                } else {
                    println!("enter final `else` block – i.e., `name` is not `_`, next token is not a `{{` and current token is not a `::` or `::*`");
                    println!("current token: `{:?}`", self.peek_current());
                    println!(
                        "token precedence: `{:?}`\n",
                        self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                    );

                    let id = self.parse_primary();
                    println!("exit `parse_primary()`");
                    println!("current token: `{:?}`", self.peek_current());
                    println!(
                        "token precedence: `{:?}`\n",
                        self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                    );
                    // self.consume_token();

                    println!("return to `parse_prefix()` else block");
                    // println!("consume token");
                    println!("current token: `{:?}`", self.peek_current());
                    println!(
                        "token precedence: `{:?}`\n",
                        self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                    );

                    id
                }
            }
            Some(Token::SelfType { .. }) => {
                if let Some(Token::LBrace { .. }) = self.peek_ahead_by(1) {
                    self.consume_token();

                    let path = PathExpr::parse(self, PathPrefix::SelfType)?;

                    StructExpr::parse(self, path)
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
            Some(Token::Minus { .. }) => UnaryExpr::parse(self, UnaryOp::Negate),
            Some(Token::Bang { .. }) => UnaryExpr::parse(self, UnaryOp::Not),
            Some(Token::Ampersand { .. }) => BorrowExpr::parse(self, ReferenceOp::Borrow),

            Some(Token::AmpersandMut { .. }) => BorrowExpr::parse(self, ReferenceOp::MutableBorrow),

            Some(Token::Asterisk { .. }) => DereferenceExpr::parse(self),
            Some(Token::Unsafe { .. }) => BlockExpr::parse(self),

            Some(Token::LParen { .. }) => {
                if let Some(Token::Comma { .. }) = self.peek_ahead_by(2) {
                    TupleExpr::parse(self)
                } else {
                    self.parse_primary()
                }
            }
            Some(Token::LBrace { .. }) => BlockExpr::parse(self),

            Some(Token::LBracket { .. }) => ArrayExpr::parse(self),

            Some(Token::Pipe { .. } | Token::DblPipe { .. }) => ClosureExpr::parse(self),
            Some(Token::Dot { .. }) => {
                let next_token = self.peek_ahead_by(1);

                match next_token {
                    Some(Token::Identifier { .. } | Token::UIntLiteral { .. }) => {
                        self.consume_token();
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        let token = self.peek_ahead_by(1);

                        self.log_error(ParserErrorKind::UnexpectedToken {
                            expected: "identifier or unsigned integer after `.`".to_string(),
                            found: token,
                        });
                        Err(ErrorsEmitted)
                    }
                }
            }
            Some(Token::QuestionMark { .. }) => match self.peek_behind_by(1) {
                Some(Token::Identifier { .. }) => {
                    self.consume_token();
                    Err(ErrorsEmitted)
                }
                _ => {
                    self.log_error(ParserErrorKind::UnexpectedToken {
                        expected: "expression before `?`".to_string(),
                        found: token,
                    });
                    Err(ErrorsEmitted)
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
                    let expression = self.parse_expression(Precedence::Range)?;

                    let to = match expression.clone() {
                        Expression::Literal(l) => match l {
                            Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => {
                                Ok(expression)
                            }
                            _ => {
                                self.log_unexpected_str("numeric literal");
                                Err(ErrorsEmitted)
                            }
                        },
                        Expression::Path(_) => {
                            self.log_unexpected_str("path expression");
                            Err(ErrorsEmitted)
                        }
                        _ => {
                            self.log_unexpected_str("numeric literal or path expression");
                            Err(ErrorsEmitted)
                        }
                    }?;

                    Ok(Expression::Range(RangeExpr {
                        from_opt: None,
                        op: RangeOp::RangeExclusive,
                        to_opt: Some(Box::new(to)),
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
                let expression = self.parse_expression(Precedence::Range)?;

                let to = match expression.clone() {
                    Expression::Literal(l) => match l {
                        Literal::Int(_) | Literal::UInt(_) | Literal::BigUInt(_) => Ok(expression),
                        _ => {
                            self.log_unexpected_str("numeric literal");
                            Err(ErrorsEmitted)
                        }
                    },
                    Expression::Path(_) => {
                        self.log_unexpected_str("path expression");
                        Err(ErrorsEmitted)
                    }
                    _ => {
                        self.log_unexpected_str("numeric literal or path expression");
                        Err(ErrorsEmitted)
                    }
                }?;

                Ok(Expression::Range(RangeExpr {
                    from_opt: None,
                    op: RangeOp::RangeInclusive,
                    to_opt: Some(Box::new(to)),
                }))
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

            Some(Token::Return { .. }) => ReturnExpr::parse(self),

            _ => {
                println!("enter default match arm (unexpected token or none)");
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );

                self.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "prefix expression".to_string(),
                    found: self.peek_current(),
                });
                Err(ErrorsEmitted)
            }
        }
    }

    // Parse infix expressions
    fn parse_infix(
        &self,
    ) -> Option<fn(&mut Self, Expression) -> Result<Expression, ErrorsEmitted>> {
        println!("enter `parse_infix()`");
        println!("current token: `{:?}`", self.peek_current());
        println!(
            "token precedence: `{:?}`\n",
            self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
        );

        match &self.peek_current() {
            Some(Token::Dot { .. }) => {
                match self.context {
                    ParserContext::FieldAccess => Some(FieldAccessExpr::parse),
                    ParserContext::MethodCall => Some(MethodCallExpr::parse),
                    ParserContext::TupleIndex => Some(TupleIndexExpr::parse),
                    _ => None, // Default to no infix parser
                }
            }
            Some(Token::Plus { .. }) => Some(BinaryExpr::parse), // Handle binary addition
            Some(Token::Asterisk { .. }) => {
                if self.context == ParserContext::Unary {
                    None // No infix parser for unary dereference
                } else {
                    Some(BinaryExpr::parse) // Multiplication
                }
            }
            Some(Token::Minus { .. }) => {
                if self.context == ParserContext::Unary {
                    None // No infix parser for unary negation
                } else {
                    Some(BinaryExpr::parse) // Binary minus
                }
            }
            Some(
                Token::Slash { .. }
                | Token::Percent { .. }
                | Token::DblAmpersand { .. }
                | Token::DblAsterisk { .. },
            ) => Some(BinaryExpr::parse),
            Some(Token::LParen { .. }) => {
                // TODO: resolve similarity between `CallExpr` and `TupleStructExpr` (symbol table)
                Some(CallExpr::parse)
            }

            Some(Token::LBracket { .. }) => Some(IndexExpr::parse),

            Some(Token::Equals { .. }) => Some(AssignmentExpr::parse),

            _ => {
                println!("enter default match arm (unexpected token or none)");
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );

                None
            } // Default to no infix parser
        }
    }

    // /// Parse infix expressions (e.g., binary operators), where the respective token type
    // /// appears in the middle of an expression.
    // fn parse_infix(&mut self, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
    //     let token = self.consume_token();

    //     match token {
    //         Some(Token::Plus { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::Add,
    //         )?)),
    //         Some(Token::Minus { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::Subtract,
    //         )?)),
    //         Some(Token::Asterisk { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::Multiply,
    //         )?)),
    //         Some(Token::Slash { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::Divide,
    //         )?)),
    //         Some(Token::Percent { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::Modulus,
    //         )?)),
    //         Some(Token::DblEquals { .. }) => Ok(Expression::Comparison(ComparisonExpr::parse(
    //             self,
    //             left_expr,
    //             ComparisonOp::Equal,
    //         )?)),
    //         Some(Token::BangEquals { .. }) => Ok(Expression::Comparison(ComparisonExpr::parse(
    //             self,
    //             left_expr,
    //             ComparisonOp::NotEqual,
    //         )?)),
    //         Some(Token::LessThan { .. }) => Ok(Expression::Comparison(ComparisonExpr::parse(
    //             self,
    //             left_expr,
    //             ComparisonOp::LessThan,
    //         )?)),
    //         Some(Token::LessThanEquals { .. }) => Ok(Expression::Comparison(
    //             ComparisonExpr::parse(self, left_expr, ComparisonOp::LessEqual)?,
    //         )),
    //         Some(Token::GreaterThan { .. }) => Ok(Expression::Comparison(ComparisonExpr::parse(
    //             self,
    //             left_expr,
    //             ComparisonOp::GreaterThan,
    //         )?)),
    //         Some(Token::GreaterThanEquals { .. }) => Ok(Expression::Comparison(
    //             ComparisonExpr::parse(self, left_expr, ComparisonOp::GreaterEqual)?,
    //         )),
    //         Some(Token::Equals { .. }) => Ok(Expression::Assignment(AssignmentExpr::parse(
    //             self, left_expr,
    //         )?)),
    //         Some(Token::PlusEquals { .. }) => Ok(Expression::CompoundAssignment(
    //             CompoundAssignmentExpr::parse(self, left_expr, CompoundAssignmentOp::AddAssign)?,
    //         )),
    //         Some(Token::MinusEquals { .. }) => Ok(Expression::CompoundAssignment(
    //             CompoundAssignmentExpr::parse(
    //                 self,
    //                 left_expr,
    //                 CompoundAssignmentOp::SubtractAssign,
    //             )?,
    //         )),
    //         Some(Token::AsteriskEquals { .. }) => Ok(Expression::CompoundAssignment(
    //             CompoundAssignmentExpr::parse(
    //                 self,
    //                 left_expr,
    //                 CompoundAssignmentOp::MultiplyAssign,
    //             )?,
    //         )),
    //         Some(Token::SlashEquals { .. }) => Ok(Expression::CompoundAssignment(
    //             CompoundAssignmentExpr::parse(self, left_expr, CompoundAssignmentOp::DivideAssign)?,
    //         )),
    //         Some(Token::PercentEquals { .. }) => Ok(Expression::CompoundAssignment(
    //             CompoundAssignmentExpr::parse(
    //                 self,
    //                 left_expr,
    //                 CompoundAssignmentOp::ModulusAssign,
    //             )?,
    //         )),
    //         Some(Token::DblAmpersand { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::LogicalAnd,
    //         )?)),
    //         Some(Token::DblPipe { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::LogicalOr,
    //         )?)),
    //         Some(Token::Ampersand { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::BitwiseAnd,
    //         )?)),
    //         Some(Token::Pipe { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::BitwiseOr,
    //         )?)),
    //         Some(Token::Caret { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::BitwiseXor,
    //         )?)),
    //         Some(Token::DblLessThan { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::ShiftLeft,
    //         )?)),
    //         Some(Token::DblGreaterThan { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::ShiftRight,
    //         )?)),
    //         Some(Token::DblAsterisk { .. }) => Ok(Expression::Binary(BinaryExpr::parse(
    //             self,
    //             left_expr,
    //             BinaryOp::Exponentiation,
    //         )?)),
    // Some(Token::LParen { .. }) => {
    //     // TODO: resolve similarity between `CallExpr` and `TupleStructExpr` (symbol table)
    //     let expr = CallExpr::parse(self, left_expr)?;
    //     Ok(Expression::Call(expr))
    // }
    // Some(Token::LBracket { .. }) => {
    //     let expr = IndexExpr::parse(self, left_expr)?;
    //     Ok(Expression::Index(expr))
    // }

    // Some(Token::As { .. }) => {
    //     let new_type = Type::parse(self)?;
    //     let operand = ValueExpr::try_from(left_expr).map_err(|e| {
    //         self.log_error(e);
    //         ErrorsEmitted
    //     })?;

    //     let expr = TypeCastExpr {
    //         operand: Box::new(operand),
    //         kw_as: Keyword::As,
    //         new_type,
    //     };

    //     Ok(Expression::TypeCast(expr))
    // }
    // Some(Token::QuestionMark { .. }) => Ok(Expression::Unwrap(UnwrapExpr {
    //     expression: Box::new(ValueExpr::try_from(left_expr).map_err(|e| {
    //         self.log_error(e);
    //         ErrorsEmitted
    //     })?),
    //     op: UnwrapOp,
    // })),
    // Some(Token::Dot { .. }) => match self.peek_current() {
    //     Some(Token::Identifier { .. }) => match self.peek_ahead_by(1) {
    //         Some(Token::LParen { .. }) => Ok(Expression::MethodCall(
    //             MethodCallExpr::parse(self, left_expr)?,
    //         )),
    //         _ => Ok(Expression::FieldAccess(FieldAccessExpr::parse(
    //             self, left_expr,
    //         )?)),
    //     },
    //     Some(Token::UIntLiteral { .. }) => Ok(Expression::TupleIndex(
    //         TupleIndexExpr::parse(self, left_expr)?,
    //     )),
    //     _ => {
    //         self.log_error(ParserErrorKind::UnexpectedToken {
    //             expected: "identifier or index".to_string(),
    //             found: token,
    //         });
    //         Err(ErrorsEmitted)
    //     }
    // },
    // Some(Token::DblDot { .. }) => {
    //     let expr = RangeExpr::parse(self, left_expr, RangeOp::RangeExclusive)?;
    //     Ok(Expression::Range(expr))
    // }
    // Some(Token::DotDotEquals { .. }) => {
    //     let expr = RangeExpr::parse(self, left_expr, RangeOp::RangeInclusive)?;
    //     Ok(Expression::Range(expr))
    // }
    // _ => {
    //     self.log_error(ParserErrorKind::UnexpectedToken {
    //         expected: "infix expression".to_string(),
    //         found: self.peek_current(),
    //     });
    //     Err(ErrorsEmitted)
    // }
    //     }
    // }

    ///////////////////////////////////////////////////////////////////////////
    /// STATEMENT
    ///////////////////////////////////////////////////////////////////////////

    /// Parse a statement (i.e., let statement, item or expression).
    fn parse_statement(&mut self) -> Result<Statement, ErrorsEmitted> {
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
                    self.consume_token();
                }

                println!("exit `parse_expression()`");
                println!("current token: `{:?}`", self.peek_current());
                println!(
                    "token precedence: `{:?}`\n",
                    self.get_precedence(&self.peek_current().unwrap_or(Token::EOF))
                );

                statement
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// HELPERS
    ///////////////////////////////////////////////////////////////////////////

    /// Peek at the token at the current index in the `TokenStream`.
    fn peek_current(&self) -> Option<Token> {
        if self.current < self.stream.tokens().len() {
            self.stream.tokens().get(self.current).cloned()
        } else {
            None
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

    // Get the precedence of the next token
    fn peek_precedence(&self) -> Precedence {
        println!("enter `peek_precedence()`");
        println!("current token: `{:?}`", self.peek_current());

        let precedence = self.get_precedence(&self.peek_ahead_by(1).unwrap_or(Token::EOF));

        println!("next_precedence: `{:?}`\n", precedence.clone());

        precedence
    }

    /// Advance the parser and return the current token.
    fn consume_token(&mut self) -> Option<Token> {
        if let Some(t) = self.peek_current() {
            self.current += 1;
            Some(t)
        } else {
            self.log_error(ParserErrorKind::UnexpectedEndOfInput);
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

    fn expect_keyword(&mut self, expected: TokenType) -> Result<Keyword, ErrorsEmitted> {
        let token = self.consume_token().unwrap_or(Token::EOF);

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
                self.log_unexpected_token(expected);
                Err(ErrorsEmitted)
            }
        }
    }

    fn expect_delimiter(&mut self, expected: TokenType) -> Result<Delimiter, ErrorsEmitted> {
        let token = self.consume_token().ok_or({
            self.log_error(ParserErrorKind::MissingDelimiter {
                delim: expected.clone(),
            });

            ErrorsEmitted
        })?;

        match token.token_type() {
            TokenType::LParen => Ok(Delimiter::LParen),
            TokenType::RParen => Ok(Delimiter::RParen),
            TokenType::LBrace => Ok(Delimiter::LBrace),
            TokenType::RBrace => Ok(Delimiter::RBrace),
            TokenType::LBracket => Ok(Delimiter::LBracket),
            TokenType::RBracket => Ok(Delimiter::RBracket),
            _ => {
                self.log_unexpected_token(expected);
                Err(ErrorsEmitted)
            }
        }
    }

    fn expect_separator(&mut self, expected: TokenType) -> Result<Separator, ErrorsEmitted> {
        let token = self.consume_token().unwrap_or(Token::EOF);

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
                self.log_unexpected_token(expected);
                Err(ErrorsEmitted)
            }
        }
    }

    /// Retrieve the respective precedence level of a given operator.
    // fn precedence(&mut self, token: &Token) -> Precedence {
    //     match token {
    //         Token::As { .. } => Precedence::TypeCast,
    //         Token::LParen { .. } => Precedence::Call, // TODO: what about tuple structs?
    //         Token::LBracket { .. } => Precedence::Index,
    //         Token::Dot { .. } => match self.peek_ahead_by(1) {
    //             Some(Token::Identifier { .. }) => match self.peek_ahead_by(2) {
    //                 Some(Token::LParen { .. }) => Precedence::MethodCall,
    //                 _ => Precedence::FieldAccess,
    //             },
    //             Some(Token::UIntLiteral { .. }) => Precedence::Index,
    //             _ => Precedence::Lowest,
    //         },
    //         Token::DblColon { .. } => Precedence::Path,
    //         Token::ColonColonAsterisk { .. } => Precedence::Path,
    //         Token::Bang { .. } => Precedence::Unary,
    //         Token::Percent { .. } => Precedence::Remainder,
    //         Token::Ampersand { .. } => {
    //             if self.peek_behind_by(1).is_some() {
    //                 Precedence::BitwiseAnd
    //             } else {
    //                 Precedence::Unary
    //             }
    //         }
    //         Token::Asterisk { .. } => {
    //             if self.peek_behind_by(1).is_some() {
    //                 Precedence::Product
    //             } else {
    //                 Precedence::Unary
    //             }
    //         }
    //         Token::Plus { .. } => Precedence::Sum,
    //         Token::Minus { .. } => {
    //             if self.peek_behind_by(1).is_some() {
    //                 Precedence::Difference
    //             } else {
    //                 Precedence::Unary
    //             }
    //         }

    //         Token::Some { .. } | Token::None { .. } | Token::Ok { .. } | Token::Err { .. } => {
    //             Precedence::Unary
    //         }
    //         Token::Slash { .. } => Precedence::Quotient,
    //         Token::LessThan { .. } => Precedence::LessThan,
    //         Token::Equals { .. } => Precedence::Assignment,
    //         Token::GreaterThan { .. } => Precedence::GreaterThan,
    //         Token::QuestionMark { .. } => Precedence::Unwrap,
    //         Token::Caret { .. } => Precedence::BitwiseXor,
    //         Token::Pipe { .. } => {
    //             if self.peek_behind_by(1).is_some() {
    //                 Precedence::Lowest // closure with arguments
    //             } else {
    //                 Precedence::BitwiseOr
    //             }
    //         }
    //         Token::DblDot { .. } => Precedence::Range,
    //         Token::DotDotEquals { .. } => Precedence::Range,
    //         Token::BangEquals { .. } => Precedence::NotEqual,
    //         Token::PercentEquals { .. } => Precedence::CompoundAssignment,
    //         Token::DblAsterisk { .. } => Precedence::Exponentiation,
    //         Token::AsteriskEquals { .. } => Precedence::CompoundAssignment,
    //         Token::DblAmpersand { .. } => Precedence::LogicalAnd,
    //         Token::AmpersandMut { .. } => Precedence::Unary,
    //         Token::PlusEquals { .. } => Precedence::CompoundAssignment,
    //         Token::MinusEquals { .. } => Precedence::CompoundAssignment,
    //         Token::SlashEquals { .. } => Precedence::CompoundAssignment,
    //         Token::DblLessThan { .. } => Precedence::Shift,
    //         Token::LessThanEquals { .. } => Precedence::LessThanOrEqual,
    //         Token::DblEquals { .. } => Precedence::Equal,
    //         Token::DblGreaterThan { .. } => Precedence::Shift,
    //         Token::GreaterThanEquals { .. } => Precedence::GreaterThanOrEqual,
    //         Token::DblPipe { .. } => {
    //             if self.peek_behind_by(1).is_some() {
    //                 Precedence::Lowest // closure without arguments
    //             } else {
    //                 Precedence::LogicalOr
    //             }
    //         }
    //         _ => Precedence::Lowest,
    //     }
    // }

    /// Log information about an error that occurred during parsing, including where
    /// the error occurred.
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

    fn log_unexpected_str(&mut self, expected: &str) {
        self.log_error(ParserErrorKind::UnexpectedToken {
            expected: expected.to_string(),
            found: self.peek_current(),
        });

        self.consume_token();
    }

    fn log_unexpected_token(&mut self, expected: TokenType) {
        self.log_error(ParserErrorKind::UnexpectedToken {
            expected: expected.to_string(),
            found: self.peek_current(),
        });

        self.consume_token();
    }

    /// Retrieve any errors that occurred during parsing.
    pub fn errors(&self) -> &[CompilerError<ParserErrorKind>] {
        &self.errors
    }

    fn get_identifier_patt(&mut self) -> Result<Pattern, ErrorsEmitted> {
        let kw_ref_opt = if let Some(Token::Ref { .. }) = self.peek_current() {
            self.consume_token();
            Some(Keyword::Ref)
        } else {
            None
        };

        let kw_mut_opt = if let Some(Token::Mut { .. }) = self.peek_current() {
            self.consume_token();
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
