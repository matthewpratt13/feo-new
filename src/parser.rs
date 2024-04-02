#![allow(dead_code)]

use std::collections::HashMap;

use crate::{
    error::{ParseErrorContext, ParserErrorKind},
    number::{IntKind, UIntKind},
    token::{Token, TokenStream},
    U256,
}; // final // final final

// Precedence levels enum
#[derive(Debug, Clone, Copy)]
enum Precedence {
    Prefix(u8),
    Infix(u8),
}

// final
// Define AST nodes
#[derive(Debug, Clone)]
struct StructField {
    name: String,
    value: Expression,
}

// final
// Define AST nodes
#[derive(Debug, Clone)]
enum Expression {
    Literal(Literal),
    Identifier(String),
    UnaryOp(UnaryOp, Box<Expression>),
    BinaryOp(BinaryOp, Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    ForIn(Box<Expression>, Box<Expression>, Box<Expression>),
    Array(Vec<Expression>),
    Tuple(Vec<Expression>),
    Block(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    FieldAccess(Box<Expression>, String),
    Cast(Box<Expression>, Type),
    ErrorPropagate(Box<Expression>, Box<Expression>),
    Struct(Vec<StructField>),
}

#[derive(Debug, Clone)]
enum Literal {
    Int(IntKind),
    UInt(UIntKind),
    U256(U256),
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone)]
enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Assign,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
}

// final
#[derive(Debug, Clone)]
enum Statement {
    Let(String, Expression),
    Expr(Expression),
    // Add more statement types as needed
}

#[derive(Debug, Clone)]
enum Type {
    Int,
    UInt,
    U256,
    Float,
    String,
    Char,
    Bool,
}

///////////////////////////////////////////////////////////////////////////////

// final
// Parser struct
// struct Parser<'a> {
//     stream: TokenStream<'a>,
//     current: usize,
// }

// final final
// Parser struct
// struct Parser<'a> {
//     tokens: &'a [Token],
//     current: usize,
//     precedences: HashMap<Token, (u8, u8)>, // (prefix, infix) precedence levels
// }

// final final final
// Parser struct
// struct Parser<'a> {
//     tokens: &'a [Token],
//     current: usize,
//     precedences: HashMap<Token, Precedence>,
// }

// Parser struct with error handling
#[derive(Debug)]
struct Parser<'a> {
    stream: TokenStream,
    current: usize,
    error_contexts: Vec<ParseErrorContext<'a>>,
    precedences: HashMap<Token, Precedence>,
}

///////////////////////////////////////////////////////////////////////////////

impl<'a> Parser<'a> {
    fn new(stream: TokenStream) -> Self {
        Parser {
            stream,
            current: 0,
            error_contexts: Vec::new(),
            precedences: HashMap::new(),
        }
    }

    // fn new(tokens: &'a [Token]) -> Self {
    //     Parser {
    //         tokens,
    //         current: 0,
    //         error_contexts: Vec::new(),
    //         precedences: HashMap::new(),
    //     }
    // }

    // final final
    // fn new(tokens: &'a [Token]) -> Self {
    //     let mut parser = Parser {
    //         tokens,
    //         current: 0,
    //         precedences: HashMap::new(),
    //     };
    //     parser.init_precedences();
    //     parser
    // }

    // final final final
    // fn new(tokens: &'a [Token]) -> Self {
    //     let mut parser = Parser {
    //         tokens,
    //         current: 0,
    //         precedences: HashMap::new(),
    //     };
    //     parser.init_precedences();
    //     parser
    // }

    // final final
    // fn init_precedences(&mut self) {
    //     // Define precedence levels for operators
    //     self.precedences.insert(Token::Minus, (9, 8)); // Negation
    //     self.precedences.insert(Token::Not, (9, 8)); // Logical Not
    //     self.precedences.insert(Token::Asterisk, (7, 6)); // Multiplication
    //     self.precedences.insert(Token::Slash, (7, 6)); // Division
    //     self.precedences.insert(Token::Plus, (5, 4)); // Addition
    //     self.precedences.insert(Token::Minus, (5, 4)); // Subtraction
    //     self.precedences.insert(Token::Equal, (3, 2)); // Equality
    //     self.precedences.insert(Token::NotEqual, (3, 2)); // Inequality
    //     self.precedences.insert(Token::Less, (3, 2)); // Less than
    //     self.precedences.insert(Token::LessEqual, (3, 2)); // Less than or equal to
    //     self.precedences.insert(Token::Greater, (3, 2)); // Greater than
    //     self.precedences.insert(Token::GreaterEqual, (3, 2)); // Greater than or equal to
    //     self.precedences.insert(Token::And, (1, 0)); // Logical And
    //     self.precedences.insert(Token::Or, (1, 0)); // Logical Or
    // }

    // final final final
    // fn init_precedences(&mut self) {
    //     // Define precedence levels for operators
    //     self.precedences.insert(Token::Minus, Precedence::Prefix(9)); // Negation
    //     self.precedences.insert(Token::Not, Precedence::Prefix(9)); // Logical Not
    //     self.precedences.insert(Token::Asterisk, Precedence::Infix(7)); // Multiplication
    //     self.precedences.insert(Token::Slash, Precedence::Infix(7)); // Division
    //     self.precedences.insert(Token::Plus, Precedence::Infix(5)); // Addition
    //     self.precedences.insert(Token::Minus, Precedence::Infix(5)); // Subtraction
    //     // Define precedences for other operators...
    // }

    // Method to handle parsing errors
    //  fn report_error(&mut self, line: usize, column: usize, message: String) {
    //     let snippet = "..."; // Extract a snippet of the source code if needed
    //     let error_context = ParseErrorContext::new(line, column, message, snippet);
    //     self.error_contexts.push(error_context);
    // }

    ///////////////////////////////////////////////////////////////////////////

    // final
    fn parse(&mut self) -> Result<Vec<Statement>, ParserErrorKind> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }

    ///////////////////////////////////////////////////////////////////////////

    // final
    fn parse_statement(&mut self) -> Result<Statement, ParserErrorKind> {
        let token = self.consume();
        match token {
            Some(Token::Let { .. }) => self.parse_let_statement(),
            Some(Token::If { .. }) => self.parse_if_expression(),
            Some(Token::For { .. }) => self.parse_for_in_expression(),
            Some(Token::LBrace { .. }) => self.parse_block_expression(),
            _ => {
                self.unconsume();
                self.parse_expression_statement()
            }
        }
    }

    // final - 6
    // fn parse_statement(&mut self) -> Result<Statement, String> {
    //     let token = self.consume()?;
    //     match token {
    //         Token::Let => self.parse_let_statement(),
    //         _ => Err(format!("Expected 'let' keyword, found {:?}", token)),
    //     }
    // }

    ///////////////////////////////////////////////////////////////////////////

    // final
    fn parse_let_statement(&mut self) -> Result<Statement, ParserErrorKind> {
        let identifier = self.expect_identifier()?;

        self.expect_token(Token::Equals {
            punc: '=',
            span: self.stream.span(),
        })?;

        let value = self.parse_expression(Precedence::Prefix(0))?;

        self.expect_token(Token::Semicolon {
            punc: ';',
            span: self.stream.span(),
        })?;

        Ok(Statement::Let(identifier, value))
    }

    // final - 6
    // fn parse_let_statement(&mut self) -> Result<Statement, String> {
    //     let ident_expr = self.parse_expression(0)?;

    //     // Expect assignment operator
    //     let token = self.consume()?;
    //     if token != Token::Assignment {
    //         return Err(format!("Expected '=', found {:?}", token));
    //     }

    //     let value_expr = self.parse_expression(0)?;

    //     // Expect semicolon
    //     let token = self.consume()?;
    //     if token != Token::Semicolon {
    //         return Err(format!("Expected ';', found {:?}", token));
    //     }

    //     Ok(Statement::Let(Box::new(ident_expr), Box::new(value_expr)))
    // }

    // final
    fn parse_expression_statement(&mut self) -> Result<Statement, ParserErrorKind> {
        let expression = &self.parse_expression(Precedence::Prefix(0))?;
        self.expect_token(Token::Semicolon {
            punc: ';',
            span: self.stream.span(),
        })?;
        Ok(Statement::Expr(expression.clone()))
    }

    ///////////////////////////////////////////////////////////////////////////

    // final final final
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParserErrorKind> {
        let mut left_expr = self.parse_prefix()?;

        while let Some(next_token_precedence) =
            self.precedence(&self.peek().ok_or(ParserErrorKind::TokenNotFound)?)
        {
            match precedence {
                Precedence::Prefix(prefix_precedence)
                    if prefix_precedence >= next_token_precedence.inner() =>
                {
                    break
                }
                Precedence::Infix(infix_precedence)
                    if infix_precedence > next_token_precedence.inner() =>
                {
                    break
                }
                _ => (),
            }
            left_expr = self.parse_infix(left_expr, next_token_precedence)?;
        }

        Ok(left_expr)
    }

    // final final
    // fn parse_expression(&mut self, precedence: u8) -> Result<Expr, String> {
    //     let mut left_expr = self.parse_prefix()?;

    //     while let Some((prefix_precedence, infix_precedence)) = self.get_precedence(&self.peek()) {
    //         if precedence >= infix_precedence {
    //             break;
    //         }
    //         left_expr = self.parse_infix(left_expr, prefix_precedence)?;
    //     }

    //     Ok(left_expr)
    // }

    // final - 7 / original (no final)
    // fn parse_expression(&mut self, precedence: u8) -> Result<Expr, String> {
    //     let mut left_expr = self.parse_prefix()?;

    //     while let Some(token) = self.peek() {
    //         if let Some(op) = self.get_binary_op(token) {
    //             let op_precedence = self.get_precedence(op);

    //             if op_precedence < precedence {
    //                 break;
    //             }

    //             self.advance();

    //             let right_expr = self.parse_infix(left_expr.clone(), op)?;
    //             left_expr = right_expr;
    //         } else {
    //             break;
    //         }
    //     }

    //     Ok(left_expr)
    // }

    // let mut left_expr = self.parse_prefix()?;

    //     while let Some(token) = self.peek() {
    //         if let Some(op) = self.get_binary_op(token) {
    //             let op_precedence = self.get_precedence(op);

    //             if op_precedence < precedence {
    //                 break;
    //             }

    //             self.advance();

    //             let right_expr = self.parse_infix(left_expr.clone(), op)?;
    //             left_expr = right_expr;
    //         } else {
    //             break;
    //         }
    //     }

    //     Ok(left_expr)
    // }

    // final final
    fn parse_prefix(&mut self) -> Result<Expression, ParserErrorKind> {
        match self.consume() {
            Some(token) => match token {
                Token::IntLiteral { value, .. } => Ok(Expression::Literal(Literal::Int(value))),
                Token::UIntLiteral { value, .. } => Ok(Expression::Literal(Literal::UInt(value))),
                Token::U256Literal { value, .. } => Ok(Expression::Literal(Literal::U256(value))),
                Token::StringLiteral { value, .. } => {
                    Ok(Expression::Literal(Literal::String(value)))
                }
                Token::BoolLiteral { value, .. } => Ok(Expression::Literal(Literal::Bool(value))),
                Token::Identifier { name, .. } => Ok(Expression::Identifier(name)),
                Token::Minus { .. } => {
                    let expr = self.parse_expression(Precedence::Prefix(9))?;
                    Ok(Expression::UnaryOp(UnaryOp::Negate, Box::new(expr)))
                }
                Token::Bang { .. } => {
                    let expr = self.parse_expression(Precedence::Prefix(9))?;
                    Ok(Expression::UnaryOp(UnaryOp::Not, Box::new(expr)))
                }
                Token::LParen { .. } => {
                    let expr = self.parse_expression(Precedence::Prefix(0))?;
                    self.expect(Token::RParen {
                        delim: ')',
                        span: self.stream.span(),
                    })?;
                    Ok(expr)
                }
                _ => Err(ParserErrorKind::InvalidToken { token }),
            },
            None => Err(ParserErrorKind::UnexpectedEndOfInput),
        }
    }

    // final - 7 / original (no final)
    // fn parse_prefix(&mut self) -> Result<Expr, String> {
    //     let token = self.consume()?;

    //     match token {
    //         Token::IntLiteral(value) => Ok(Expr::IntLiteral(value)),
    //         Token::LParen => {
    //             let expr = self.parse_expression(0)?;

    //             if self.consume() != Ok(Token::RParen) {
    //                 return Err("Expected closing parenthesis".to_string());
    //             }

    //             Ok(expr)
    //         }
    //         _ => Err(format!("Unexpected token: {:?}", token)),
    //     }
    // }

    // final final final
    // fn parse_infix(&mut self, left_expr: Expr) -> Result<Expr, String> {
    //     // Infix parsing implementation...
    //     unimplemented!()
    // }

    // final final
    fn parse_infix(
        &mut self,
        left_expr: Expression,
        precedence: Precedence,
    ) -> Result<Expression, ParserErrorKind> {
        let token = self
            .consume()
            .ok_or(ParserErrorKind::UnexpectedEndOfInput)?;
        match token {
            Token::Asterisk { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Multiply,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::Slash { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Divide,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::Plus { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::Minus { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Subtract,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::DblEquals { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Equal,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::BangEquals { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::NotEqual,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::LessThan { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Less,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::LessThanEquals { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::LessEqual,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::GreaterThan { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Greater,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::GreaterThanEquals { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::GreaterEqual,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::DblAmpersand { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::And,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            Token::DblPipe { .. } => {
                let right_expr = self.parse_expression(precedence)?;
                Ok(Expression::BinaryOp(
                    BinaryOp::Or,
                    Box::new(left_expr),
                    Box::new(right_expr),
                ))
            }
            _ => Err(ParserErrorKind::InvalidToken { token }),
        }
    }

    // final - 7 / original (no final)
    // fn parse_infix(&mut self, left_expr: Expr, op: BinaryOp) -> Result<Expr, String> {
    //     let right_expr = self.parse_expression(self.get_precedence(op) + 1)?;

    //     Ok(Expr::BinaryOp(op, Box::new(left_expr), Box::new(right_expr)))
    // }

    ///////////////////////////////////////////////////////////////////////////

    // final - 3 (inner expression of a parenthesized expression)
    // fn parse_primary(&mut self) -> Result<Expr, String> {
    //     let token = self.consume()?;
    //     match token {
    //         Token::LParen => {
    //             let expr = self.parse_expression(0)?;
    //             self.expect_token(Token::RParen)?;
    //             Ok(Expr::Parenthesized(Box::new(expr)))
    //         }
    //         Token::Identifier(identifier) => Ok(Expr::Identifier(identifier)),
    //         Token::IntLiteral(value) => Ok(Expr::IntLiteral(value)),
    //         _ => Err(format!("Unexpected token: {:?}", token)),
    //     }
    // }

    ///////////////////////////////////////////////////////////////////////////

    // final
    fn parse_call_expression(&mut self, callee: Expression) -> Result<Expression, ParserErrorKind> {
        let mut arguments = Vec::new();

        // Expect opening parenthesis
        let token = self.consume().ok_or(ParserErrorKind::TokenNotFound)?;
        if token
            != (Token::LParen {
                delim: '(',
                span: self.stream.span(),
            })
        {
            return Err(ParserErrorKind::UnexpectedToken {
                expected: "`(`".to_string(),
                found: token,
            });
        }

        // Parse arguments
        loop {
            if let Some(Token::RParen { delim: ')', .. }) = self.peek() {
                // End of arguments
                self.advance();
                break;
            }

            let arg_expr = self.parse_expression(Precedence::Infix(0))?;
            arguments.push(arg_expr);

            // Expect comma or closing parenthesis
            let token = self.consume().ok_or(ParserErrorKind::TokenNotFound)?;
            match token {
                Token::Comma { .. } => continue, // More arguments
                Token::RParen { .. } => break,   // End of arguments
                _ => {
                    return Err(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `)`".to_string(),
                        found: token,
                    })
                }
            }
        }

        Ok(Expression::Call(Box::new(callee), arguments))
    }

    // non-existent
    // fn parse_binary_expression(
    //     &mut self,
    //     precedence: u8,
    //     lhs: Expression,
    // ) -> Result<Expression, String> {
    //     // Your existing implementation of parse_binary_expression
    //     unimplemented!()
    // }

    // final
    fn parse_index_expression(
        &mut self,
        array_expr: Expression,
    ) -> Result<Expression, ParserErrorKind> {
        self.expect_token(Token::LBracket {
            delim: '{',
            span: self.stream.span(),
        })?;
        let index_expr = self.parse_expression(Precedence::Infix(0))?;
        self.expect_token(Token::RBracket {
            delim: '}',
            span: self.stream.span(),
        })?;
        Ok(Expression::Index(
            Box::new(array_expr),
            Box::new(index_expr),
        ))
    }

    // final
    fn parse_field_access_expression(
        &mut self,
        object_expr: Expression,
    ) -> Result<Expression, ParserErrorKind> {
        self.expect_token(Token::Dot {
            punc: '.',
            span: self.stream.span(),
        })?;
        let field_token = self.consume().ok_or(ParserErrorKind::TokenNotFound)?;
        if let Token::Identifier { name, .. } = field_token {
            Ok(Expression::FieldAccess(Box::new(object_expr), name))
        } else {
            Err(ParserErrorKind::UnexpectedToken {
                expected: "identifier after `.`".to_string(),
                found: field_token,
            })
        }
    }

    // final
    fn parse_if_expression(&mut self) -> Result<Statement, ParserErrorKind> {
        self.expect_token(Token::If {
            name: "if".to_string(),
            span: self.stream.span(),
        })?;
        self.expect_token(Token::LParen {
            delim: '(',
            span: self.stream.span(),
        })?;
        let condition = Box::new(self.parse_expression(Precedence::Infix(0))?);
        self.expect_token(Token::RParen {
            delim: ')',
            span: self.stream.span(),
        })?;
        let true_branch = Box::new(self.parse_expression(Precedence::Infix(0))?);

        let false_branch = if self.match_token(Token::Else {
            name: "else".to_string(),
            span: self.stream.span(),
        }) {
            Some(Box::new(self.parse_expression(Precedence::Infix(0))?))
        } else {
            None
        };

        Ok(Statement::Expr(Expression::If(
            condition,
            true_branch,
            false_branch,
        )))
    }

    // final
    fn parse_for_in_expression(&mut self) -> Result<Statement, ParserErrorKind> {
        self.expect_token(Token::For {
            name: "for".to_string(),
            span: self.stream.span(),
        })?;
        self.expect_token(Token::Let {
            name: "let".to_string(),
            span: self.stream.span(),
        })?; // Assuming for-in loops are of the form "for let variable in iterable"
        let variable = Box::new(Expression::Identifier(self.consume_identifier()?));
        self.expect_token(Token::In {
            name: "in".to_string(),
            span: self.stream.span(),
        })?;
        let iterable = Box::new(self.parse_expression(Precedence::Infix(0))?);
        let body = Box::new(self.parse_expression(Precedence::Infix(0))?);
        Ok(Statement::Expr(Expression::ForIn(variable, iterable, body)))
    }

    // final
    fn parse_block_expression(&mut self) -> Result<Statement, ParserErrorKind> {
        self.expect_token(Token::LBrace {
            delim: '{',
            span: self.stream.span(),
        })?;
        let mut expressions = Vec::new();
        while !self.check_token(Token::RBrace {
            delim: '}',
            span: self.stream.span(),
        }) {
            expressions.push(self.parse_expression(Precedence::Infix(0))?);
        }
        self.expect_token(Token::RBrace {
            delim: '}',
            span: self.stream.span(),
        })?;
        Ok(Statement::Expr(Expression::Block(expressions)))
    }

    // final
    fn parse_array_expression(&mut self) -> Result<Expression, ParserErrorKind> {
        self.expect_token(Token::LBracket {
            delim: '[',
            span: self.stream.span(),
        })?;
        let mut elements = Vec::new();
        while !self.check_token(Token::RBracket {
            delim: ']',
            span: self.stream.span(),
        }) {
            elements.push(self.parse_expression(Precedence::Infix(0))?);
            if !self.match_token(Token::Comma {
                punc: '.',
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

    // final
    fn parse_tuple_expression(&mut self) -> Result<Expression, ParserErrorKind> {
        self.expect_token(Token::LParen {
            delim: '(',
            span: self.stream.span(),
        })?;
        let mut elements = Vec::new();
        while !self.check_token(Token::RParen {
            delim: ')',
            span: self.stream.span(),
        }) {
            elements.push(self.parse_expression(Precedence::Infix(0))?);
            if !self.match_token(Token::Comma {
                punc: '.',
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

    // final
    fn parse_struct(&mut self) -> Result<Expression, ParserErrorKind> {
        let mut fields = Vec::new();

        // Expect opening brace
        let token = self.consume().ok_or(ParserErrorKind::TokenNotFound)?;
        if token
            != (Token::LBrace {
                delim: '{',
                span: self.stream.span(),
            })
        {
            return Err(ParserErrorKind::UnexpectedToken {
                expected: "`{`".to_string(),
                found: token,
            });
        }

        // Parse struct fields
        loop {
            // Expect field name
            let field_name = match self.consume().ok_or(ParserErrorKind::TokenNotFound)? {
                Token::Identifier { name, .. } => name,
                Token::RBrace { .. } => break, // End of struct
                _ => {
                    return Err(ParserErrorKind::UnexpectedToken {
                        expected: "identifier or `}`".to_string(),
                        found: self
                            .stream
                            .tokens()
                            .get(self.current)
                            .cloned()
                            .ok_or(ParserErrorKind::TokenNotFound)?,
                    })
                }
            };

            // Expect colon
            let token = self.consume().ok_or(ParserErrorKind::TokenNotFound)?;
            if token
                != (Token::Colon {
                    punc: '.',
                    span: self.stream.span(),
                })
            {
                return Err(ParserErrorKind::UnexpectedToken {
                    expected: "`:`".to_string(),
                    found: token,
                });
            }

            // Parse field value
            let field_value = self.parse_expression(Precedence::Infix(0))?;

            fields.push(StructField {
                name: field_name,
                value: field_value,
            });

            // Expect comma or closing brace
            let token = self.consume().ok_or(ParserErrorKind::TokenNotFound)?;
            match token {
                Token::Comma { .. } => continue, // More fields
                Token::RBrace { .. } => break,   // End of struct
                _ => {
                    return Err(ParserErrorKind::UnexpectedToken {
                        expected: "`,` or `}`".to_string(),
                        found: token,
                    })
                }
            }
        }

        Ok(Expression::Struct(fields))
    }

    ///////////////////////////////////////////////////////////////////////////

    // final
    fn expect_identifier(&mut self) -> Result<String, ParserErrorKind> {
        match self.consume().ok_or(ParserErrorKind::TokenNotFound)? {
            Token::Identifier { name, .. } => Ok(name),
            _ => Err(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self
                    .stream
                    .tokens()
                    .get(self.current)
                    .cloned()
                    .ok_or(ParserErrorKind::TokenNotFound)?,
            }),
        }
    }

    // final
    fn consume_identifier(&mut self) -> Result<String, ParserErrorKind> {
        if let Token::Identifier { name, .. } =
            self.consume().ok_or(ParserErrorKind::TokenNotFound)?
        {
            Ok(name)
        } else {
            Err(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self
                    .stream
                    .tokens()
                    .get(self.current)
                    .cloned()
                    .ok_or(ParserErrorKind::TokenNotFound)?,
            })
        }
    }

    // final
    fn match_token(&mut self, expected: Token) -> bool {
        if self.check_token(expected.clone()) {
            self.current += 1;
            true
        } else {
            false
        }
    }

    // final
    fn check_token(&self, expected: Token) -> bool {
        if self.current < self.stream.tokens().len() {
            self.stream.tokens()[self.current] == expected
        } else {
            false
        }
    }

    // final
    fn expect_token(&mut self, expected: Token) -> Result<(), ParserErrorKind> {
        let token = self.consume().ok_or(ParserErrorKind::TokenNotFound)?;
        if token == expected {
            Ok(())
        } else {
            Err(ParserErrorKind::UnexpectedToken {
                expected: format!("`{:#?}`", expected),
                found: token,
            })
        }
    }

    // final final final
    fn expect(&mut self, expected: Token) -> Result<(), ParserErrorKind> {
        match self.consume() {
            Some(token) if token == expected => Ok(()),
            Some(token) => Err(ParserErrorKind::UnexpectedToken {
                expected: format!("`{:#?}`", expected),
                found: token,
            }),
            None => Err(ParserErrorKind::UnexpectedEndOfInput),
        }
    }

    // final -7 / original
    fn get_binary_op(&self, token: &Token) -> Option<BinaryOp> {
        match token {
            Token::Plus { .. } => Some(BinaryOp::Add),
            Token::Minus { .. } => Some(BinaryOp::Subtract),
            _ => None,
        }
    }

    // final final
    fn get_precedence(&self, token: &Token) -> Option<Precedence> {
        match self.precedences.get(token) {
            Some(p) => Some(*p),
            None => None,
        }
    }

    // final -7 / original
    // fn get_precedence(&self, op: BinaryOp) -> u8 {
    //     match op {
    //         BinaryOp::Add | BinaryOp::Subtract => 1,
    //     }
    // }

    // final final final
    fn precedence(&self, token: &Token) -> Option<Precedence> {
        self.precedences.get(token).cloned()
    }

    // final - 7 / original / final final final
    fn peek(&self) -> Option<Token> {
        self.stream.tokens().get(self.current).cloned()
    }

    // final final final
    fn consume(&mut self) -> Option<Token> {
        let token = self.peek();
        if token.is_some() {
            self.current += 1;
        }
        token
    }

    // final
    // fn consume(&mut self) -> Result<Token, String> {
    //     if !self.is_at_end() {
    //         let token = self.tokens[self.current].clone();
    //         self.current += 1;
    //         Ok(token)
    //     } else {
    //         Err("Unexpected end of input".to_string())
    //     }
    // }

    // final - 1
    // fn consume(&mut self) -> Result<Token, String> {
    //     if self.current < self.tokens.len() {
    //         let token = self.tokens[self.current].clone();
    //         self.current += 1;
    //         Ok(token)
    //     } else {
    //         Err("Unexpected end of input".to_string())
    //     }
    // }

    // final - 7 / original (no final)
    fn advance(&mut self) {
        self.current += 1;
    }

    // final
    fn unconsume(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    // final
    fn is_at_end(&self) -> bool {
        self.current >= self.stream.tokens().len()
    }
}

impl Precedence {
    fn inner(self) -> u8 {
        match self {
            Precedence::Prefix(i) => i,
            Precedence::Infix(i) => i,
        }
    }
}

///////////////////////////////////////////////////////////////////////////////

// final final
// fn main() {
//     let tokens = vec![
//         Token::Minus,
//         Token::IntLiteral(5),
//         Token::Asterisk,
//         Token::LParen,
//         Token::IntLiteral(3),
//         Token::Plus,
//         Token::IntLiteral(4),
//         Token::RParen,
//         Token::Semicolon,
//     ];

//     let mut parser = Parser::new(&tokens);
//     match parser.parse_expression(0) {
//         Ok(expr) => println!("Parsed expression: {:?}", expr),
//         Err(err) => println!("Error: {}", err),
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final
// fn main() {
//     // Tokenize your code
//     let tokens = vec![Token::Let, Token::Identifier("x".to_string()), Token::Assignment, Token::IntLiteral(42), Token::Semicolon];
//     let mut parser = Parser::new(&tokens);

//     // Parse statements
//     match parser.parse() {
//         Ok(statements) => {
//             println!("Parsed statements:");
//             for statement in statements {
//                 println!("{:?}", statement);
//             }
//         }
//         Err(err) => {
//             println!("Error: {}", err);
//         }
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 1 (block expression)
// fn main() {
//     // Example usage
//     let tokens = vec![
//         Token::LBrace,
//         Token::Identifier("x".to_string()),
//         Token::IntLiteral(1),
//         Token::RBrace,
//     ];

//     let mut parser = Parser::new(&tokens);
//     if let Ok(expression) = parser.parse_expression(0) {
//         println!("Parsed expression: {:?}", expression);
//     } else {
//         println!("Failed to parse expression");
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 2 (if expression)
// fn main() {
//     // Example usage
//     let tokens = vec![
//         Token::If,
//         Token::LParen,
//         Token::Identifier("condition".to_string()),
//         Token::RParen,
//         Token::LBrace,
//         Token::IntLiteral(1),
//         Token::RBrace,
//         Token::Else,
//         Token::LBrace,
//         Token::IntLiteral(2),
//         Token::RBrace,
//         Token::For,
//         Token::Identifier("let".to_string()),
//         Token::Identifier("element".to_string()),
//         Token::In,
//         Token::LBrace,
//         Token::IntLiteral(1),
//         Token::Comma,
//         Token::IntLiteral(2),
//         Token::Comma,
//         Token::IntLiteral(3),
//         Token::RBrace,
//         Token::LBrace,
//         Token::Identifier("element".to_string()),
//         Token::RBrace,
//         Token::LParen,
//         Token::IntLiteral(1),
//         Token::Comma,
//         Token::IntLiteral(2),
//         Token::Comma,
//         Token::IntLiteral(3),
//         Token::RParen,
//     ];

//     let mut parser = Parser::new(&tokens);
//     if let Ok(expression) = parser.parse_expression(0) {
//         println!("Parsed expression: {:?}", expression);
//     } else {
//         println!("Failed to parse expression");
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 3 (parenthesized expression)
// fn main() {
//     // Example usage
//     let tokens = vec![
//         Token::LParen,
//         Token::IntLiteral(3),
//         Token::Plus,
//         Token::IntLiteral(5),
//         Token::RParen,
//         Token::Asterisk,
//         Token::IntLiteral(2),
//         Token::Dot,
//         Token::Identifier("field".to_string()),
//         Token::LBracket,
//         Token::IntLiteral(1),
//         Token::RBracket,
//     ];

//     let mut parser = Parser::new(&tokens);
//     if let Ok(expression) = parser.parse_expression(0) {
//         println!("Parsed expression: {:?}", expression);
//     } else {
//         println!("Failed to parse expression");
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 4 (call expression)
// fn main() {
//     // Tokenize your code

//     let tokens = vec![/* Tokenize your code here */];
//     let mut parser = Parser::new(&tokens);

//     // Parse expressions, statements, etc.

//     // Example: Parsing a call expression
//     let callee_expr = Expr::Identifier("some_function".to_string());
//     if let Ok(expr) = parser.parse_call_expression(callee_expr) {
//         println!("Parsed call expression: {:?}", expr);
//     } else {
//         println!("Failed to parse call expression");
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 5 (struct)
// fn main() {
//     // Tokenize your code

//     let tokens = vec![/* Tokenize your code here */];
//     let mut parser = Parser::new(&tokens);

//     // Parse expressions, statements, etc.

//     if let Ok(expr) = parser.parse_struct() {
//         println!("Parsed struct: {:?}", expr);
//     } else {
//         println!("Failed to parse struct");
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 6 (let statement)
// fn main() {
//     // Tokenize your code

//     let tokens = vec![/* Tokenize your code here */];
//     let mut parser = Parser::new(&tokens);

//     // Parse statements
//     if let Ok(statement) = parser.parse_statement() {
//         println!("Parsed statement: {:?}", statement);
//     } else {
//         println!("Failed to parse statement");
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 7 / original (binary op)
// fn main() {
//     let tokens = vec![
//         Token::IntLiteral(5),
//         Token::Plus,
//         Token::IntLiteral(3),
//         Token::Minus,
//         Token::IntLiteral(2),
//         Token::EOF,
//     ];

//     let mut parser = Parser::new(&tokens);

//     if let Ok(expr) = parser.parse_expression(0) {
//         println!("Parsed expression: {}", expr);
//     } else {
//         println!("Failed to parse expression");
//     }
// }

///////////////////////////////////////////////////////////////////////////////

// final - 8 (template)
// fn main() {
//     let tokens = vec![/* Tokenize your code here */];
//     let mut parser = Parser::new(&tokens);

//     // Example: Parsing an expression
//     if let Ok(expr) = parser.parse_expression(0) {
//         // Handle parsed expression
//     } else {
//         // Handle parsing error
//     }
// }
