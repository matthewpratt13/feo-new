#![allow(dead_code)]
#![allow(unused_variables)]

use crate::{
    number::{IntKind, UIntKind},
    span::Span,
    token::Token,
    U256,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    name: String,
    span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int { value: IntKind, span: Span },
    UInt { value: UIntKind, span: Span },
    U256 { value: U256, span: Span },
    String { value: String, span: Span },
    Char { value: char, span: Span },
    Bool { value: bool, span: Span },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Let { span: Span },
    Func { span: Span },
    // Add span to all keyword variants
}

#[derive(Debug, Clone, PartialEq)]
pub enum Delimiter {
    LParen { span: Span },
    RParen { span: Span },
    LBrace { span: Span },
    RBrace { span: Span },
    LBracket { span: Span },
    RBracket { span: Span },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Punctuation {
    Comma { span: Span },
    Dot { span: Span },
    Semicolon { span: Span },
    // Add span to all punctuation variants
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenGroup {
    Identifier(Identifier),
    Literal(Literal),
    Keyword(Keyword),
    Delimiter(Delimiter),
    Punctuation(Punctuation),
}

impl TokenGroup {
    /// Convenience methods to convert from and to original `Token`
    fn from_token(token: Token) -> TokenGroup {
        match token {
            // Match and convert to the appropriate grouped variant
            Token::Identifier { name, span } => TokenGroup::Identifier(Identifier { name, span }),
            Token::IntLiteral { value, span } => TokenGroup::Literal(Literal::Int { value, span }),
            Token::UIntLiteral { value, span } => {
                TokenGroup::Literal(Literal::UInt { value, span })
            }
            Token::U256Literal { value, span } => {
                TokenGroup::Literal(Literal::U256 { value, span })
            }
            Token::StringLiteral { value, span } => {
                TokenGroup::Literal(Literal::String { value, span })
            }
            Token::CharLiteral { value, span } => {
                TokenGroup::Literal(Literal::Char { value, span })
            }
            Token::BoolLiteral { value, span } => {
                TokenGroup::Literal(Literal::Bool { value, span })
            }

            Token::LParen { span, .. } => TokenGroup::Delimiter(Delimiter::LParen { span }),
            Token::RParen { span, .. } => TokenGroup::Delimiter(Delimiter::RParen { span }),
            Token::LBrace { span, .. } => TokenGroup::Delimiter(Delimiter::LBrace { span }),
            Token::RBrace { span, .. } => TokenGroup::Delimiter(Delimiter::RBrace { span }),
            Token::LBracket { span, .. } => TokenGroup::Delimiter(Delimiter::LBracket { span }),
            Token::RBracket { span, .. } => TokenGroup::Delimiter(Delimiter::RBracket { span }),
            Token::Let { name, span } => todo!(),
            Token::Mut { name, span } => todo!(),
            Token::Ref { name, span } => todo!(),
            Token::Pub { name, span } => todo!(),
            Token::Func { name, span } => todo!(),
            Token::Contact { name, span } => todo!(),
            Token::Abstract { name, span } => todo!(),
            Token::Library { name, span } => todo!(),
            Token::Payable { name, span } => todo!(),
            Token::Storage { name, span } => todo!(),
            Token::Topic { name, span } => todo!(),
            Token::Test { name, span } => todo!(),
            Token::Return { name, span } => todo!(),
            Token::Struct { name, span } => todo!(),
            Token::Enum { name, span } => todo!(),
            Token::Trait { name, span } => todo!(),
            Token::Impl { name, span } => todo!(),
            Token::Module { name, span } => todo!(),
            Token::Extern { name, span } => todo!(),
            Token::Import { name, span } => todo!(),
            Token::Package { name, span } => todo!(),
            Token::Super { name, span } => todo!(),
            Token::SelfKeyword { name, span } => todo!(),
            Token::Const { name, span } => todo!(),
            Token::Static { name, span } => todo!(),
            Token::Unsafe { name, span } => todo!(),
            Token::Type { name, span } => todo!(),
            Token::As { name, span } => todo!(),
            Token::If { name, span } => todo!(),
            Token::Else { name, span } => todo!(),
            Token::Match { name, span } => todo!(),
            Token::For { name, span } => todo!(),
            Token::In { name, span } => todo!(),
            Token::Loop { name, span } => todo!(),
            Token::While { name, span } => todo!(),
            Token::Break { name, span } => todo!(),
            Token::Continue { name, span } => todo!(),
            Token::IntType { name, span } => todo!(),
            Token::UIntType { name, span } => todo!(),
            Token::U256Type { name, span } => todo!(),
            Token::FloatType { name, span } => todo!(),
            Token::StringType { name, span } => todo!(),
            Token::CharType { name, span } => todo!(),
            Token::BoolType { name, span } => todo!(),
            Token::Colon { punc, span } => todo!(),
            Token::Semicolon { punc, span } => todo!(),
            Token::Comma { punc, span } => todo!(),
            Token::Dot { punc, span } => todo!(),
            Token::Bang { punc, span } => todo!(),
            Token::HashSign { punc, span } => todo!(),
            Token::Percent { punc, span } => todo!(),
            Token::Ampersand { punc, span } => todo!(),
            Token::Asterisk { punc, span } => todo!(),
            Token::Plus { punc, span } => todo!(),
            Token::Minus { punc, span } => todo!(),
            Token::Slash { punc, span } => todo!(),
            Token::LessThan { punc, span } => todo!(),
            Token::Equals { punc, span } => todo!(),
            Token::GreaterThan { punc, span } => todo!(),
            Token::QuestionMark { punc, span } => todo!(),
            Token::Caret { punc, span } => todo!(),
            Token::Pipe { punc, span } => todo!(),
            Token::DblDot { punc, span } => todo!(),
            Token::DotDotEquals { punc, span } => todo!(),
            Token::DblColon { punc, span } => todo!(),
            Token::ColonColonAsterisk { punc, span } => todo!(),
            Token::HashBang { punc, span } => todo!(),
            Token::BangEquals { punc, span } => todo!(),
            Token::PercentEquals { punc, span } => todo!(),
            Token::AsteriskEquals { punc, span } => todo!(),
            Token::DblAmpersand { punc, span } => todo!(),
            Token::PlusEquals { punc, span } => todo!(),
            Token::MinusEquals { punc, span } => todo!(),
            Token::SlashEquals { punc, span } => todo!(),
            Token::DblLessThan { punc, span } => todo!(),
            Token::LessThanEquals { punc, span } => todo!(),
            Token::DblEquals { punc, span } => todo!(),
            Token::DblGreaterThan { punc, span } => todo!(),
            Token::GreaterThanEquals { punc, span } => todo!(),
            Token::ThinArrow { punc, span } => todo!(),
            Token::FatArrow { punc, span } => todo!(),
            Token::DblPipe { punc, span } => todo!(),
            Token::Comment { comment, span } => todo!(),
            Token::DocComment { comment, span } => todo!(),
            Token::EOF => todo!(),
        }
    }

    fn to_token(&self) -> Option<Token> {
        match self {
            // Match and convert back to original Token variant
            // For example:
            TokenGroup::Identifier(id) => Some(Token::Identifier {
                name: id.name.clone(),
                span: id.span.clone(),
            }),
            TokenGroup::Literal(lit) => match lit {
                Literal::Int { value, span } => Some(Token::IntLiteral {
                    value: *value,
                    span: span.clone(),
                }),
                Literal::UInt { value, span } => Some(Token::UIntLiteral {
                    value: *value,
                    span: span.clone(),
                }),
                Literal::U256 { value, span } => Some(Token::U256Literal {
                    value: *value,
                    span: span.clone(),
                }),
                Literal::String { value, span } => Some(Token::StringLiteral {
                    value: value.clone(),
                    span: span.clone(),
                }),

                Literal::Char { value, span } => Some(Token::CharLiteral {
                    value: *value,
                    span: span.clone(),
                }),

                Literal::Bool { value, span } => Some(Token::BoolLiteral {
                    value: *value,
                    span: span.clone(),
                }),
            },

            TokenGroup::Keyword(kw) => match kw {
                _ => todo!(),
            },

            TokenGroup::Delimiter(delim) => match delim {
                Delimiter::LParen { span } => Some(Token::LParen {
                    delim: '(',
                    span: span.clone(),
                }),
                Delimiter::RParen { span } => Some(Token::RParen {
                    delim: ')',
                    span: span.clone(),
                }),
                Delimiter::LBrace { span } => Some(Token::LParen {
                    delim: '{',
                    span: span.clone(),
                }),
                Delimiter::RBrace { span } => Some(Token::LParen {
                    delim: '}',
                    span: span.clone(),
                }),
                Delimiter::LBracket { span } => Some(Token::LParen {
                    delim: '[',
                    span: span.clone(),
                }),
                Delimiter::RBracket { span } => Some(Token::LParen {
                    delim: ']',
                    span: span.clone(),
                }),
            },

            TokenGroup::Punctuation(punc) => match punc {
                _ => todo!(),
            },
        }
    }
}
