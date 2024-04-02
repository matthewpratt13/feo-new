#![allow(dead_code)]

use crate::{
    number::{IntKind, UIntKind},
    span::Span,
    U256,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Identifier { name: String, span: Span },

    // literals
    IntLiteral { value: IntKind, span: Span },
    UIntLiteral { value: UIntKind, span: Span },
    U256Literal { value: U256, span: Span },
    StringLiteral { value: String, span: Span },
    CharLiteral { value: char, span: Span },
    BoolLiteral { value: bool, span: Span },

    // keywords
    Let { name: String, span: Span },
    Mut { name: String, span: Span },
    Ref { name: String, span: Span },
    Pub { name: String, span: Span },
    Func { name: String, span: Span },
    Contact { name: String, span: Span },
    Abstract { name: String, span: Span },
    Library { name: String, span: Span },
    Payable { name: String, span: Span },
    Storage { name: String, span: Span },
    Topic { name: String, span: Span },
    Test { name: String, span: Span },
    Return { name: String, span: Span },
    Struct { name: String, span: Span },
    Enum { name: String, span: Span },
    Trait { name: String, span: Span },
    Impl { name: String, span: Span },
    Module { name: String, span: Span },
    Extern { name: String, span: Span },
    Import { name: String, span: Span },
    Package { name: String, span: Span },
    Super { name: String, span: Span },
    SelfKeyword { name: String, span: Span },
    Const { name: String, span: Span },
    Static { name: String, span: Span },
    Unsafe { name: String, span: Span },
    Type { name: String, span: Span },
    As { name: String, span: Span },
    If { name: String, span: Span },
    Else { name: String, span: Span },
    Match { name: String, span: Span },
    For { name: String, span: Span },
    In { name: String, span: Span },
    Loop { name: String, span: Span },
    While { name: String, span: Span },
    Break { name: String, span: Span },
    Continue { name: String, span: Span },
    IntType { name: String, span: Span },
    UIntType { name: String, span: Span },
    U256Type { name: String, span: Span },
    FloatType { name: String, span: Span },
    StringType { name: String, span: Span },
    CharType { name: String, span: Span },
    BoolType { name: String, span: Span },

    // delimiters
    LParen { delim: char, span: Span },
    RParen { delim: char, span: Span },
    LBrace { delim: char, span: Span },
    RBrace { delim: char, span: Span },
    LBracket { delim: char, span: Span },
    RBracket { delim: char, span: Span },

    // separators
    Colon { punc: char, span: Span },
    Semicolon { punc: char, span: Span },
    Comma { punc: char, span: Span },
    Dot { punc: char, span: Span },

    // operators
    Bang { punc: char, span: Span },
    HashSign { punc: char, span: Span },
    Percent { punc: char, span: Span },
    Ampersand { punc: char, span: Span },
    Asterisk { punc: char, span: Span },
    Plus { punc: char, span: Span },
    Minus { punc: char, span: Span },
    Slash { punc: char, span: Span },
    LessThan { punc: char, span: Span },
    Equals { punc: char, span: Span },
    GreaterThan { punc: char, span: Span },
    QuestionMark { punc: char, span: Span },
    Caret { punc: char, span: Span },
    Pipe { punc: char, span: Span },
    DblDot { punc: String, span: Span },
    DotDotEquals { punc: String, span: Span },
    DblColon { punc: String, span: Span },
    ColonColonAsterisk { punc: String, span: Span },
    HashBang { punc: String, span: Span },
    BangEquals { punc: String, span: Span },
    PercentEquals { punc: String, span: Span },
    AsteriskEquals { punc: String, span: Span },
    DblAmpersand { punc: String, span: Span },
    PlusEquals { punc: String, span: Span },
    MinusEquals { punc: String, span: Span },
    SlashEquals { punc: String, span: Span },
    DblLessThan { punc: String, span: Span },
    LessThanEquals { punc: String, span: Span },
    DblEquals { punc: String, span: Span },
    DblGreaterThan { punc: String, span: Span },
    GreaterThanEquals { punc: String, span: Span },
    ThinArrow { punc: String, span: Span },
    FatArrow { punc: String, span: Span },
    DblPipe { punc: String, span: Span },

    // comments
    Comment { comment: String, span: Span },
    DocComment { comment: String, span: Span },

    EOF,
}

#[derive(Debug, Clone)]
pub struct TokenStream {
    tokens: Vec<Token>,
    span: Span,
}

impl<'a> TokenStream {
    pub fn new(tokens: &'a [Token], input: &'a str, start: usize, end: usize) -> Self {
        let span = Span::new(input, start, end);

        TokenStream {
            tokens: tokens.to_vec(),
            span,
        }
    }

    pub fn tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }
}
