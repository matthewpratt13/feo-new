#![allow(dead_code)]

use std::fmt;

use crate::{
    ast::{BigUInt, Byte, Bytes, Hash, Int, Str, UInt},
    span::Span,
};

/// Enum representing the different types of tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
    Identifier { name: String, span: Span },

    // literals
    IntLiteral { value: Int, span: Span },
    UIntLiteral { value: UInt, span: Span },
    BigUIntLiteral { value: BigUInt, span: Span },
    ByteLiteral { value: Byte, span: Span },
    BytesLiteral { value: Bytes, span: Span },
    HashLiteral { value: Hash, span: Span },
    StrLiteral { value: Str, span: Span },
    CharLiteral { value: char, span: Span },
    BoolLiteral { value: bool, span: Span },

    // keywords
    Let { name: String, span: Span },
    Mut { name: String, span: Span },
    Ref { name: String, span: Span },
    Pub { name: String, span: Span },
    Func { name: String, span: Span },
    Contract { name: String, span: Span }, // type of module, notated `#![contract]`
    Library { name: String, span: Span },  // type of module, notated `#![library]`
    Script { name: String, span: Span },   // type of module, notated `#![script]`
    Interface { name: String, span: Span }, // type of module, notated `#![interface]`
    Constructor { name: String, span: Span }, // type of function notated `#[constructor]`
    Modifier { name: String, span: Span }, // type of function, notated `#[modifier]`
    Test { name: String, span: Span },     // type of function, notated `#[test]`
    View { name: String, span: Span },     // function attribute, notated` #[view]`
    Extern { name: String, span: Span },   // function attribute, notated `#[extern]`
    Payable { name: String, span: Span },  // function attribute, notated `#[payable]`
    Event { name: String, span: Span },    // type of struct, notated `#[event]`
    Error { name: String, span: Span },    // type of struct, notated `#[error]`
    Storage { name: String, span: Span },  // variable attribute, notated `#[storage]
    Topic { name: String, span: Span },    // variable attribute, notated `#[topic]`
    Calldata { name: String, span: Span }, // variable attribute, notated `#[calldata]`
    Unsafe { name: String, span: Span },   // variable attribute, notated `#![unsafe]`
    Return { name: String, span: Span },
    Struct { name: String, span: Span },
    Enum { name: String, span: Span },
    Trait { name: String, span: Span },
    Impl { name: String, span: Span },
    Mod { name: String, span: Span },
    Import { name: String, span: Span },
    Package { name: String, span: Span },
    Super { name: String, span: Span },
    SelfKeyword { name: String, span: Span },
    Const { name: String, span: Span },
    Static { name: String, span: Span },
    Alias { name: String, span: Span },
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

    // types
    I32Type { name: String, span: Span },
    I64Type { name: String, span: Span },
    I128Type { name: String, span: Span },
    U8Type { name: String, span: Span },
    U16Type { name: String, span: Span },
    U32Type { name: String, span: Span },
    U64Type { name: String, span: Span },
    U128Type { name: String, span: Span },
    U256Type { name: String, span: Span },
    U512Type { name: String, span: Span },
    ByteType { name: String, span: Span },
    B2Type { name: String, span: Span },
    B4Type { name: String, span: Span },
    B8Type { name: String, span: Span },
    B16Type { name: String, span: Span },
    B32Type { name: String, span: Span },
    H160Type { name: String, span: Span },
    H256Type { name: String, span: Span },
    H512Type { name: String, span: Span },
    StringType { name: String, span: Span }, // reserved as a token, but not used
    StrType { name: String, span: Span },
    CharType { name: String, span: Span },
    BoolType { name: String, span: Span },
    SelfType { name: String, span: Span },
    CustomType { name: String, span: Span },

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
    DblColon { punc: String, span: Span },
    ColonColonAsterisk { punc: String, span: Span },
    HashSign { punc: char, span: Span },
    HashBang { punc: String, span: Span },
    ThinArrow { punc: String, span: Span },
    FatArrow { punc: String, span: Span },
    // Underscore { name: String, span: Span },

    // operators
    Bang { punc: char, span: Span },
    DollarSign { punc: char, span: Span },
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
    AtSign { punc: char, span: Span },
    Backslash { punc: char, span: Span },
    Caret { punc: char, span: Span },
    Backtick { punc: char, span: Span },
    Pipe { punc: char, span: Span },
    DblDot { punc: String, span: Span },
    DotDotEquals { punc: String, span: Span },
    BangEquals { punc: String, span: Span },
    PercentEquals { punc: String, span: Span },
    DblAsterisk { punc: String, span: Span },
    AsteriskEquals { punc: String, span: Span },
    DblAmpersand { punc: String, span: Span },
    AmpersandMut { punc: String, span: Span },
    PlusEquals { punc: String, span: Span },
    MinusEquals { punc: String, span: Span },
    SlashEquals { punc: String, span: Span },
    DblLessThan { punc: String, span: Span },
    LessThanEquals { punc: String, span: Span },
    DblEquals { punc: String, span: Span },
    DblGreaterThan { punc: String, span: Span },
    GreaterThanEquals { punc: String, span: Span },
    DblPipe { punc: String, span: Span },

    LineComment { span: Span },
    BlockComment { span: Span },

    DocComment { comment: String, span: Span },

    UnrecognizedChar { punc: char, span: Span },

    EOF,
}

impl Token {
    /// Retrieve the `Span` of a given `Token`.
    pub fn span(&self) -> Span {
        match self.clone() {
            Token::Identifier { span, .. } => span,
            Token::IntLiteral { span, .. } => span,
            Token::UIntLiteral { span, .. } => span,
            Token::BigUIntLiteral { span, .. } => span,
            Token::ByteLiteral { span, .. } => span,
            Token::BytesLiteral { span, .. } => span,
            Token::HashLiteral { span, .. } => span,
            Token::StrLiteral { span, .. } => span,
            Token::CharLiteral { span, .. } => span,
            Token::BoolLiteral { span, .. } => span,
            Token::Let { span, .. } => span,
            Token::Mut { span, .. } => span,
            Token::Ref { span, .. } => span,
            Token::Pub { span, .. } => span,
            Token::Func { span, .. } => span,
            Token::Contract { span, .. } => span,
            Token::Library { span, .. } => span,
            Token::Script { span, .. } => span,
            Token::Interface { span, .. } => span,
            Token::Constructor { span, .. } => span,
            Token::Modifier { span, .. } => span,
            Token::Test { span, .. } => span,
            Token::View { span, .. } => span,
            Token::Extern { span, .. } => span,
            Token::Payable { span, .. } => span,
            Token::Event { span, .. } => span,
            Token::Error { span, .. } => span,
            Token::Storage { span, .. } => span,
            Token::Topic { span, .. } => span,
            Token::Calldata { span, .. } => span,
            Token::Unsafe { span, .. } => span,
            Token::Return { span, .. } => span,
            Token::Struct { span, .. } => span,
            Token::Enum { span, .. } => span,
            Token::Trait { span, .. } => span,
            Token::Impl { span, .. } => span,
            Token::Mod { span, .. } => span,
            Token::Import { span, .. } => span,
            Token::Package { span, .. } => span,
            Token::Super { span, .. } => span,
            Token::SelfKeyword { span, .. } => span,
            Token::Const { span, .. } => span,
            Token::Static { span, .. } => span,
            Token::Alias { span, .. } => span,
            Token::As { span, .. } => span,
            Token::If { span, .. } => span,
            Token::Else { span, .. } => span,
            Token::Match { span, .. } => span,
            Token::For { span, .. } => span,
            Token::In { span, .. } => span,
            Token::Loop { span, .. } => span,
            Token::While { span, .. } => span,
            Token::Break { span, .. } => span,
            Token::Continue { span, .. } => span,
            Token::I32Type { span, .. } => span,
            Token::I64Type { span, .. } => span,
            Token::I128Type { span, .. } => span,
            Token::U8Type { span, .. } => span,
            Token::U16Type { span, .. } => span,
            Token::U32Type { span, .. } => span,
            Token::U64Type { span, .. } => span,
            Token::U128Type { span, .. } => span,
            Token::U256Type { span, .. } => span,
            Token::U512Type { span, .. } => span,
            Token::ByteType { span, .. } => span,
            Token::B2Type { span, .. } => span,
            Token::B4Type { span, .. } => span,
            Token::B8Type { span, .. } => span,
            Token::B16Type { span, .. } => span,
            Token::B32Type { span, .. } => span,
            Token::H160Type { span, .. } => span,
            Token::H256Type { span, .. } => span,
            Token::H512Type { span, .. } => span,
            Token::StringType { span, .. } => span,
            Token::StrType { span, .. } => span,
            Token::CharType { span, .. } => span,
            Token::BoolType { span, .. } => span,
            Token::SelfType { span, .. } => span,
            Token::CustomType { span, .. } => span,
            Token::LParen { span, .. } => span,
            Token::RParen { span, .. } => span,
            Token::LBrace { span, .. } => span,
            Token::RBrace { span, .. } => span,
            Token::LBracket { span, .. } => span,
            Token::RBracket { span, .. } => span,
            Token::Colon { span, .. } => span,
            Token::Semicolon { span, .. } => span,
            Token::Comma { span, .. } => span,
            Token::Dot { span, .. } => span,
            Token::DblColon { span, .. } => span,
            Token::ColonColonAsterisk { span, .. } => span,
            Token::HashSign { span, .. } => span,
            Token::HashBang { span, .. } => span,
            Token::ThinArrow { span, .. } => span,
            Token::FatArrow { span, .. } => span,
            Token::Bang { span, .. } => span,
            Token::DollarSign { span, .. } => span,
            Token::Percent { span, .. } => span,
            Token::Ampersand { span, .. } => span,
            Token::Asterisk { span, .. } => span,
            Token::Plus { span, .. } => span,
            Token::Minus { span, .. } => span,
            Token::Slash { span, .. } => span,
            Token::LessThan { span, .. } => span,
            Token::Equals { span, .. } => span,
            Token::GreaterThan { span, .. } => span,
            Token::QuestionMark { span, .. } => span,
            Token::AtSign { span, .. } => span,
            Token::Backslash { span, .. } => span,
            Token::Caret { span, .. } => span,
            Token::Backtick { span, .. } => span,
            Token::Pipe { span, .. } => span,
            Token::DblDot { span, .. } => span,
            Token::DotDotEquals { span, .. } => span,
            Token::BangEquals { span, .. } => span,
            Token::PercentEquals { span, .. } => span,
            Token::DblAsterisk { span, .. } => span,
            Token::AsteriskEquals { span, .. } => span,
            Token::DblAmpersand { span, .. } => span,
            Token::AmpersandMut { span, .. } => span,
            Token::PlusEquals { span, .. } => span,
            Token::MinusEquals { span, .. } => span,
            Token::SlashEquals { span, .. } => span,
            Token::DblLessThan { span, .. } => span,
            Token::LessThanEquals { span, .. } => span,
            Token::DblEquals { span, .. } => span,
            Token::DblGreaterThan { span, .. } => span,
            Token::GreaterThanEquals { span, .. } => span,
            Token::DblPipe { span, .. } => span,
            Token::LineComment { span, .. } => span,
            Token::BlockComment { span, .. } => span,
            Token::DocComment { span, .. } => span,
            Token::UnrecognizedChar { span, .. } => span,
            Token::EOF => Span::new("", 0, 0),
        }
    }
}

/// Collection of `Token` as a result of the lexing process.
#[derive(Debug, Clone)]
pub struct TokenStream {
    tokens: Vec<Token>,
    span: Span,
}

impl TokenStream {
    /// Constructor method.
    pub fn new(tokens: &[Token], input: &str, start: usize, end: usize) -> Self {
        let span = Span::new(input, start, end);

        TokenStream {
            tokens: tokens.to_vec(),
            span,
        }
    }

    /// Get the tokens in the stream.
    pub fn tokens(&self) -> Vec<Token> {
        self.tokens.clone()
    }

    /// Get the stream span.
    pub fn span(&self) -> Span {
        self.span.clone()
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.tokens)
    }
}
