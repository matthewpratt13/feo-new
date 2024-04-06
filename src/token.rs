#![allow(dead_code)]

use std::fmt;

use crate::{
    ast::{IntKind, UIntKind},
    span::Span,
    H160, H256, U256,
};

/// Enum representing the different types of tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token<'a> {
    Identifier { name: &'a str, span: Span<'a> },

    // literals
    IntLiteral { value: IntKind, span: Span<'a> },
    UIntLiteral { value: UIntKind, span: Span<'a> },
    U256Literal { value: U256, span: Span<'a> },
    H256Literal { value: H256, span: Span<'a> },
    AddressLiteral { value: H160, span: Span<'a> },
    StringLiteral { value: Vec<u8>, span: Span<'a> },
    CharLiteral { value: char, span: Span<'a> },
    BoolLiteral { value: bool, span: Span<'a> },
    BytesLiteral { value: &'static [u8], span: Span<'a> },

    // keywords
    Let { name: &'a str, span: Span<'a> },
    Mut { name: &'a str, span: Span<'a> },
    Ref { name: &'a str, span: Span<'a> },
    Pub { name: &'a str, span: Span<'a> },
    Func { name: &'a str, span: Span<'a> },
    Contract { name: &'a str, span: Span<'a> }, // type of module, notated `#![contract]`
    Library { name: &'a str, span: Span<'a> },  // type of module, notated `#![library]`
    Interface { name: &'a str, span: Span<'a> }, // type of trait, notated `#![interface]`
    Script { name: &'a str, span: Span<'a> },   // type of module, notated `#![script]`
    Constructor { name: &'a str, span: Span<'a> }, // type of function, notated `#![constructor]`
    Modifier { name: &'a str, span: Span<'a> }, // type of function, notated `#![modifier]`
    Test { name: &'a str, span: Span<'a> },     // type of function, notated `#![test]`
    Event { name: &'a str, span: Span<'a> },    // type of struct, notated `#![event]`
    Error { name: &'a str, span: Span<'a> },    // type of struct, notated `#![error]`
    Abstract { name: &'a str, span: Span<'a> }, // attribute, notated `#[abstract]`
    Payable { name: &'a str, span: Span<'a> },  // attribute, notated `#[payable]`
    Storage { name: &'a str, span: Span<'a> },  // attribute, notated `#[storage]`
    Topic { name: &'a str, span: Span<'a> },    // attribute, notated `#[topic]`
    Calldata { name: &'a str, span: Span<'a> }, // attribute, notated `#[calldata]`
    Return { name: &'a str, span: Span<'a> },
    Struct { name: &'a str, span: Span<'a> },
    Enum { name: &'a str, span: Span<'a> },
    Trait { name: &'a str, span: Span<'a> },
    Impl { name: &'a str, span: Span<'a> },
    Module { name: &'a str, span: Span<'a> },
    Extern { name: &'a str, span: Span<'a> }, // attribute, notated `#[extern]`
    Import { name: &'a str, span: Span<'a> },
    Package { name: &'a str, span: Span<'a> },
    Super { name: &'a str, span: Span<'a> },
    SelfKeyword { name: &'a str, span: Span<'a> },
    Const { name: &'a str, span: Span<'a> },
    Static { name: &'a str, span: Span<'a> },
    Unsafe { name: &'a str, span: Span<'a> }, // attribute, notated `#[unsafe]`
    Alias { name: &'a str, span: Span<'a> },
    As { name: &'a str, span: Span<'a> },
    If { name: &'a str, span: Span<'a> },
    Else { name: &'a str, span: Span<'a> },
    Match { name: &'a str, span: Span<'a> },
    For { name: &'a str, span: Span<'a> },
    In { name: &'a str, span: Span<'a> },
    Loop { name: &'a str, span: Span<'a> },
    While { name: &'a str, span: Span<'a> },
    Break { name: &'a str, span: Span<'a> },
    Continue { name: &'a str, span: Span<'a> },

    // types
    I32Type { name: &'a str, span: Span<'a> },
    I64Type { name: &'a str, span: Span<'a> },
    I128Type { name: &'a str, span: Span<'a> },
    U8Type { name: &'a str, span: Span<'a> },
    U16Type { name: &'a str, span: Span<'a> },
    U32Type { name: &'a str, span: Span<'a> },
    U64Type { name: &'a str, span: Span<'a> },
    U128Type { name: &'a str, span: Span<'a> },
    U256Type { name: &'a str, span: Span<'a> },
    H256Type { name: &'a str, span: Span<'a> },
    AddressType { name: &'a str, span: Span<'a> },
    StringType { name: &'a str, span: Span<'a> },
    CharType { name: &'a str, span: Span<'a> },
    BoolType { name: &'a str, span: Span<'a> },
    BytesType { name: &'a str, span: Span<'a> },
    SelfType { name: &'a str, span: Span<'a> },
    CustomType { name: &'a str, span: Span<'a> },

    // delimiters
    LParen { delim: char, span: Span<'a> },
    RParen { delim: char, span: Span<'a> },
    LBrace { delim: char, span: Span<'a> },
    RBrace { delim: char, span: Span<'a> },
    LBracket { delim: char, span: Span<'a> },
    RBracket { delim: char, span: Span<'a> },

    // separators
    Colon { punc: char, span: Span<'a> },
    Semicolon { punc: char, span: Span<'a> },
    Comma { punc: char, span: Span<'a> },
    FullStop { punc: char, span: Span<'a> },
    DblColon { punc: &'a str, span: Span<'a> },
    ColonColonAsterisk { punc: &'a str, span: Span<'a> },
    HashSign { punc: char, span: Span<'a> },
    HashBang { punc: &'a str, span: Span<'a> },
    ThinArrow { punc: &'a str, span: Span<'a> },
    FatArrow { punc: &'a str, span: Span<'a> },
    Underscore { name: &'a str, span: Span<'a> },

    // operators
    Bang { punc: char, span: Span<'a> },
    DollarSign { punc: char, span: Span<'a> },
    Percent { punc: char, span: Span<'a> },
    Ampersand { punc: char, span: Span<'a> },
    Asterisk { punc: char, span: Span<'a> },
    Plus { punc: char, span: Span<'a> },
    Minus { punc: char, span: Span<'a> },
    Slash { punc: char, span: Span<'a> },
    LessThan { punc: char, span: Span<'a> },
    Equals { punc: char, span: Span<'a> },
    GreaterThan { punc: char, span: Span<'a> },
    QuestionMark { punc: char, span: Span<'a> },
    AtSign { punc: char, span: Span<'a> },
    Backslash { punc: char, span: Span<'a> },
    Caret { punc: char, span: Span<'a> },
    Backtick { punc: char, span: Span<'a> },
    Pipe { punc: char, span: Span<'a> },
    DblDot { punc: &'a str, span: Span<'a> },
    DotDotEquals { punc: &'a str, span: Span<'a> },
    BangEquals { punc: &'a str, span: Span<'a> },
    PercentEquals { punc: &'a str, span: Span<'a> },
    DblAsterisk { punc: &'a str, span: Span<'a> },
    AsteriskEquals { punc: &'a str, span: Span<'a> },
    DblAmpersand { punc: &'a str, span: Span<'a> },
    PlusEquals { punc: &'a str, span: Span<'a> },
    MinusEquals { punc: &'a str, span: Span<'a> },
    SlashEquals { punc: &'a str, span: Span<'a> },
    DblLessThan { punc: &'a str, span: Span<'a> },
    LessThanEquals { punc: &'a str, span: Span<'a> },
    DblEquals { punc: &'a str, span: Span<'a> },
    DblGreaterThan { punc: &'a str, span: Span<'a> },
    GreaterThanEquals { punc: &'a str, span: Span<'a> },
    DblPipe { punc: &'a str, span: Span<'a> },

    LineComment { span: Span<'a> },
    BlockComment { span: Span<'a> },

    DocComment { comment: &'a str, span: Span<'a> },

    EOF,
}

/// Collection of `Token` as a result of the lexing process.
#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    tokens: Vec<Token<'a>>,
    span: Span<'a>,
}

impl<'a> TokenStream<'a> {
    /// Constructor method.
    pub fn new(tokens: &'a [Token], input: &'a str, start: usize, end: usize) -> Self {
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
    pub fn span(&self) -> Span<'a> {
        self.span.clone()
    }
}

impl<'a> fmt::Display for TokenStream<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.tokens)
    }
}
