#![allow(dead_code)]

use std::fmt;

use crate::{
    ast::{Bytes, IntKind, UIntKind},
    span::Span,
    H160, H256, U256,
};

/// Enum representing the different types of tokens.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
    Identifier { name: String, span: Span },

    // literals
    IntLiteral { value: IntKind, span: Span },
    UIntLiteral { value: UIntKind, span: Span },
    U256Literal { value: U256, span: Span },
    H160Literal { value: H160, span: Span },
    H256Literal { value: H256, span: Span },
    ByteLiteral { value: u8, span: Span },
    BytesLiteral { value: Bytes, span: Span },
    StringLiteral { value: Vec<u8>, span: Span },
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
    Interface { name: String, span: Span }, // type of trait, notated `#![interface]`
    Script { name: String, span: Span },   // type of module, notated `#![script]`
    Constructor { name: String, span: Span }, // type of function, notated `#![constructor]`
    Modifier { name: String, span: Span }, // type of function, notated `#![modifier]`
    Test { name: String, span: Span },     // type of function, notated `#![test]`
    Event { name: String, span: Span },    // type of struct, notated `#![event]`
    Error { name: String, span: Span },    // type of struct, notated `#![error]`
    Abstract { name: String, span: Span }, // attribute, notated `#[abstract]`
    Payable { name: String, span: Span },  // attribute, notated `#[payable]`
    Storage { name: String, span: Span },  // attribute, notated `#[storage]`
    Topic { name: String, span: Span },    // attribute, notated `#[topic]`
    Calldata { name: String, span: Span }, // attribute, notated `#[calldata]`
    Return { name: String, span: Span },
    Struct { name: String, span: Span },
    Enum { name: String, span: Span },
    Trait { name: String, span: Span },
    Impl { name: String, span: Span },
    Module { name: String, span: Span },
    Extern { name: String, span: Span }, // attribute, notated `#[extern]`
    Import { name: String, span: Span },
    Package { name: String, span: Span },
    Super { name: String, span: Span },
    SelfKeyword { name: String, span: Span },
    Const { name: String, span: Span },
    Static { name: String, span: Span },
    Unsafe { name: String, span: Span }, // attribute, notated `#[unsafe]`
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
    H256Type { name: String, span: Span },
    ByteType { name: String, span: Span },
    B2Type { name: String, span: Span },
    B3Type { name: String, span: Span },
    B4Type { name: String, span: Span },
    B5Type { name: String, span: Span },
    B6Type { name: String, span: Span },
    B7Type { name: String, span: Span },
    B8Type { name: String, span: Span },
    B9Type { name: String, span: Span },
    B10Type { name: String, span: Span },
    B11Type { name: String, span: Span },
    B12Type { name: String, span: Span },
    B13Type { name: String, span: Span },
    B14Type { name: String, span: Span },
    B15Type { name: String, span: Span },
    B16Type { name: String, span: Span },
    B17Type { name: String, span: Span },
    B18Type { name: String, span: Span },
    B19Type { name: String, span: Span },
    B20Type { name: String, span: Span },
    B21Type { name: String, span: Span },
    B22Type { name: String, span: Span },
    B23Type { name: String, span: Span },
    B24Type { name: String, span: Span },
    B25Type { name: String, span: Span },
    B26Type { name: String, span: Span },
    B27Type { name: String, span: Span },
    B28Type { name: String, span: Span },
    B29Type { name: String, span: Span },
    B30Type { name: String, span: Span },
    B31Type { name: String, span: Span },
    B32Type { name: String, span: Span },
    H160Type { name: String, span: Span },
    StringType { name: String, span: Span },
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
    FullStop { punc: char, span: Span },
    DblColon { punc: String, span: Span },
    ColonColonAsterisk { punc: String, span: Span },
    HashSign { punc: char, span: Span },
    HashBang { punc: String, span: Span },
    ThinArrow { punc: String, span: Span },
    FatArrow { punc: String, span: Span },
    Underscore { name: String, span: Span },

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

    EOF,
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
