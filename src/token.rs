use core::fmt;

use crate::{
    ast::{BigUInt, Bool, Byte, Bytes, Char, Hash, Int, Str, UInt},
    span::Span,
};

/// Enum representing the different types of tokens.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum Token {
    // includes `_`
    Identifier {
        name: String,
        span: Span,
    },

    // literals
    IntLiteral {
        value: Int,
        span: Span,
    },
    UIntLiteral {
        value: UInt,
        span: Span,
    },
    BigUIntLiteral {
        value: BigUInt,
        span: Span,
    },
    ByteLiteral {
        value: Byte,
        span: Span,
    },
    BytesLiteral {
        value: Bytes,
        span: Span,
    },
    HashLiteral {
        value: Hash,
        span: Span,
    },
    StrLiteral {
        value: Str,
        span: Span,
    },
    CharLiteral {
        value: Char,
        span: Span,
    },
    BoolLiteral {
        value: Bool,
        span: Span,
    },

    // keywords
    Let {
        name: String,
        span: Span,
    },
    Mut {
        name: String,
        span: Span,
    },
    Ref {
        name: String,
        span: Span,
    },
    Pub {
        name: String,
        span: Span,
    },
    Func {
        name: String,
        span: Span,
    },
    Contract {
        name: String,
        span: Span,
    }, // type of module, notated `#![contract]`
    Library {
        name: String,
        span: Span,
    }, // type of module, notated `#![library]`
    Script {
        name: String,
        span: Span,
    }, // type of module, notated `#![script]`
    Interface {
        name: String,
        span: Span,
    }, // type of trait, notated `#![interface]`
    Abstract {
        name: String,
        span: Span,
    }, // type of trait, notated `#![abstract]`
    Constructor {
        name: String,
        span: Span,
    }, // type of function notated `#[constructor]`
    Modifier {
        name: String,
        span: Span,
    }, // type of function, notated `#[modifier]`
    Test {
        name: String,
        span: Span,
    }, // type of function, notated `#[test]`
    View {
        name: String,
        span: Span,
    }, // function attribute, notated` #[view]`
    Extern {
        name: String,
        span: Span,
    }, // function attribute, notated `#[extern]`
    Payable {
        name: String,
        span: Span,
    }, // function attribute, notated `#[payable]`
    Event {
        name: String,
        span: Span,
    }, // type of struct, notated `#[event]`
    Error {
        name: String,
        span: Span,
    }, // type of struct, notated `#[error]`
    Storage {
        name: String,
        span: Span,
    }, // variable attribute, notated `#[storage]
    Topic {
        name: String,
        span: Span,
    }, // variable attribute, notated `#[topic]`
    Calldata {
        name: String,
        span: Span,
    }, // variable attribute, notated `#[calldata]`
    Return {
        name: String,
        span: Span,
    },
    Struct {
        name: String,
        span: Span,
    },
    Enum {
        name: String,
        span: Span,
    },
    Trait {
        name: String,
        span: Span,
    },
    Impl {
        name: String,
        span: Span,
    },
    Module {
        name: String,
        span: Span,
    },
    Import {
        name: String,
        span: Span,
    },
    Package {
        name: String,
        span: Span,
    },
    Super {
        name: String,
        span: Span,
    },
    SelfKeyword {
        name: String,
        span: Span,
    },
    Const {
        name: String,
        span: Span,
    },
    Static {
        name: String,
        span: Span,
    },
    Unsafe {
        name: String,
        span: Span,
    }, // inner attribute, notated #![unsafe]
    Alias {
        name: String,
        span: Span,
    },
    As {
        name: String,
        span: Span,
    },
    If {
        name: String,
        span: Span,
    },
    Else {
        name: String,
        span: Span,
    },
    Match {
        name: String,
        span: Span,
    },
    For {
        name: String,
        span: Span,
    },
    In {
        name: String,
        span: Span,
    },
    Loop {
        name: String,
        span: Span,
    },
    While {
        name: String,
        span: Span,
    },
    Break {
        name: String,
        span: Span,
    },
    Continue {
        name: String,
        span: Span,
    },
    Some {
        name: String,
        span: Span,
    },
    None {
        name: String,
        span: Span,
    },
    Ok {
        name: String,
        span: Span,
    },
    Err {
        name: String,
        span: Span,
    },

    // types
    I32Type {
        name: String,
        span: Span,
    },
    I64Type {
        name: String,
        span: Span,
    },
    I128Type {
        name: String,
        span: Span,
    },
    U8Type {
        name: String,
        span: Span,
    },
    U16Type {
        name: String,
        span: Span,
    },
    U32Type {
        name: String,
        span: Span,
    },
    U64Type {
        name: String,
        span: Span,
    },
    U128Type {
        name: String,
        span: Span,
    },
    U256Type {
        name: String,
        span: Span,
    },
    U512Type {
        name: String,
        span: Span,
    },
    ByteType {
        name: String,
        span: Span,
    },
    B2Type {
        name: String,
        span: Span,
    },
    B4Type {
        name: String,
        span: Span,
    },
    B8Type {
        name: String,
        span: Span,
    },
    B16Type {
        name: String,
        span: Span,
    },
    B32Type {
        name: String,
        span: Span,
    },
    H160Type {
        name: String,
        span: Span,
    },
    H256Type {
        name: String,
        span: Span,
    },
    H512Type {
        name: String,
        span: Span,
    },
    StringType {
        name: String,
        span: Span,
    }, // reserved as a token, but not used (yet)
    StrType {
        name: String,
        span: Span,
    },
    CharType {
        name: String,
        span: Span,
    },
    BoolType {
        name: String,
        span: Span,
    },
    SelfType {
        name: String,
        span: Span,
    },
    VecType {
        name: String,
        span: Span,
    },
    MappingType {
        name: String,
        span: Span,
    },
    OptionType {
        name: String,
        span: Span,
    },
    ResultType {
        name: String,
        span: Span,
    },

    // delimiters
    LParen {
        delim: char,
        span: Span,
    },
    RParen {
        delim: char,
        span: Span,
    },
    LBrace {
        delim: char,
        span: Span,
    },
    RBrace {
        delim: char,
        span: Span,
    },
    LBracket {
        delim: char,
        span: Span,
    },
    RBracket {
        delim: char,
        span: Span,
    },

    // separators
    Colon {
        punc: char,
        span: Span,
    },
    Semicolon {
        punc: char,
        span: Span,
    },
    Comma {
        punc: char,
        span: Span,
    },
    Dot {
        punc: char,
        span: Span,
    },
    DblColon {
        punc: String,
        span: Span,
    },
    ColonColonAsterisk {
        punc: String,
        span: Span,
    },
    ThinArrow {
        punc: String,
        span: Span,
    },
    FatArrow {
        punc: String,
        span: Span,
    },

    // operators
    Bang {
        punc: char,
        span: Span,
    },
    Percent {
        punc: char,
        span: Span,
    },
    Ampersand {
        punc: char,
        span: Span,
    },
    Asterisk {
        punc: char,
        span: Span,
    },
    Plus {
        punc: char,
        span: Span,
    },
    Minus {
        punc: char,
        span: Span,
    },
    Slash {
        punc: char,
        span: Span,
    },
    LessThan {
        punc: char,
        span: Span,
    },
    Equals {
        punc: char,
        span: Span,
    },
    GreaterThan {
        punc: char,
        span: Span,
    },
    QuestionMark {
        punc: char,
        span: Span,
    },
    Backslash {
        punc: char,
        span: Span,
    },
    Caret {
        punc: char,
        span: Span,
    },
    Pipe {
        punc: char,
        span: Span,
    },
    DblDot {
        punc: String,
        span: Span,
    },
    DotDotEquals {
        punc: String,
        span: Span,
    },
    BangEquals {
        punc: String,
        span: Span,
    },
    PercentEquals {
        punc: String,
        span: Span,
    },
    DblAsterisk {
        punc: String,
        span: Span,
    },
    AsteriskEquals {
        punc: String,
        span: Span,
    },
    DblAmpersand {
        punc: String,
        span: Span,
    },
    AmpersandMut {
        punc: String,
        span: Span,
    },
    PlusEquals {
        punc: String,
        span: Span,
    },
    MinusEquals {
        punc: String,
        span: Span,
    },
    SlashEquals {
        punc: String,
        span: Span,
    },
    DblLessThan {
        punc: String,
        span: Span,
    },
    LessThanEquals {
        punc: String,
        span: Span,
    },
    DblEquals {
        punc: String,
        span: Span,
    },
    DblGreaterThan {
        punc: String,
        span: Span,
    },
    GreaterThanEquals {
        punc: String,
        span: Span,
    },
    DblPipe {
        punc: String,
        span: Span,
    },

    // comments
    LineComment {
        span: Span,
    },
    BlockComment {
        span: Span,
    },
    DocComment {
        comment: String,
        span: Span,
        comment_type: DocCommentType,
    },

    UnrecognizedChar {
        punc: char,
        span: Span,
    },

    EOF,
}

impl Token {
    /// Convert a `Token` into a `TokenType` for more streamlined parsing –
    /// i.e., excluding span information.
    pub(crate) fn token_type(&self) -> TokenType {
        match self.clone() {
            Token::Identifier { name, .. } => TokenType::Iden(name),
            Token::IntLiteral { value, .. } => TokenType::IntLit(value),
            Token::UIntLiteral { value, .. } => TokenType::UIntLit(value),
            Token::BigUIntLiteral { value, .. } => TokenType::BigUIntLit(value),
            Token::ByteLiteral { value, .. } => TokenType::ByteLit(value),
            Token::BytesLiteral { value, .. } => TokenType::BytesLit(value),
            Token::HashLiteral { value, .. } => TokenType::HashLit(value),
            Token::StrLiteral { value, .. } => TokenType::StrLit(value),
            Token::CharLiteral { value, .. } => TokenType::CharLit(value),
            Token::BoolLiteral { value, .. } => TokenType::BoolLit(value),
            Token::Let { .. } => TokenType::Let,
            Token::Mut { .. } => TokenType::Mut,
            Token::Ref { .. } => TokenType::Ref,
            Token::Pub { .. } => TokenType::Pub,
            Token::Func { .. } => TokenType::Func,
            Token::Contract { .. } => TokenType::Contract,
            Token::Library { .. } => TokenType::Library,
            Token::Script { .. } => TokenType::Script,
            Token::Interface { .. } => TokenType::Interface,
            Token::Abstract { .. } => TokenType::Abstract,
            Token::Constructor { .. } => TokenType::Constructor,
            Token::Modifier { .. } => TokenType::Modifier,
            Token::Test { .. } => TokenType::Test,
            Token::View { .. } => TokenType::View,
            Token::Extern { .. } => TokenType::Extern,
            Token::Payable { .. } => TokenType::Payable,
            Token::Event { .. } => TokenType::Event,
            Token::Error { .. } => TokenType::Error,
            Token::Storage { .. } => TokenType::Storage,
            Token::Topic { .. } => TokenType::Topic,
            Token::Calldata { .. } => TokenType::Calldata,
            Token::Return { .. } => TokenType::Return,
            Token::Struct { .. } => TokenType::Struct,
            Token::Enum { .. } => TokenType::Enum,
            Token::Trait { .. } => TokenType::Trait,
            Token::Impl { .. } => TokenType::Impl,
            Token::Module { .. } => TokenType::Module,
            Token::Import { .. } => TokenType::Import,
            Token::Package { .. } => TokenType::Package,
            Token::Super { .. } => TokenType::Super,
            Token::SelfKeyword { .. } => TokenType::SelfKeyword,
            Token::Const { .. } => TokenType::Const,
            Token::Static { .. } => TokenType::Static,
            Token::Unsafe { .. } => TokenType::Unsafe,
            Token::Alias { .. } => TokenType::Alias,
            Token::As { .. } => TokenType::As,
            Token::If { .. } => TokenType::If,
            Token::Else { .. } => TokenType::Else,
            Token::Match { .. } => TokenType::Match,
            Token::For { .. } => TokenType::For,
            Token::In { .. } => TokenType::In,
            Token::Loop { .. } => TokenType::Loop,
            Token::While { .. } => TokenType::While,
            Token::Break { .. } => TokenType::Break,
            Token::Continue { .. } => TokenType::Continue,
            Token::Some { .. } => TokenType::Some,
            Token::None { .. } => TokenType::None,
            Token::Ok { .. } => TokenType::Ok,
            Token::Err { .. } => TokenType::Err,
            Token::I32Type { .. } => TokenType::I32Type,
            Token::I64Type { .. } => TokenType::I64Type,
            Token::I128Type { .. } => TokenType::I128Type,
            Token::U8Type { .. } => TokenType::U8Type,
            Token::U16Type { .. } => TokenType::U16Type,
            Token::U32Type { .. } => TokenType::U32Type,
            Token::U64Type { .. } => TokenType::U64Type,
            Token::U128Type { .. } => TokenType::U128Type,
            Token::U256Type { .. } => TokenType::U256Type,
            Token::U512Type { .. } => TokenType::U512Type,
            Token::ByteType { .. } => TokenType::ByteType,
            Token::B2Type { .. } => TokenType::B2Type,
            Token::B4Type { .. } => TokenType::B4Type,
            Token::B8Type { .. } => TokenType::B8Type,
            Token::B16Type { .. } => TokenType::B16Type,
            Token::B32Type { .. } => TokenType::B32Type,
            Token::H160Type { .. } => TokenType::H160Type,
            Token::H256Type { .. } => TokenType::H256Type,
            Token::H512Type { .. } => TokenType::H512Type,
            Token::StringType { .. } => TokenType::StringType,
            Token::StrType { .. } => TokenType::StrType,
            Token::CharType { .. } => TokenType::CharType,
            Token::BoolType { .. } => TokenType::BoolType,
            Token::SelfType { .. } => TokenType::SelfType,
            Token::VecType { .. } => TokenType::VecType,
            Token::MappingType { .. } => TokenType::MappingType,
            Token::OptionType { .. } => TokenType::OptionType,
            Token::ResultType { .. } => TokenType::ResultType,
            Token::LParen { .. } => TokenType::LParen,
            Token::RParen { .. } => TokenType::RParen,
            Token::LBrace { .. } => TokenType::LBrace,
            Token::RBrace { .. } => TokenType::RBrace,
            Token::LBracket { .. } => TokenType::LBracket,
            Token::RBracket { .. } => TokenType::RBracket,
            Token::Colon { .. } => TokenType::Colon,
            Token::Semicolon { .. } => TokenType::Semicolon,
            Token::Comma { .. } => TokenType::Comma,
            Token::Dot { .. } => TokenType::Dot,
            Token::DblColon { .. } => TokenType::DblColon,
            Token::ColonColonAsterisk { .. } => TokenType::ColonColonAsterisk,
            Token::ThinArrow { .. } => TokenType::ThinArrow,
            Token::FatArrow { .. } => TokenType::FatArrow,
            Token::Bang { .. } => TokenType::Bang,
            Token::Percent { .. } => TokenType::Percent,
            Token::Ampersand { .. } => TokenType::Ampersand,
            Token::Asterisk { .. } => TokenType::Asterisk,
            Token::Plus { .. } => TokenType::Plus,
            Token::Minus { .. } => TokenType::Minus,
            Token::Slash { .. } => TokenType::Slash,
            Token::LessThan { .. } => TokenType::LessThan,
            Token::Equals { .. } => TokenType::Equals,
            Token::GreaterThan { .. } => TokenType::GreaterThan,
            Token::QuestionMark { .. } => TokenType::QuestionMark,
            Token::Backslash { .. } => TokenType::Backslash,
            Token::Caret { .. } => TokenType::Caret,
            Token::Pipe { .. } => TokenType::Pipe,
            Token::DblDot { .. } => TokenType::DblDot,
            Token::DotDotEquals { .. } => TokenType::DotDotEquals,
            Token::BangEquals { .. } => TokenType::BangEquals,
            Token::PercentEquals { .. } => TokenType::PercentEquals,
            Token::DblAsterisk { .. } => TokenType::DblAsterisk,
            Token::AsteriskEquals { .. } => TokenType::AsteriskEquals,
            Token::DblAmpersand { .. } => TokenType::DblAmpersand,
            Token::AmpersandMut { .. } => TokenType::AmpersandMut,
            Token::PlusEquals { .. } => TokenType::PlusEquals,
            Token::MinusEquals { .. } => TokenType::MinusEquals,
            Token::SlashEquals { .. } => TokenType::SlashEquals,
            Token::DblLessThan { .. } => TokenType::DblLessThan,
            Token::LessThanEquals { .. } => TokenType::LessThanEquals,
            Token::DblEquals { .. } => TokenType::DblEquals,
            Token::DblGreaterThan { .. } => TokenType::DblGreaterThan,
            Token::GreaterThanEquals { .. } => TokenType::GreaterThanEquals,
            Token::DblPipe { .. } => TokenType::DblPipe,
            Token::LineComment { .. } => TokenType::LineComment,
            Token::BlockComment { .. } => TokenType::BlockComment,
            Token::DocComment { comment, .. } => TokenType::DocComment(comment),
            Token::UnrecognizedChar { punc, .. } => TokenType::UnrecognizedChar(punc),
            Token::EOF => TokenType::EOF,
        }
    }

    /// Retrieve the span of a given token.
    pub(crate) fn span(&self) -> Span {
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
            Token::Abstract { span, .. } => span,
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
            Token::Module { span, .. } => span,
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
            Token::Some { span, .. } => span,
            Token::None { span, .. } => span,
            Token::Ok { span, .. } => span,
            Token::Err { span, .. } => span,
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
            Token::VecType { span, .. } => span,
            Token::MappingType { span, .. } => span,
            Token::OptionType { span, .. } => span,
            Token::ResultType { span, .. } => span,
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
            Token::ThinArrow { span, .. } => span,
            Token::FatArrow { span, .. } => span,
            Token::Bang { span, .. } => span,
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
            Token::Backslash { span, .. } => span,
            Token::Caret { span, .. } => span,
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

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier { name, .. } => {
                f.debug_struct("Identifier").field("name", name).finish()
            }
            Self::IntLiteral { value, .. } => {
                f.debug_struct("IntLiteral").field("value", value).finish()
            }
            Self::UIntLiteral { value, .. } => {
                f.debug_struct("UIntLiteral").field("value", value).finish()
            }
            Self::BigUIntLiteral { value, .. } => f
                .debug_struct("BigUIntLiteral")
                .field("value", value)
                .finish(),
            Self::ByteLiteral { value, .. } => {
                f.debug_struct("ByteLiteral").field("value", value).finish()
            }
            Self::BytesLiteral { value, .. } => f
                .debug_struct("BytesLiteral")
                .field("value", value)
                .finish(),
            Self::HashLiteral { value, .. } => {
                f.debug_struct("HashLiteral").field("value", value).finish()
            }
            Self::StrLiteral { value, .. } => {
                f.debug_struct("StrLiteral").field("value", value).finish()
            }
            Self::CharLiteral { value, .. } => {
                f.debug_struct("CharLiteral").field("value", value).finish()
            }
            Self::BoolLiteral { value, .. } => {
                f.debug_struct("BoolLiteral").field("value", value).finish()
            }
            Self::Let { name, .. } => f.debug_struct("Let").field("name", name).finish(),
            Self::Mut { name, .. } => f.debug_struct("Mut").field("name", name).finish(),
            Self::Ref { name, .. } => f.debug_struct("Ref").field("name", name).finish(),
            Self::Pub { name, .. } => f.debug_struct("Pub").field("name", name).finish(),
            Self::Func { name, .. } => f.debug_struct("Func").field("name", name).finish(),
            Self::Contract { name, .. } => f.debug_struct("Contract").field("name", name).finish(),
            Self::Library { name, .. } => f.debug_struct("Library").field("name", name).finish(),
            Self::Script { name, .. } => f.debug_struct("Script").field("name", name).finish(),
            Self::Interface { name, .. } => {
                f.debug_struct("Interface").field("name", name).finish()
            }
            Self::Abstract { name, .. } => f.debug_struct("Abstract").field("name", name).finish(),
            Self::Constructor { name, .. } => {
                f.debug_struct("Constructor").field("name", name).finish()
            }
            Self::Modifier { name, .. } => f.debug_struct("Modifier").field("name", name).finish(),
            Self::Test { name, .. } => f.debug_struct("Test").field("name", name).finish(),
            Self::View { name, .. } => f.debug_struct("View").field("name", name).finish(),
            Self::Extern { name, .. } => f.debug_struct("Extern").field("name", name).finish(),
            Self::Payable { name, .. } => f.debug_struct("Payable").field("name", name).finish(),
            Self::Event { name, .. } => f.debug_struct("Event").field("name", name).finish(),
            Self::Error { name, .. } => f.debug_struct("Error").field("name", name).finish(),
            Self::Storage { name, .. } => f.debug_struct("Storage").field("name", name).finish(),
            Self::Topic { name, .. } => f.debug_struct("Topic").field("name", name).finish(),
            Self::Calldata { name, .. } => f.debug_struct("Calldata").field("name", name).finish(),
            Self::Return { name, .. } => f.debug_struct("Return").field("name", name).finish(),
            Self::Struct { name, .. } => f.debug_struct("Struct").field("name", name).finish(),
            Self::Enum { name, .. } => f.debug_struct("Enum").field("name", name).finish(),
            Self::Trait { name, .. } => f.debug_struct("Trait").field("name", name).finish(),
            Self::Impl { name, .. } => f.debug_struct("Impl").field("name", name).finish(),
            Self::Module { name, .. } => f.debug_struct("Module").field("name", name).finish(),
            Self::Import { name, .. } => f.debug_struct("Import").field("name", name).finish(),
            Self::Package { name, .. } => f.debug_struct("Package").field("name", name).finish(),
            Self::Super { name, .. } => f.debug_struct("Super").field("name", name).finish(),
            Self::SelfKeyword { name, .. } => {
                f.debug_struct("SelfKeyword").field("name", name).finish()
            }
            Self::Const { name, .. } => f.debug_struct("Const").field("name", name).finish(),
            Self::Static { name, .. } => f.debug_struct("Static").field("name", name).finish(),
            Self::Unsafe { name, .. } => f.debug_struct("Unsafe").field("name", name).finish(),
            Self::Alias { name, .. } => f.debug_struct("Alias").field("name", name).finish(),
            Self::As { name, .. } => f.debug_struct("As").field("name", name).finish(),
            Self::If { name, .. } => f.debug_struct("If").field("name", name).finish(),
            Self::Else { name, .. } => f.debug_struct("Else").field("name", name).finish(),
            Self::Match { name, .. } => f.debug_struct("Match").field("name", name).finish(),
            Self::For { name, .. } => f.debug_struct("For").field("name", name).finish(),
            Self::In { name, .. } => f.debug_struct("In").field("name", name).finish(),
            Self::Loop { name, .. } => f.debug_struct("Loop").field("name", name).finish(),
            Self::While { name, .. } => f.debug_struct("While").field("name", name).finish(),
            Self::Break { name, .. } => f.debug_struct("Break").field("name", name).finish(),
            Self::Continue { name, .. } => f.debug_struct("Continue").field("name", name).finish(),
            Self::Some { name, .. } => f.debug_struct("Some").field("name", name).finish(),
            Self::None { name, .. } => f.debug_struct("None").field("name", name).finish(),
            Self::Ok { name, .. } => f.debug_struct("Ok").field("name", name).finish(),
            Self::Err { name, .. } => f.debug_struct("Err").field("name", name).finish(),
            Self::I32Type { name, .. } => f.debug_struct("I32Type").field("name", name).finish(),
            Self::I64Type { name, .. } => f.debug_struct("I64Type").field("name", name).finish(),
            Self::I128Type { name, .. } => f.debug_struct("I128Type").field("name", name).finish(),
            Self::U8Type { name, .. } => f.debug_struct("U8Type").field("name", name).finish(),
            Self::U16Type { name, .. } => f.debug_struct("U16Type").field("name", name).finish(),
            Self::U32Type { name, .. } => f.debug_struct("U32Type").field("name", name).finish(),
            Self::U64Type { name, .. } => f.debug_struct("U64Type").field("name", name).finish(),
            Self::U128Type { name, .. } => f.debug_struct("U128Type").field("name", name).finish(),
            Self::U256Type { name, .. } => f.debug_struct("U256Type").field("name", name).finish(),
            Self::U512Type { name, .. } => f.debug_struct("U512Type").field("name", name).finish(),
            Self::ByteType { name, .. } => f.debug_struct("ByteType").field("name", name).finish(),
            Self::B2Type { name, .. } => f.debug_struct("B2Type").field("name", name).finish(),
            Self::B4Type { name, .. } => f.debug_struct("B4Type").field("name", name).finish(),
            Self::B8Type { name, .. } => f.debug_struct("B8Type").field("name", name).finish(),
            Self::B16Type { name, .. } => f.debug_struct("B16Type").field("name", name).finish(),
            Self::B32Type { name, .. } => f.debug_struct("B32Type").field("name", name).finish(),
            Self::H160Type { name, .. } => f.debug_struct("H160Type").field("name", name).finish(),
            Self::H256Type { name, .. } => f.debug_struct("H256Type").field("name", name).finish(),
            Self::H512Type { name, .. } => f.debug_struct("H512Type").field("name", name).finish(),
            Self::StringType { name, .. } => {
                f.debug_struct("StringType").field("name", name).finish()
            }
            Self::StrType { name, .. } => f.debug_struct("StrType").field("name", name).finish(),
            Self::CharType { name, .. } => f.debug_struct("CharType").field("name", name).finish(),
            Self::BoolType { name, .. } => f.debug_struct("BoolType").field("name", name).finish(),
            Self::SelfType { name, .. } => f.debug_struct("SelfType").field("name", name).finish(),
            Self::VecType { name, .. } => f.debug_struct("VecType").field("name", name).finish(),
            Self::MappingType { name, .. } => {
                f.debug_struct("MappingType").field("name", name).finish()
            }
            Self::OptionType { name, .. } => {
                f.debug_struct("OptionType").field("name", name).finish()
            }
            Self::ResultType { name, .. } => {
                f.debug_struct("ResultType").field("name", name).finish()
            }
            Self::LParen { delim, .. } => f.debug_struct("LParen").field("delim", delim).finish(),
            Self::RParen { delim, .. } => f.debug_struct("RParen").field("delim", delim).finish(),
            Self::LBrace { delim, .. } => f.debug_struct("LBrace").field("delim", delim).finish(),
            Self::RBrace { delim, .. } => f.debug_struct("RBrace").field("delim", delim).finish(),
            Self::LBracket { delim, .. } => {
                f.debug_struct("LBracket").field("delim", delim).finish()
            }
            Self::RBracket { delim, .. } => {
                f.debug_struct("RBracket").field("delim", delim).finish()
            }
            Self::Colon { punc, .. } => f.debug_struct("Colon").field("punc", punc).finish(),
            Self::Semicolon { punc, .. } => {
                f.debug_struct("Semicolon").field("punc", punc).finish()
            }
            Self::Comma { punc, .. } => f.debug_struct("Comma").field("punc", punc).finish(),
            Self::Dot { punc, .. } => f.debug_struct("Dot").field("punc", punc).finish(),
            Self::DblColon { punc, .. } => f.debug_struct("DblColon").field("punc", punc).finish(),
            Self::ColonColonAsterisk { punc, .. } => f
                .debug_struct("ColonColonAsterisk")
                .field("punc", punc)
                .finish(),
            Self::ThinArrow { punc, .. } => {
                f.debug_struct("ThinArrow").field("punc", punc).finish()
            }
            Self::FatArrow { punc, .. } => f.debug_struct("FatArrow").field("punc", punc).finish(),
            Self::Bang { punc, .. } => f.debug_struct("Bang").field("punc", punc).finish(),
            Self::Percent { punc, .. } => f.debug_struct("Percent").field("punc", punc).finish(),
            Self::Ampersand { punc, .. } => {
                f.debug_struct("Ampersand").field("punc", punc).finish()
            }
            Self::Asterisk { punc, .. } => f.debug_struct("Asterisk").field("punc", punc).finish(),
            Self::Plus { punc, .. } => f.debug_struct("Plus").field("punc", punc).finish(),
            Self::Minus { punc, .. } => f.debug_struct("Minus").field("punc", punc).finish(),
            Self::Slash { punc, .. } => f.debug_struct("Slash").field("punc", punc).finish(),
            Self::LessThan { punc, .. } => f.debug_struct("LessThan").field("punc", punc).finish(),
            Self::Equals { punc, .. } => f.debug_struct("Equals").field("punc", punc).finish(),
            Self::GreaterThan { punc, .. } => {
                f.debug_struct("GreaterThan").field("punc", punc).finish()
            }
            Self::QuestionMark { punc, .. } => {
                f.debug_struct("QuestionMark").field("punc", punc).finish()
            }
            Self::Backslash { punc, .. } => {
                f.debug_struct("Backslash").field("punc", punc).finish()
            }
            Self::Caret { punc, .. } => f.debug_struct("Caret").field("punc", punc).finish(),
            Self::Pipe { punc, .. } => f.debug_struct("Pipe").field("punc", punc).finish(),
            Self::DblDot { punc, .. } => f.debug_struct("DblDot").field("punc", punc).finish(),
            Self::DotDotEquals { punc, .. } => {
                f.debug_struct("DotDotEquals").field("punc", punc).finish()
            }
            Self::BangEquals { punc, .. } => {
                f.debug_struct("BangEquals").field("punc", punc).finish()
            }
            Self::PercentEquals { punc, .. } => {
                f.debug_struct("PercentEquals").field("punc", punc).finish()
            }
            Self::DblAsterisk { punc, .. } => {
                f.debug_struct("DblAsterisk").field("punc", punc).finish()
            }
            Self::AsteriskEquals { punc, .. } => f
                .debug_struct("AsteriskEquals")
                .field("punc", punc)
                .finish(),
            Self::DblAmpersand { punc, .. } => {
                f.debug_struct("DblAmpersand").field("punc", punc).finish()
            }
            Self::AmpersandMut { punc, .. } => {
                f.debug_struct("AmpersandMut").field("punc", punc).finish()
            }
            Self::PlusEquals { punc, .. } => {
                f.debug_struct("PlusEquals").field("punc", punc).finish()
            }
            Self::MinusEquals { punc, .. } => {
                f.debug_struct("MinusEquals").field("punc", punc).finish()
            }
            Self::SlashEquals { punc, .. } => {
                f.debug_struct("SlashEquals").field("punc", punc).finish()
            }
            Self::DblLessThan { punc, .. } => {
                f.debug_struct("DblLessThan").field("punc", punc).finish()
            }
            Self::LessThanEquals { punc, .. } => f
                .debug_struct("LessThanEquals")
                .field("punc", punc)
                .finish(),
            Self::DblEquals { punc, .. } => {
                f.debug_struct("DblEquals").field("punc", punc).finish()
            }
            Self::DblGreaterThan { punc, .. } => f
                .debug_struct("DblGreaterThan")
                .field("punc", punc)
                .finish(),
            Self::GreaterThanEquals { punc, .. } => f
                .debug_struct("GreaterThanEquals")
                .field("punc", punc)
                .finish(),
            Self::DblPipe { punc, .. } => f.debug_struct("DblPipe").field("punc", punc).finish(),
            Self::LineComment { .. } => f.debug_struct("LineComment").finish(),
            Self::BlockComment { .. } => f.debug_struct("BlockComment").finish(),
            Self::DocComment { comment, .. } => f
                .debug_struct("DocComment")
                .field("comment", comment)
                .finish(),
            Self::UnrecognizedChar { punc, .. } => f
                .debug_struct("UnrecognizedChar")
                .field("punc", punc)
                .finish(),
            Self::EOF => write!(f, "EOF"),
        }
    }
}

/// Enum representing the different doc comment types.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum DocCommentType {
    InnerDocComment, // `//!`
    OuterDocComment, // `///`
}

/// Enum representing the different token types, without extra information (except for literals).
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) enum TokenType {
    Iden(String),
    IntLit(Int),
    UIntLit(UInt),
    BigUIntLit(BigUInt),
    ByteLit(Byte),
    BytesLit(Bytes),
    HashLit(Hash),
    StrLit(Str),
    CharLit(Char),
    BoolLit(Bool),
    Let,
    Mut,
    Ref,
    Pub,
    Func,
    Contract,
    Library,
    Script,
    Interface,
    Abstract,
    Constructor,
    Modifier,
    Test,
    View,
    Extern,
    Payable,
    Event,
    Error,
    Storage,
    Topic,
    Calldata,
    Unsafe,
    Return,
    Struct,
    Enum,
    Trait,
    Impl,
    Module,
    Import,
    Package,
    Super,
    SelfKeyword,
    Const,
    Static,
    Alias,
    As,
    If,
    Else,
    Match,
    For,
    In,
    Loop,
    While,
    Break,
    Continue,
    Some,
    None,
    Ok,
    Err,
    I32Type,
    I64Type,
    I128Type,
    U8Type,
    U16Type,
    U32Type,
    U64Type,
    U128Type,
    U256Type,
    U512Type,
    ByteType,
    B2Type,
    B4Type,
    B8Type,
    B16Type,
    B32Type,
    H160Type,
    H256Type,
    H512Type,
    StringType,
    StrType,
    CharType,
    BoolType,
    SelfType,
    VecType,
    MappingType,
    OptionType,
    ResultType,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Semicolon,
    Comma,
    Dot,
    DblColon,
    ColonColonAsterisk,
    ThinArrow,
    FatArrow,
    Bang,
    Percent,
    Ampersand,
    Asterisk,
    Plus,
    Minus,
    Slash,
    LessThan,
    Equals,
    GreaterThan,
    QuestionMark,
    Backslash,
    Caret,
    Pipe,
    DblDot,
    DotDotEquals,
    BangEquals,
    PercentEquals,
    DblAsterisk,
    AsteriskEquals,
    DblAmpersand,
    AmpersandMut,
    PlusEquals,
    MinusEquals,
    SlashEquals,
    DblLessThan,
    LessThanEquals,
    DblEquals,
    DblGreaterThan,
    GreaterThanEquals,
    DblPipe,
    LineComment,
    BlockComment,
    DocComment(String),
    UnrecognizedChar(char),
    EOF,
}

/// Collection of `Token` resulting from the tokenization process.
#[derive(Debug, Clone)]
pub(crate) struct TokenStream {
    tokens: Vec<Token>,
    span: Span,
}

impl TokenStream {
    /// Constructor method.
    /// Stores the source data used in parsing (i.e., tokens).
    pub(crate) fn new(tokens: Vec<Token>, input: &str, start: usize, end: usize) -> Self {
        TokenStream {
            tokens,
            span: Span::new(input, start, end),
        }
    }

    /// Get the tokens in the stream.
    pub(crate) fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// Get the stream span.
    pub(crate) fn span(&self) -> Span {
        self.span.clone()
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}", self.tokens)
    }
}
