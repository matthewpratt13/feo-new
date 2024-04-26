#![allow(dead_code)]

use std::fmt;

use crate::{
    ast::{BigUInt, Byte, Bytes, Hash, Int, Str, UInt},
    span::Span,
};

/// Enum representing the different types of tokens.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
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
        value: char,
        span: Span,
    },
    BoolLiteral {
        value: bool,
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
    HashSign {
        punc: char,
        span: Span,
    },
    HashBang {
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
    DollarSign {
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
    /// Retrieve the span of a given token.
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Identifier { name, .. } => write!(f, "{}", name),
            Token::IntLiteral { value, .. } => write!(f, "{:?}", value),
            Token::UIntLiteral { value, .. } => write!(f, "{:?}", value),
            Token::BigUIntLiteral { value, .. } => write!(f, "{:?}", value),
            Token::ByteLiteral { value, .. } => write!(f, "{:?}", value),
            Token::BytesLiteral { value, .. } => write!(f, "{:?}", value),
            Token::HashLiteral { value, .. } => write!(f, "{:?}", value),
            Token::StrLiteral { value, .. } => write!(f, "{:?}", value),
            Token::CharLiteral { value, .. } => write!(f, "{:?}", value),
            Token::BoolLiteral { value, .. } => write!(f, "{:?}", value),
            Token::Let { .. } => write!(f, "`let`"),
            Token::Mut { .. } => write!(f, "`mut`"),
            Token::Ref { .. } => write!(f, "`ref`"),
            Token::Pub { .. } => write!(f, "`pub`"),
            Token::Func { .. } => write!(f, "`func`"),
            Token::Contract { .. } => write!(f, "`contract`"),
            Token::Library { .. } => write!(f, "`library`"),
            Token::Script { .. } => write!(f, "`script`"),
            Token::Interface { .. } => write!(f, "`interface`"),
            Token::Constructor { .. } => write!(f, "`constructor`"),
            Token::Modifier { .. } => write!(f, "`modifier`"),
            Token::Test { .. } => write!(f, "`test`"),
            Token::View { .. } => write!(f, "`view`"),
            Token::Extern { .. } => write!(f, "`extern`"),
            Token::Payable { .. } => write!(f, "`payable`"),
            Token::Event { .. } => write!(f, "`event`"),
            Token::Error { .. } => write!(f, "`error`"),
            Token::Storage { .. } => write!(f, "`storage`"),
            Token::Topic { .. } => write!(f, "`topic`"),
            Token::Calldata { .. } => write!(f, "`calldata`"),
            Token::Return { .. } => write!(f, "`return`"),
            Token::Struct { .. } => write!(f, "`struct`"),
            Token::Enum { .. } => write!(f, "`enum`"),
            Token::Trait { .. } => write!(f, "`trait`"),
            Token::Impl { .. } => write!(f, "`impl`"),
            Token::Module { .. } => write!(f, "`module`"),
            Token::Import { .. } => write!(f, "`import`"),
            Token::Package { .. } => write!(f, "`package`"),
            Token::Super { .. } => write!(f, "`super`"),
            Token::SelfKeyword { .. } => write!(f, "`self`"),
            Token::Const { .. } => write!(f, "`const`"),
            Token::Static { .. } => write!(f, "`static`"),
            Token::Unsafe { .. } => write!(f, "`unsafe`"),
            Token::Alias { .. } => write!(f, "`alias`"),
            Token::As { .. } => write!(f, "`as`"),
            Token::If { .. } => write!(f, "`if`"),
            Token::Else { .. } => write!(f, "`else`"),
            Token::Match { .. } => write!(f, "`match`"),
            Token::For { .. } => write!(f, "`for`"),
            Token::In { .. } => write!(f, "`in`"),
            Token::Loop { .. } => write!(f, "`loop`"),
            Token::While { .. } => write!(f, "`while`"),
            Token::Break { .. } => write!(f, "`break`"),
            Token::Continue { .. } => write!(f, "`continue`"),
            Token::Some { .. } => write!(f, "`some`"),
            Token::None { .. } => write!(f, "`none`"),
            Token::Ok { .. } => write!(f, "`Ok`"),
            Token::Err { .. } => write!(f, "`Err`"),
            Token::I32Type { .. } => write!(f, "`i32`",),
            Token::I64Type { .. } => write!(f, "`i64`",),
            Token::I128Type { .. } => write!(f, "i128`",),
            Token::U8Type { .. } => write!(f, "`u8`",),
            Token::U16Type { .. } => write!(f, "`u16`",),
            Token::U32Type { .. } => write!(f, "`u32`",),
            Token::U64Type { .. } => write!(f, "`u64`",),
            Token::U128Type { .. } => write!(f, "`u128`",),
            Token::U256Type { .. } => write!(f, "`u256`",),
            Token::U512Type { .. } => write!(f, "`u512`",),
            Token::ByteType { .. } => write!(f, "`byte`"),
            Token::B2Type { .. } => write!(f, "`b2`",),
            Token::B4Type { .. } => write!(f, "`b4`",),
            Token::B8Type { .. } => write!(f, "`b8`",),
            Token::B16Type { .. } => write!(f, "`b16`",),
            Token::B32Type { .. } => write!(f, "`b32`",),
            Token::H160Type { .. } => write!(f, "`h160`",),
            Token::H256Type { .. } => write!(f, "`h256`",),
            Token::H512Type { .. } => write!(f, "`h512`",),
            Token::StringType { .. } => write!(f, "`String`",),
            Token::StrType { .. } => write!(f, "`str`",),
            Token::CharType { .. } => write!(f, "`char`",),
            Token::BoolType { .. } => write!(f, "`bool`",),
            Token::SelfType { .. } => write!(f, "`Self`",),
            Token::VecType { .. } => write!(f, "`Vec<T>`",),
            Token::MappingType { .. } => write!(f, "`Mapping<K, V>`"),
            Token::OptionType { .. } => write!(f, "`Option<T>`",),
            Token::ResultType { .. } => write!(f, "`Result<T, E>`",),
            Token::LParen { .. } => write!(f, "`(`"),
            Token::RParen { .. } => write!(f, "`)`"),
            Token::LBrace { .. } => write!(f, "`{{`"),
            Token::RBrace { .. } => write!(f, "`}}`"),
            Token::LBracket { .. } => write!(f, "`]`"),
            Token::RBracket { .. } => write!(f, "`]`"),
            Token::Colon { .. } => write!(f, "`:`"),
            Token::Semicolon { .. } => write!(f, "`;`"),
            Token::Comma { .. } => write!(f, "`,`"),
            Token::Dot { .. } => write!(f, "`.`"),
            Token::DblColon { .. } => write!(f, "`::`"),
            Token::ColonColonAsterisk { .. } => write!(f, "`::*`"),
            Token::HashSign { .. } => write!(f, "`#`"),
            Token::HashBang { .. } => write!(f, "`#!`"),
            Token::ThinArrow { .. } => write!(f, "`->`"),
            Token::FatArrow { .. } => write!(f, "`=>`"),
            Token::Bang { .. } => write!(f, "`!`"),
            Token::DollarSign { .. } => write!(f, "`$`"),
            Token::Percent { .. } => write!(f, "`%`"),
            Token::Ampersand { .. } => write!(f, "`&`"),
            Token::Asterisk { .. } => write!(f, "`*`"),
            Token::Plus { .. } => write!(f, "`+`"),
            Token::Minus { .. } => write!(f, "`-`"),
            Token::Slash { .. } => write!(f, "`/`"),
            Token::LessThan { .. } => write!(f, "`<`"),
            Token::Equals { .. } => write!(f, "`=`"),
            Token::GreaterThan { .. } => write!(f, "`>`"),
            Token::QuestionMark { .. } => write!(f, "`?`"),
            Token::Backslash { .. } => write!(f, "`\\`"),
            Token::Caret { .. } => write!(f, "`^`"),
            Token::Pipe { .. } => write!(f, "`|`"),
            Token::DblDot { .. } => write!(f, "``.."),
            Token::DotDotEquals { .. } => write!(f, "`..=`"),
            Token::BangEquals { .. } => write!(f, "`!=`"),
            Token::PercentEquals { .. } => write!(f, "`%=`"),
            Token::DblAsterisk { .. } => write!(f, "`**`"),
            Token::AsteriskEquals { .. } => write!(f, "`*=`"),
            Token::DblAmpersand { .. } => write!(f, "`&&`"),
            Token::AmpersandMut { .. } => write!(f, "`&mut`"),
            Token::PlusEquals { .. } => write!(f, "`+=`"),
            Token::MinusEquals { .. } => write!(f, "`-=`"),
            Token::SlashEquals { .. } => write!(f, "`/=`"),
            Token::DblLessThan { .. } => write!(f, "`<<`"),
            Token::LessThanEquals { .. } => write!(f, "`<=`"),
            Token::DblEquals { .. } => write!(f, "`==`"),
            Token::DblGreaterThan { .. } => write!(f, "`>>`"),
            Token::GreaterThanEquals { .. } => write!(f, "`>=`"),
            Token::DblPipe { .. } => write!(f, "`||`"),
            Token::LineComment { .. } => write!(f, ""),
            Token::BlockComment { .. } => write!(f, ""),
            Token::DocComment { comment, .. } => write!(f, "{}", comment),
            Token::UnrecognizedChar { punc, .. } => write!(f, "`{:?}`", punc),
            Token::EOF => write!(f, "EOF"),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier { name, span } => f.debug_struct("Identifier").field("name", name).finish(),
            Self::IntLiteral { value, span } => f.debug_struct("IntLiteral").field("value", value).finish(),
            Self::UIntLiteral { value, span } => f.debug_struct("UIntLiteral").field("value", value).finish(),
            Self::BigUIntLiteral { value, span } => f.debug_struct("BigUIntLiteral").field("value", value).finish(),
            Self::ByteLiteral { value, span } => f.debug_struct("ByteLiteral").field("value", value).finish(),
            Self::BytesLiteral { value, span } => f.debug_struct("BytesLiteral").field("value", value).finish(),
            Self::HashLiteral { value, span } => f.debug_struct("HashLiteral").field("value", value).finish(),
            Self::StrLiteral { value, span } => f.debug_struct("StrLiteral").field("value", value).finish(),
            Self::CharLiteral { value, span } => f.debug_struct("CharLiteral").field("value", value).finish(),
            Self::BoolLiteral { value, span } => f.debug_struct("BoolLiteral").field("value", value).finish(),
            Self::Let { name, span } => f.debug_struct("Let").field("name", name).finish(),
            Self::Mut { name, span } => f.debug_struct("Mut").field("name", name).finish(),
            Self::Ref { name, span } => f.debug_struct("Ref").field("name", name).finish(),
            Self::Pub { name, span } => f.debug_struct("Pub").field("name", name).finish(),
            Self::Func { name, span } => f.debug_struct("Func").field("name", name).finish(),
            Self::Contract { name, span } => f.debug_struct("Contract").field("name", name).finish(),
            Self::Library { name, span } => f.debug_struct("Library").field("name", name).finish(),
            Self::Script { name, span } => f.debug_struct("Script").field("name", name).finish(),
            Self::Interface { name, span } => f.debug_struct("Interface").field("name", name).finish(),
            Self::Constructor { name, span } => f.debug_struct("Constructor").field("name", name).finish(),
            Self::Modifier { name, span } => f.debug_struct("Modifier").field("name", name).finish(),
            Self::Test { name, span } => f.debug_struct("Test").field("name", name).finish(),
            Self::View { name, span } => f.debug_struct("View").field("name", name).finish(),
            Self::Extern { name, span } => f.debug_struct("Extern").field("name", name).finish(),
            Self::Payable { name, span } => f.debug_struct("Payable").field("name", name).finish(),
            Self::Event { name, span } => f.debug_struct("Event").field("name", name).finish(),
            Self::Error { name, span } => f.debug_struct("Error").field("name", name).finish(),
            Self::Storage { name, span } => f.debug_struct("Storage").field("name", name).finish(),
            Self::Topic { name, span } => f.debug_struct("Topic").field("name", name).finish(),
            Self::Calldata { name, span } => f.debug_struct("Calldata").field("name", name).finish(),
            Self::Return { name, span } => f.debug_struct("Return").field("name", name).finish(),
            Self::Struct { name, span } => f.debug_struct("Struct").field("name", name).finish(),
            Self::Enum { name, span } => f.debug_struct("Enum").field("name", name).finish(),
            Self::Trait { name, span } => f.debug_struct("Trait").field("name", name).finish(),
            Self::Impl { name, span } => f.debug_struct("Impl").field("name", name).finish(),
            Self::Module { name, span } => f.debug_struct("Module").field("name", name).finish(),
            Self::Import { name, span } => f.debug_struct("Import").field("name", name).finish(),
            Self::Package { name, span } => f.debug_struct("Package").field("name", name).finish(),
            Self::Super { name, span } => f.debug_struct("Super").field("name", name).finish(),
            Self::SelfKeyword { name, span } => f.debug_struct("SelfKeyword").field("name", name).finish(),
            Self::Const { name, span } => f.debug_struct("Const").field("name", name).finish(),
            Self::Static { name, span } => f.debug_struct("Static").field("name", name).finish(),
            Self::Unsafe { name, span } => f.debug_struct("Unsafe").field("name", name).finish(),
            Self::Alias { name, span } => f.debug_struct("Alias").field("name", name).finish(),
            Self::As { name, span } => f.debug_struct("As").field("name", name).finish(),
            Self::If { name, span } => f.debug_struct("If").field("name", name).finish(),
            Self::Else { name, span } => f.debug_struct("Else").field("name", name).finish(),
            Self::Match { name, span } => f.debug_struct("Match").field("name", name).finish(),
            Self::For { name, span } => f.debug_struct("For").field("name", name).finish(),
            Self::In { name, span } => f.debug_struct("In").field("name", name).finish(),
            Self::Loop { name, span } => f.debug_struct("Loop").field("name", name).finish(),
            Self::While { name, span } => f.debug_struct("While").field("name", name).finish(),
            Self::Break { name, span } => f.debug_struct("Break").field("name", name).finish(),
            Self::Continue { name, span } => f.debug_struct("Continue").field("name", name).finish(),
            Self::Some { name, span } => f.debug_struct("Some").field("name", name).finish(),
            Self::None { name, span } => f.debug_struct("None").field("name", name).finish(),
            Self::Ok { name, span } => f.debug_struct("Ok").field("name", name).finish(),
            Self::Err { name, span } => f.debug_struct("Err").field("name", name).finish(),
            Self::I32Type { name, span } => f.debug_struct("I32Type").field("name", name).finish(),
            Self::I64Type { name, span } => f.debug_struct("I64Type").field("name", name).finish(),
            Self::I128Type { name, span } => f.debug_struct("I128Type").field("name", name).finish(),
            Self::U8Type { name, span } => f.debug_struct("U8Type").field("name", name).finish(),
            Self::U16Type { name, span } => f.debug_struct("U16Type").field("name", name).finish(),
            Self::U32Type { name, span } => f.debug_struct("U32Type").field("name", name).finish(),
            Self::U64Type { name, span } => f.debug_struct("U64Type").field("name", name).finish(),
            Self::U128Type { name, span } => f.debug_struct("U128Type").field("name", name).finish(),
            Self::U256Type { name, span } => f.debug_struct("U256Type").field("name", name).finish(),
            Self::U512Type { name, span } => f.debug_struct("U512Type").field("name", name).finish(),
            Self::ByteType { name, span } => f.debug_struct("ByteType").field("name", name).finish(),
            Self::B2Type { name, span } => f.debug_struct("B2Type").field("name", name).finish(),
            Self::B4Type { name, span } => f.debug_struct("B4Type").field("name", name).finish(),
            Self::B8Type { name, span } => f.debug_struct("B8Type").field("name", name).finish(),
            Self::B16Type { name, span } => f.debug_struct("B16Type").field("name", name).finish(),
            Self::B32Type { name, span } => f.debug_struct("B32Type").field("name", name).finish(),
            Self::H160Type { name, span } => f.debug_struct("H160Type").field("name", name).finish(),
            Self::H256Type { name, span } => f.debug_struct("H256Type").field("name", name).finish(),
            Self::H512Type { name, span } => f.debug_struct("H512Type").field("name", name).finish(),
            Self::StringType { name, span } => f.debug_struct("StringType").field("name", name).finish(),
            Self::StrType { name, span } => f.debug_struct("StrType").field("name", name).finish(),
            Self::CharType { name, span } => f.debug_struct("CharType").field("name", name).finish(),
            Self::BoolType { name, span } => f.debug_struct("BoolType").field("name", name).finish(),
            Self::SelfType { name, span } => f.debug_struct("SelfType").field("name", name).finish(),
            Self::VecType { name, span } => f.debug_struct("VecType").field("name", name).finish(),
            Self::MappingType { name, span } => f.debug_struct("MappingType").field("name", name).finish(),
            Self::OptionType { name, span } => f.debug_struct("OptionType").field("name", name).finish(),
            Self::ResultType { name, span } => f.debug_struct("ResultType").field("name", name).finish(),
            Self::LParen { delim, span } => f.debug_struct("LParen").field("delim", delim).finish(),
            Self::RParen { delim, span } => f.debug_struct("RParen").field("delim", delim).finish(),
            Self::LBrace { delim, span } => f.debug_struct("LBrace").field("delim", delim).finish(),
            Self::RBrace { delim, span } => f.debug_struct("RBrace").field("delim", delim).finish(),
            Self::LBracket { delim, span } => f.debug_struct("LBracket").field("delim", delim).finish(),
            Self::RBracket { delim, span } => f.debug_struct("RBracket").field("delim", delim).finish(),
            Self::Colon { punc, span } => f.debug_struct("Colon").field("punc", punc).finish(),
            Self::Semicolon { punc, span } => f.debug_struct("Semicolon").field("punc", punc).finish(),
            Self::Comma { punc, span } => f.debug_struct("Comma").field("punc", punc).finish(),
            Self::Dot { punc, span } => f.debug_struct("Dot").field("punc", punc).finish(),
            Self::DblColon { punc, span } => f.debug_struct("DblColon").field("punc", punc).finish(),
            Self::ColonColonAsterisk { punc, span } => f.debug_struct("ColonColonAsterisk").field("punc", punc).finish(),
            Self::HashSign { punc, span } => f.debug_struct("HashSign").field("punc", punc).finish(),
            Self::HashBang { punc, span } => f.debug_struct("HashBang").field("punc", punc).finish(),
            Self::ThinArrow { punc, span } => f.debug_struct("ThinArrow").field("punc", punc).finish(),
            Self::FatArrow { punc, span } => f.debug_struct("FatArrow").field("punc", punc).finish(),
            Self::Bang { punc, span } => f.debug_struct("Bang").field("punc", punc).finish(),
            Self::DollarSign { punc, span } => f.debug_struct("DollarSign").field("punc", punc).finish(),
            Self::Percent { punc, span } => f.debug_struct("Percent").field("punc", punc).finish(),
            Self::Ampersand { punc, span } => f.debug_struct("Ampersand").field("punc", punc).finish(),
            Self::Asterisk { punc, span } => f.debug_struct("Asterisk").field("punc", punc).finish(),
            Self::Plus { punc, span } => f.debug_struct("Plus").field("punc", punc).finish(),
            Self::Minus { punc, span } => f.debug_struct("Minus").field("punc", punc).finish(),
            Self::Slash { punc, span } => f.debug_struct("Slash").field("punc", punc).finish(),
            Self::LessThan { punc, span } => f.debug_struct("LessThan").field("punc", punc).finish(),
            Self::Equals { punc, span } => f.debug_struct("Equals").field("punc", punc).finish(),
            Self::GreaterThan { punc, span } => f.debug_struct("GreaterThan").field("punc", punc).finish(),
            Self::QuestionMark { punc, span } => f.debug_struct("QuestionMark").field("punc", punc).finish(),
            Self::Backslash { punc, span } => f.debug_struct("Backslash").field("punc", punc).finish(),
            Self::Caret { punc, span } => f.debug_struct("Caret").field("punc", punc).finish(),
            Self::Pipe { punc, span } => f.debug_struct("Pipe").field("punc", punc).finish(),
            Self::DblDot { punc, span } => f.debug_struct("DblDot").field("punc", punc).finish(),
            Self::DotDotEquals { punc, span } => f.debug_struct("DotDotEquals").field("punc", punc).finish(),
            Self::BangEquals { punc, span } => f.debug_struct("BangEquals").field("punc", punc).finish(),
            Self::PercentEquals { punc, span } => f.debug_struct("PercentEquals").field("punc", punc).finish(),
            Self::DblAsterisk { punc, span } => f.debug_struct("DblAsterisk").field("punc", punc).finish(),
            Self::AsteriskEquals { punc, span } => f.debug_struct("AsteriskEquals").field("punc", punc).finish(),
            Self::DblAmpersand { punc, span } => f.debug_struct("DblAmpersand").field("punc", punc).finish(),
            Self::AmpersandMut { punc, span } => f.debug_struct("AmpersandMut").field("punc", punc).finish(),
            Self::PlusEquals { punc, span } => f.debug_struct("PlusEquals").field("punc", punc).finish(),
            Self::MinusEquals { punc, span } => f.debug_struct("MinusEquals").field("punc", punc).finish(),
            Self::SlashEquals { punc, span } => f.debug_struct("SlashEquals").field("punc", punc).finish(),
            Self::DblLessThan { punc, span } => f.debug_struct("DblLessThan").field("punc", punc).finish(),
            Self::LessThanEquals { punc, span } => f.debug_struct("LessThanEquals").field("punc", punc).finish(),
            Self::DblEquals { punc, span } => f.debug_struct("DblEquals").field("punc", punc).finish(),
            Self::DblGreaterThan { punc, span } => f.debug_struct("DblGreaterThan").field("punc", punc).finish(),
            Self::GreaterThanEquals { punc, span } => f.debug_struct("GreaterThanEquals").field("punc", punc).finish(),
            Self::DblPipe { punc, span } => f.debug_struct("DblPipe").field("punc", punc).finish(),
            Self::LineComment { span } => f.debug_struct("LineComment").finish(),
            Self::BlockComment { span } => f.debug_struct("BlockComment").finish(),
            Self::DocComment { comment, span, comment_type } => f.debug_struct("DocComment").field("comment", comment).field("comment_type", comment_type).finish(),
            Self::UnrecognizedChar { punc, span } => f.debug_struct("UnrecognizedChar").field("punc", punc).finish(),
            Self::EOF => write!(f, "EOF"),
        }
    }
}

/// Enum representing the different doc comment types.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DocCommentType {
    InnerDocComment, // `//!`
    OuterDocComment, // `///`
}

/// Collection of `Token` resulting from the tokenization process.
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
