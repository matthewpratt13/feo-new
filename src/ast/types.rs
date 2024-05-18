use core::fmt;

pub use crate::{B16, B2, B32, B4, B8, H160, H256, H512, U256, U512};

use super::{FunctionOrMethodParam, Identifier, PathRoot, Type};

/// Wrappers for the different signed integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Int {
    I32(i32),
    I64(i64),
    I128(i128),
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Int::I32(t) => write!(f, "{}", t),
            Int::I64(t) => write!(f, "{}", t),
            Int::I128(t) => write!(f, "{}", t),
        }
    }
}

/// Wrappers for the different unsigned integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UInt {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

impl fmt::Display for UInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UInt::U8(t) => write!(f, "{}", t),
            UInt::U16(t) => write!(f, "{}", t),
            UInt::U32(t) => write!(f, "{}", t),
            UInt::U64(t) => write!(f, "{}", t),
            UInt::U128(t) => write!(f, "{}", t),
        }
    }
}

/// Wrappers for the different large unsigned integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BigUInt {
    U256(U256),
    U512(U512),
}

impl fmt::Display for BigUInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BigUInt::U256(t) => write!(f, "{}", t),
            BigUInt::U512(t) => write!(f, "{}", t),
        }
    }
}

/// Struct that wraps a `u8` into a `Byte` type that is treated as a single ASCII character
/// in a byte string type. This is different to the native Unicode `char` type, which is
/// a UTF-8 encoded character of one (`u8`) to four bytes (`u32`) – i.e., variable length.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Byte(u8);

impl From<u8> for Byte {
    fn from(value: u8) -> Self {
        Byte(value)
    }
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// Wrappers for the different static byte array (`Bytes`) types.
/// Analogous to `[u8; 2]`, `[u8; 4]`, `[u8; 8]`, `[u8; 16]` and `[u8; 32]`
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bytes {
    B2(B2),
    B4(B4),
    B8(B8),
    B16(B16),
    B32(B32),
}

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Bytes::B2(t) => write!(f, "{}", t),
            Bytes::B4(t) => write!(f, "{}", t),
            Bytes::B8(t) => write!(f, "{}", t),
            Bytes::B16(t) => write!(f, "{}", t),
            Bytes::B32(t) => write!(f, "{}", t),
        }
    }
}

/// Wrappers for the different hash types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Hash {
    H160(H160),
    H256(H256),
    H512(H512),
}

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Hash::H160(t) => write!(f, "{}", t),
            Hash::H256(t) => write!(f, "{}", t),
            Hash::H512(t) => write!(f, "{}", t),
        }
    }
}

/// Wrapper for the `char` type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Char(char);

impl From<char> for Char {
    fn from(value: char) -> Self {
        Char(value)
    }
}

impl fmt::Display for Char {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// Struct that wraps a `Vec<u8>` into a dynamic byte array (string literal).
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Str(Vec<Byte>);

impl From<&str> for Str {
    fn from(value: &str) -> Self {
        Str::from(value.as_bytes())
    }
}

impl From<&[u8]> for Str {
    fn from(value: &[u8]) -> Self {
        let mut bytes: Vec<Byte> = Vec::new();
        value.into_iter().for_each(|b| bytes.push(Byte::from(*b)));
        Str(bytes)
    }
}

impl From<Vec<Byte>> for Str {
    fn from(value: Vec<Byte>) -> Self {
       Str(value)
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// Wrapper for the `bool` type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Bool(bool);

impl From<bool> for Bool {
    fn from(value: bool) -> Self {
        Bool(value)
    }
}

impl fmt::Display for Bool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Function pointer type: `func([<param>]) [-> <Type>]`
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionPtr {
    pub(crate) function_name: Identifier,
    pub(crate) params_opt: Option<Vec<FunctionOrMethodParam>>,
    pub(crate) return_type_opt: Option<Box<Type>>,
}

impl fmt::Display for FunctionPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}({:?}) [-> {:?}]",
            self.function_name, self.params_opt, self.return_type_opt
        )
    }
}

/// Unit struct that represents the `Self` type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SelfType;

impl fmt::Display for SelfType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Self")
    }
}

/// Unit struct that represents the unit type `()`.
#[derive(Debug, Clone, PartialEq)]
pub struct Unit;

impl fmt::Display for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "()")
    }
}

/// Struct that represents an inferred type.
#[derive(Debug, Clone, PartialEq)]
pub struct InferredType {
    pub underscore: Identifier,
}

impl fmt::Display for InferredType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_")
    }
}

/// Struct that represents the path to user-defined type (e.g., struct, enum and trait)
#[derive(Debug, Clone, PartialEq)]
pub struct PathType {
    pub(crate) path_root: PathRoot,
    pub(crate) tree_opt: Option<Vec<Identifier>>,
}
