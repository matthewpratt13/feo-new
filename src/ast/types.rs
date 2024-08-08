use core::fmt;

pub use crate::{B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512};

use super::{FunctionOrMethodParam, Identifier, Type};

/// Wrappers for the different signed integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntType {
    I32(i32),
    I64(i64),
}

impl fmt::Display for IntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntType::I32(t) => write!(f, "{}", t),
            IntType::I64(t) => write!(f, "{}", t),
        }
    }
}

/// Wrappers for the different unsigned integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UIntType {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

impl From<usize> for UIntType {
    fn from(value: usize) -> Self {
        UIntType::U64(value as u64)
    }
}

impl fmt::Display for UIntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UIntType::U8(t) => write!(f, "{}", t),
            UIntType::U16(t) => write!(f, "{}", t),
            UIntType::U32(t) => write!(f, "{}", t),
            UIntType::U64(t) => write!(f, "{}", t),
        }
    }
}

/// Wrappers for the different large unsigned integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BigUIntType {
    U256(U256),
    U512(U512),
}

impl fmt::Display for BigUIntType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BigUIntType::U256(t) => write!(f, "{}", t),
            BigUIntType::U512(t) => write!(f, "{}", t),
        }
    }
}

/// Wrappers for the different floating-point types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum FloatType {
    F32(F32),
    F64(F64),
}

impl fmt::Display for FloatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatType::F32(t) => write!(f, "{}", t),
            FloatType::F64(t) => write!(f, "{}", t),
        }
    }
}

/// Struct that wraps a `u8` into a `Byte` type that is treated as a single ASCII character
/// in a byte string type. This is different to the native Unicode `char` type, which is
/// a UTF-8 encoded character of one (`u8`) to four bytes (`u32`) – i.e., variable length.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ByteType(pub u8);

impl From<u8> for ByteType {
    fn from(value: u8) -> Self {
        ByteType(value)
    }
}

impl fmt::Display for ByteType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Byte(0x{:x})", self.0)
    }
}

/// Wrappers for the different fixed-length byte string (`Bytes`) types.
/// Analogous to `[u8; 2]`, `[u8; 4]`, `[u8; 8]`, `[u8; 16]` and `[u8; 32]`
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BytesType {
    B2(B2),
    B4(B4),
    B8(B8),
    B16(B16),
    B32(B32),
}

impl fmt::Display for BytesType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BytesType::B2(t) => write!(f, "{}", t),
            BytesType::B4(t) => write!(f, "{}", t),
            BytesType::B8(t) => write!(f, "{}", t),
            BytesType::B16(t) => write!(f, "{}", t),
            BytesType::B32(t) => write!(f, "{}", t),
        }
    }
}

/// Wrappers for the different hash types.
/// Analogous to `[u8; 20]`, `[u8; 32]` and `[u8; 64]`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HashType {
    H160(H160),
    H256(H256),
    H512(H512),
}

impl fmt::Display for self::HashType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HashType::H160(t) => write!(f, "{}", t),
            HashType::H256(t) => write!(f, "{}", t),
            HashType::H512(t) => write!(f, "{}", t),
        }
    }
}

/// Wrapper for the `char` type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CharType(char);

impl From<char> for CharType {
    fn from(value: char) -> Self {
        CharType(value)
    }
}

impl fmt::Display for CharType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Struct that wraps a `Vec<Byte>` into a string literal of arbitrary length.
/// This type should be treated as static (i.e., not be growable / updatable), despite having
/// a dynamic inner type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct StrType(Vec<ByteType>);

impl From<&str> for StrType {
    fn from(value: &str) -> Self {
        StrType::from(value.as_bytes())
    }
}

impl From<&[u8]> for StrType {
    fn from(value: &[u8]) -> Self {
        let mut bytes: Vec<ByteType> = Vec::with_capacity(value.len());
        value
            .into_iter()
            .for_each(|b| bytes.push(ByteType::from(*b)));
        StrType(bytes)
    }
}

impl From<Vec<ByteType>> for StrType {
    fn from(value: Vec<ByteType>) -> Self {
        StrType(value)
    }
}

impl fmt::Display for StrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

/// Wrapper for the `bool` type.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BoolType(bool);

impl From<bool> for BoolType {
    fn from(value: bool) -> Self {
        BoolType(value)
    }
}

impl fmt::Display for BoolType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Function pointer type: `func(<param>) -> <Type>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionPtr {
    pub(crate) params_opt: Option<Vec<FunctionOrMethodParam>>,
    pub(crate) return_type_opt: Option<Box<Type>>,
}

impl fmt::Display for FunctionPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "func({:?}) [-> {:?}]",
            self.params_opt.clone().unwrap_or(Vec::new()),
            self.return_type_opt
                .clone()
                .unwrap_or(Box::new(Type::UnitType(UnitType)))
        )
    }
}

/// Struct that represents an inferred type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferredType {
    pub(crate) name: Identifier,
}

impl fmt::Display for InferredType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Unit struct that represents the `Self` type.
#[derive(Debug, Clone, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub struct SelfType;

impl fmt::Display for SelfType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Self")
    }
}

/// Struct that represents the path to user-defined type (e.g., struct, enum and trait)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypePath {
    pub(crate) associated_type_path_prefix_opt: Option<Vec<Identifier>>,
    pub(crate) type_name: Identifier,
}

/// Unit struct that represents the unit type `()`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnitType;

impl fmt::Display for UnitType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "()")
    }
}
