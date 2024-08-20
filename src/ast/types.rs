use super::{FunctionOrMethodParam, Identifier, Type};

use crate::error::ParserErrorKind;

pub use crate::{B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512};

use core::fmt;

/// Wrappers for the different signed integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Int {
    I32(i32),
    I64(i64),
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Int::I32(t) => write!(f, "{}", t),
            Int::I64(t) => write!(f, "{}", t),
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
}

impl From<usize> for UInt {
    fn from(value: usize) -> Self {
        UInt::U64(value as u64)
    }
}

impl fmt::Display for UInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UInt::U8(t) => write!(f, "{}", t),
            UInt::U16(t) => write!(f, "{}", t),
            UInt::U32(t) => write!(f, "{}", t),
            UInt::U64(t) => write!(f, "{}", t),
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

/// Wrappers for the different floating-point types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Float {
    F32(F32),
    F64(F64),
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Float::F32(t) => write!(f, "{}", t),
            Float::F64(t) => write!(f, "{}", t),
        }
    }
}

/// Struct that wraps a `u8` into a `Byte` type that is treated as a single ASCII character
/// in a byte string type. This is different to the native Unicode `char` type, which is
/// a UTF-8 encoded character of one (`u8`) to four bytes (`u32`) – i.e., variable length.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Byte(pub u8);

impl Byte {
    fn to_hex(self) -> String {
        format!("0x{:x}", self.0)
    }

    fn bytes_to_string(bytes: Vec<Byte>) -> Result<String, ParserErrorKind> {
        let mut buf = String::new();

        buf.push_str("\"");

        let iter = bytes.into_iter().map(|b| b.0 as u16);

        let chars = char::decode_utf16(iter)
            .map(|c| c.map_err(|e| ParserErrorKind::StrDecodeError(e)))
            .collect::<Vec<_>>();

        for char_res in chars {
            if let Ok(c) = char_res {
                buf.push(c)
            }
        }

        buf.push_str("\"");

        Ok(buf)
    }
}

impl From<u8> for Byte {
    fn from(value: u8) -> Self {
        Byte(value)
    }
}

impl fmt::Display for Byte {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_hex())
    }
}

/// Wrappers for the different fixed-length byte string (`Bytes`) types.
/// Analogous to `[u8; 2]`, `[u8; 4]`, `[u8; 8]`, `[u8; 16]` and `[u8; 32]`
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bytes {
    B2(B2),
    B4(B4),
    B8(B8),
    B16(B16),
    B32(B32),
}

impl Bytes {
    #[allow(dead_code)]
    pub fn to_hex_bytes(self) -> Vec<String> {
        let strings = match self {
            Bytes::B2(b) => {
                b.0.into_iter()
                    .map(|b| Byte(b).to_hex())
                    .collect::<Vec<_>>()
            }
            Bytes::B4(b) => {
                b.0.into_iter()
                    .map(|b| Byte(b).to_hex())
                    .collect::<Vec<_>>()
            }
            Bytes::B8(b) => {
                b.0.into_iter()
                    .map(|b| Byte(b).to_hex())
                    .collect::<Vec<_>>()
            }
            Bytes::B16(b) => {
                b.0.into_iter()
                    .map(|b| Byte(b).to_hex())
                    .collect::<Vec<_>>()
            }
            Bytes::B32(b) => {
                b.0.into_iter()
                    .map(|b| Byte(b).to_hex())
                    .collect::<Vec<_>>()
            }
        };

        strings
    }

    pub fn as_string(&self) -> String {
        let string = match self.clone() {
            Bytes::B2(b) => {
                Byte::bytes_to_string(b.0.into_iter().map(|byte| Byte(byte)).collect::<Vec<_>>())
            }
            Bytes::B4(b) => {
                Byte::bytes_to_string(b.0.into_iter().map(|byte| Byte(byte)).collect::<Vec<_>>())
            }
            Bytes::B8(b) => {
                Byte::bytes_to_string(b.0.into_iter().map(|byte| Byte(byte)).collect::<Vec<_>>())
            }
            Bytes::B16(b) => {
                Byte::bytes_to_string(b.0.into_iter().map(|byte| Byte(byte)).collect::<Vec<_>>())
            }
            Bytes::B32(b) => {
                Byte::bytes_to_string(b.0.into_iter().map(|byte| Byte(byte)).collect::<Vec<_>>())
            }
        };

        let mut buf = String::from("b");

        buf.push_str(&string.unwrap_or("".to_string()));

        buf
    }
}

impl fmt::Display for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Bytes::B2(_) => write!(f, "{}", self.as_string()),
            Bytes::B4(_) => write!(f, "{}", self.as_string()),
            Bytes::B8(_) => write!(f, "{}", self.as_string()),
            Bytes::B16(_) => write!(f, "{}", self.as_string()),
            Bytes::B32(_) => write!(f, "{}", self.as_string()),
        }
    }
}

/// Wrappers for the different hash types.
/// Analogous to `[u8; 20]`, `[u8; 32]` and `[u8; 64]`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Hash {
    H160(H160),
    H256(H256),
    H512(H512),
}

impl fmt::Display for self::Hash {
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
        write!(f, "{}", self.0)
    }
}

/// Struct that wraps a `Vec<Byte>` into a string literal of arbitrary length.
/// This type should be treated as static (i.e., not be growable / updatable), despite having
/// a dynamic inner type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Str(Vec<Byte>);

impl Str {
    pub fn as_string(&self) -> String {
        Byte::bytes_to_string(self.clone().0)
            .clone()
            .unwrap_or("".to_string())
    }

    #[allow(dead_code)]
    pub fn as_bytes(&self) -> &[Byte] {
        self.0.as_slice()
    }
}

impl From<&str> for Str {
    fn from(value: &str) -> Self {
        Str::from(value.as_bytes())
    }
}

impl From<&[u8]> for Str {
    fn from(value: &[u8]) -> Self {
        Str::from(value.to_vec())
    }
}

impl From<Vec<u8>> for Str {
    fn from(value: Vec<u8>) -> Self {
        let bytes = value.into_iter().map(|v| Byte::from(v)).collect::<Vec<_>>();
        Str::from(bytes)
    }
}

impl From<Vec<Byte>> for Str {
    fn from(value: Vec<Byte>) -> Self {
        Str(value)
    }
}

impl fmt::Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.clone().as_string())
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
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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
