pub use crate::{B16, B2, B32, B4, B8, H160, H256, H512, U256, U512};

use super::{Expression, Identifier};

/// Wrappers for the different signed integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Int {
    I32(i32),
    I64(i64),
    I128(i128),
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

/// Wrappers for the different large unsigned integer types.
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BigUInt {
    U256(U256),
    U512(U512),
}

/// Struct that wraps a `u8` into a `Byte` type that is usually treated as a single text character.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Byte(pub u8);

/// Wrappers for the different static byte array (`Bytes`) types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bytes {
    B2(B2),
    B4(B4),
    B8(B8),
    B16(B16),
    B32(B32),
}

/// Wrappers for the different hash types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Hash {
    H160(H160),
    H256(H256),
    H512(H512),
}

/// Type alias representing a path to an `Item` or local variable.
pub type PathType = Expression;

/// Struct that wraps a `Vec<u8>` into a dynamic byte array (string literal).
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Str(pub Vec<u8>);

/// Wrapper for the `char` type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Char(pub char);

/// Wrapper for the `bool` type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Bool(pub bool);

/// Unit struct that represents the unit type `()`.
#[derive(Debug, Clone, PartialEq)]
pub struct Unit;

/// Unit struct that represents the `Self` type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SelfType;

/// Struct that represents an inferred type.
#[derive(Debug, Clone, PartialEq)]
pub struct InferredType {
    pub underscore: Identifier,
}
