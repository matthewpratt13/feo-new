pub use crate::{
    B10, B11, B12, B13, B14, B15, B16, B17, B18, B19, B2, B20, B21, B22, B23, B24, B25, B26, B27,
    B28, B29, B3, B30, B31, B32, B4, B5, B6, B7, B8, B9, H160, H256, H512, U256, U512,
};

/// Wrappers for the different signed integer types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntKind {
    I32(i32),
    I64(i64),
    I128(i128),
}

/// Wrappers for the different unsigned integer types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UIntKind {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

/// Wrappers for the different big unsigned integer types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum BigUIntKind {
    U256(U256),
    U512(U512),
}

/// Struct that wraps a `u8` into a new type that is usually treated as a single text character.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Byte(pub u8);

/// Wrappers for the different byte array types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bytes {
    B2(B2),
    B3(B3),
    B4(B4),
    B5(B5),
    B6(B6),
    B7(B7),
    B8(B8),
    B9(B9),
    B10(B10),
    B11(B11),
    B12(B12),
    B13(B13),
    B14(B14),
    B15(B15),
    B16(B16),
    B17(B17),
    B18(B18),
    B19(B19),
    B20(B20),
    B21(B21),
    B22(B22),
    B23(B23),
    B24(B24),
    B25(B25),
    B26(B26),
    B27(B27),
    B28(B28),
    B29(B29),
    B30(B30),
    B31(B31),
    B32(B32),
}

/// Wrappers for the different hash types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum HashKind {
    H160(H160),
    H256(H256),
    H512(H512),
}
