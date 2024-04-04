#![allow(dead_code)]

use core::hash::Hash;

/// Enum representing the different signed integer types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntKind {
    I32(i32),
    I64(i64),
}

/// Enum representing the different unsigned integer types.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UIntKind {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}
