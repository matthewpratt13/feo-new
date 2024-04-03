#![allow(dead_code)]

use core::hash::Hash;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum IntKind {
    I32(i32),
    I64(i64),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum UIntKind {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}
