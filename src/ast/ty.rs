

#[derive(Debug, Clone)]
pub enum Primitive {
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    U512,
    Byte, // `u8`
    B2,
    B4,
    B8,
    B16,
    B32,
    H160,
    H256,
    H512,
    Str,
    Char,
    Bool,
}