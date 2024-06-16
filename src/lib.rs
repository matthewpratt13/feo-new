use fixed_hash::{construct_fixed_hash, impl_fixed_hash_conversions};
use ordered_float::OrderedFloat;
use uint::construct_uint;

mod ast;
mod error;
mod keccak;
mod lexer;
mod logger;
mod parser;
mod semantic_analyser;
mod span;
mod token;

pub type F32 = OrderedFloat<f32>;
pub type F64 = OrderedFloat<f64>;

// hexadecimal 256-bit (`[u64; 4]`) unsigned integer
construct_uint! {
    pub struct U256(4);
}

// hexadecimal 512-bit (`[u64; 8]`) unsigned integer
construct_uint! {
    pub struct U512(8);
}

// 20-byte (`[u8; 20]`) fixed-size hash (e.g., EVM address)
construct_fixed_hash! {
    pub struct H160(20);
}

// 32-byte (`[u8; 32]`) fixed-size hash
construct_fixed_hash! {
    pub struct H256(32);
}

// 64-byte (`[u8; 64]`) fixed-size hash
construct_fixed_hash! {
    pub struct H512(64);
}

// fixed-size byte strings (2–32 chars)
construct_fixed_hash! {
    pub struct B2(2);
}

construct_fixed_hash! {
    pub struct B4(4);
}

construct_fixed_hash! {
    pub struct B8(8);
}

construct_fixed_hash! {
    pub struct B16(16);
}

construct_fixed_hash! {
    pub struct B32(32);
}

impl_fixed_hash_conversions!(H256, H160);
impl_fixed_hash_conversions!(H512, H256);
