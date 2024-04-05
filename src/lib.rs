use uint::construct_uint;
use fixed_hash::{construct_fixed_hash, impl_fixed_hash_conversions};

construct_uint! {
    pub struct U256(4);
}

construct_fixed_hash! {
    pub struct H256(32);
}

construct_fixed_hash! {
    pub struct H160(20);
}

impl_fixed_hash_conversions!(H256, H160);

mod ast;
mod error;
mod lexer;
mod parser;
mod span;
mod token;
