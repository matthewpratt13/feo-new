use fixed_hash::{construct_fixed_hash, impl_fixed_hash_conversions};
use uint::construct_uint;

mod ast;
mod error;
mod keccak;
mod lexer;
mod parser;
mod span;
mod token;

// hexadecimal 256-bit unsigned integer
construct_uint! {
    pub struct U256(4);
}

// hexadecimal 512-bit unsigned integer
construct_uint! {
    pub struct U512(8);
}

// 20-byte fixed-size hash (e.g., EVM address)
construct_fixed_hash! {
    pub struct H160(20);
}

// 256-bit (32-byte) fixed-size hash
construct_fixed_hash! {
    pub struct H256(32);
}

// 512-bit (64-byte) fixed-size hash
construct_fixed_hash! {
    pub struct H512(64);
}

// fixed size byte array literals (2–32 elements)
construct_fixed_hash! {
    pub struct B2(2);
}

construct_fixed_hash! {
    pub struct B3(3);
}

construct_fixed_hash! {
    pub struct B4(4);
}

construct_fixed_hash! {
    pub struct B5(5);
}

construct_fixed_hash! {
    pub struct B6(6);
}

construct_fixed_hash! {
    pub struct B7(7);
}

construct_fixed_hash! {
    pub struct B8(8);
}

construct_fixed_hash! {
    pub struct B9(9);
}

construct_fixed_hash! {
    pub struct B10(10);
}

construct_fixed_hash! {
    pub struct B11(11);
}

construct_fixed_hash! {
    pub struct B12(12);
}

construct_fixed_hash! {
    pub struct B13(13);
}

construct_fixed_hash! {
    pub struct B14(14);
}

construct_fixed_hash! {
    pub struct B15(15);
}

construct_fixed_hash! {
    pub struct B16(16);
}

construct_fixed_hash! {
    pub struct B17(17);
}

construct_fixed_hash! {
    pub struct B18(18);
}

construct_fixed_hash! {
    pub struct B19(19);
}

construct_fixed_hash! {
    pub struct B20(20);
}

construct_fixed_hash! {
    pub struct B21(21);
}

construct_fixed_hash! {
    pub struct B22(22);
}

construct_fixed_hash! {
    pub struct B23(23);
}

construct_fixed_hash! {
    pub struct B24(24);
}

construct_fixed_hash! {
    pub struct B25(25);
}

construct_fixed_hash! {
    pub struct B26(26);
}

construct_fixed_hash! {
    pub struct B27(27);
}

construct_fixed_hash! {
    pub struct B28(28);
}

construct_fixed_hash! {
    pub struct B29(29);
}

construct_fixed_hash! {
    pub struct B30(30);
}

construct_fixed_hash! {
    pub struct B31(31);
}

construct_fixed_hash! {
    pub struct B32(32);
}

impl_fixed_hash_conversions!(H256, H160);
impl_fixed_hash_conversions!(H512, H256);
