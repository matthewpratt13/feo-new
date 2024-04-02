use uint::construct_uint;

construct_uint! {
    pub struct U256(4);
}

mod ast;
mod error;
mod lexer;
mod number;
mod parser;
mod span;
mod token;
