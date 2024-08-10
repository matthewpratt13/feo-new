use crate::{
    ast::{Expression, TupleStructExpr},
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
};

use core::fmt;

impl ParseOperatorExpr for TupleStructExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        todo!()
    }
}

impl fmt::Debug for TupleStructExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TupleStructExpr")
            .field("struct_path", &self.struct_path)
            .field("struct_elements_opt", &self.struct_elements_opt)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_tuple_struct_expr() -> Result<(), ()> {
        todo!()
    }
}
