use crate::{
    ast::{Delimiter, Expression, PathExpr, TupleStructExpr},
    error::ErrorsEmitted,
    parser::{collection, ParseOperatorExpr, Parser, Precedence},
    span::Spanned,
    token::Token,
};

use core::fmt;

impl ParseOperatorExpr for TupleStructExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let open_paren = match parser.current_token() {
            Some(Token::LParen { .. }) => {
                let position = parser.current_position();
                parser.next_token();
                Ok(Delimiter::LParen { position })
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let struct_elements_opt =
            collection::get_expressions(parser, Precedence::Lowest, &open_paren)?;

        let last_token = parser.current_token();

        match parser.current_token() {
            Some(Token::RParen { .. }) => {
                let span = parser.get_span(left_expr_span, &last_token.unwrap().span());

                parser.next_token();

                Ok(Expression::TupleStruct(TupleStructExpr {
                    struct_path: PathExpr::from(left_expr),
                    struct_elements_opt,
                    span,
                }))
            }
            _ => {
                parser.log_unmatched_delimiter(&open_paren);
                Err(ErrorsEmitted)
            }
        }
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
