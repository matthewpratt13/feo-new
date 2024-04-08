use crate::{
    ast::{expression::BlockExpr, Expression},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

pub trait ParseExpression {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted>;
}

/// Parse a block expression (i.e., `{ expr1; expr2; ... }`).
impl ParseExpression for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        parser.expect_token(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut expressions: Vec<Expression> = Vec::new();

        // parse expressions until a closing brace
        while !parser.is_expected_token(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        }) {
            expressions.push(parser.parse_expression(Precedence::Lowest)?);
        }

        parser.expect_token(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        Ok(Expression::Block(expressions))
    }
}
