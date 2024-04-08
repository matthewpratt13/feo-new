use crate::{
    ast::{
        expression::{BlockExpr, FieldAccessExpr},
        Expression, Identifier, Separator,
    },
    error::{ErrorsEmitted, ParserErrorKind},
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

/// Parse a field access expression (i.e., `object.field`).
pub(crate) fn parse_field_access_expression(
    parser: &mut Parser,
    object: Expression,
) -> Result<Expression, ErrorsEmitted> {
    parser.expect_token(Token::FullStop {
        punc: '.',
        span: parser.stream.span(),
    })?;

    let token = parser.consume_token();

    if let Ok(Token::Identifier { name, .. }) = token {
        let expr = FieldAccessExpr {
            object: Box::new(object),
            dot: Separator::FullStop,
            field: Identifier(name),
        };

        Ok(Expression::FieldAccess(expr))
    } else {
        parser.log_error(ParserErrorKind::UnexpectedToken {
            expected: "identifier after `.`".to_string(),
            found: token?,
        });
        Err(ErrorsEmitted(()))
    }
}
