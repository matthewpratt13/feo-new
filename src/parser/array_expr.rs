use crate::{
    ast::{ArrayExpr, Delimiter, Expression},
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

impl ArrayExpr {
    pub(crate) fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let mut elements: Vec<Expression> = Vec::new();

        while !parser.is_expected_token(&Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        }) {
            let element = parser.parse_expression(Precedence::Lowest)?;
            elements.push(element);

            if !parser.tokens_match(Token::Comma {
                punc: ',',
                span: parser.stream.span(),
            }) {
                break;
            }
        }

        let close_bracket = parser.expect_delimiter(Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        });

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(ArrayExpr {
            open_bracket: Delimiter::LBracket,
            elements,
            close_bracket: close_bracket?,
        })
    }
}
