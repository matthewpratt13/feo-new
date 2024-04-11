use crate::{
    ast::{
        ArrayExpr, BlockExpr, ClosureExpr, Expression, GroupedExpr, PathExpr, Statement, TupleExpr,
    },
    error::ErrorsEmitted,
    token::Token,
};

use super::{Parser, Precedence};

pub(crate) trait ParseCompoundExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

impl ParseCompoundExpr for PathExpr {
    fn parse(parser: &mut Parser) -> Result<PathExpr, ErrorsEmitted> {
        todo!()
    }
}

impl ParseCompoundExpr for ClosureExpr {
    fn parse(parser: &mut Parser) -> Result<ClosureExpr, ErrorsEmitted> {
        todo!()
    }
}

impl ParseCompoundExpr for ArrayExpr {
    fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
        let open_bracket = parser.expect_delimiter(Token::LBracket {
            delim: '[',
            span: parser.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        while !parser.is_expected_token(&Token::RBracket {
            delim: ']',
            span: parser.stream.span(),
        }) {
            elements.push(parser.parse_expression(Precedence::Lowest)?);

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
        })?;

        Ok(ArrayExpr {
            open_bracket,
            elements,
            close_bracket,
        })
    }
}

impl ParseCompoundExpr for TupleExpr {
    fn parse(parser: &mut Parser) -> Result<TupleExpr, ErrorsEmitted> {
        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let mut elements: Vec<Expression> = Vec::new();

        while !parser.is_expected_token(&Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        }) {
            elements.push(parser.parse_expression(Precedence::Lowest)?);

            if !parser.tokens_match(Token::Comma {
                punc: ',',
                span: parser.stream.span(),
            }) {
                break;
            }
        }

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(TupleExpr {
            open_paren,
            elements,
            close_paren,
        })
    }
}

impl ParseCompoundExpr for BlockExpr {
    fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
        let open_brace = parser.expect_delimiter(Token::LBrace {
            delim: '{',
            span: parser.stream.span(),
        })?;

        let mut statements: Vec<Statement> = Vec::new();

        // parse expressions until a closing brace
        while !parser.is_expected_token(&Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        }) {
            statements.push(parser.parse_statement()?);
        }

        let terminal_expression_opt = if let Ok(e) = parser.parse_expression(Precedence::Lowest) {
            Some(Box::new(e))
        } else {
            None
        };

        let close_brace = parser.expect_delimiter(Token::RBrace {
            delim: '}',
            span: parser.stream.span(),
        })?;

        Ok(BlockExpr {
            open_brace,
            statements,
            terminal_expression_opt,
            close_brace,
        })
    }
}

impl ParseCompoundExpr for GroupedExpr {
    fn parse(parser: &mut Parser) -> Result<GroupedExpr, ErrorsEmitted> {
        let open_paren = parser.expect_delimiter(Token::RParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        Ok(GroupedExpr {
            open_paren,
            expression: Box::new(expression),
            close_paren,
        })
    }
}
