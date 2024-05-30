use crate::{
    ast::{BlockExpr, Expression, ForInExpr, Keyword, Literal},
    error::{ErrorsEmitted, ParserErrorKind},
    parser::{ParseConstructExpr, ParseControlExpr, Parser, Precedence},
    token::Token,
};

impl ParseControlExpr for ForInExpr {
    fn parse(parser: &mut Parser) -> Result<ForInExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_for = if let Some(Token::For { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::For)
        } else {
            parser.log_unexpected_token("`for`");
            Err(ErrorsEmitted)
        }?;

        let pattern = match parser.current_token() {
            Some(Token::In { .. }) => {
                parser.log_missing("patt", "iterator element");
                Err(ErrorsEmitted)
            }
            Some(Token::EOF) | None => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }

            _ => parser.parse_pattern(),
        }?;

        let kw_in = match parser.current_token() {
            Some(Token::In { .. }) => {
                parser.next_token();
                Ok(Keyword::In)
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`in`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`in`");
                Err(ErrorsEmitted)
            }
        }?;

        let expression = parser.parse_expression(Precedence::Lowest)?;

        let iterator = match expression {
            Expression::Literal(l) => match l {
                Literal::Int { .. } | Literal::UInt { .. } | Literal::BigUInt { .. } => {
                    Ok(Box::new(Expression::Literal(l)))
                }
                _ => {
                    parser.log_unexpected_token("numeric value");
                    Err(ErrorsEmitted)
                }
            },
            Expression::Path(p) => Ok(Box::new(Expression::Path(p))),
            Expression::MethodCall(mc) => Ok(Box::new(Expression::MethodCall(mc))),
            Expression::FieldAccess(fa) => Ok(Box::new(Expression::FieldAccess(fa))),
            Expression::Call(c) => Ok(Box::new(Expression::Call(c))),
            Expression::Index(i) => Ok(Box::new(Expression::Index(i))),
            Expression::TupleIndex(ti) => Ok(Box::new(Expression::TupleIndex(ti))),
            Expression::Unwrap(unw) => Ok(Box::new(Expression::Unwrap(unw))),
            Expression::Unary(una) => Ok(Box::new(Expression::Unary(una))),
            Expression::Reference(r) => Ok(Box::new(Expression::Reference(r))),
            Expression::Dereference(d) => Ok(Box::new(Expression::Dereference(d))),
            Expression::TypeCast(tc) => Ok(Box::new(Expression::TypeCast(tc))),
            Expression::Binary(b) => Ok(Box::new(Expression::Binary(b))),
            Expression::Grouped(g) => Ok(Box::new(Expression::Grouped(g))),
            Expression::Range(r) => Ok(Box::new(Expression::Range(r))),
            Expression::Closure(c) => Ok(Box::new(Expression::Closure(c))),
            Expression::Underscore(u) => Ok(Box::new(Expression::Underscore(u))),
            Expression::Array(a) => Ok(Box::new(Expression::Array(a))),
            Expression::Tuple(t) => Ok(Box::new(Expression::Tuple(t))),
            Expression::Mapping(m) => Ok(Box::new(Expression::Mapping(m))),
            Expression::Block(b) => Ok(Box::new(Expression::Block(b))),

            _ => {
                parser.log_error(ParserErrorKind::UnexpectedExpression {
                    expected: "iterable expression".to_string(),
                    found: format!("{}", &expression),
                });
                Err(ErrorsEmitted)
            }
        }?;

        let block = match parser.current_token() {
            Some(Token::LBrace { .. }) => Ok(BlockExpr::parse(parser)?),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`{`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let span = parser.get_span(&first_token.unwrap().span(), &block.span);

        let expr = ForInExpr {
            kw_for,
            pattern: Box::new(pattern),
            kw_in,
            iterator,
            block,
            span,
        };

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_for_in_expr() -> Result<(), ()> {
        let input = r#"
        for x in 0..=5 {
            x += 1;

            let y = 15;

            for z in y {
                print("foo")
            }
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
