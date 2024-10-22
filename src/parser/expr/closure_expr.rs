use core::fmt;

use crate::{
    ast::{
        BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Delimiter, Expression, IdentifierPatt,
        Type,
    },
    error::ErrorsEmitted,
    parser::{get_collection, ParseConstructExpr, ParsePattern, Parser, Precedence},
    span::Spanned,
    token::{Token, TokenType},
};

impl ParseConstructExpr for ClosureExpr {
    fn parse(parser: &mut Parser) -> Result<ClosureExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let closure_params = match &first_token {
            Some(Token::Pipe { .. }) => {
                let position = parser.current_position();
                let open_pipe = Delimiter::Pipe { position };
                parser.next_token();

                let vec_opt = get_collection(parser, parse_closure_param, &open_pipe)?;

                match parser.current_token() {
                    Some(Token::Pipe { .. }) => {
                        if vec_opt.is_some() {
                            parser.next_token();
                            Ok(ClosureParams::Some(vec_opt.unwrap()))
                        } else {
                            parser.emit_missing_node("patt", "closure parameters");
                            parser.next_token();
                            Err(ErrorsEmitted)
                        }
                    }
                    _ => {
                        parser.warn_unmatched_delimiter(&open_pipe);
                        parser.next_token();
                        Err(ErrorsEmitted)
                    }
                }
            }
            Some(Token::DblPipe { .. }) => {
                parser.next_token();
                Ok(ClosureParams::None)
            }
            _ => {
                parser.emit_unexpected_token(&format!(
                    "{} or {}",
                    TokenType::Pipe,
                    TokenType::DblPipe
                ));
                Err(ErrorsEmitted)
            }
        }?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.current_token() {
            parser.next_token();

            match parser.current_token() {
                Some(Token::LBrace { .. }) => {
                    parser.emit_missing_node("type", "closure return type");
                    parser.next_token();
                    Err(ErrorsEmitted)
                }
                Some(Token::EOF { .. }) | None => {
                    parser.emit_unexpected_eoi();
                    Err(ErrorsEmitted)
                }

                _ => Ok(Some(Box::new(Type::parse(parser)?))),
            }
        } else {
            Ok(None)
        }?;

        let body_expression = if return_type_opt.is_some() {
            Expression::Block(BlockExpr::parse(parser)?)
        } else {
            parser.parse_expression(Precedence::Lowest)?
        };

        let start_span = first_token.unwrap().span();
        let span = parser.get_span(&start_span, &body_expression.span());

        Ok(ClosureExpr {
            closure_params,
            return_type_opt,
            body_expression: Box::new(body_expression),
            span,
        })
    }
}

impl fmt::Debug for ClosureExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ClosureExpr")
            .field("closure_params", &self.closure_params)
            .field("return_type_opt", &self.return_type_opt)
            .field("body_expression", &self.body_expression)
            .finish()
    }
}

fn parse_closure_param(parser: &mut Parser) -> Result<ClosureParam, ErrorsEmitted> {
    let position = parser.current_position();
    let open_pipe = Delimiter::Pipe { position };

    let param_name = match parser.current_token() {
        Some(Token::Identifier { .. } | Token::Ref { .. } | Token::Mut { .. }) => {
            IdentifierPatt::parse_patt(parser)
        }

        Some(Token::EOF) | None => {
            parser.emit_unexpected_eoi();
            parser.emit_missing_node("patt", "identifier pattern");
            Err(ErrorsEmitted)
        }

        _ => {
            parser.emit_unexpected_token(&format!(
                "identifier, {} or {}",
                TokenType::Ref,
                TokenType::Mut
            ));
            Err(ErrorsEmitted)
        }
    }?;

    let type_ann_opt = if let Some(Token::Colon { .. }) = parser.current_token() {
        parser.next_token();

        match parser.current_token() {
            Some(Token::Comma { .. } | Token::Pipe { .. }) => {
                parser.emit_missing_node("type", "closure parameter type annotation");
                parser.next_token();
                Err(ErrorsEmitted)
            }
            Some(Token::EOF { .. }) | None => {
                parser.emit_unexpected_eoi();
                parser.warn_unmatched_delimiter(&open_pipe);
                Err(ErrorsEmitted)
            }

            _ => Ok(Some(Box::new(Type::parse(parser)?))),
        }
    } else {
        Ok(None)
    }?;

    Ok(ClosureParam {
        param_name,
        type_ann_opt,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_closure_expr_without_block() -> Result<(), ()> {
        let input = r#"|| 2 + 2"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_closure_expr_with_block() -> Result<(), ()> {
        let input = r#"|world: str| -> () {
            print("hello {}", world)
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
