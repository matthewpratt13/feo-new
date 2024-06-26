use crate::{
    ast::{BlockExpr, GroupedExpr, IfExpr, Keyword},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, ParseControlExpr, Parser},
    token::Token,
};

impl ParseControlExpr for IfExpr {
    fn parse(parser: &mut Parser) -> Result<IfExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_if = if let Some(Token::If { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::If)
        } else {
            parser.log_unexpected_token("`if`");
            Err(ErrorsEmitted)
        }?;

        let condition = match parser.current_token() {
            Some(Token::LParen { .. }) => Ok(Box::new(GroupedExpr::parse(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`(`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`(`");
                Err(ErrorsEmitted)
            }
        }?;

        let if_block = match parser.current_token() {
            Some(Token::LBrace { .. }) => Ok(Box::new(BlockExpr::parse(parser)?)),
            Some(Token::EOF) | None => {
                parser.log_missing_token("`{`");
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("`{`");
                Err(ErrorsEmitted)
            }
        }?;

        let (else_if_blocks_opt, trailing_else_block_opt) = parse_else_blocks(parser)?;

        let span = match (&else_if_blocks_opt, &trailing_else_block_opt) {
            (None, None) => parser.get_span_by_token(&first_token.unwrap()),
            (None, Some(b)) => parser.get_span(&first_token.unwrap().span(), &b.span),
            (Some(v), None) => match v.last().clone() {
                Some(i) => {
                    let expr = *i.clone();
                    parser.get_span(&first_token.unwrap().span(), &expr.span)
                }
                None => parser.get_span_by_token(&first_token.unwrap()),
            },
            (Some(_), Some(b)) => parser.get_span(&first_token.unwrap().span(), &b.span),
        };

        let expr = IfExpr {
            kw_if,
            condition,
            if_block,
            else_if_blocks_opt,
            trailing_else_block_opt,
            span,
        };

        Ok(expr)
    }
}

fn parse_else_blocks(
    parser: &mut Parser,
) -> Result<(Option<Vec<Box<IfExpr>>>, Option<BlockExpr>), ErrorsEmitted> {
    let mut else_if_blocks: Vec<Box<IfExpr>> = Vec::new();

    let mut trailing_else_block_opt: Option<BlockExpr> = None;

    while let Some(Token::Else { .. }) = parser.current_token() {
        parser.next_token();

        match parser.current_token() {
            Some(Token::If { .. }) => {
                let if_expr = Box::new(IfExpr::parse(parser)?);
                else_if_blocks.push(if_expr);
            }
            Some(Token::LBrace { .. }) => {
                let block = BlockExpr::parse(parser)?;
                trailing_else_block_opt = Some(block);
                break;
            }
            Some(Token::EOF) | None => {
                parser.log_missing_token("`if` or `{`");
                return Err(ErrorsEmitted);
            }
            _ => {
                parser.log_unexpected_token("`if` or `{`");
                return Err(ErrorsEmitted);
            }
        }
    }

    let else_if_blocks_opt = match else_if_blocks.is_empty() {
        true => None,
        false => Some(else_if_blocks),
    };

    Ok((else_if_blocks_opt, trailing_else_block_opt))
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_if_expr() -> Result<(), ()> {
        let input = r#"
        if (x < 5) {
            if (x > 2) {
                return true;
            }
        } else if (x == 5) {
            return true; 
        } else {
            return false;
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
