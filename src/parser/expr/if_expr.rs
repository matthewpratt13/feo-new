use crate::{
    ast::{BlockExpr, IfExpr, Keyword},
    error::ErrorsEmitted,
    parser::{ParseConstructExpr, ParseControlExpr, Parser},
    token::{Token, TokenType},
};

use core::fmt;

impl ParseControlExpr for IfExpr {
    fn parse(parser: &mut Parser) -> Result<IfExpr, ErrorsEmitted> {
        let first_token = parser.current_token().cloned();

        let kw_if = if let Some(Token::If { .. }) = &first_token {
            parser.next_token();
            Ok(Keyword::If)
        } else {
            parser.log_unexpected_token(&TokenType::If.to_string());
            Err(ErrorsEmitted)
        }?;

        let condition = Box::new(parser.expect_grouped_expr()?);

        let if_block = Box::new(parser.expect_block()?);

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

        Ok(IfExpr {
            kw_if,
            condition,
            if_block,
            else_if_blocks_opt,
            trailing_else_block_opt,
            span,
        })
    }
}

impl fmt::Debug for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IfExpr")
            .field("condition", &self.condition)
            .field("if_block", &self.if_block)
            .field("else_if_blocks_opt", &self.else_if_blocks_opt)
            .field("trailing_else_block_opt", &self.trailing_else_block_opt)
            .finish()
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
                parser.log_unexpected_eoi();
                parser.log_missing_token(&format!("{} or {}", TokenType::If, TokenType::LBrace));
                return Err(ErrorsEmitted);
            }
            _ => {
                parser.log_unexpected_token(&format!("{} or {}", TokenType::If, TokenType::LBrace));
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
            Ok(expr) => Ok(println!("{expr:#?}")),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
