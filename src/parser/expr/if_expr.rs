use crate::{
    ast::{BlockExpr, Expression, GroupedExpr, IfExpr, Keyword},
    error::ErrorsEmitted,
    parser::{ParseConstruct, ParseControl, Parser},
    token::Token,
};

impl ParseControl for IfExpr {
    fn parse(parser: &mut Parser) -> Result<Expression, ErrorsEmitted> {
        let kw_if = if let Some(Token::If { .. }) = parser.current_token() {
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

        let expr = IfExpr {
            kw_if,
            condition,
            if_block,
            else_if_blocks_opt,
            trailing_else_block_opt,
        };

        Ok(Expression::If(expr))
    }
}

fn parse_else_blocks(
    parser: &mut Parser,
) -> Result<
    (
        Option<Vec<(Keyword, Box<Expression>)>>,
        Option<(Keyword, Box<Expression>)>,
    ),
    ErrorsEmitted,
> {
    let mut else_if_blocks: Vec<(Keyword, Box<Expression>)> = Vec::new();

    let mut trailing_else_block_opt: Option<(Keyword, Box<Expression>)> = None;

    while let Some(Token::Else { .. }) = parser.current_token() {
        parser.next_token();

        match parser.current_token() {
            Some(Token::If { .. }) => {
                let if_expr = Box::new(IfExpr::parse(parser)?);
                else_if_blocks.push((Keyword::Else, if_expr));
            }
            Some(Token::LBrace { .. }) => {
                let block = Box::new(BlockExpr::parse(parser)?);
                trailing_else_block_opt = Some((Keyword::Else, block));
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
    use crate::{logger::LogLevel, parser::test_utils};

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

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
