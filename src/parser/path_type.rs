use crate::{
    ast::{Identifier, PathRoot, PathType, SelfType},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    token::Token,
};

use super::Parser;

impl PathType {
    pub(crate) fn parse(
        parser: &mut Parser,
        token: Option<Token>,
    ) -> Result<PathType, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `PathType::parse()`"),
        );
        parser.log_current_token(false);

        let mut tree: Vec<Identifier> = Vec::new();

        let path_root = match token {
            Some(Token::Identifier { name, .. }) => {
                Ok(PathRoot::Identifier(Identifier::from(&name)))
            }
            Some(Token::SelfKeyword { .. }) => Ok(PathRoot::SelfKeyword),
            Some(Token::SelfType { .. }) => Ok(PathRoot::SelfType(SelfType)),
            Some(Token::Package { .. }) => Ok(PathRoot::Package),
            Some(Token::Super { .. }) => Ok(PathRoot::Super),
            _ => {
                parser.log_unexpected_token(
                    "path root (identifier, `package`, `super`, `self` or `Self`)",
                );
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        while let Some(Token::DblColon { .. }) = parser.current_token() {
            match parser.peek_ahead_by(1) {
                Some(Token::Identifier { name, .. }) => {
                    parser.next_token();
                    parser.next_token();

                    tree.push(Identifier(name));
                }
                Some(Token::LBrace { .. }) => break,
                Some(Token::EOF) | None => {
                    parser.log_unexpected_eoi();
                    return Err(ErrorsEmitted);
                }
                _ => {
                    parser.log_unexpected_token("identifier");
                    return Err(ErrorsEmitted);
                }
            }
        }


        let path_type = PathType {
            path_root,
            tree_opt: {
                match tree.is_empty() {
                    true => None,
                    false => Some(tree),
                }
            },
        };

        parser
            .logger
            .log(LogLevel::Debug, LogMsg::from("exiting `PathExpr::parse()`"));
        parser.log_current_token(false);

        Ok(path_type)
    }
}
