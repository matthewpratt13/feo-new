use crate::{
    ast::{Identifier, PathPatt, PathRoot, Pattern, SelfType},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::Parser,
    token::Token,
};

impl PathPatt {
    pub(crate) fn parse(parser: &mut Parser) -> Result<Pattern, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg::from("entering `PathPatt::parse()`"),
        );
        parser.log_current_token(false);

        let mut tree: Vec<Identifier> = Vec::new();

        let path_root = match parser.current_token() {
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
            if let Some(Token::Identifier { name, .. }) = parser.peek_ahead_by(1) {
                parser.next_token();
                parser.next_token();

                tree.push(Identifier(name));
            } else {
                break;
            }
        }

        let expr = PathPatt {
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
            .log(LogLevel::Debug, LogMsg::from("exiting `PathPatt::parse()`"));
        parser.log_current_token(false);

        Ok(Pattern::PathPatt(expr))
    }
}
