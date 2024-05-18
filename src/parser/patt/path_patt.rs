use crate::{
    ast::{Identifier, PathPatt, PathRoot, SelfType},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::{ParsePattern, Parser},
    token::Token,
};

impl ParsePattern for PathPatt {
    fn parse_patt(parser: &mut Parser) -> Result<PathPatt, ErrorsEmitted> {
        // **log event and current token** [REMOVE IN PROD]
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

        let patt = PathPatt {
            path_root,
            tree_opt: {
                match tree.is_empty() {
                    true => None,
                    false => Some(tree),
                }
            },
        };

        // **log event and current token** [REMOVE IN PROD]
        parser
            .logger
            .log(LogLevel::Debug, LogMsg::from("exiting `PathPatt::parse()`"));
        parser.log_current_token(false);

        Ok(patt)
    }
}
