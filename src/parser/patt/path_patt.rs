use crate::{
    ast::{Identifier, PathPatt, PathPrefix, Pattern},
    error::ErrorsEmitted,
    logger::{LogLevel, LogMsg},
    parser::Parser,
    token::Token,
};

impl PathPatt {
    pub(crate) fn parse(parser: &mut Parser, root: PathPrefix) -> Result<Pattern, ErrorsEmitted> {
        parser.logger.log(
            LogLevel::Debug,
            LogMsg("entering `PathPatt::parse()`".to_string()),
        );
        parser.log_current_token(false);

        let mut tree: Vec<Identifier> = Vec::new();

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
            root,
            tree_opt: {
                match tree.is_empty() {
                    true => None,
                    false => Some(tree),
                }
            },
        };

        parser.logger.log(
            LogLevel::Debug,
            LogMsg("exiting `PathPatt::parse()`".to_string()),
        );
        parser.log_current_token(false);

        Ok(Pattern::PathPatt(expr))
    }
}
