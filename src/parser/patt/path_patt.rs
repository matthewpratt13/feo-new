use crate::{
    ast::{Identifier, PathPatt, PathRoot, SelfType},
    error::ErrorsEmitted,
    log_trace,
    parser::{ParsePattern, Parser},
    token::{Token, TokenType},
};

use core::fmt;

impl ParsePattern for PathPatt {
    fn parse_patt(parser: &mut Parser) -> Result<PathPatt, ErrorsEmitted> {
        log_trace!(parser.logger, "entering `PathPatt::parse()`");
        parser.log_current_token(false);

        let mut tree: Vec<Identifier> = Vec::new();

        let path_root = match parser.current_token() {
            Some(Token::Identifier { name, .. }) => {
                Ok(PathRoot::Identifier(Identifier::from(name)))
            }
            Some(Token::SelfKeyword { .. }) => Ok(PathRoot::SelfKeyword),
            Some(Token::SelfType { .. }) => Ok(PathRoot::SelfType(SelfType)),
            Some(Token::Lib { .. }) => Ok(PathRoot::Lib),
            Some(Token::Super { .. }) => Ok(PathRoot::Super),
            _ => {
                parser.emit_unexpected_token(&format!(
                    "path root (identifier, {}, {}, {} or {})",
                    TokenType::Lib,
                    TokenType::Super,
                    TokenType::SelfKeyword,
                    TokenType::SelfType,
                ));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        while let Some(Token::DblColon { .. }) = parser.current_token() {
            if let Some(Token::Identifier { name, .. }) = parser.peek_ahead_by(1).cloned() {
                parser.next_token();
                parser.next_token();

                tree.push(Identifier::from(&name));
            } else {
                break;
            }
        }

        log_trace!(parser.logger, "exiting `PathPatt::parse()`");
        parser.log_current_token(false);

        Ok(PathPatt {
            path_root,
            tree_opt: {
                match tree.is_empty() {
                    true => None,
                    false => Some(tree),
                }
            },
        })
    }
}

impl fmt::Display for PathPatt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut segments: Vec<String> = Vec::new();

        segments.push(self.path_root.to_string());

        if let Some(v) = &self.tree_opt {
            for i in v {
                segments.push(i.to_string());
            }
        }

        let full_path = segments.join("::");

        write!(f, "{}", full_path)
    }
}
