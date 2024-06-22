use core::fmt;

use crate::{
    ast::{Identifier, PathExpr, PathPatt, PathRoot, PathType, SelfType},
    error::ErrorsEmitted,
    token::Token,
};

use super::Parser;

impl PathType {
    pub(crate) fn parse(
        parser: &mut Parser,
        token: Option<Token>,
    ) -> Result<PathType, ErrorsEmitted> {
        parser.logger.debug("entering `PathType::parse()`");
        parser.log_current_token(false);

        let mut path: Vec<Identifier> = Vec::new();

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

        if let Some(
            Token::Comma { .. }
            | Token::LBrace { .. }
            | Token::RParen { .. }
            | Token::RBrace { .. }
            | Token::Semicolon { .. }
            | Token::GreaterThan { .. }
            | Token::For { .. }
            | Token::EOF,
        )
        | None = parser.current_token()
        {
            return Ok(PathType {
                associated_type_path_prefix_opt: None,
                type_name: Identifier::from(&path_root.to_string()),
            });
        }

        path.push(Identifier::from(&path_root.to_string()));

        while let Some(Token::DblColon { .. }) = parser.current_token() {
            match parser.peek_ahead_by(1).cloned() {
                Some(Token::Identifier { name, .. }) => {
                    parser.next_token();
                    parser.next_token();

                    path.push(Identifier::from(&name));
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

        let type_name = path.pop().unwrap();

        let prefix = path;

        parser.next_token();

        let path_type = PathType {
            associated_type_path_prefix_opt: {
                if prefix.is_empty() {
                    None
                } else {
                    Some(prefix)
                }
            },
            type_name,
        };

        ////////////////////////////////////////////////////////////////////////////////
        parser.logger.debug("exiting `PathType::parse()`");
        parser.logger.debug(&format!("parsed path: {}", path_type));
        parser.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        Ok(path_type)
    }
}

impl From<Identifier> for PathType {
    fn from(value: Identifier) -> Self {
        PathType {
            associated_type_path_prefix_opt: None,
            type_name: value,
        }
    }
}

impl From<PathExpr> for PathType {
    fn from(value: PathExpr) -> Self {
        let mut path_segment_names: Vec<Identifier> = Vec::new();

        path_segment_names.push(Identifier::from(&value.path_root.to_string()));

        match value.tree_opt {
            Some(v) => {
                v.into_iter().for_each(|i| path_segment_names.push(i));
            }
            None => (),
        }

        let type_name = path_segment_names.pop().expect("empty path expression");

        PathType {
            associated_type_path_prefix_opt: {
                if path_segment_names.is_empty() {
                    None
                } else {
                    Some(path_segment_names)
                }
            },
            type_name,
        }
    }
}

impl From<PathPatt> for PathType {
    fn from(value: PathPatt) -> Self {
        let mut path_segment_names: Vec<Identifier> = Vec::new();

        path_segment_names.push(Identifier::from(&value.path_root.to_string()));

        match value.tree_opt {
            Some(v) => {
                v.into_iter().for_each(|i| path_segment_names.push(i));
            }
            None => (),
        }

        let type_name = path_segment_names.pop().expect("empty path expression");

        PathType {
            associated_type_path_prefix_opt: {
                if path_segment_names.is_empty() {
                    None
                } else {
                    Some(path_segment_names)
                }
            },
            type_name,
        }
    }
}

impl fmt::Display for PathType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut segments: Vec<String> = Vec::new();

        if let Some(v) = &self.associated_type_path_prefix_opt {
            for i in v {
                segments.push(i.to_string());
            }
        }

        segments.push(self.type_name.to_string());

        let full_path = segments.join("::");

        write!(f, "{}", full_path)
    }
}
