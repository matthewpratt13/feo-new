use core::fmt;

use crate::{
    ast::{Identifier, PathExpr, PathPatt, PathRoot, PathSegment, PathType, SelfType},
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
            Some(Token::Lib { .. }) => Ok(PathRoot::Lib),
            Some(Token::Super { .. }) => Ok(PathRoot::Super),
            _ => {
                parser.log_unexpected_token(
                    "path root (identifier, `lib`, `super`, `self` or `Self`)",
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

impl From<PathSegment> for PathType {
    fn from(value: PathSegment) -> Self {
        if let Some(p_sub) = value.subset_opt {
            let mut full_path: Vec<Identifier> = Vec::new();

            if let Some(v) = value.root.associated_type_path_prefix_opt {
                v.into_iter().for_each(|id| full_path.push(id));
            }

            full_path.push(value.root.type_name);

            for tree in p_sub.nested_trees {
                for p_seg in tree.path_segments {
                    if let Some(v) = p_seg.root.associated_type_path_prefix_opt {
                        v.into_iter().for_each(|id| full_path.push(id));
                    }

                    full_path.push(p_seg.root.type_name);
                }
            }

            let type_name = if let Some(id) = full_path.pop() {
                id
            } else {
                Identifier::from("")
            };

            PathType {
                associated_type_path_prefix_opt: Some(full_path),
                type_name,
            }
        } else {
            value.root
        }
    }
}

impl From<Vec<Identifier>> for PathType {
    fn from(value: Vec<Identifier>) -> Self {
        if value.len() > 1 {
            let mut path_prefix: Vec<Identifier> = Vec::new();

            for id in value {
                path_prefix.push(id);
            }

            let type_name = path_prefix.pop().unwrap();

            PathType {
                associated_type_path_prefix_opt: Some(path_prefix),
                type_name,
            }
        } else if value.len() == 1 {
            let type_name = value.get(0).cloned().unwrap();

            PathType {
                associated_type_path_prefix_opt: None,
                type_name,
            }
        } else {
            PathType::from(Identifier::from(""))
        }
    }
}

impl From<PathType> for Vec<Identifier> {
    fn from(value: PathType) -> Self {
        let mut prefix = if let Some(t) = value.associated_type_path_prefix_opt {
            t
        } else {
            Vec::new()
        };

        prefix.push(value.type_name);

        prefix
    }
}

impl fmt::Display for PathType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut segments: Vec<String> = Vec::new();

        // if let Some(v) = &self.associated_type_path_prefix_opt {
        //     for i in v {
        //         segments.push(i.to_string());
        //     }
        // }

        // segments.push(self.type_name.to_string());

        for id in Vec::<Identifier>::from(self.clone()) {
            segments.push(id.to_string())
        }

        let full_path = segments.join("::");

        write!(f, "{}", full_path)
    }
}

pub(crate) fn build_item_path(root: &PathType, item_path: PathType) -> PathType {
    if let Some(prefix) = &root.associated_type_path_prefix_opt {
        let mut path = prefix.to_vec();

        path.push(root.type_name.clone());

        path.append(&mut Vec::<Identifier>::from(item_path));

        let type_name = if let Some(id) = path.pop() {
            id
        } else {
            Identifier::from("")
        };

        PathType {
            associated_type_path_prefix_opt: Some(path),
            type_name,
        }
    } else {
        if root.type_name == Identifier::from("") {
            let path = &mut Vec::<Identifier>::from(item_path);

            let type_name = if let Some(id) = path.pop() {
                id
            } else {
                Identifier::from("")
            };

            PathType {
                associated_type_path_prefix_opt: {
                    if path.is_empty() {
                        None
                    } else {
                        Some(path.to_vec())
                    }
                },
                type_name,
            }
        } else {
            let mut suffix = Vec::<Identifier>::from(item_path);

            let mut path = vec![root.type_name.clone()];

            if suffix.get(0) == path.get(0) {
                path.clear();
            }

            path.append(&mut suffix);

            let type_name = if let Some(id) = path.pop() {
                id
            } else {
                Identifier::from("")
            };

            PathType {
                associated_type_path_prefix_opt: {
                    if path.is_empty() {
                        None
                    } else {
                        Some(path)
                    }
                },
                type_name,
            }
        }
    }
}
