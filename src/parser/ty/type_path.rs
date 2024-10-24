use core::fmt;

use crate::{
    ast::{Identifier, PathExpr, PathPatt, PathRoot, PathSegment, SelfType, Type, TypePath},
    error::ErrorsEmitted,
    log_trace,
    parser::Parser,
    semantic_analyser::{FormatItem, ToIdentifier},
    token::{Token, TokenType},
};

impl TypePath {
    pub(crate) fn parse(
        parser: &mut Parser,
        token: Option<Token>,
    ) -> Result<TypePath, ErrorsEmitted> {
        log_trace!(parser.logger, "entering `PathType::parse()` …");
        parser.log_current_token(false);

        let mut path: Vec<Identifier> = Vec::new();

        let path_root = match &token {
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
            return Ok(TypePath {
                associated_type_path_prefix_opt: None,
                type_name: Identifier::from(&path_root.to_string()),
            });
        }

        path.push(Identifier::from(&path_root.to_string()));

        parser.next_token();

        while let Some(Token::DblColon { .. }) = parser.current_token() {
            match parser.peek_ahead_by(1).cloned() {
                Some(Token::Identifier { name, .. }) => {
                    parser.next_token();
                    parser.next_token();

                    path.push(Identifier::from(&name));
                }
                Some(Token::LBrace { .. }) => break,
                Some(Token::EOF) | None => {
                    parser.emit_unexpected_eoi();
                    return Err(ErrorsEmitted);
                }
                _ => {
                    parser.emit_unexpected_token("identifier");
                    return Err(ErrorsEmitted);
                }
            }
        }

        let type_name = path.pop().unwrap();

        let prefix = path;

        let path_type = TypePath {
            associated_type_path_prefix_opt: {
                match prefix.is_empty() {
                    true => None,
                    false => Some(prefix),
                }
            },
            type_name,
        };

        ////////////////////////////////////////////////////////////////////////////////
        log_trace!(parser.logger, "exiting `PathType::parse()` …");
        log_trace!(parser.logger, "parsed path: `{path_type}`");
        parser.log_current_token(false);
        ////////////////////////////////////////////////////////////////////////////////

        Ok(path_type)
    }

    pub(crate) fn strip_prefix(&mut self) -> TypePath {
        if let Some(ids) = self.associated_type_path_prefix_opt.as_mut() {
            ids.remove(0);
            TypePath::from(ids.clone()).join(self.type_name.to_type_path())
        } else {
            self.to_owned()
        }
    }

    pub(crate) fn strip_suffix(&mut self) -> TypePath {
        if let Some(ids) = &self.associated_type_path_prefix_opt {
            TypePath::from(ids.clone())
        } else {
            self.to_owned()
        }
    }

    pub(crate) fn join(&self, item_path: TypePath) -> TypePath {
        if let Some(prefix) = &self.associated_type_path_prefix_opt {
            let mut path = prefix.to_vec();

            path.push(self.type_name.clone());

            path.append(&mut Vec::<Identifier>::from(item_path));

            let type_name = if let Some(id) = path.pop() {
                id
            } else {
                Identifier::from("")
            };

            TypePath {
                associated_type_path_prefix_opt: Some(path),
                type_name,
            }
        } else {
            if self.type_name == Identifier::from("") {
                let path = &mut Vec::<Identifier>::from(item_path);

                let type_name = if let Some(id) = path.pop() {
                    id
                } else {
                    Identifier::from("")
                };

                TypePath {
                    associated_type_path_prefix_opt: {
                        match path.is_empty() {
                            true => None,
                            false => Some(path.to_vec()),
                        }
                    },
                    type_name,
                }
            } else {
                let mut suffix = Vec::<Identifier>::from(item_path);

                let mut path = vec![self.type_name.clone()];

                if suffix.get(0) == path.get(0) {
                    path.clear();
                }

                path.append(&mut suffix);

                let type_name = if let Some(id) = path.pop() {
                    id
                } else {
                    Identifier::from("")
                };

                TypePath {
                    associated_type_path_prefix_opt: {
                        match path.is_empty() {
                            true => None,
                            false => Some(path),
                        }
                    },
                    type_name,
                }
            }
        }
    }
}

impl FormatItem for TypePath {}

impl ToIdentifier for TypePath {}

impl From<PathExpr> for TypePath {
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

        TypePath {
            associated_type_path_prefix_opt: {
                match path_segment_names.is_empty() {
                    true => None,
                    false => Some(path_segment_names),
                }
            },
            type_name,
        }
    }
}

impl From<PathPatt> for TypePath {
    fn from(value: PathPatt) -> Self {
        let mut path_segment_names: Vec<Identifier> = Vec::new();

        path_segment_names.push(Identifier::from(&value.path_root.to_string()));

        match value.tree_opt {
            Some(v) => {
                v.into_iter().for_each(|i| path_segment_names.push(i));
            }
            None => (),
        }

        let type_name = path_segment_names.pop().expect("empty path pattern");

        TypePath {
            associated_type_path_prefix_opt: {
                match path_segment_names.is_empty() {
                    true => None,
                    false => Some(path_segment_names),
                }
            },
            type_name,
        }
    }
}

impl From<PathSegment> for Vec<TypePath> {
    fn from(value: PathSegment) -> Self {
        let mut paths: Vec<TypePath> = Vec::new();

        if let Some(v) = value.root.associated_type_path_prefix_opt {
            let mut root: Vec<Identifier> = Vec::new();

            for id in v {
                root.push(id)
            }

            root.push(value.root.type_name);

            if let Some(p_sub) = value.subset_opt {
                for t in p_sub.nested_trees {
                    for p_seg in t.path_segments {
                        for p in Vec::<TypePath>::from(p_seg) {
                            let path = TypePath::from(root.clone()).join(p);
                            paths.push(path)
                        }
                    }
                }
            } else {
                paths.push(TypePath::from(root));
            }
        } else {
            paths.push(value.root);
        }

        paths
    }
}

impl From<Type> for TypePath {
    fn from(value: Type) -> Self {
        match value {
            Type::UserDefined(p) => p,
            t => t.to_identifier().to_type_path(),
        }
    }
}

impl From<Identifier> for TypePath {
    fn from(value: Identifier) -> Self {
        let path_segments = value
            .to_string()
            .split("::")
            .collect::<Vec<_>>()
            .into_iter()
            .map(|seg| Identifier::from(seg))
            .collect::<Vec<_>>();

        TypePath::from(path_segments)
    }
}

impl From<Vec<Identifier>> for TypePath {
    fn from(value: Vec<Identifier>) -> Self {
        if value.len() > 1 {
            let mut path_prefix: Vec<Identifier> = Vec::new();

            for id in value {
                path_prefix.push(id);
            }

            let type_name = path_prefix.pop().unwrap();

            TypePath {
                associated_type_path_prefix_opt: Some(path_prefix),
                type_name,
            }
        } else if value.len() == 1 {
            let type_name = value.get(0).cloned().unwrap();

            TypePath {
                associated_type_path_prefix_opt: None,
                type_name,
            }
        } else {
            TypePath::from(Identifier::from(""))
        }
    }
}

impl From<TypePath> for Vec<Identifier> {
    fn from(value: TypePath) -> Self {
        let mut paths: Vec<Identifier> = Vec::new();

        if let Some(v) = &value.associated_type_path_prefix_opt {
            for id in v {
                paths.push(id.clone())
            }
        }

        paths.push(value.type_name.clone());

        paths
    }
}

impl fmt::Display for TypePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut segments: Vec<String> = Vec::new();

        for id in Vec::<Identifier>::from(self.clone()) {
            segments.push(id.to_string())
        }

        let full_path = segments.join("::");

        write!(f, "{}", full_path)
    }
}

pub(crate) fn get_type_paths(segments: Vec<PathSegment>) -> Vec<TypePath> {
    let mut paths: Vec<TypePath> = Vec::new();

    for seg in segments {
        let root = seg.root;

        if let Some(subset) = seg.subset_opt {
            for tree in subset.nested_trees {
                for tree_seg in tree.path_segments {
                    let path = root.join(tree_seg.root);
                    paths.push(path);
                }
            }
        } else {
            paths.push(root);
        }
    }

    paths
}
