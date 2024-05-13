use core::fmt;

use super::{Delimiter, Identifier, Keyword, PathPrefix, Pattern, RangeOp, ReferenceOp, Separator};

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Struct representing a collection of elements in a tuple pattern.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TuplePattElements {
    pub(crate) elements: Vec<(Pattern, Separator)>, // single-element tuple must have trailing comma
    pub(crate) final_element_opt: Option<Box<Pattern>>,
}

/// Struct representing a single field in a struct pattern, with a name and value
/// and optional attributes.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StructPattField {
    pub(crate) field_name: Identifier,
    pub(crate) field_value: Pattern,
}

///////////////////////////////////////////////////////////////////////////
// STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedPatt {
    pub(crate) open_paren: Delimiter,
    pub(crate) pattern: Box<Pattern>,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierPatt {
    pub(crate) kw_ref_opt: Option<Keyword>,
    pub(crate) kw_mut_opt: Option<Keyword>,
    pub(crate) name: Identifier,
}

impl fmt::Display for IdentifierPatt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}{:?}{}", self.kw_ref_opt, self.kw_mut_opt, self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NonePatt {
    pub(crate) kw_none: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathPatt {
    pub(crate) root: PathPrefix,
    pub(crate) tree_opt: Option<Vec<Identifier>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangePatt {
    pub(crate) from_opt: Option<Box<Pattern>>,
    pub(crate) range_op: RangeOp, // `..` or `..=`
    pub(crate) to_opt: Option<Box<Pattern>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReferencePatt {
    pub(crate) reference_op: ReferenceOp,
    pub(crate) pattern: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RestPatt {
    pub(crate) dbl_dot: RangeOp,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResultPatt {
    pub(crate) kw_ok_or_err: Keyword,
    pub(crate) pattern: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SomePatt {
    pub(crate) kw_some: Keyword,
    pub(crate) pattern: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPatt {
    pub(crate) path: PathPatt,
    pub(crate) open_brace: Delimiter,
    pub(crate) fields_opt: Option<Vec<StructPattField>>,
    pub(crate) close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePatt {
    pub(crate) open_paren: Delimiter,
    pub(crate) tuple_patt_elements: TuplePattElements,
    pub(crate) close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WildcardPatt {
    pub(crate) underscore: Identifier,
}
