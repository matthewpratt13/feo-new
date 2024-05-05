use super::{Delimiter, Identifier, Keyword, PathPrefix, Pattern, RangeOp, Separator};

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Struct representing a collection of elements in a tuple pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattElements {
    pub elements: Vec<(Pattern, Separator)>, // single-element tuple must have trailing comma
    pub final_element_opt: Option<Box<Pattern>>,
}

/// Struct representing a single field in a struct pattern, with a name and value
/// and optional attributes.
#[derive(Debug, Clone, PartialEq)]
pub struct StructPattField {
    pub field_name: Identifier,
    pub field_value: Pattern,
}

///////////////////////////////////////////////////////////////////////////
// STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedPatt {
    pub open_paren: Delimiter,
    pub pattern: Box<Pattern>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierPatt {
    pub kw_ref_opt: Option<Keyword>,
    pub kw_mut_opt: Option<Keyword>,
    pub name: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NonePatt {
    pub kw_none: Keyword,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathPatt {
    pub root: PathPrefix,
    pub tree_opt: Option<Vec<Identifier>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangePatt {
    pub from_opt: Option<Box<Pattern>>,
    pub range_op: RangeOp, // `..` or `..=`
    pub to_opt: Option<Box<Pattern>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RestPatt {
    pub dbl_dot: RangeOp,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResultPatt {
    pub kw_ok_or_err: Keyword,
    pub expression: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SomePatt {
    pub kw_some: Keyword,
    pub expression: Box<Pattern>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPatt {
    pub path: PathPatt,
    pub open_brace: Delimiter,
    pub fields_opt: Option<Vec<StructPattField>>,
    pub close_brace: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePatt {
    pub open_paren: Delimiter,
    pub elements_opt: Option<TuplePattElements>,
    pub close_paren: Delimiter,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WildcardPatt {
    pub underscore: Identifier,
}
