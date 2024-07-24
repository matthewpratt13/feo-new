use core::fmt;

use super::{Identifier, Keyword, PathRoot, Pattern, RangeOp, ReferenceOp};

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Struct representing a single field in a struct pattern, with a name and value
/// and optional attributes.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct StructPattField {
    pub(crate) field_name: Identifier,
    pub(crate) field_value: Pattern,
}

/// Struct representing a collection of elements in a tuple pattern.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct TuplePattElements {
    pub(crate) elements: Vec<Pattern>,
    pub(crate) final_element_opt: Option<Box<Pattern>>,
}

impl fmt::Display for TuplePattElements {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}, {:?}",
            self.elements,
            self.final_element_opt
                .clone()
                .unwrap_or(Box::new(Pattern::NonePatt(NonePatt {
                    kw_none: Keyword::None
                })))
        )
    }
}

///////////////////////////////////////////////////////////////////////////
// STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct GroupedPatt {
    pub(crate) inner_pattern: Box<Pattern>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IdentifierPatt {
    pub(crate) kw_ref_opt: Option<Keyword>,
    pub(crate) kw_mut_opt: Option<Keyword>,
    pub(crate) name: Identifier,
}

impl fmt::Display for IdentifierPatt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} {:?} {}",
            self.kw_ref_opt, self.kw_mut_opt, self.name
        )
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct NonePatt {
    pub(crate) kw_none: Keyword,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PathPatt {
    pub(crate) path_root: PathRoot,
    pub(crate) tree_opt: Option<Vec<Identifier>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct RangePatt {
    pub(crate) from_pattern_opt: Option<Box<Pattern>>,
    pub(crate) range_op: RangeOp, // `..` or `..=`
    pub(crate) to_pattern_opt: Option<Box<Pattern>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ReferencePatt {
    pub(crate) reference_op: ReferenceOp,
    pub(crate) pattern: Box<Pattern>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct RestPatt {
    pub(crate) dbl_dot: RangeOp,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ResultPatt {
    pub(crate) kw_ok_or_err: Keyword,
    pub(crate) pattern: Box<GroupedPatt>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SomePatt {
    pub(crate) kw_some: Keyword,
    pub(crate) pattern: Box<GroupedPatt>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructPatt {
    pub(crate) struct_path: PathPatt,
    pub(crate) struct_fields_opt: Option<Vec<StructPattField>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TuplePatt {
    pub(crate) tuple_patt_elements: TuplePattElements,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TupleStructPatt {
    pub(crate) struct_path: PathPatt,
    pub(crate) struct_elements_opt: Option<Vec<Pattern>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct WildcardPatt {
    pub(crate) underscore: Identifier,
}
