use core::fmt;

use crate::error::ParserErrorKind;

use super::{
    BigUInt, Bool, Byte, Bytes, Char, Expression, Float, Identifier, Int, Keyword, Literal,
    PathRoot, Pattern, RangeOp, ReferenceOp, Str, TypePath, UInt, U512,
};

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

/// Enum representing the literals in a pattern context
#[derive(Clone, Hash, PartialEq, Eq)]
pub(crate) enum LiteralPatt {
    Int { value: Int },
    UInt { value: UInt },
    BigUInt { value: BigUInt },
    Float { value: Float },
    Byte { value: Byte },
    Bytes { value: Bytes },
    Hash { value: crate::ast::Hash },
    Str { value: Str },
    Char { value: Char },
    Bool { value: Bool },
}

impl From<Literal> for LiteralPatt {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Int { value, .. } => LiteralPatt::Int { value },
            Literal::UInt { value, .. } => LiteralPatt::UInt { value },
            Literal::BigUInt { value, .. } => LiteralPatt::BigUInt { value },
            Literal::Float { value, .. } => LiteralPatt::Float { value },
            Literal::Byte { value, .. } => LiteralPatt::Byte { value },
            Literal::Bytes { value, .. } => LiteralPatt::Bytes { value },
            Literal::Hash { value, .. } => LiteralPatt::Hash { value },
            Literal::Str { value, .. } => LiteralPatt::Str { value },
            Literal::Char { value, .. } => LiteralPatt::Char { value },
            Literal::Bool { value, .. } => LiteralPatt::Bool { value },
        }
    }
}

impl fmt::Display for LiteralPatt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralPatt::Int { value } => write!(f, "{}", value),
            LiteralPatt::UInt { value } => write!(f, "{}", value),
            LiteralPatt::BigUInt { value } => write!(f, "{}", value),
            LiteralPatt::Float { value } => write!(f, "{}", value),
            LiteralPatt::Byte { value } => write!(f, "{}", value),
            LiteralPatt::Bytes { value } => write!(f, "{}", value),
            LiteralPatt::Hash { value } => write!(f, "{}", value),
            LiteralPatt::Str { value } => write!(f, "{}", value),
            LiteralPatt::Char { value } => write!(f, "{}", value),
            LiteralPatt::Bool { value } => write!(f, "{}", value),
        }
    }
}

impl fmt::Debug for LiteralPatt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int { value, .. } => f.debug_struct("Int").field("value", value).finish(),
            Self::UInt { value, .. } => f.debug_struct("UInt").field("value", value).finish(),
            Self::BigUInt { value, .. } => f.debug_struct("BigUInt").field("value", value).finish(),
            Self::Float { value, .. } => f.debug_struct("Float").field("value", value).finish(),
            Self::Byte { value, .. } => f.debug_struct("Byte").field("value", value).finish(),
            Self::Bytes { value, .. } => f.debug_struct("Bytes").field("value", value).finish(),
            Self::Hash { value, .. } => f.debug_struct("Hash").field("value", value).finish(),
            Self::Str { value, .. } => f.debug_struct("Str").field("value", value).finish(),
            Self::Char { value, .. } => f.debug_struct("Char").field("value", value).finish(),
            Self::Bool { value, .. } => f.debug_struct("Bool").field("value", value).finish(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MappingPatt {
    pub(crate) pairs: Vec<(Box<Pattern>, Box<Pattern>)>,
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

impl TryFrom<Expression> for Pattern {
    type Error = ParserErrorKind;

    fn try_from(value: Expression) -> Result<Self, Self::Error> {
        match value {
            Expression::Literal(l) => Ok(Pattern::LiteralPatt(LiteralPatt::from(l))),
            Expression::Path(p) => Ok(Pattern::PathPatt(PathPatt {
                path_root: p.path_root,
                tree_opt: p.tree_opt,
            })),
            Expression::Reference(r) => Ok(Pattern::ReferencePatt(ReferencePatt {
                reference_op: r.reference_op,
                pattern: Box::new(Pattern::try_from(*r.expression)?),
            })),
            Expression::Grouped(g) => Ok(Pattern::try_from(*g.inner_expression)?),
            Expression::Range(r) => {
                if r.from_expr_opt.is_none() && r.to_expr_opt.is_none() {
                    if r.range_op == RangeOp::RangeExclusive {
                        Ok(Pattern::RestPatt(RestPatt {
                            dbl_dot: r.range_op,
                        }))
                    } else {
                        Err(ParserErrorKind::UnexpectedRangeOp {
                            expected: format!("`{}`", RangeOp::RangeExclusive),
                            found: format!("`{}`", r.range_op),
                        })
                    }
                } else {
                    Ok(Pattern::RangePatt(RangePatt {
                        from_pattern_opt: {
                            if let Some(e) = r.from_expr_opt {
                                Some(Box::new(Pattern::try_from(Expression::from(*e))?))
                            } else {
                                None
                            }
                        },
                        range_op: r.range_op,
                        to_pattern_opt: {
                            if let Some(e) = r.to_expr_opt {
                                Some(Box::new(Pattern::try_from(Expression::from(*e))?))
                            } else {
                                None
                            }
                        },
                    }))
                }
            }
            Expression::Underscore(u) => Ok(Pattern::WildcardPatt(WildcardPatt {
                underscore: u.underscore,
            })),
            Expression::Tuple(t) => Ok(Pattern::TuplePatt(TuplePatt {
                tuple_patt_elements: {
                    let mut elements: Vec<Pattern> = Vec::new();

                    for te in t.tuple_elements.elements {
                        elements.push(Pattern::try_from(te)?)
                    }

                    let final_element_opt = if let Some(e) = t.tuple_elements.final_element_opt {
                        Some(Box::new(Pattern::try_from(*e)?))
                    } else {
                        None
                    };

                    TuplePattElements {
                        elements,
                        final_element_opt,
                    }
                },
            })),
            Expression::Struct(s) => Ok(Pattern::StructPatt(StructPatt {
                struct_path: PathPatt {
                    path_root: s.struct_path.path_root,
                    tree_opt: s.struct_path.tree_opt,
                },
                struct_fields_opt: {
                    if let Some(sf) = s.struct_fields_opt {
                        let mut struct_patt_fields: Vec<StructPattField> = Vec::new();
                        {
                            for f in sf {
                                struct_patt_fields.push(StructPattField {
                                    field_name: f.field_name,
                                    field_value: Pattern::try_from(*f.field_value)?,
                                })
                            }
                        };

                        Some(struct_patt_fields)
                    } else {
                        None
                    }
                },
            })),
            Expression::SomeExpr(s) => Ok(Pattern::SomePatt(SomePatt {
                kw_some: s.kw_some,
                pattern: {
                    let expr = Expression::Grouped(*s.expression);
                    let patt = Pattern::try_from(expr)?;

                    Box::new(GroupedPatt {
                        inner_pattern: Box::new(patt),
                    })
                },
            })),
            Expression::NoneExpr(n) => Ok(Pattern::NonePatt(NonePatt { kw_none: n.kw_none })),
            Expression::ResultExpr(r) => Ok(Pattern::ResultPatt(ResultPatt {
                kw_ok_or_err: r.kw_ok_or_err,
                pattern: {
                    let expr = Expression::Grouped(*r.expression);
                    let patt = Pattern::try_from(expr)?;

                    Box::new(GroupedPatt {
                        inner_pattern: Box::new(patt),
                    })
                },
            })),
            _ => Err(ParserErrorKind::UnexpectedExpression {
                expected: "pattern-like expression".to_string(),
                found: format!("{}", value),
            }),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            Pattern::LiteralPatt(l) => write!(f, "{l}"),
            Pattern::IdentifierPatt(id) => write!(
                f,
                "{}{}{}",
                {
                    if let Some(r) = id.kw_ref_opt {
                        format!("{} ", r)
                    } else {
                        "".to_string()
                    }
                },
                {
                    if let Some(m) = id.kw_mut_opt {
                        format!("{} ", m)
                    } else {
                        "".to_string()
                    }
                },
                id.name
            ),
            Pattern::PathPatt(pth) => write!(f, "{}", TypePath::from(pth)),
            Pattern::ReferencePatt(r) => write!(f, "{:?}", r),
            Pattern::GroupedPatt(g) => write!(f, "({})", *g.inner_pattern),
            Pattern::RangePatt(rng) => write!(
                f,
                "{}{}{}",
                rng.from_pattern_opt
                    .unwrap_or(Box::new(Pattern::LiteralPatt(LiteralPatt::Int {
                        value: Int::I64(i64::MIN),
                    }))),
                rng.range_op,
                rng.to_pattern_opt.unwrap_or(Box::new(Pattern::LiteralPatt(
                    LiteralPatt::BigUInt {
                        value: BigUInt::U512(U512::MAX),
                    }
                )))
            ),
            Pattern::TuplePatt(tup) => write!(f, "( {} )", tup.tuple_patt_elements),
            Pattern::StructPatt(strc) => {
                write!(
                    f,
                    "{} {{ {:?} }}",
                    strc.struct_path,
                    strc.struct_fields_opt.unwrap_or(Vec::new())
                )
            }
            Pattern::TupleStructPatt(ts) => {
                write!(
                    f,
                    "{} ( {:?} )",
                    ts.struct_path,
                    ts.struct_elements_opt.unwrap_or(Vec::new())
                )
            }
            Pattern::MappingPatt(m) => {
                write!(f, "{{ {:?} }}", m.pairs)
            }
            Pattern::WildcardPatt(_) => write!(f, "*"),
            Pattern::RestPatt(_) => write!(f, ".."),
            Pattern::SomePatt(som) => write!(f, "Some{}", Pattern::GroupedPatt(*som.pattern)),
            Pattern::NonePatt(_) => write!(f, "None"),
            Pattern::ResultPatt(res) => write!(f, "{}", {
                match res.kw_ok_or_err {
                    Keyword::Ok => format!("Ok{}", Pattern::GroupedPatt(*res.pattern)),
                    Keyword::Err => format!("Err{}", Pattern::GroupedPatt(*res.pattern)),
                    _ => "".to_string(),
                }
            }),
        }
    }
}
