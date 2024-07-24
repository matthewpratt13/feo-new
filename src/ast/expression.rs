use core::fmt;

use crate::span::{Span, Spanned};

use super::{
    AssigneeExpr, AssignmentOp, BigUInt, BinaryOp, ComparisonOp, CompoundAssignmentOp,
    DereferenceOp, Expression, Identifier, IdentifierPatt, InnerAttr, Int, Keyword, Literal,
    OuterAttr, Pattern, RangeOp, ReferenceOp, SelfType, Statement, Type, TypeCastOp, TypePath,
    UInt, UnaryOp, Unit, UnwrapOp, ValueExpr, U512,
};

///////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////

/// Enum representing whether or not a closure has parameters in its definition.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ClosureParams {
    Some(Vec<ClosureParam>), // `| <param>: <Type>, .. |`
    None,                    // `||`
}

/// Enum representing the different path root options.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) enum PathRoot {
    Lib,
    Super,
    SelfKeyword,
    SelfType(SelfType),
    Identifier(Identifier),
}

impl fmt::Display for PathRoot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathRoot::Lib => write!(f, "lib"),
            PathRoot::Super => write!(f, "super"),
            PathRoot::SelfKeyword => write!(f, "self"),
            PathRoot::SelfType(s) => write!(f, "{s}"),
            PathRoot::Identifier(i) => write!(f, "{i}"),
        }
    }
}

/// Struct representing a closure parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ClosureParam {
    pub(crate) param_name: IdentifierPatt,
    pub(crate) type_ann_opt: Option<Box<Type>>,
}

/// Struct representing a key-value pair in a mapping.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MappingPair {
    pub(crate) key: Pattern,
    pub(crate) value: Box<Expression>,
}

/// Struct representing a single arm in a match statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct MatchArm {
    pub(crate) matched_pattern: Pattern,
    pub(crate) guard_opt: Option<Box<Expression>>, // `<case> if <expr>`
    pub(crate) arm_expression: Box<Expression>,
}

impl fmt::Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} => {} {},",
            self.matched_pattern,
            {
                if let Some(e) = &self.guard_opt {
                    format!("{}", *e)
                } else {
                    "".to_string()
                }
            },
            self.arm_expression
        )
    }
}

/// Struct representing a single field in a struct assignee expression, with a name, assignee
/// and optional attributes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructAssigneeExprField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) field_name: Identifier,
    pub(crate) field_value: Box<AssigneeExpr>,
}

/// Struct representing a single field in a struct expression, with a name, value
/// and optional attributes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StructField {
    pub(crate) attributes_opt: Option<Vec<OuterAttr>>,
    pub(crate) field_name: Identifier,
    pub(crate) field_value: Box<Expression>,
}

/// Struct representing a collection of elements in a tuple expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TupleElements {
    pub(crate) elements: Vec<Expression>,
    pub(crate) final_element_opt: Option<Box<Expression>>,
}

impl fmt::Display for TupleElements {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}, {:?}",
            self.elements,
            self.final_element_opt
                .clone()
                .unwrap_or(Box::new(Expression::NoneExpr(NoneExpr {
                    kw_none: Keyword::None,
                    span: Span::default()
                })))
        )
    }
}

///////////////////////////////////////////////////////////////////////////
// AST NODE STRUCTURES
///////////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq, Eq)]
pub struct ArrayExpr {
    pub(crate) elements_opt: Option<Vec<Expression>>, // arrays can be empty, hence optional
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct AssignmentExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) assignment_op: AssignmentOp,
    pub(crate) rhs: ValueExpr,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub(crate) lhs: Box<ValueExpr>,
    pub(crate) binary_op: BinaryOp,
    pub(crate) rhs: Box<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct BlockExpr {
    pub(crate) attributes_opt: Option<Vec<InnerAttr>>,
    pub(crate) statements_opt: Option<Vec<Statement>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct BreakExpr {
    pub(crate) kw_break: Keyword,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct CallExpr {
    pub(crate) callee: AssigneeExpr, // function being called
    pub(crate) args_opt: Option<Vec<Expression>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ClosureExpr {
    pub(crate) closure_params: ClosureParams,
    pub(crate) return_type_opt: Option<Box<Type>>,
    pub(crate) body_expression: Box<Expression>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ComparisonExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) comparison_op: ComparisonOp,
    pub(crate) rhs: AssigneeExpr,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct CompoundAssignmentExpr {
    pub(crate) lhs: AssigneeExpr,
    pub(crate) compound_assignment_op: CompoundAssignmentOp,
    pub(crate) rhs: ValueExpr,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ContinueExpr {
    pub(crate) kw_continue: Keyword,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct DereferenceExpr {
    pub(crate) dereference_op: DereferenceOp,
    pub(crate) assignee_expr: AssigneeExpr,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct FieldAccessExpr {
    pub(crate) object: Box<AssigneeExpr>,
    pub(crate) field_name: Identifier,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ForInExpr {
    pub(crate) kw_for: Keyword,
    pub(crate) pattern: Box<Pattern>,
    pub(crate) kw_in: Keyword,
    pub(crate) iterator: Box<Expression>,
    pub(crate) block: BlockExpr,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct GroupedExpr {
    pub(crate) inner_expression: Box<Expression>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct IfExpr {
    pub(crate) kw_if: Keyword,
    pub(crate) condition: Box<GroupedExpr>,
    pub(crate) if_block: Box<BlockExpr>,
    pub(crate) else_if_blocks_opt: Option<Vec<Box<IfExpr>>>,
    pub(crate) trailing_else_block_opt: Option<BlockExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct IndexExpr {
    pub(crate) array: Box<AssigneeExpr>,
    pub(crate) index: Box<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct MappingExpr {
    pub(crate) pairs_opt: Option<Vec<MappingPair>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct MatchExpr {
    pub(crate) kw_match: Keyword,
    pub(crate) scrutinee: AssigneeExpr, // expression to be matched against
    pub(crate) match_arms_opt: Option<Vec<MatchArm>>,
    pub(crate) final_arm: Box<MatchArm>, // default case
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct MethodCallExpr {
    pub(crate) receiver: Box<AssigneeExpr>,
    pub(crate) method_name: Identifier,
    pub(crate) args_opt: Option<Vec<Expression>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct NoneExpr {
    pub(crate) kw_none: Keyword,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct PathExpr {
    pub(crate) path_root: PathRoot,
    pub(crate) tree_opt: Option<Vec<Identifier>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct RangeExpr {
    pub(crate) from_expr_opt: Option<Box<AssigneeExpr>>,
    pub(crate) range_op: RangeOp, // `..` or `..=`
    pub(crate) to_expr_opt: Option<Box<AssigneeExpr>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ReferenceExpr {
    pub(crate) reference_op: ReferenceOp,
    pub(crate) expression: Box<Expression>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ResultExpr {
    pub(crate) kw_ok_or_err: Keyword,
    pub(crate) expression: Box<GroupedExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ReturnExpr {
    pub(crate) kw_return: Keyword,
    pub(crate) expression_opt: Option<Box<Expression>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct SomeExpr {
    pub(crate) kw_some: Keyword,
    pub(crate) expression: Box<GroupedExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct StructExpr {
    pub(crate) struct_path: PathExpr,
    pub(crate) struct_fields_opt: Option<Vec<StructField>>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TupleExpr {
    pub(crate) tuple_elements: TupleElements,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TupleIndexExpr {
    pub(crate) tuple: Box<AssigneeExpr>,
    pub(crate) index: UInt,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TypeCastExpr {
    pub(crate) value: Box<ValueExpr>,
    pub(crate) type_cast_op: TypeCastOp, // `as`
    pub(crate) new_type: Box<Type>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct UnderscoreExpr {
    pub(crate) underscore: Identifier,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    pub(crate) unary_op: UnaryOp,
    pub(crate) value_expr: Box<ValueExpr>,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct UnwrapExpr {
    pub(crate) value_expr: Box<ValueExpr>,
    pub(crate) unwrap_op: UnwrapOp,
    pub(crate) span: Span,
}

#[derive(Clone, PartialEq, Eq)]
pub struct WhileExpr {
    pub(crate) kw_while: Keyword,
    pub(crate) condition: Box<GroupedExpr>,
    pub(crate) block: BlockExpr,
    pub(crate) span: Span,
}

impl Spanned for Expression {
    fn span(&self) -> crate::span::Span {
        match self.clone() {
            Expression::Literal(l) => l.span(),
            Expression::Path(p) => p.span,
            Expression::MethodCall(mc) => mc.span,
            Expression::FieldAccess(fa) => fa.span,
            Expression::Call(c) => c.span,
            Expression::Index(i) => i.span,
            Expression::TupleIndex(ti) => ti.span,
            Expression::Unwrap(unw) => unw.span,
            Expression::Unary(una) => una.span,
            Expression::Reference(r) => r.span,
            Expression::Dereference(de) => de.span,
            Expression::TypeCast(tc) => tc.span,
            Expression::Binary(b) => b.span,
            Expression::Comparison(c) => c.span,
            Expression::Grouped(g) => g.span,
            Expression::Range(r) => r.span,
            Expression::Assignment(a) => a.span,
            Expression::CompoundAssignment(ca) => ca.span,
            Expression::Return(r) => r.span,
            Expression::Break(b) => b.span,
            Expression::Continue(c) => c.span,
            Expression::Underscore(u) => u.span,
            Expression::Closure(c) => c.span,
            Expression::Array(a) => a.span,
            Expression::Tuple(t) => t.span,
            Expression::Struct(s) => s.span,
            Expression::Mapping(m) => m.span,
            Expression::Block(b) => b.span,
            Expression::If(i) => i.span,
            Expression::Match(m) => m.span,
            Expression::ForIn(fi) => fi.span,
            Expression::While(w) => w.span,
            Expression::SomeExpr(s) => s.span,
            Expression::NoneExpr(n) => n.span,
            Expression::ResultExpr(r) => r.span,
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.clone() {
            Expression::Literal(l) => write!(f, "{l}"),
            Expression::Path(pth) => write!(f, "{pth}"),
            Expression::MethodCall(mc) => write!(f, "{}.{}()", mc.receiver, mc.method_name),
            Expression::FieldAccess(fa) => write!(f, "{}.{}", fa.object, fa.field_name),
            Expression::Call(cal) => write!(
                f,
                "{}({:?})",
                cal.callee,
                cal.args_opt.unwrap_or(Vec::new())
            ),
            Expression::Index(i) => write!(f, "{}[{}]", *i.array, *i.index),
            Expression::TupleIndex(ti) => write!(f, "{}.{}", *ti.tuple, ti.index),
            Expression::Unwrap(unw) => write!(f, "{}?", *unw.value_expr),
            Expression::Unary(una) => write!(f, "{}{}", una.unary_op, *una.value_expr),
            Expression::Reference(r) => write!(f, "{}{}", r.reference_op, r.expression),
            Expression::Dereference(dr) => write!(f, "*{}", dr.assignee_expr),
            Expression::TypeCast(tc) => write!(f, "{} as {}", tc.value, tc.new_type),
            Expression::Binary(bin) => write!(f, "{} {} {}", bin.lhs, bin.binary_op, bin.rhs),
            Expression::Comparison(cmp) => {
                write!(f, "{} {} {}", cmp.lhs, cmp.comparison_op, cmp.rhs)
            }
            Expression::Grouped(grp) => write!(f, "({})", *grp.inner_expression),
            Expression::Range(rng) => write!(
                f,
                "{}{}{}",
                rng.from_expr_opt
                    .unwrap_or(Box::new(AssigneeExpr::Literal(Literal::Int {
                        value: Int::I64(i64::MIN),
                        span: rng.span.clone()
                    }))),
                rng.range_op,
                rng.to_expr_opt
                    .unwrap_or(Box::new(AssigneeExpr::Literal(Literal::BigUInt {
                        value: BigUInt::U512(U512::MAX),
                        span: rng.span
                    })))
            ),
            Expression::Assignment(asn) => write!(f, "{} = {}", asn.lhs, asn.rhs),
            Expression::CompoundAssignment(casn) => write!(
                f,
                "{} {} {}",
                casn.lhs, casn.compound_assignment_op, casn.rhs
            ),
            Expression::Return(ret) => write!(
                f,
                "return {}",
                ret.expression_opt
                    .unwrap_or(Box::new(Expression::Tuple(TupleExpr {
                        tuple_elements: TupleElements {
                            elements: Vec::new(),
                            final_element_opt: None
                        },
                        span: ret.span,
                    })))
            ),
            Expression::Break(_) => write!(f, "break"),
            Expression::Continue(_) => write!(f, "continue"),
            Expression::Underscore(_) => write!(f, "_"),
            Expression::Closure(clo) => write!(
                f,
                "({:?}) -> {} {}",
                clo.closure_params,
                clo.return_type_opt
                    .clone()
                    .unwrap_or(Box::new(Type::UnitType(Unit))),
                clo.body_expression
            ),
            Expression::Array(arr) => write!(f, "[ {:?} ]", arr.elements_opt.unwrap_or(Vec::new())),
            Expression::Tuple(tup) => write!(f, "( {} )", tup.tuple_elements),
            Expression::Struct(strc) => {
                write!(
                    f,
                    "{} {{ {:?} }}",
                    strc.struct_path,
                    strc.struct_fields_opt.unwrap_or(Vec::new())
                )
            }
            Expression::Mapping(map) => write!(f, "{{ {:?} }}", map.pairs_opt),
            Expression::Block(blk) => {
                write!(f, "{{ {:?} }}", blk)
            }
            Expression::If(ifex) => write!(
                f,
                "if {} {}{}{}",
                Expression::Grouped(*ifex.condition),
                Expression::Block(*ifex.if_block),
                {
                    if let Some(e) = ifex.else_if_blocks_opt {
                        format!(" else {:?}", e)
                    } else {
                        "".to_string()
                    }
                },
                {
                    if let Some(e) = ifex.trailing_else_block_opt {
                        format!(" else {:?}", e)
                    } else {
                        "".to_string()
                    }
                }
            ),
            Expression::Match(mat) => write!(
                f,
                "match {} {{ {}, {} }}",
                mat.scrutinee,
                {
                    if let Some(m) = mat.match_arms_opt {
                        format!("{:?}", m)
                    } else {
                        "".to_string()
                    }
                },
                *mat.final_arm
            ),
            Expression::ForIn(fi) => write!(
                f,
                "for {} in {} {}",
                fi.pattern,
                fi.iterator,
                Expression::Block(fi.block)
            ),
            Expression::While(whl) => write!(
                f,
                "while {} {}",
                Expression::Grouped(*whl.condition),
                Expression::Block(whl.block)
            ),
            Expression::SomeExpr(som) => {
                write!(f, "Some{}", Expression::Grouped(*som.expression))
            }
            Expression::NoneExpr(_) => write!(f, "None"),
            Expression::ResultExpr(res) => write!(f, "{}", {
                match res.kw_ok_or_err {
                    Keyword::Ok => format!("Ok{}", Expression::Grouped(*res.expression)),
                    Keyword::Err => format!("Err{}", Expression::Grouped(*res.expression)),
                    _ => "".to_string(),
                }
            }),
        }
    }
}

impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0) => f.debug_tuple("Literal").field(arg0).finish(),
            Self::Path(arg0) => f
                .debug_struct("PathExpr")
                .field("path_root", &arg0.path_root)
                .field("tree_opt", &arg0.tree_opt)
                .finish(),
            Self::MethodCall(arg0) => f
                .debug_struct("MethodCall")
                .field("receiver", &arg0.receiver)
                .field("method_name", &arg0.method_name)
                .field("args_opt", &arg0.args_opt)
                .finish(),
            Self::FieldAccess(arg0) => f
                .debug_struct("FieldAccess")
                .field("object", &arg0.object)
                .field("field_name", &arg0.field_name)
                .finish(),
            Self::Call(arg0) => f
                .debug_struct("Call")
                .field("callee", &arg0.callee)
                .field("args_opt", &arg0.args_opt)
                .finish(),
            Self::Index(arg0) => f
                .debug_struct("Index")
                .field("array", &arg0.array)
                .field("index", &arg0.index)
                .finish(),
            Self::TupleIndex(arg0) => f
                .debug_struct("TupleIndex")
                .field("tuple", &arg0.tuple)
                .field("index", &arg0.index)
                .finish(),
            Self::Unwrap(arg0) => f
                .debug_struct("Unwrap")
                .field("value_expr", &arg0.value_expr)
                .finish(),
            Self::Unary(arg0) => f
                .debug_struct("Unary")
                .field("unary_op", &arg0.unary_op)
                .field("value_expr", &arg0.value_expr)
                .finish(),
            Self::Reference(arg0) => f
                .debug_struct("Reference")
                .field("reference_op", &arg0.reference_op)
                .field("expression", &arg0.expression)
                .finish(),
            Self::Dereference(arg0) => f
                .debug_struct("Dereference")
                .field("assignee_expr", &arg0.assignee_expr)
                .finish(),
            Self::TypeCast(arg0) => f
                .debug_struct("TypeCast")
                .field("value", &arg0.value)
                .field("new_type", &arg0.new_type)
                .finish(),
            Self::Binary(arg0) => f
                .debug_struct("Binary")
                .field("lhs", &arg0.lhs)
                .field("binary_op", &arg0.binary_op)
                .field("rhs", &arg0.rhs)
                .finish(),
            Self::Comparison(arg0) => f
                .debug_struct("Comparison")
                .field("lhs", &arg0.lhs)
                .field("comparison_op", &arg0.comparison_op)
                .field("rhs", &arg0.rhs)
                .finish(),
            Self::Grouped(arg0) => f
                .debug_struct("Grouped")
                .field("inner_expr", &arg0.inner_expression)
                .finish(),
            Self::Range(arg0) => f
                .debug_struct("Range")
                .field("from_expr_opt", &arg0.from_expr_opt)
                .field("range_op", &arg0.range_op)
                .field("to_expr_opt", &arg0.to_expr_opt)
                .finish(),
            Self::Assignment(arg0) => f
                .debug_struct("Assignment")
                .field("lhs", &arg0.lhs)
                .field("rhs", &arg0.rhs)
                .finish(),
            Self::CompoundAssignment(arg0) => f
                .debug_struct("CompoundAssignment")
                .field("lhs", &arg0.lhs)
                .field("compound_assignment_op", &arg0.compound_assignment_op)
                .field("rhs", &arg0.rhs)
                .finish(),
            Self::Return(arg0) => f
                .debug_struct("Return")
                .field("expression_opt", &arg0.expression_opt)
                .finish(),
            Self::Break(arg0) => f
                .debug_struct("Break")
                .field("kw_break", &arg0.kw_break)
                .finish(),
            Self::Continue(arg0) => f
                .debug_struct("Continue")
                .field("kw_continue", &arg0.kw_continue)
                .finish(),
            Self::Underscore(arg0) => f
                .debug_struct("Underscore")
                .field("underscore", &arg0.underscore)
                .finish(),
            Self::Closure(arg0) => f
                .debug_struct("Closure")
                .field("closure_params", &arg0.closure_params)
                .field("return_type_opt", &arg0.return_type_opt)
                .field("body_expression", &arg0.body_expression)
                .finish(),
            Self::Array(arg0) => f
                .debug_struct("Array")
                .field("elements_opt", &arg0.elements_opt)
                .finish(),
            Self::Tuple(arg0) => f
                .debug_struct("Tuple")
                .field("tuple_elements", &arg0.tuple_elements)
                .finish(),
            Self::Struct(arg0) => f
                .debug_struct("Struct")
                .field("struct_path", &TypePath::from(arg0.struct_path.clone()))
                .field("struct_fields_opt", &arg0.struct_fields_opt)
                .finish(),
            Self::Mapping(arg0) => f
                .debug_struct("Mapping")
                .field("pairs_opt", &arg0.pairs_opt)
                .finish(),
            Self::Block(arg0) => f
                .debug_struct("Block")
                .field("attributes_opt", &arg0.attributes_opt)
                .field("statements_opt", &arg0.statements_opt)
                .finish(),
            Self::If(arg0) => f
                .debug_struct("If")
                .field("condition", &arg0.condition)
                .field("if_block", &arg0.if_block)
                .field("else_if_blocks_opt", &arg0.else_if_blocks_opt)
                .field("trailing_else_block_opt", &arg0.trailing_else_block_opt)
                .finish(),
            Self::Match(arg0) => f
                .debug_struct("Match")
                .field("scrutinee", &arg0.scrutinee)
                .field("match_arms_opt", &arg0.match_arms_opt)
                .field("final_arm", &arg0.final_arm)
                .finish(),
            Self::ForIn(arg0) => f
                .debug_struct("ForIn")
                .field("pattern", &arg0.pattern)
                .field("iterator", &arg0.iterator)
                .field("block", &arg0.block)
                .finish(),
            Self::While(arg0) => f
                .debug_struct("While")
                .field("condition", &arg0.condition)
                .field("block", &arg0.block)
                .finish(),
            Self::SomeExpr(arg0) => f
                .debug_struct("SomeExpr")
                .field("expression", &arg0.expression)
                .finish(),
            Self::NoneExpr(arg0) => f
                .debug_struct("NoneExpr")
                .field("kw_none", &arg0.kw_none)
                .finish(),
            Self::ResultExpr(arg0) => f
                .debug_struct("ResultExpr")
                .field("expression", &arg0.expression)
                .finish(),
        }
    }
}

impl fmt::Debug for BreakExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BreakExpr")
            .field("kw_break", &self.kw_break)
            .finish()
    }
}

impl fmt::Debug for ContinueExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ContinueExpr")
            .field("kw_continue", &self.kw_continue)
            .finish()
    }
}

impl fmt::Debug for NoneExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NoneExpr")
            .field("kw_none", &self.kw_none)
            .finish()
    }
}

impl fmt::Debug for UnderscoreExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UnderscoreExpr")
            .field("underscore", &self.underscore)
            .finish()
    }
}
