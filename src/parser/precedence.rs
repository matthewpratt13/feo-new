/// Enum representing the different precedence levels of operators, respectively.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,             // `break`, `return`, closure
    Assignment,         // `=`
    CompoundAssignment, // `+=`, `-=`, `*/`, `/=`, `%=`
    Range,              // `..`, `..=` (requires parentheses)
    LogicalOr,          // `||`
    LogicalAnd,         // `&&`
    Equal,              // `==`
    NotEqual,           // `!=`
    LessThan,           // `<`
    GreaterThan,        // `>`
    LessThanOrEqual,    // `<=`
    GreaterThanOrEqual, // `>=`
    BitwiseOr,          // `|`
    BitwiseXor,         // `^`
    BitwiseAnd,         // `&`
    Shift,              // `«`, `»`
    Sum,                // `+`
    Difference,         // `-`
    Product,            // `*`
    Quotient,           // `/`
    Remainder,          // `%`
    Exponentiation,     // `**` // TODO: add to `parser_binary_expr()`
    TypeCast,           // "as"
    Unary,              // `-`, `*` `!`, `&`,`&mut`
    Unwrap,             // `?`
    Index,              // `x[0]`
    Call,               // `foo(bar)`
    FieldAccess,        // foo.bar
    MethodCall,         // foo.bar()
    Path,               // `package::module::Item`
}
