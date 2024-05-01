/// Enum representing the different precedence levels of operators, from lowest to highest.
/// Operations with higher precedences get evaluated first, and vice versa.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,             // `return`, `break`, closure
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
    Exponentiation,     // `**`
    TypeCast,           // "as"
    Unary,              // `-`, `!`, `*`, `&`,`&mut`
    Unwrap,             // `?`
    Index,              // `array[0]`
    Call,               // `callee(foo)`
    FieldAccess,        // object.field
    MethodCall,         // receive.method()
    TupleIndex,         // tuple.0
    Path,               // `package::module::Item`
}
