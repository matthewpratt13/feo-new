use crate::ast::Expression;

use core::fmt;

pub(crate) trait FormatString
where
    Self: fmt::Display,
{
    fn to_backtick_string(&self) -> String {
        format!("`{}`", self)
    }
}

pub(crate) trait ToExpression {
    fn to_expression(&self) -> Expression
    where
        Self: Clone + fmt::Debug + TryFrom<Expression>,
        Expression: From<Self>,
    {
        Expression::from(self.clone())
    }
}
