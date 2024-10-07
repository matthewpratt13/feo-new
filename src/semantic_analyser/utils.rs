use crate::ast::{Expression, Identifier};

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

pub(crate) trait ToIdentifier
where
    Self: fmt::Display,
{
    fn to_identifier(&self) -> Identifier {
        Identifier::from(&self.to_string())
    }
}
