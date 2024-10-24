use core::fmt;

use crate::ast::{Expression, Identifier};

pub(crate) trait FormatItem
where
    Self: fmt::Display,
{
    fn to_backtick_string(&self) -> String {
        format!("`{}`", self)
    }
}

pub(crate) trait FormatParams {
    fn param_strings(&self) -> Vec<String>;
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
