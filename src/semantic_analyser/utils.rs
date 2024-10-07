use core::fmt;

pub(crate) trait FormatString
where
    Self: fmt::Display,
{
    fn to_backtick_string(&self) -> String {
        format!("`{}`", self)
    }
}
