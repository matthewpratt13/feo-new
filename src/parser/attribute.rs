use core::fmt;

use crate::{
    ast::{InnerAttr, OuterAttr},
    token::Token,
};

use super::Parser;

impl InnerAttr {
    pub(crate) fn inner_attr(parser: &Parser) -> Option<InnerAttr> {
        match parser.current_token() {
            Some(Token::Abstract { .. }) => Some(InnerAttr::Abstract),
            Some(Token::Contract { .. }) => Some(InnerAttr::Contract),
            Some(Token::Library { .. }) => Some(InnerAttr::Library),
            Some(Token::Interface { .. }) => Some(InnerAttr::Interface),
            Some(Token::Script { .. }) => Some(InnerAttr::Script),
            Some(Token::Unsafe { .. }) => Some(InnerAttr::Unsafe),
            _ => None,
        }
    }
}

impl fmt::Display for InnerAttr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InnerAttr::Abstract => write!(f, "#![abstract]"),
            InnerAttr::Contract => write!(f, "#![contract]"),
            InnerAttr::Interface => write!(f, "#![interface]"),
            InnerAttr::Library => write!(f, "#![library]"),
            InnerAttr::Script => write!(f, "#![script]"),
            InnerAttr::Unsafe => write!(f, "#![unsafe]"),
        }
    }
}

impl OuterAttr {
    pub(crate) fn outer_attr(parser: &Parser) -> Option<OuterAttr> {
        match parser.current_token() {
            Some(Token::Calldata { .. }) => Some(OuterAttr::Calldata),
            Some(Token::Constructor { .. }) => Some(OuterAttr::Constructor),
            Some(Token::Error { .. }) => Some(OuterAttr::Error),
            Some(Token::Event { .. }) => Some(OuterAttr::Event),
            Some(Token::Extern { .. }) => Some(OuterAttr::Extern),
            Some(Token::Modifier { .. }) => Some(OuterAttr::Modifier),
            Some(Token::Payable { .. }) => Some(OuterAttr::Payable),
            Some(Token::Storage { .. }) => Some(OuterAttr::Storage),
            Some(Token::Test { .. }) => Some(OuterAttr::Test),
            Some(Token::Topic { .. }) => Some(OuterAttr::Topic),
            Some(Token::View { .. }) => Some(OuterAttr::View),
            _ => None,
        }
    }
}

impl fmt::Display for OuterAttr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OuterAttr::Calldata => write!(f, "#[calldata]"),
            OuterAttr::Constructor => write!(f, "#[constructor]"),
            OuterAttr::Error => write!(f, "#[error]"),
            OuterAttr::Event => write!(f, "#[event]"),
            OuterAttr::Extern => write!(f, "#[extern]"),
            OuterAttr::Modifier => write!(f, "#[modifier]"),
            OuterAttr::Payable => write!(f, "#[payable]"),
            OuterAttr::Storage => write!(f, "#[storage]"),
            OuterAttr::Test => write!(f, "#[test]"),
            OuterAttr::Topic => write!(f, "#[topic]"),
            OuterAttr::View => write!(f, "#[view]"),
        }
    }
}
