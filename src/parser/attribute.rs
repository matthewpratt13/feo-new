use crate::{
    ast::{InnerAttr, OuterAttr},
    token::Token,
};

use super::Parser;

impl InnerAttr {
    pub(crate) fn inner_attr(parser: &Parser) -> Option<InnerAttr> {
        match parser.current_token().as_ref() {
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

impl OuterAttr {
    pub(crate) fn outer_attr(parser: &Parser) -> Option<OuterAttr> {
        match parser.current_token().as_ref() {
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
