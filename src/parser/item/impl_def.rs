use crate::{
    ast::{
        AliasDecl, ConstantDecl, Delimiter, FunctionItem, Identifier, InherentImplDef,
        InherentImplItem, Keyword, OuterAttr, PathExpr, PathPrefix, TraitImplDef, TraitImplItem,
        Type, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    logger::{LogLevel, LogMsg},
    token::Token,
};

use super::{collection, ParseAssociatedItem, ParseDeclaration, ParseDefinition, Parser};

impl ParseDefinition for InherentImplDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        _visibility: Visibility,
    ) -> Result<InherentImplDef, ErrorsEmitted> {
        let kw_impl = if let Some(Token::Impl { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Impl)
        } else {
            parser.log_unexpected_token("`impl`");
            Err(ErrorsEmitted)
        }?;

        let nominal_type = Type::parse(parser)?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
            // TODO: handle `None` case (`MissingToken`)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let associated_items_opt = collection::get_associated_items::<InherentImplItem>(parser)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(&open_brace);
            Err(ErrorsEmitted)
        }?;

        Ok(InherentImplDef {
            attributes_opt,
            kw_impl,
            nominal_type,
            open_brace,
            associated_items_opt,
            close_brace,
        })
    }
}

impl ParseDefinition for TraitImplDef {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        _visibility: Visibility,
    ) -> Result<TraitImplDef, ErrorsEmitted> {
        let kw_impl = if let Some(Token::Impl { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::Impl)
        } else {
            parser.log_unexpected_token("`impl`");
            Err(ErrorsEmitted)
        }?;

        let token = parser.current_token();

        let implemented_trait_path = if let Some(Token::Identifier { name, .. }) = token {
            let path = PathExpr::parse(parser, PathPrefix::Identifier(Identifier(name)));
            parser.next_token();
            path
            // TODO: handle `None` case (`UnexpectedEndOfInput`)
        } else {
            parser.log_unexpected_token("implemented trait path");
            parser.next_token();

            Err(ErrorsEmitted)
        }?;

        let kw_for = if let Some(Token::For { .. }) = parser.current_token() {
            parser.next_token();
            Ok(Keyword::For)
        } else {
            parser.log_unexpected_token("`for`");
            Err(ErrorsEmitted)
        }?;

        let implementing_type = Type::parse(parser)?;

        let open_brace = if let Some(Token::LBrace { .. }) = parser.next_token() {
            Ok(Delimiter::LBrace)
            // TODO: handle `None` case (`MissingToken`)
        } else {
            parser.log_unexpected_token("`{`");
            Err(ErrorsEmitted)
        }?;

        let associated_items_opt = collection::get_associated_items::<TraitImplItem>(parser)?;

        let close_brace = if let Some(Token::RBrace { .. }) = parser.next_token() {
            Ok(Delimiter::RBrace)
        } else {
            parser.log_missing_token("`}`");
            parser.log_unmatched_delimiter(&open_brace);
            Err(ErrorsEmitted)
        }?;

        Ok(TraitImplDef {
            attributes_opt,
            kw_impl,
            implemented_trait_path,
            kw_for,
            implementing_type,
            open_brace,
            associated_items_opt,
            close_brace,
        })
    }
}

impl ParseAssociatedItem for InherentImplItem {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<InherentImplItem, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Const { .. }) => {
                let constant_decl = ConstantDecl::parse(parser, attributes_opt, visibility)?;
                if constant_decl.value_opt.is_none() {
                    parser.logger.log(
                        LogLevel::Warning,
                        LogMsg::from("assigned value cannot be `None`"),
                    );
                    Err(ErrorsEmitted)
                } else {
                    Ok(InherentImplItem::ConstantDecl(constant_decl))
                }
            }

            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes_opt, visibility)?;
                if function_def.block_opt.is_none() {
                    // TODO: should be `MissingItems`
                    parser.log_error(ParserErrorKind::ExtraTokens {
                        token: parser.current_token(),
                        msg: "functions in implementation blocks must have bodies".to_string(),
                    });
                    Err(ErrorsEmitted)
                } else {
                    Ok(InherentImplItem::FunctionDef(function_def))
                }
            }
            _ => {
                parser.log_unexpected_token("`const` or `func`");
                Err(ErrorsEmitted)
            }
        }
    }
}

impl ParseAssociatedItem for TraitImplItem {
    fn parse(
        parser: &mut Parser,
        attributes_opt: Option<Vec<OuterAttr>>,
        visibility: Visibility,
    ) -> Result<TraitImplItem, ErrorsEmitted> {
        match parser.current_token() {
            Some(Token::Const { .. }) => {
                let constant_decl = ConstantDecl::parse(parser, attributes_opt, visibility)?;
                if constant_decl.value_opt.is_none() {
                    parser.logger.log(
                        LogLevel::Warning,
                        LogMsg::from("assigned value cannot be `None`"),
                    );
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitImplItem::ConstantDecl(constant_decl))
                }
            }
            Some(Token::Alias { .. }) => {
                let alias_decl = AliasDecl::parse(parser, attributes_opt, visibility)?;
                Ok(TraitImplItem::AliasDecl(alias_decl))
            }
            Some(Token::Func { .. }) => {
                let function_def = FunctionItem::parse(parser, attributes_opt, visibility)?;
                if function_def.block_opt.is_none() {
                    // TODO: should be `MissingItems`
                    parser.log_error(ParserErrorKind::ExtraTokens {
                        token: parser.current_token(),
                        msg: "functions in implementation blocks must have bodies".to_string(),
                    });
                    Err(ErrorsEmitted)
                } else {
                    Ok(TraitImplItem::FunctionDef(function_def))
                }
            }
            _ => {
                parser.log_unexpected_token("`const`, `alias` or `func`");
                Err(ErrorsEmitted)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_impl_def_inherent() -> Result<(), ()> {
        let input = r#"
        impl Foo {
            #[storage]
            const BAR: str = "bar";

            #[constructor]
            pub func new(param1: char, param2: bool) -> Foo {
                Foo {
                    param1: param1,
                    param2: param2
                }
            }

            #[view]
            pub func get_param1(&self) -> char {
                self.param1
            }

            func set_param2(&mut self) {
                self.param2 = false
            }
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }

    #[test]
    fn parse_impl_def_trait() -> Result<(), ()> {
        let input = r#"
        impl Foo for Bar {
            #[storage]
            const BAZ: str = "baz";
            alias FooBar = u64;

            pub func sender(&self) -> h160 {
                self.address
            }

            pub func transfer(&mut self, to: h160, amount: u256) -> Event {
                self.balance -= amount;
                to.balance += amount;

                Event { msg: "transfer", to: to, amount: amount }
            }
        }"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.logs())),
        }
    }
}
