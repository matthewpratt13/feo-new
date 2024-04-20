use crate::{
    ast::{
        BlockExpr, FunctionItem, FunctionOrMethodParam, FunctionParam, Identifier, Keyword,
        OuterAttr, SelfParam, UnaryOp, Visibility,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::Parser;

impl FunctionItem {
    pub(crate) fn parse(
        parser: &mut Parser,
        attributes: Vec<OuterAttr>,
        visibility: Visibility,
    ) -> Result<FunctionItem, ErrorsEmitted> {
        let kw_func = parser.expect_keyword(Token::Func {
            name: "func".to_string(),
            span: parser.stream.span(),
        })?;

        let mut params: Vec<FunctionOrMethodParam> = Vec::new();

        let token = parser.consume_token();

        let function_name = if let Some(Token::Identifier { name, .. }) = token {
            Ok(Identifier(name))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        }?;

        let open_paren = parser.expect_delimiter(Token::LParen {
            delim: '(',
            span: parser.stream.span(),
        })?;

        while let Some(t) = parser.consume_token() {
            if let Token::Ampersand { .. } | Token::AmpersandMut { .. } | Token::Identifier { .. } =
                t
            {
                let param = FunctionOrMethodParam::parse(parser)?;
                params.push(param);
            } else {
                parser.log_error(ParserErrorKind::UnexpectedToken {
                    expected: "`&`, `&mut` or identifier".to_string(),
                    found: Some(t),
                });
            }
        }

        let close_paren = parser.expect_delimiter(Token::RParen {
            delim: ')',
            span: parser.stream.span(),
        })?;

        let return_type_opt = if let Some(Token::ThinArrow { .. }) = parser.peek_current() {
            parser.consume_token();
            Some(Box::new(parser.get_type()?))
        } else {
            None
        };

        let block_opt = if let Some(Token::LBrace { .. }) = parser.peek_current() {
            Some(BlockExpr::parse(parser)?)
        } else {
            None
        };

        if !parser.errors().is_empty() {
            return Err(ErrorsEmitted(()));
        }

        Ok(FunctionItem {
            attributes_opt: {
                if attributes.is_empty() {
                    None
                } else {
                    Some(attributes)
                }
            },
            visibility,
            kw_func,
            function_name,
            open_paren,
            params_opt: {
                if params.is_empty() {
                    None
                } else {
                    Some(params)
                }
            },
            close_paren,
            return_type_opt,
            block_opt,
        })
    }
}

impl FunctionOrMethodParam {
    fn parse(parser: &mut Parser) -> Result<FunctionOrMethodParam, ErrorsEmitted> {
        let token = parser.peek_current();

        let prefix_opt = if let Some(Token::Ampersand { .. } | Token::AmpersandMut { .. }) = token {
            parser.consume_token();
            Some(UnaryOp::Reference)
        } else {
            None
        };

        let token = parser.consume_token();

        let param = if let Some(Token::SelfKeyword { .. }) = token {
            let self_param = SelfParam {
                prefix_opt,
                kw_self: Keyword::SelfKeyword,
            };

            Ok(FunctionOrMethodParam::MethodParam(self_param))
        } else if let Some(Token::Identifier { name, .. }) = token {
            let _ = parser.expect_separator(Token::Colon {
                punc: ':',
                span: parser.stream.span(),
            })?;

            let param_type = Box::new(parser.get_type()?);

            let function_param = FunctionParam {
                param: Identifier(name),
                param_type,
            };

            Ok(FunctionOrMethodParam::FunctionParam(function_param))
        } else {
            parser.log_error(ParserErrorKind::UnexpectedToken {
                expected: "`self` or identifier".to_string(),
                found: token,
            });
            Err(ErrorsEmitted(()))
        };

        param
    }
}
