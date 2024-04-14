use crate::{
    ast::{
        ArrayExpr, BinaryOp, BlockExpr, ClosureExpr, ClosureParam, ClosureParams, Delimiter,
        Expression, GroupedExpr, Identifier, Separator, Statement, TupleExpr,
    },
    error::{ErrorsEmitted, ParserErrorKind},
    token::Token,
};

use super::{Parser, Precedence};

pub(crate) trait ParseCompoundExpr
where
    Self: Sized,
{
    fn parse(parser: &mut Parser) -> Result<Self, ErrorsEmitted>;
}

// impl ParseCompoundExpr for ArrayExpr {
//     fn parse(parser: &mut Parser) -> Result<ArrayExpr, ErrorsEmitted> {
//         let mut elements: Vec<Expression> = Vec::new();

//         while !parser.is_expected_token(&Token::RBracket {
//             delim: ']',
//             span: parser.stream.span(),
//         }) {
//             let element = parser.parse_expression(Precedence::Lowest)?;
//             elements.push(element);

//             if !parser.tokens_match(Token::Comma {
//                 punc: ',',
//                 span: parser.stream.span(),
//             }) {
//                 break;
//             }
//         }

//         let close_bracket = parser.expect_delimiter(Token::RBracket {
//             delim: ']',
//             span: parser.stream.span(),
//         });

//         if !parser.errors().is_empty() {
//             return Err(ErrorsEmitted(()));
//         }

//         Ok(ArrayExpr {
//             open_bracket: Delimiter::LBracket,
//             elements,
//             close_bracket: close_bracket?,
//         })
//     }
// }

// impl ParseCompoundExpr for BlockExpr {
//     fn parse(parser: &mut Parser) -> Result<BlockExpr, ErrorsEmitted> {
//         let mut statements: Vec<Statement> = Vec::new();

//         while !parser.is_expected_token(&Token::RBrace {
//             delim: '}',
//             span: parser.stream.span(),
//         }) {
//             let statement = parser.parse_statement()?;
//             statements.push(statement);
//         }

//         let terminal_expression_opt = if let Ok(e) = parser.parse_expression(Precedence::Lowest) {
//             Some(Box::new(e))
//         } else {
//             None
//         };

//         let close_brace = parser.expect_delimiter(Token::RBrace {
//             delim: '}',
//             span: parser.stream.span(),
//         });

//         if statements.is_empty() {
//             parser.log_error(ParserErrorKind::TokenNotFound {
//                 expected: "statement".to_string(),
//             });
//         }

//         if !parser.errors().is_empty() {
//             return Err(ErrorsEmitted(()));
//         }

//         Ok(BlockExpr {
//             open_brace: Delimiter::LBrace,
//             statements,
//             terminal_expression_opt,
//             close_brace: close_brace?,
//         })
//     }
// }

