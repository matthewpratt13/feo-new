use crate::{
    ast::{
        AssigneeExpr, AssignmentExpr, AssignmentOp, CompoundAssignmentExpr, CompoundAssignmentOp,
        Expression,
    },
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    span::Spanned,
    token::{Token, TokenType},
};

use core::fmt;

impl ParseOperatorExpr for AssignmentExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let lhs: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().cloned().unwrap_or(Token::EOF);

        let assignment_op = parser
            .expect_token(TokenType::Equals)
            .and_then(|_| Ok(AssignmentOp))?;

        let precedence = parser.get_precedence(&operator_token);

        let rhs = parser.parse_value_expr(precedence)?;

        let span = parser.get_span(left_expr_span, &rhs.span());

        let expr = AssignmentExpr {
            lhs,
            assignment_op,
            rhs,
            span,
        };

        Ok(Expression::Assignment(expr))
    }
}

impl fmt::Debug for AssignmentExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AssignmentExpr")
            .field("lhs", &self.lhs)
            .field("assignment_op", &self.assignment_op)
            .field("rhs", &self.rhs)
            .finish()
    }
}

impl ParseOperatorExpr for CompoundAssignmentExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let left_expr_span = &left_expr.span();

        let lhs: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = parser.current_token().cloned().unwrap_or(Token::EOF);

        let compound_assignment_op = match &operator_token.token_type() {
            TokenType::PlusEquals => Ok(CompoundAssignmentOp::AddAssign),
            TokenType::MinusEquals => Ok(CompoundAssignmentOp::SubtractAssign),
            TokenType::AsteriskEquals => Ok(CompoundAssignmentOp::MultiplyAssign),
            TokenType::SlashEquals => Ok(CompoundAssignmentOp::DivideAssign),
            TokenType::PercentEquals => Ok(CompoundAssignmentOp::ModulusAssign),
            _ => {
                parser.log_unexpected_token(&format!(
                    "compound assignment operator ({}, {}, {}, {} or {})",
                    TokenType::PlusEquals,
                    TokenType::MinusEquals,
                    TokenType::AsteriskEquals,
                    TokenType::SlashEquals,
                    TokenType::PercentEquals,
                ));
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let precedence = parser.get_precedence(&operator_token);

        let rhs = parser.parse_value_expr(precedence)?;

        let span = parser.get_span(left_expr_span, &rhs.span());

        let expr = CompoundAssignmentExpr {
            lhs,
            compound_assignment_op,
            rhs,
            span,
        };

        Ok(Expression::CompoundAssignment(expr))
    }
}

impl fmt::Debug for CompoundAssignmentExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompoundAssignmentExpr")
            .field("lhs", &self.lhs)
            .field("compound_assignment_op", &self.compound_assignment_op)
            .field("rhs", &self.rhs)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        logger::LogLevel,
        parser::{test_utils, Precedence},
    };

    #[test]
    fn parse_assignment_expr() -> Result<(), ()> {
        let input = r#"x = 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }

    #[test]
    fn parse_compound_assignment_plus_equals() -> Result<(), ()> {
        let input = r#"x += 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.errors)),
        }
    }
}
