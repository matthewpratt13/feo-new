use crate::{
    ast::{
        AssigneeExpr, AssignmentExpr, AssignmentOp, CompoundAssignmentExpr, CompoundAssignmentOp,
        Expression, ValueExpr,
    },
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    token::{Token, TokenType},
};

impl ParseOperatorExpr for AssignmentExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let assignment_op = match operator_token {
            Token::Equals { .. } => {
                parser.next_token();
                Ok(AssignmentOp)
            }
            Token::EOF => {
                parser.log_unexpected_eoi();
                Err(ErrorsEmitted)
            }
            _ => {
                parser.log_unexpected_token("assignment operator (`=`)");
                Err(ErrorsEmitted)
            }
        }?;

        let precedence = parser.get_precedence(&operator_token);

        let right_expr = parser.parse_expression(precedence)?;

        let lhs = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let rhs = ValueExpr::try_from(right_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = AssignmentExpr {
            lhs,
            assignment_op,
            rhs,
        };

        Ok(Expression::Assignment(expr))
    }
}

impl ParseOperatorExpr for CompoundAssignmentExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let operator_token = parser.current_token().unwrap_or(Token::EOF);

        let compound_assignment_op = match operator_token.token_type() {
            TokenType::PlusEquals => Ok(CompoundAssignmentOp::AddAssign),
            TokenType::MinusEquals => Ok(CompoundAssignmentOp::SubtractAssign),
            TokenType::AsteriskEquals => Ok(CompoundAssignmentOp::MultiplyAssign),
            TokenType::SlashEquals => Ok(CompoundAssignmentOp::DivideAssign),
            TokenType::PercentEquals => Ok(CompoundAssignmentOp::ModulusAssign),
            _ => {
                parser.log_unexpected_token(
                    "compound assignment operator (`+=`, `-=`, `*=`, `/=` or `%=`)",
                );
                Err(ErrorsEmitted)
            }
        }?;

        parser.next_token();

        let precedence = parser.get_precedence(&operator_token);

        let right_expr = parser.parse_expression(precedence)?;

        let lhs = AssigneeExpr::try_from(left_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let rhs = ValueExpr::try_from(right_expr).map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let expr = CompoundAssignmentExpr {
            lhs,
            compound_assignment_op,
            rhs,
        };

        Ok(Expression::CompoundAssignment(expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{logger::LogLevel, parser::test_utils};

    #[test]
    fn parse_assignment_expr() -> Result<(), ()> {
        let input = r#"x = 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_compound_assignment_plus_equals() -> Result<(), ()> {
        let input = r#"x += 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let statements = parser.parse();

        match statements {
            Ok(t) => Ok(println!("{:#?}", t)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
