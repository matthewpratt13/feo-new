use crate::{
    ast::{
        AssigneeExpr, AssignmentExpr, AssignmentOp, CompoundAssignmentExpr, CompoundAssignmentOp,
        Expression,
    },
    error::ErrorsEmitted,
    parser::{ParseOperatorExpr, Parser},
    token::{Token, TokenType},
};

impl ParseOperatorExpr for AssignmentExpr {
    fn parse(parser: &mut Parser, left_expr: Expression) -> Result<Expression, ErrorsEmitted> {
        let lhs: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = &parser.current_token().cloned().unwrap_or(Token::EOF);

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

        let precedence = parser.get_precedence(operator_token);

        let rhs = parser.parse_value_expr(precedence)?;

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
        let lhs: AssigneeExpr = left_expr.try_into().map_err(|e| {
            parser.log_error(e);
            ErrorsEmitted
        })?;

        let operator_token = &parser.current_token().cloned().unwrap_or(Token::EOF);

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

        let precedence = parser.get_precedence(operator_token);

        let rhs = parser.parse_value_expr(precedence)?;

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
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }

    #[test]
    fn parse_compound_assignment_plus_equals() -> Result<(), ()> {
        let input = r#"x += 5"#;

        let mut parser = test_utils::get_parser(input, LogLevel::Debug, false);

        let expression = parser.parse_expression(Precedence::Lowest);

        match expression {
            Ok(e) => Ok(println!("{:#?}", e)),
            Err(_) => Err(println!("{:#?}", parser.logger.messages())),
        }
    }
}
