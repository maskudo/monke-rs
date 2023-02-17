use crate::parser::ast::{Expr, Literal, Program, Stmt};

use self::object::Object;

mod object;

#[derive(Debug)]
pub struct Evaluator {}

impl Evaluator {
    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program.statements {
            match self.eval_stmt(statement) {
                obj => result = obj,
                _ => return None,
            };
        }
        result
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Option<Object> {
        match stmt {
            Stmt::ExprStmt(expr) => self.eval_expr(expr),
            _ => None,
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Option<Object> {
        match expr {
            Expr::Literal(literal) => self.eval_literal(literal),
            _ => None,
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Option<Object> {
        match literal {
            Literal::Int(value) => Some(Object::Int(value)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::{
        lexer::{self, Lexer},
        parser::{ast::Program, Parser},
    };

    use super::{object::Object, Evaluator};

    fn eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut evaluator = Evaluator {};
        evaluator.eval(program)
    }

    #[test]
    fn test_eval_integer_expr() {
        let tests = vec![
            ("5", Some(Object::Int(5))),
            ("10", Some(Object::Int(10))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
