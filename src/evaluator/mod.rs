use crate::parser::ast::{self, Expr, Literal, Program, Stmt};

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
            Expr::Prefix(prefix, expr) => self.eval_prefix(prefix, *expr),
            _ => None,
        }
    }

    fn eval_prefix(&mut self, prefix: ast::Prefix, expr: Expr) -> Option<Object> {
        match prefix {
            ast::Prefix::Not => self.eval_expr(expr).and_then(|value| match value {
                Object::Bool(bool) => Some(Object::Bool(!bool)),
                _ => return None,
            }),
            ast::Prefix::Plus => self.eval_expr(expr).and_then(|obj| match obj {
                Object::Int(int) => Some(Object::Int(int)),
                _ => return None,
            }),
            ast::Prefix::Minus => self.eval_expr(expr).and_then(|obj| match obj {
                Object::Int(int) => Some(Object::Int(-int)),
                _ => return None,
            }),
        }
    }

    fn eval_literal(&mut self, literal: Literal) -> Option<Object> {
        match literal {
            Literal::Int(value) => Some(Object::Int(value)),
            Literal::String(value) => Some(Object::String(value)),
            Literal::Bool(value) => Some(Object::Bool(value)),
        }
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::{lexer::Lexer, parser::Parser};

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
            (("-5"), Some(Object::Int(-5))),
            (("+5"), Some(Object::Int(5))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_eval_bool_expr() {
        let tests = vec![
            ("true", Some(Object::Bool(true))),
            ("false", Some(Object::Bool(false))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_eval_bang_operator() {
        //no truthy and falsey values
        let tests = vec![
            ("!true", Some(Object::Bool(false))),
            ("!false", Some(Object::Bool(true))),
            ("!!false", Some(Object::Bool(false))),
            ("!!true", Some(Object::Bool(true))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
