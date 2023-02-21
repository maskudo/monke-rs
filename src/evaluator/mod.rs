use crate::parser::ast::{self, Expr, Infix, Literal, Program, Stmt};

use self::object::Object;

mod object;

#[derive(Debug)]
pub struct Evaluator {}

impl Evaluator {
    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program.statements {
            match self.eval_stmt(statement) {
                Some(Object::ReturnValue(value)) => return Some(*value),
                obj => result = obj,
            };
        }
        result
    }

    fn eval_stmt(&mut self, stmt: Stmt) -> Option<Object> {
        match stmt {
            Stmt::ExprStmt(expr) => self.eval_expr(expr),
            Stmt::Return(expr) => {
                let value = match self.eval_expr(expr) {
                    Some(value) => value,
                    None => return None,
                };
                Some(Object::ReturnValue(Box::new(value)))
            }
            _ => None,
        }
    }

    fn eval_block_stmt(&mut self, stmts: Vec<Stmt>) -> Option<Object> {
        let mut result: Option<Object> = None;
        for stmt in stmts {
            match self.eval_stmt(stmt) {
                Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)),
                obj => result = obj,
            };
        }
        result
    }

    fn eval_expr(&mut self, expr: Expr) -> Option<Object> {
        match expr {
            Expr::Literal(literal) => self.eval_literal(literal),
            Expr::Prefix(prefix, expr) => self.eval_prefix(prefix, *expr),
            Expr::Infix(left, infix, right) => self.eval_infix(*left, infix, *right),
            Expr::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_else(*condition, consequence, alternative),
            _ => None,
        }
    }

    fn eval_if_else(
        &mut self,
        condition: Expr,
        consequence: Vec<Stmt>,
        alternative: Option<Vec<Stmt>>,
    ) -> Option<Object> {
        let condition = self.eval_expr(condition);
        if self.is_truthy(condition?) {
            self.eval_block_stmt(consequence)
        } else {
            match alternative {
                Some(alt) => self.eval_block_stmt(alt),
                None => Some(Object::Null),
            }
        }
    }

    fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Null => false,
            Object::Int(0) => false,
            Object::Bool(false) => false,
            Object::String(s) => {
                if s == String::from("") {
                    return false;
                }
                true
            }
            _ => true,
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

    fn eval_infix(&mut self, left: Expr, infix: ast::Infix, right: Expr) -> Option<Object> {
        match self.eval_expr(left)? {
            Object::Int(left) => match self.eval_expr(right)? {
                Object::Int(right) => Some(self.eval_infix_int_expr(left, infix, right)),
                _ => return None,
            },
            Object::Bool(left) => match self.eval_expr(right)? {
                Object::Bool(right) => self.eval_infix_bool_expr(left, infix, right),
                _ => return None,
            },
            Object::String(left) => match self.eval_expr(right)? {
                Object::String(right) => self.eval_infix_string_expr(left, infix, right),
                _ => return None,
            },
            _ => None,
        }
    }

    fn eval_infix_int_expr(&mut self, left: i64, infix: Infix, right: i64) -> Object {
        //possible panic when value crosses range of i64
        match infix {
            Infix::Plus => Object::Int(left + right),
            Infix::Minus => Object::Int(left - right),
            Infix::Multiply => Object::Int(left * right),
            Infix::Divide => Object::Int(left / right),
            Infix::LessThan => Object::Bool(left < right),
            Infix::GreaterThan => Object::Bool(left > right),
            Infix::LessEqual => Object::Bool(left <= right),
            Infix::GreaterEqual => Object::Bool(left >= right),
            Infix::Equal => Object::Bool(left == right),
            Infix::NotEqual => Object::Bool(left != right),
        }
    }

    fn eval_infix_bool_expr(&mut self, left: bool, infix: Infix, right: bool) -> Option<Object> {
        match infix {
            Infix::Equal => Some(Object::Bool(left == right)),
            Infix::NotEqual => Some(Object::Bool(left != right)),
            _ => None,
        }
    }

    fn eval_infix_string_expr(
        &mut self,
        left: String,
        infix: Infix,
        right: String,
    ) -> Option<Object> {
        match infix {
            Infix::Plus => Some(Object::String(format!("{}{}", left, right))),
            _ => None,
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
    fn test_return_stmt() {
        let tests = vec![
            ("return 10;", Some(Object::Int(10))),
            ("return 10; 9;", Some(Object::Int(10))),
            (("return 2*5; 9;"), Some(Object::Int(10))),
            (("9;return 2*5;9;"), Some(Object::Int(10))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_if_else_expr() {
        let tests = vec![
            ("if (true) {10}", Some(Object::Int(10))),
            ("if (false) {10}", Some(Object::Null)),
            (("if (1) {10}"), Some(Object::Int(10))),
            (("if (1<2) {10}"), Some(Object::Int(10))),
            (("if (1>2) {10}"), Some(Object::Null)),
            (("if (1>2) {10} else {20}"), Some(Object::Int(20))),
            (("if (1<2) {10} else {20}"), Some(Object::Int(10))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_eval_integer_expr() {
        let tests = vec![
            ("5", Some(Object::Int(5))),
            ("10", Some(Object::Int(10))),
            (("-5"), Some(Object::Int(-5))),
            (("+5"), Some(Object::Int(5))),
            (("5 + 5 + 5 -10"), Some(Object::Int(5))),
            (("5 + 5 + 5 + -10"), Some(Object::Int(5))),
            (("5 * 5 / 5"), Some(Object::Int(5))),
            (("(5 + 5) / 5"), Some(Object::Int(2))),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_eval_string_expr() {
        let tests = vec![
            (
                "\"hello world!\";",
                Some(Object::String(String::from("hello world!"))),
            ),
            (
                "\"hello \" + \"world!\";",
                Some(Object::String(String::from("hello world!"))),
            ),
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
            ("false==false", Some(Object::Bool(true))),
            ("false==true", Some(Object::Bool(false))),
            ("false!=true", Some(Object::Bool(true))),
            ("false!=false", Some(Object::Bool(false))),
            (("(5 > 5)"), Some(Object::Bool(false))),
            (("(5 >= 5)"), Some(Object::Bool(true))),
            (("(5 - 5) > 5"), Some(Object::Bool(false))),
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
