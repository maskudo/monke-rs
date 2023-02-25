use std::cell::RefCell;
use std::rc::Rc;

use crate::parser::ast::{self, Expr, Ident, Infix, Literal, Program, Stmt};

use self::env::Env;
use self::object::Object;

pub mod env;
mod object;

#[derive(Debug)]
pub struct Evaluator {
    env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new(env: Rc<RefCell<Env>>) -> Self {
        Evaluator { env }
    }
    pub fn eval(&mut self, program: Program) -> Option<Object> {
        let mut result: Option<Object> = None;
        for statement in program.statements {
            match self.eval_stmt(statement) {
                Some(Object::ReturnValue(value)) => return Some(*value),
                Some(Object::Error(err)) => return Some(Object::Error(err)),
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
            Stmt::Let(name, value) => {
                let value = self.eval_expr(value)?;
                let is_error = match value {
                    Object::Error(_) => true,
                    _ => false,
                };
                if is_error {
                    Some(value)
                } else {
                    let Ident(name) = name;
                    self.env.borrow_mut().set(name, value);
                    None
                }
            }
        }
    }

    fn eval_block_stmt(&mut self, stmts: Vec<Stmt>) -> Option<Object> {
        let mut result: Option<Object> = None;
        for stmt in stmts {
            match self.eval_stmt(stmt) {
                Some(Object::ReturnValue(value)) => return Some(Object::ReturnValue(value)),
                Some(Object::Error(err)) => return Some(Object::Error(err)),
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
            Expr::Ident(ident) => Some(self.eval_ident(ident)),
            Expr::Function { parameters, body } => Some(Object::Function {
                parameters,
                body,
                env: Rc::new(RefCell::new(Env::new())),
            }),
            Expr::Call {
                function,
                arguments,
            } => Some(self.eval_call_expr(function, arguments)),
        }
    }

    fn eval_call_expr(&mut self, function: Box<Expr>, arguments: Vec<Expr>) -> Object {
        let args: Vec<Object> = arguments
            .iter()
            .map(|expr| self.eval_expr(expr.clone()).unwrap_or(Object::Null))
            .collect();
        let (params, body, env) = match self.eval_expr(*function) {
            Some(Object::Function {
                parameters,
                body,
                env,
            }) => (parameters, body, env),
            Some(obj) => return Object::Error(format!("{} is not a valid function", obj)),
            None => return Object::Null,
        };

        if params.len() != arguments.len() {
            return Object::Error(format!(
                "wrong number of arguments: expected {} but {} given",
                params.len(),
                arguments.len()
            ));
        };

        let current_env = Rc::clone(&self.env);
        let mut scoped_env = Env::new_enclosed_env(Rc::clone(&env));
        let list = params.iter().zip(args.iter());
        for (_, (ident, value)) in list.enumerate() {
            let Ident(name) = ident.clone();
            scoped_env.set(name, value.clone());
        }

        self.env = Rc::new(RefCell::new(scoped_env));
        println!("{:?}", self.env);
        let object = self.eval_block_stmt(body);

        self.env = current_env;
        match object {
            Some(obj) => obj,
            None => Object::Null,
        }
    }

    fn eval_ident(&self, ident: Ident) -> Object {
        let Ident(ident) = ident;
        let value = self.env.borrow_mut().get(&ident);
        match value {
            Some(value) => value,
            None => Object::Error(format!("identifier not found: {}", ident)),
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
                _ => return Some(Object::Error(format!("unknown operator: !{}", value))),
            }),
            ast::Prefix::Plus => self.eval_expr(expr).and_then(|obj| match obj {
                Object::Int(int) => Some(Object::Int(int)),
                _ => return Some(Object::Error(format!("unknown operator: +{}", obj))),
            }),
            ast::Prefix::Minus => self.eval_expr(expr).and_then(|obj| match obj {
                Object::Int(int) => Some(Object::Int(-int)),
                _ => return Some(Object::Error(format!("unknown operator: -{}", obj))),
            }),
        }
    }

    fn eval_infix(&mut self, left: Expr, infix: ast::Infix, right: Expr) -> Option<Object> {
        match self.eval_expr(left.clone())? {
            Object::Int(left) => match self.eval_expr(right.clone())? {
                Object::Int(right) => Some(self.eval_infix_int_expr(left, infix, right)),
                _ => {
                    return Some(Object::Error(format!(
                        "type mismatch: {} {} {}",
                        left, infix, right
                    )))
                }
            },
            Object::Bool(left) => match self.eval_expr(right.clone())? {
                Object::Bool(right) => self.eval_infix_bool_expr(left, infix, right),
                _ => {
                    return Some(Object::Error(format!(
                        "type mismatch: {} {} {}",
                        left, infix, right
                    )))
                }
            },
            Object::String(left) => match self.eval_expr(right.clone())? {
                Object::String(right) => self.eval_infix_string_expr(left, infix, right),
                _ => {
                    return Some(Object::Error(format!(
                        "type mismatch: {} {} {}",
                        left, infix, right
                    )))
                }
            },
            _ => Some(Object::Error(format!(
                "unknown operator: {} {} {}",
                left, infix, right
            ))),
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
            _ => Some(Object::Error(format!(
                "unknown operator: {} {} {}",
                left, infix, right
            ))),
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
            _ => Some(Object::Error(format!(
                "unknown operator: {} {} {}",
                left, infix, right
            ))),
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
    use std::{cell::RefCell, rc::Rc, vec};

    use crate::{
        lexer::Lexer,
        parser::{
            ast::{Ident, Stmt},
            Parser,
        },
    };

    use super::{env::Env, object::Object, Evaluator};

    fn eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Env::new()));
        let mut evaluator = Evaluator::new(env);
        evaluator.eval(program)
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true;",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "5 + true; 5;",
                Some(Object::Error(String::from("type mismatch: 5 + true"))),
            ),
            (
                "-true;",
                Some(Object::Error(String::from("unknown operator: -true"))),
            ),
            (
                "true + false;",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "5; \"hello\" + false; 5",
                Some(Object::Error(String::from("type mismatch: hello + false"))),
            ),
            (
                "5; true + false; 5",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "if (10 > 1) { true + false; }",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                Some(Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "foobar",
                Some(Object::Error(String::from("identifier not found: foobar"))),
            ),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
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

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Some(Object::Int(5))),
            ("let a = 5*5; a;", Some(Object::Int(25))),
            ("let a = 5; let b = a; b;", Some(Object::Int(5))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Some(Object::Int(15)),
            ),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_function_object() {
        let tests = vec![(
            "fn(x) {x+2;};",
            Some(Object::Function {
                parameters: vec![Ident(String::from("x"))],
                body: vec![Stmt::ExprStmt(crate::parser::ast::Expr::Infix(
                    Box::new(crate::parser::ast::Expr::Ident(Ident(String::from("x")))),
                    crate::parser::ast::Infix::Plus,
                    Box::new(crate::parser::ast::Expr::Literal(
                        crate::parser::ast::Literal::Int(2),
                    )),
                ))],
                env: Rc::new(RefCell::new(Env::new())),
            }),
        )];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) {x;}; identity(5);",
                Some(Object::Int(5)),
            ),
            (
                "let identity = fn(x) {return x;}; identity(5);",
                Some(Object::Int(5)),
            ),
            (
                "let double = fn(x) {x*2}; double(5);",
                Some(Object::Int(10)),
            ),
            (
                "let add = fn(x, y) {x+y}; add(5, 5);",
                Some(Object::Int(10)),
            ),
            (
                "let add = fn(x, y) {x+y}; add(5 + 5, add(5,5));",
                Some(Object::Int(20)),
            ),
            ("fn(x) {x;}(5)", Some(Object::Int(5))),
            // #TODO
            // (
            //     "let newAdder = fn(x) {
            //         fn(y) { x + y };
            //     };
            //     let addTwo = newAdder(2);
            //     addTwo(2);",
            //     Some(Object::Int(4)),
            // ),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
