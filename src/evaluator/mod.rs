use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::parser::ast::{self, Expr, Ident, Infix, Literal, Program, Stmt};

use self::env::Env;
use self::object::Object;

pub mod builtin;
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
                env: Rc::clone(&self.env),
            }),
            Expr::Call {
                function,
                arguments,
            } => Some(self.eval_call_expr(function, arguments)),
            Expr::Index(left, index) => {
                let left = self.eval_expr(*left)?;
                let index = self.eval_expr(*index)?;
                Some(self.eval_index_expr(left, index))
            } // _ => Some(Object::Error(String::from("not implemented"))),
        }
    }

    fn eval_index_expr(&mut self, left: Object, index: Object) -> Object {
        match left {
            Object::Array(ref array) => {
                if let Object::Int(i) = index {
                    self.eval_array_index_expr(array.clone(), i)
                } else {
                    Object::Error(format!("index is not an integer, got:{}", index))
                }
            }
            Object::Hash(ref hash) => match index {
                Object::Int(_) | Object::Bool(_) | Object::String(_) => match hash.get(&index) {
                    Some(o) => o.clone(),
                    None => Object::Null,
                },
                Object::Error(_) => index,
                _ => Object::Error(format!("unhashable key: {}", index)),
            },
            _ => Object::Error(format!("unknown operator on {}: {}", left, index)),
        }
    }

    fn eval_array_index_expr(&mut self, array: Vec<Object>, index: i64) -> Object {
        match array.get(index as usize) {
            Some(o) => o.clone(),
            None => Object::Error(format!("index out of range: {}", index)),
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
            Some(Object::Builtin(no_of_params, f)) => {
                if no_of_params == args.len() as u8 {
                    return f(args);
                } else {
                    return Object::Error(format!(
                        "wrong number of arguments, expected {}, got {}",
                        no_of_params,
                        args.len()
                    ));
                }
            }
            Some(obj) => return Object::Error(format!("{} is not a valid function", obj)),
            None => return Object::Null,
        };

        if params.len() != args.len() {
            return Object::Error(format!(
                "wrong number of arguments: expected {} but {} given",
                params.len(),
                args.len()
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
            Literal::Array(objects) => Some(self.eval_array_literal(objects)),
            Literal::Hash(pairs) => Some(self.eval_hash_literal(pairs)),
            // _ => Some(Object::Error(format!("not implemented"))),
        }
    }

    fn eval_array_literal(&mut self, objects: Vec<Expr>) -> Object {
        Object::Array(
            objects
                .iter()
                .map(|obj| self.eval_expr(obj.clone()).unwrap_or(Object::Null))
                .collect::<Vec<_>>(),
        )
    }

    fn eval_hash_literal(&mut self, pairs: Vec<(Expr, Expr)>) -> Object {
        let mut hash = HashMap::new();

        for (key, val) in pairs {
            let key = self.eval_expr(key).unwrap_or(Object::Null);
            if let Object::Error(err) = &key {
                return Object::Error(err.to_owned());
            }
            let val = self.eval_expr(val).unwrap_or(Object::Null);
            if let Object::Error(err) = &val {
                return Object::Error(err.to_owned());
            }
            hash.insert(key, val);
        }
        Object::Hash(hash)
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, collections::HashMap, rc::Rc, vec};

    use crate::{
        lexer::Lexer,
        parser::{
            ast::{Ident, Stmt},
            Parser,
        },
    };

    use super::{builtin::new_builtins, env::Env, object::Object, Evaluator};

    fn eval(input: &str) -> Option<Object> {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        // let env = Rc::new(RefCell::new(Env::new()));
        let env = Rc::new(RefCell::new(Env::from(new_builtins())));
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
                env: Rc::new(RefCell::new(Env::from(new_builtins()))),
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
            (
                "let newAdder = fn(x) {
                    fn(y) { x + y };
                };
                let addTwo = newAdder(2);
                addTwo(2);",
                Some(Object::Int(4)),
            ),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_built_in_function() {
        let tests = vec![
            ("len(\"\")", Some(Object::Int(0))),
            ("len(\"four\")", Some(Object::Int(4))),
            ("len(\"hello world\")", Some(Object::Int(11))),
            (
                "len(1)",
                Some(Object::Error(String::from(
                    "argument to `len` must be array, got 1",
                ))),
            ),
            (
                "len(\"one\", \"two\")",
                Some(Object::Error(String::from(
                    "wrong number of arguments, expected 1, got 2",
                ))),
            ),
            ("first([1,2,3])", Some(Object::Int(1))),
            ("first([])", Some(Object::Null)),
            (
                "first(\"string\")",
                Some(Object::Error(format!(
                    "argument to `first` must be array, got string"
                ))),
            ),
            (
                "first(1)",
                Some(Object::Error(format!(
                    "argument to `first` must be array, got 1"
                ))),
            ),
            ("last([1,2,3])", Some(Object::Int(3))),
            ("last([])", Some(Object::Null)),
            (
                "last(\"string\")",
                Some(Object::Error(format!(
                    "argument to `last` must be array, got string"
                ))),
            ),
            (
                "last(1)",
                Some(Object::Error(format!(
                    "argument to `last` must be array, got 1"
                ))),
            ),
            (
                "rest([1,2,3])",
                Some(Object::Array(vec![Object::Int(2), Object::Int(3)])),
            ),
            ("rest([5])", Some(Object::Array(vec![]))),
            ("rest([])", Some(Object::Null)),
            (
                "rest(\"string\")",
                Some(Object::Error(format!(
                    "argument to `rest` must be array, got string"
                ))),
            ),
            (
                "rest(1)",
                Some(Object::Error(format!(
                    "argument to `rest` must be array, got 1"
                ))),
            ),
            (
                "push([1,2,3], 4)",
                Some(Object::Array(vec![
                    Object::Int(1),
                    Object::Int(2),
                    Object::Int(3),
                    Object::Int(4),
                ])),
            ),
            ("push([],1)", Some(Object::Array(vec![Object::Int(1)]))),
            (
                "push(\"string\", 1)",
                Some(Object::Error(format!(
                    "argument to `push` must be array, got string"
                ))),
            ),
            (
                "push(1,1)",
                Some(Object::Error(format!(
                    "argument to `push` must be array, got 1"
                ))),
            ),
            //TODO
            // (
            //     "map([1,2,3], fn(x){return x+1;});",
            //     Some(Object::Array(vec![
            //         Object::Int(2),
            //         Object::Int(3),
            //         Object::Int(4),
            //     ])),
            // ),
        ];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_array_literal() {
        let tests = vec![(
            "[1, 2 * 2, 3 + 3]",
            Some(Object::Array(vec![
                Object::Int(1),
                Object::Int(4),
                Object::Int(6),
            ])),
        )];
        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_array_index_expr() {
        let tests = vec![
            ("[1, 2, 3][0]", Some(Object::Int(1))),
            ("[1, 2, 3][1]", Some(Object::Int(2))),
            ("let i = 0; [1][i]", Some(Object::Int(1))),
            ("[1, 2, 3][1 + 1];", Some(Object::Int(3))),
            ("let myArray = [1, 2, 3]; myArray[2];", Some(Object::Int(3))),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(Object::Int(6)),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Some(Object::Int(2)),
            ),
            (
                "[1, 2, 3][3]",
                Some(Object::Error(format!("index out of range: 3"))),
            ),
            (
                "[1, 2, 3][-1]",
                Some(Object::Error(format!("index out of range: -1"))),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_hash_literal() {
        let input = r#"
let two = "two";
{
  "one": 10 - 9,
  two: 1 + 1,
  "thr" + "ee": 6 / 2,
  4: 4,
  true: 5,
  false: 6
}
"#;

        let mut hash = HashMap::new();
        hash.insert(Object::String(String::from("one")), Object::Int(1));
        hash.insert(Object::String(String::from("two")), Object::Int(2));
        hash.insert(Object::String(String::from("three")), Object::Int(3));
        hash.insert(Object::Int(4), Object::Int(4));
        hash.insert(Object::Bool(true), Object::Int(5));
        hash.insert(Object::Bool(false), Object::Int(6));

        assert_eq!(Some(Object::Hash(hash)), eval(input),);
    }

    #[test]
    fn test_hash_index_expr() {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"]", Some(Object::Int(5))),
            ("{\"foo\": 5}[\"bar\"]", Some(Object::Null)),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Some(Object::Int(5))),
            ("{}[\"foo\"]", Some(Object::Null)),
            ("{5: 5}[5]", Some(Object::Int(5))),
            ("{true: 5}[true]", Some(Object::Int(5))),
            ("{false: 5}[false]", Some(Object::Int(5))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }
}
