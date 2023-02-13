#![allow(dead_code, unused_variables)]
pub mod ast;
use self::ast::{Expr, Ident, Infix, Literal, Precedence, Program, Stmt};
use core::fmt;
use std::fmt::Display;

use super::lexer::token::Token;
use super::lexer::Lexer;

#[derive(Debug, Clone, Copy)]
pub enum ParseErrorKind {
    UnexpectedToken,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    message: String,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind, message: String) -> Self {
        ParseError { kind, message }
    }
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseErrorKind::UnexpectedToken => write!(f, "Unexpected Token"),
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)
    }
}

pub type ParseErrors = Vec<ParseError>;

#[derive(Clone, Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: ParseErrors,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Parser {
            lexer,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        p
    }

    pub fn errors(&self) -> ParseErrors {
        self.errors.clone()
    }

    fn peek_error(&mut self, t: Token) {
        let msg = format!(
            "expected next token to be {:?} but got {:?} instead",
            t, self.peek_token
        );
        self.errors
            .push(ParseError::new(ParseErrorKind::UnexpectedToken, msg));
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn cur_token_is(&self, t: Token) -> bool {
        self.cur_token == t
    }

    fn peek_token_is(&self, t: Token) -> bool {
        self.peek_token == t
    }
    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token_is(token.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(token.clone());
            false
        }
    }

    fn peek_precedence(&self) -> Precedence {
        Parser::get_precedence(&self.peek_token)
    }
    fn get_precedence(token: &Token) -> Precedence {
        match token {
            Token::Equal | Token::NotEqual => Precedence::EQUALS,
            Token::LessThan
            | Token::GreaterThan
            | Token::LessThanEqual
            | Token::GreaterThanEqual => Precedence::LESSGREATER,
            Token::Plus | Token::Minus => Precedence::SUM,
            Token::Divide | Token::Multiply => Precedence::PRODUCT,
            Token::Not => Precedence::PREFIX,
            Token::LParen => Precedence::CALL,
            _ => Precedence::LOWEST,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        Parser::get_precedence(&self.cur_token)
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.cur_token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Stmt> {
        match self.parse_expression(Precedence::LOWEST) {
            Some(expr) => {
                if self.peek_token_is(Token::SemiColon) {
                    self.next_token()
                }
                Some(Stmt::ExprStmt(expr))
            }
            None => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut left = match self.cur_token {
            Token::Ident(_) => self.parse_ident_expr(),
            Token::IntLiteral(_) => self.parse_int_literal_expr(),
            Token::BoolLiteral(_) => self.parse_boolean(),
            Token::StringLiteral(_) => self.parse_string_literal_expr(),
            Token::Plus | Token::Minus | Token::Not => self.parse_prefix_expr(),
            Token::LParen => self.parse_grouped_expr(),
            Token::If => self.parse_if_expr(),
            Token::Function => self.parse_function_expr(),
            _ => None,
        };

        //parsing Infix
        while !self.peek_token_is(Token::SemiColon) && precedence < self.peek_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Multiply
                | Token::Divide
                | Token::Equal
                | Token::NotEqual
                | Token::LessThan
                | Token::LessThanEqual
                | Token::GreaterThan
                | Token::GreaterThanEqual => {
                    self.next_token();
                    left = self.parse_infix_expression(left.unwrap());
                }
                Token::LParen => {
                    self.next_token();
                    left = self.parse_call_expr(left.unwrap());
                }
                _ => return left,
            }
        }

        left
    }

    fn parse_infix_expression(&mut self, left: Expr) -> Option<Expr> {
        let infix = match self.cur_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Divide => Infix::Divide,
            Token::Multiply => Infix::Multiply,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            Token::LessThan => Infix::LessThan,
            Token::LessThanEqual => Infix::LessEqual,
            Token::GreaterThan => Infix::GreaterThan,
            Token::GreaterThanEqual => Infix::GreaterEqual,
            _ => return None,
        };

        let precedence = self.cur_precedence();
        self.next_token();

        self.parse_expression(precedence)
            .and_then(|expr| Some(Expr::Infix(Box::new(left), infix, Box::new(expr))))
    }

    fn parse_function_expr(&mut self) -> Option<Expr> {
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        let parameters = self.parse_function_params()?;
        // let parameters = match self.parse_function_params() {
        //     Some(params) => params,
        //     None => return None,
        // };

        if !self.expect_peek(Token::LBrace) {
            return None;
        }

        let body = self.parse_block_stmt();
        Some(Expr::Function { parameters, body })
    }

    fn parse_function_params(&mut self) -> Option<Vec<Ident>> {
        let mut identifiers: Vec<Ident> = vec![];
        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Some(identifiers);
        }
        self.next_token();

        match self.parse_ident() {
            Some(ident) => identifiers.push(ident),
            None => return None,
        }

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_ident() {
                Some(ident) => identifiers.push(ident),
                None => return None,
            };
        }

        if !self.expect_peek(Token::RParen) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_expr(&mut self, function: Expr) -> Option<Expr> {
        // let arguments = match self.parse_call_args() {
        //     Some(args) => args,
        //     None => return None,
        // };
        let arguments = self.parse_call_args()?;
        Some(Expr::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_call_args(&mut self) -> Option<Vec<Expr>> {
        let mut args = vec![];

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        match self.parse_expression(Precedence::LOWEST) {
            Some(expr) => args.push(expr),
            None => return None,
        }

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            match self.parse_expression(Precedence::LOWEST) {
                Some(expr) => args.push(expr),
                None => return None,
            };
        }

        if !self.expect_peek(Token::RParen) {
            return None;
        }
        Some(args)
    }

    fn parse_ident(&mut self) -> Option<Ident> {
        match self.cur_token {
            Token::Ident(ref mut ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        let consequence = self.parse_block_stmt();
        let mut alternative = None;

        if self.peek_token_is(Token::Else) {
            self.next_token();
            if !self.expect_peek(Token::LBrace) {
                return None;
            }
            alternative = Some(self.parse_block_stmt());
        }
        Some(Expr::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_block_stmt(&mut self) -> Vec<Stmt> {
        let mut statements: Vec<Stmt> = vec![];
        self.next_token();

        while !self.cur_token_is(Token::RBrace) && !self.cur_token_is(Token::EOF) {
            match self.parse_statement() {
                Some(stmt) => statements.push(stmt),
                None => {}
            }
            self.next_token();
        }
        statements
    }

    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.next_token();
        let expr = self.parse_expression(Precedence::LOWEST);
        if !self.expect_peek(Token::RParen) {
            None
        } else {
            expr
        }
    }
    fn parse_ident_expr(&mut self) -> Option<Expr> {
        match self.cur_token {
            Token::Ident(ref mut ident) => Some(Expr::Ident(Ident(ident.clone()))),
            _ => None,
        }
    }

    fn parse_boolean(&mut self) -> Option<Expr> {
        match self.cur_token {
            Token::BoolLiteral(bool) => Some(Expr::Literal(Literal::Bool(bool))),
            _ => None,
        }
    }

    fn parse_string_literal_expr(&mut self) -> Option<Expr> {
        match self.cur_token {
            Token::StringLiteral(ref mut s) => Some(Expr::Literal(Literal::String(s.clone()))),
            _ => None,
        }
    }

    fn parse_int_literal_expr(&mut self) -> Option<Expr> {
        match self.cur_token {
            Token::IntLiteral(int) => Some(Expr::Literal(Literal::Int(int))),
            _ => None,
        }
    }

    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = match self.cur_token {
            Token::Not => ast::Prefix::Not,
            Token::Minus => ast::Prefix::Minus,
            Token::Plus => ast::Prefix::Plus,
            _ => return None,
        };
        self.next_token();
        // match self.parse_expression(Precedence::PREFIX) {
        //     Some(expr) => Some(Expr::Prefix(prefix, Box::new(expr))),
        //     None => None,
        // }
        self.parse_expression(Precedence::PREFIX)
            .and_then(|expr| Some(Expr::Prefix(prefix, Box::new(expr))))
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        match &self.peek_token {
            Token::Ident(_) => self.next_token(),
            _ => return None,
        };

        // let name = match self.parse_ident() {
        //     Some(name) => name,
        //     None => return None,
        // };
        let name = self.parse_ident()?;

        if !self.expect_peek(Token::Assign) {
            return None;
        }
        self.next_token();

        let expr = self.parse_expression(Precedence::LOWEST)?;
        // let expr = match self.parse_expression(Precedence::LOWEST) {
        //     Some(expr) => expr,
        //     None => return None,
        // };

        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }
        Some(Stmt::Let(name, expr))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token();
        let value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(Token::SemiColon) {
            self.next_token();
        }

        value.and_then(|val| Some(Stmt::Return(val)))
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::EOF {
            match self.parse_statement() {
                Some(stmt) => program.statements.push(stmt),
                None => {}
            }
            self.next_token();
        }
        program
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use crate::lexer::Lexer;

    use super::{
        ast::{Expr, Ident, Infix, Literal, Prefix, Program, Stmt},
        Parser,
    };

    #[test]
    fn test_let_statements() {
        let input = String::from(
            "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ",
        );

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        if program.statements.len() != 3 {
            panic!(
                "program doesnt contain 3 statements, it contains {:?}",
                program.statements.len()
            );
        }
        let output = Program {
            statements: vec![
                Stmt::Let(
                    Ident(String::from("x")),
                    Expr::Literal(crate::parser::ast::Literal::Int(5)),
                ),
                Stmt::Let(
                    Ident(String::from("y")),
                    Expr::Literal(crate::parser::ast::Literal::Int(10)),
                ),
                Stmt::Let(
                    Ident(String::from("foobar")),
                    Expr::Literal(crate::parser::ast::Literal::Int(838383)),
                ),
            ],
        };
        assert_eq!(output, program);
    }

    #[test]
    fn test_return_statements() {
        let input = String::from(
            "
            return 5;
            return 10;
            return 993322;
        ",
        );

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        if program.statements.len() != 3 {
            panic!(
                "program doesnt contain 3 statements, it contains {:?}",
                program.statements.len()
            );
        }
        let output = Program {
            statements: vec![
                Stmt::Return(Expr::Literal(crate::parser::ast::Literal::Int(5))),
                Stmt::Return(Expr::Literal(crate::parser::ast::Literal::Int(10))),
                Stmt::Return(Expr::Literal(crate::parser::ast::Literal::Int(993322))),
            ],
        };
        assert_eq!(output, program);
        println!("{:?}", program);
    }

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Stmt::Let(
                    Ident(String::from("myVar")),
                    Expr::Literal(crate::parser::ast::Literal::Int(5)),
                ),
                Stmt::Let(
                    Ident(String::from("myVar")),
                    Expr::Literal(crate::parser::ast::Literal::Int(10)),
                ),
            ],
        };
        assert_eq!(
            "let myVar = 5;
let myVar = 10;
",
            program.to_string()
        );
    }

    #[test]
    fn test_bool_expression() {
        let input = String::from("true;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!("not enough statements, got {:?}", program.statements.len())
        }
        let output = Stmt::ExprStmt(Expr::Literal(Literal::Bool(true)));
        assert_eq!(program.statements[0], output);
    }
    #[test]

    fn test_identifier_expression() {
        let input = String::from("foobar;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!("not enough statements, got {:?}", program.statements.len())
        }
        let output = Stmt::ExprStmt(Expr::Ident(Ident(String::from("foobar"))));
        assert_eq!(program.statements[0], output);
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5;");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);

        if program.statements.len() != 1 {
            panic!("not enough statements, got {:?}", program.statements.len())
        }
        let output = Stmt::ExprStmt(Expr::Literal(crate::parser::ast::Literal::Int(5)));
        assert_eq!(program.statements[0], output);
    }

    #[test]
    fn test_prefix_expr() {
        let inputs = vec![
            (
                String::from("!5;"),
                Stmt::ExprStmt(Expr::Prefix(
                    Prefix::Not,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("-15;"),
                Stmt::ExprStmt(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Literal(Literal::Int(15))),
                )),
            ),
            (
                String::from("+15;"),
                Stmt::ExprStmt(Expr::Prefix(
                    Prefix::Plus,
                    Box::new(Expr::Literal(Literal::Int(15))),
                )),
            ),
            (
                String::from("!true"),
                Stmt::ExprStmt(Expr::Prefix(
                    Prefix::Not,
                    Box::new(Expr::Literal(Literal::Bool(true))),
                )),
            ),
            (
                String::from("!false"),
                Stmt::ExprStmt(Expr::Prefix(
                    Prefix::Not,
                    Box::new(Expr::Literal(Literal::Bool(false))),
                )),
            ),
        ];
        for (input, expected) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements[0], expected);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs = vec![
            (
                String::from("3 < 5 == true;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Infix::LessThan,
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Infix::Equal,
                    Box::new(Expr::Literal(Literal::Bool(true))),
                )),
            ),
            (
                String::from("a + add(b * c) + d"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Infix(
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Infix::Plus,
                        Box::new(Expr::Call {
                            function: Box::new(Expr::Ident(Ident(String::from("add")))),
                            arguments: vec![Expr::Infix(
                                Box::new(Expr::Ident(Ident(String::from("b")))),
                                Infix::Multiply,
                                Box::new(Expr::Ident(Ident(String::from("c")))),
                            )],
                        }),
                    )),
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident(String::from("d")))),
                )),
            ),
            (
                String::from("1 + (2 + 3) + 4;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Infix(
                        Box::new(Expr::Literal(Literal::Int(1))),
                        Infix::Plus,
                        Box::new(Expr::Infix(
                            Box::new(Expr::Literal(Literal::Int(2))),
                            Infix::Plus,
                            Box::new(Expr::Literal(Literal::Int(3))),
                        )),
                    )),
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(4))),
                )),
            ),
        ];
        for (input, expected) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);
            assert_eq!(program.statements[0], expected);
        }
    }

    #[test]
    fn test_infix_expr() {
        let inputs = vec![
            (
                String::from("5 + 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 - 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Minus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 * 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Multiply,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 / 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Divide,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 > 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::GreaterThan,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 < 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::LessThan,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 != 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::NotEqual,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 == 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::Equal,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 >= 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::GreaterEqual,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                String::from("5 <= 5;"),
                Stmt::ExprStmt(Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Infix::LessEqual,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
        ];
        for (input, expected) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            assert_eq!(program.statements[0], expected);
        }
    }

    #[test]
    fn test_if_expr() {
        let input = String::from("if (x < y) { x }");
        let output = Stmt::ExprStmt(Expr::If {
            condition: Box::new(Expr::Infix(
                Box::new(Expr::Ident(Ident(String::from("x")))),
                Infix::LessThan,
                Box::new(Expr::Ident(Ident(String::from("y")))),
            )),
            consequence: vec![Stmt::ExprStmt(Expr::Ident(Ident(String::from("x"))))],
            alternative: None,
        });
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(output, program.statements[0]);
    }

    #[test]
    fn test_if_else_expr() {
        let input = String::from("if (x < y) { x } else { y }");
        let output = Stmt::ExprStmt(Expr::If {
            condition: Box::new(Expr::Infix(
                Box::new(Expr::Ident(Ident(String::from("x")))),
                Infix::LessThan,
                Box::new(Expr::Ident(Ident(String::from("y")))),
            )),
            consequence: vec![Stmt::ExprStmt(Expr::Ident(Ident(String::from("x"))))],
            alternative: Some(vec![Stmt::ExprStmt(Expr::Ident(Ident(String::from("y"))))]),
        });
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(output, program.statements[0]);
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = String::from("fn(x,y) {{ x + y; }}");
        let output = Stmt::ExprStmt(Expr::Function {
            parameters: vec![Ident(String::from("x")), Ident(String::from("y"))],
            body: vec![Stmt::ExprStmt(Expr::Infix(
                Box::new(Expr::Ident(Ident(String::from("x")))),
                Infix::Plus,
                Box::new(Expr::Ident(Ident(String::from("y")))),
            ))],
        });
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(output, program.statements[0]);
    }

    #[test]
    fn test_function_parameters_parsing() {
        let inputs = vec![
            (
                String::from("fn() {{}};"),
                Stmt::ExprStmt(Expr::Function {
                    parameters: vec![],
                    body: vec![],
                }),
            ),
            (
                String::from("fn(x) {{}};"),
                Stmt::ExprStmt(Expr::Function {
                    parameters: vec![Ident(String::from("x"))],
                    body: vec![],
                }),
            ),
            (
                String::from("fn(x,y,z) {};"),
                Stmt::ExprStmt(Expr::Function {
                    parameters: vec![
                        Ident(String::from("x")),
                        Ident(String::from("y")),
                        Ident(String::from("z")),
                    ],
                    body: vec![],
                }),
            ),
        ];
        for (input, expected) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);

            let program = parser.parse_program();
            check_parser_errors(&parser);

            dbg!(&program.statements, &expected);
            assert_eq!(program.statements[0], expected);
        }
    }

    #[test]
    fn test_call_expr_parsing() {
        let input = String::from("add(1, 2 * 3, 4 + 5);");
        let output = Stmt::ExprStmt(Expr::Call {
            function: Box::new(Expr::Ident(Ident(String::from("add")))),
            arguments: vec![
                Expr::Literal(Literal::Int(1)),
                Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(2))),
                    Infix::Multiply,
                    Box::new(Expr::Literal(Literal::Int(3))),
                ),
                Expr::Infix(
                    Box::new(Expr::Literal(Literal::Int(4))),
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                ),
            ],
        });
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(&parser);
        assert_eq!(output, program.statements[0]);
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        if errors.len() == 0 {
            return;
        }
        eprintln!("parser has {} errors", errors.len());
        for error in errors.into_iter() {
            eprintln!("{:?}", error);
        }
        panic!("Parser Error!");
    }
}
