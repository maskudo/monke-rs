#![allow(dead_code, unused_variables)]
pub mod ast;
use self::ast::{Ident, Literal, Program, Stmt};

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

pub type ParseErrors = Vec<ParseError>;

#[derive(Clone, Debug)]
struct Parser {
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

    fn errors(&self) -> ParseErrors {
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

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.cur_token {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => None,
        }
    }
    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        self.next_token();
        let mut name: String = String::from("");
        if let Token::Ident(ident) = self.cur_token.clone() {
            name = ident;
        }

        if !self.expect_peek(Token::Assign) {
            return None;
        }
        let mut value: i64 = -1;
        self.next_token();
        if let Token::IntLiteral(val) = self.cur_token {
            value = val;
        }

        while !self.cur_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Stmt::Let(
            Ident(name),
            ast::Expr::Literal(Literal::Int(value)),
        ))
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.next_token();
        let mut value: i64 = 0;
        if let Token::IntLiteral(val) = self.cur_token {
            value = val;
        }

        while !self.cur_token_is(Token::SemiColon) {
            self.next_token();
        }

        Some(Stmt::Return(ast::Expr::Literal(Literal::Int(value))))
    }

    fn parse_expr_stmt(&self) -> Option<Stmt> {
        unimplemented!();
    }

    fn parse_program(&mut self) -> Program {
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
        ast::{Expr, Ident, Program, Stmt},
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
        println!("{:?}", program);
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
