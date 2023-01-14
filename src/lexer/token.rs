#![allow(dead_code)]

use std::collections::HashMap;
use Token::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Illegal,
    EOF,
    // identifier and literals
    Ident(String),
    StringLiteral(String),
    IntLiteral(i64),
    BoolLiteral(bool),
    // statements
    Assign,
    If,
    Else,
    // operators
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
    Not,
    // reserved words
    Function,
    Let,
    Return,
    // punctuations
    Comma,
    Colon,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
}

lazy_static! {
    pub static ref LOOKUP_IDENT: HashMap<&'static str, Token> = {
        let mut m = HashMap::new();
        m.insert("fn", Function);
        m.insert("let", Let);
        m.insert("true", BoolLiteral(true));
        m.insert("false", BoolLiteral(false));
        m.insert("if", If);
        m.insert("else", Else);
        m.insert("return", Return);
        m
    };
}
