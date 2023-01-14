#![allow(dead_code)]
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

