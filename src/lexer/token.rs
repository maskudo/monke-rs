#![allow(dead_code)]
#[derive(PartialEq, Clone, Debug)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    // Identifiers + literals
    IDENT,
    INT,
    // Operators
    ASSIGN,
    PLUS,
    // Delimiters
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    // Keywords
    FUNCTION,
    LET,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Tokens {
    pub tokens: Vec<Token>,
}

impl Tokens {
    pub fn new(_program: String) -> Self {
        todo!()
    }
}
