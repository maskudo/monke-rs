#![allow(dead_code)]
mod token;

use token::Token;
use token::Token::*;
use token::LOOKUP_IDENT;

pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            pos: 0,
            read_pos: 0, //point to the next character in the input
            ch: 0,
        };
        l.read_char();
        l
    }

    pub fn read_char(&mut self) {
        //check for eof
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn is_letter(ch: u8) -> bool {
        b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z' || ch == b'_'
    }

    fn is_digit(ch: u8) -> bool {
        b'0' <= ch && ch <= b'9'
    }

    fn lookup_ident(ident: String) -> Token {
        match LOOKUP_IDENT.get(ident.as_str()) {
            Some(token) => token.clone(),
            None => Ident(ident),
        }
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.pos;
        //should allow numbers in identifier name as long as the first character is a letter
        while Lexer::is_letter(self.ch) || Lexer::is_digit(self.ch) {
            self.read_char();
        }
        //not sure about this #UNSURE
        String::from_utf8_lossy(&self.input.as_bytes()[pos..self.pos]).to_string()
    }

    fn read_number(&mut self) -> Result<i64, std::num::ParseIntError> {
        let pos = self.pos;
        while Lexer::is_digit(self.ch) {
            self.read_char();
        }
        String::from_utf8_lossy(&self.input.as_bytes()[pos..self.pos])
            .to_string()
            .parse::<i64>()
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token: Token = match self.ch {
            b'=' => Assign,
            b'+' => Plus,
            b'-' => Minus,
            b'{' => LBrace,
            b'}' => RBrace,
            b'!' => Not,
            b'/' => Divide,
            b'*' => Multiply,
            b',' => Comma,
            b'>' => GreaterThan,
            b'<' => LessThan,
            b';' => SemiColon,
            b':' => Colon,
            b'(' => LParen,
            b')' => RParen,
            b'[' => LBracket,
            b']' => RBracket,
            _ => {
                if Lexer::is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token = Lexer::lookup_ident(literal);
                    token
                } else if Lexer::is_digit(self.ch) {
                    //TODO: need to handle this better
                    let literal = self.read_number().expect("Error parsing number");
                    IntLiteral(literal)
                } else {
                    Illegal
                }
            }
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use super::*;
    #[test]
    fn test_next_token() {
        let input = String::from("+-{}=");
        let tokens = vec![Plus, Minus, LBrace, RBrace, Assign];

        let mut tokenized_input = Lexer::new(input);
        for token in tokens {
            let t = tokenized_input.next_token();
            assert_eq!(token, t);
        }
        // assert_eq!(tokens, tokenized_input);
    }

    #[test]
    fn test_next_token_2() {
        let input = String::from(
            "let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);",
        );

        let tokens = vec![
            Let,
            Ident(String::from("five")),
            Assign,
            IntLiteral(5),
            SemiColon,
            Let,
            Ident(String::from("ten")),
            Assign,
            IntLiteral(10),
            SemiColon,
            Let,
            Ident(String::from("add")),
            Assign,
            Function,
            LParen,
            Ident(String::from("x")),
            Comma,
            Ident(String::from("y")),
            RParen,
            LBrace,
            Ident(String::from("x")),
            Plus,
            Ident(String::from("x")),
            SemiColon,
            RBrace,
            SemiColon,
            Let,
            Ident(String::from("result")),
            Assign,
            Ident(String::from("add")),
            LParen,
            Ident(String::from("five")),
            Comma,
            Ident(String::from("ten")),
            RParen,
            SemiColon,
            EOF,
        ];
        let mut tokenized_input = Lexer::new(input);
        for token in tokens {
            let t = tokenized_input.next_token();
            assert_eq!(token, t);
        }
    }
}
