#![allow(dead_code)]
mod token;
use token::Token::*;
use token::*;

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

    pub fn next_token(&mut self) -> Token {
        let token: Token = match self.ch {
            b'=' => Assign,
            b'+' => Plus,
            b'-' => Minus,
            b'{' => LBrace,
            b'}' => RBrace,
            _ => Token::EOF,
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod test {
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
}
