#![allow(dead_code)]
mod token;
use token::TokenType::*;
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

    pub fn new_token(token_type: TokenType, ch: u8) -> Token {
        Token {
            token_type,
            literal: String::from(ch as char),
        }
    }

    pub fn next_token(&mut self) -> Token {
        let token: Token = match self.ch {
            b'=' => Lexer::new_token(ASSIGN, b'='),
            b'+' => Lexer::new_token(PLUS, b'+'),
            b'-' => Lexer::new_token(MINUS, b'-'),
            b'{' => Lexer::new_token(LBRACE, b'{'),
            b'}' => Lexer::new_token(RBRACE, b'}'),
            _ => Lexer::new_token(EOF, b'\0'),
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
        let tokens = vec![
            Token {
                token_type: PLUS,
                literal: String::from("+"),
            },
            Token {
                token_type: MINUS,
                literal: String::from("-"),
            },
            Token {
                token_type: LBRACE,
                literal: String::from("{"),
            },
            Token {
                token_type: RBRACE,
                literal: String::from("}"),
            },
            Token {
                token_type: ASSIGN,
                literal: String::from("="),
            },
            Token {
                token_type: EOF,
                literal: String::from("\0"),
            },
        ];

        let mut tokenized_input = Lexer::new(input);
        for token in tokens {
            let t = tokenized_input.next_token();
            assert_eq!(token, t);
        }
        // assert_eq!(tokens, tokenized_input);
    }
}
