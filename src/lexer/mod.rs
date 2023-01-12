mod token;

pub struct Lexer {
    input: String,
    pos: usize,
    readPos: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            input,
            pos: 0,
            readPos: 0, //point to the next character in the input
            ch: 0,
        }
    }

    // pub fn readChar(&self) {
    //     if self.readPos >= self.input.len() {
    //         self.ch = 0;
    //     } else {
    //         self.ch = self.input.get(self.readPos) as u8;
    //     }
    //     self.pos = self.readPos;
    //     self.readPos += 1;
    // }
}

#[cfg(test)]
mod test {
    use super::*;
    use token::TokenType::*;
    use token::*;

    #[test]
    fn test_next_token() {
        let input = String::from("+-{}=");
        let tokens = Tokens {
            tokens: vec![
                Token {
                    token_type: PLUS,
                    literal: String::from("+"),
                },
                Token {
                    token_type: PLUS,
                    literal: String::from("+"),
                },
                Token {
                    token_type: PLUS,
                    literal: String::from("-"),
                },
                Token {
                    token_type: PLUS,
                    literal: String::from("{"),
                },
                Token {
                    token_type: PLUS,
                    literal: String::from("}"),
                },
                Token {
                    token_type: PLUS,
                    literal: String::from("="),
                },
            ],
        };

        let tokenized_input = Tokens::new(input);
        assert_eq!(tokens, tokenized_input);
    }
}
