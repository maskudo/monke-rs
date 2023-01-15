use linefeed::{Interface, ReadResult};
use monke_rs::lexer::token::Token;
use monke_rs::lexer::Lexer;

const PROMPT: &str = ">>";
fn main() {
    let reader = Interface::new(PROMPT).unwrap();
    reader.set_prompt(format!("{}", PROMPT).as_ref()).unwrap();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input.eq("exit") {
            break;
        }
        let mut lex = Lexer::new(input);
        loop {
            let token = lex.next_token();
            if token == Token::EOF {
                break;
            }
            println!("{:?}", token);
        }
    }
}
