use linefeed::{Interface, ReadResult};
use monke_rs::lexer::token::Token;
use monke_rs::lexer::Lexer;
use monke_rs::parser::Parser;

const PROMPT: &str = ">>";
fn main() {
    let reader = Interface::new(PROMPT).unwrap();
    reader.set_prompt(format!("{}", PROMPT).as_ref()).unwrap();

    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input.eq("exit") {
            break;
        }
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors().len() != 0 {
            for error in parser.errors() {
                eprintln!("{}", error);
            }
        } else {
            println!("{program}");
        }
    }
}
