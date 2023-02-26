use linefeed::{Interface, ReadResult};
use monke_rs::evaluator::env::Env;
use monke_rs::evaluator::Evaluator;
use monke_rs::lexer::Lexer;
use monke_rs::parser::Parser;
use std::cell::RefCell;
use std::rc::Rc;

const PROMPT: &str = ">>";
fn main() {
    let reader = Interface::new(PROMPT).unwrap();
    reader.set_prompt(format!("{}", PROMPT).as_ref()).unwrap();

    let env = Rc::new(RefCell::new(Env::new()));
    let mut evaluator = Evaluator::new(env);
    while let ReadResult::Input(input) = reader.read_line().unwrap() {
        if input.eq("exit") {
            break;
        }
        let lexer = Lexer::new(input.clone());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors().len() != 0 {
            for error in parser.errors() {
                eprintln!("{}", error);
            }
        }
        let evaluated = evaluator.eval(program);
        match evaluated {
            Some(obj) => println!("{obj}"),
            None => {}
        }
    }
}
