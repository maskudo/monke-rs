use linefeed::{Interface, ReadResult};
use monke_rs::evaluator::env::Env;
use monke_rs::evaluator::Evaluator;
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
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors().len() != 0 {
            for error in parser.errors() {
                eprintln!("{}", error);
            }
        }
        let env = Env::new();
        let mut evaluator = Evaluator::new(env);
        let evaluated = evaluator.eval(program);
        match evaluated {
            Some(obj) => println!("{obj}"),
            None => {}
        }
    }
}
