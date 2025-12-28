mod lox;
mod lex;
mod parse;

use lox::Lox;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut interpreter = Lox::new();

    if args.len() > 1 {
        let filepath = &args[1];
        interpreter.run_file(filepath);
    } else {
        interpreter.run_prompt();
    }
}
