use crate::lex::LexError;
use crate::lex::Token;

use super::lex;
use super::parse;
use std::fs;
use std::io;
use std::io::Write;

pub struct Lox {
    had_error: bool,
}

impl Lox {
    pub fn new() -> Self {
        Self { had_error: false }
    }

    pub fn run_file(&mut self, filepath: &str) {
        let contents = fs::read_to_string(filepath);
        let _ = self.run(&contents.unwrap());
        if self.had_error {
            panic!("Failed to run file: {}", filepath);
        }
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!("> ");
            io::stdout().flush().unwrap();

            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            let _ = self.run(input.as_str());
            if self.had_error {
                self.had_error = false;
            }
        }
    }

    fn run(&mut self, source: &str) -> Result<(), LexError> {
        let mut lexer = lex::Lexer::new(source.as_bytes())?;

        println!("Tokens:");
        loop {
            let tok = lexer.next_token();
            if let Ok(token) = tok {
                println!("{:?}", token);
                if token.tok == Token::EOF {
                    return Ok(());
                }
            } else {
                self.error(&lexer.token_pos(), "Error!");
            }
        }
    }

    fn error(&mut self, location: &lex::TokenLocation, msg: &str) {
        if !self.had_error {
            self.had_error = true;
            self.report_error(location, msg);
        }
    }

    fn report_error(&self, location: &lex::TokenLocation, msg: &str) {
        println!("{}", msg);
        println!("At line [{}], column [{}].", location.line, location.col);
    }
}
