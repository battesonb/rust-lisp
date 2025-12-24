use std::io::BufRead;

use rust_lisp::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

fn main() {
    let stdin = std::io::stdin().lock();
    let lines = stdin
        .lines()
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
        .join("\n");
    let lexer = Lexer::new(&lines);
    let tokens = lexer.lex();
    let parser = Parser::new(tokens);
    let statements = match parser.parse() {
        Ok(s) => s,
        Err(e) => {
            println!("{}", e.to_string());
            std::process::exit(1);
        }
    };
    let mut interpreter = Interpreter::new();
    interpreter.load_std();

    for value in statements {
        let _ = interpreter.evaluate(value);
    }
}
