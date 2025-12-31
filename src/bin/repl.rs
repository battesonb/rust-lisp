use rustyline::error::ReadlineError;

use rust_lisp::{interpreter::Interpreter, lexer::Lexer, parser::Parser};

fn main() -> rustyline::Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;
    let _ = rl.load_history("./rustlisp_history.txt");

    let mut interpreter = Interpreter::new();
    interpreter.load_std();

    loop {
        let line = match rl.readline("> ") {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Bye!");
                let _ = rl.save_history("./rustlisp_history.txt");
                break;
            }
            Err(e) => {
                println!("Unexpected error: {e}");
                break;
            }
        };

        if line == ".exit" {
            println!("Bye!");
            let _ = rl.save_history("./rustlisp_history.txt");
            break;
        }

        let _ = rl.add_history_entry(&line);
        let lexer = Lexer::new(&line);
        let tokens = lexer.lex();
        let parser = Parser::new(tokens);
        let statements = match parser.parse() {
            Ok(s) => s,
            Err(e) => {
                println!("{}", e.to_string());
                continue;
            }
        };
        for value in statements {
            match interpreter.evaluate(value) {
                Ok(result) => {
                    println!("{result}");
                }
                Err(err) => {
                    println!("ERROR: {err}");
                }
            }
        }
    }

    Ok(())
}
