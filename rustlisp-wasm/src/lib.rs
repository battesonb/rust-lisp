use rustlisp::{
    interpreter::{Interpreter, InterpreterConfig},
    lexer::Lexer,
    parser::Parser,
};
use wasm_bindgen::prelude::*;

const EXAMPLE: &'static str = r#"(defun fact (n)
  (if (<= n 1)
    1
    (* n (fact (- n 1)))))
(print (fact 5))"#;

#[wasm_bindgen(start)]
fn main() -> Result<(), JsValue> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");

    let input = document
        .get_element_by_id("input")
        .expect("Expected an input")
        .dyn_into::<web_sys::HtmlTextAreaElement>()
        .expect("Input must be a text area");
    let submit = document
        .get_element_by_id("submit")
        .expect("Expected an submit");
    let output = document
        .get_element_by_id("output")
        .expect("Expected an output")
        .dyn_into::<web_sys::HtmlElement>()
        .expect("Output must be an HTML element");

    input.set_text_content(EXAMPLE.into());

    let set_output = {
        let output = output.clone();
        move |text: String, err: bool| {
            output.set_text_content(Some(&text));
            output
                .style()
                .set_property("color", if err { "white" } else { "" })
                .expect("Failed to set output CSS color");
            output
                .style()
                .set_property("background-color", if err { "#f04f43" } else { "" })
                .expect("Failed to set output CSS background-color");
        }
    };

    let mut interpreter = Interpreter::new(InterpreterConfig {
        stdout: {
            let output = output.clone();
            Box::new(move |str: String| {
                if let Some(original) = output.text_content()
                    && !original.is_empty()
                {
                    output.set_text_content(Some(&format!("{}\n{}", original, str)));
                } else {
                    output.set_text_content(Some(&str));
                }
            })
        },
        ..Default::default()
    });
    interpreter.load_std();

    let submit_callback = Closure::<dyn FnMut(_)>::new(move |_: web_sys::MouseEvent| {
        let text = input.value();
        let lexer = Lexer::new(&text);
        let tokens = match lexer.lex() {
            Ok(s) => s,
            Err(e) => {
                set_output(e.to_string(), true);
                return;
            }
        };
        let parser = Parser::new(tokens);
        let statements = match parser.parse() {
            Ok(s) => s,
            Err(e) => {
                set_output(e.to_string(), true);
                return;
            }
        };
        for value in statements {
            match interpreter.evaluate(value) {
                Ok(result) => {
                    set_output(result.to_string(), false);
                }
                Err(e) => {
                    set_output(e.to_string(), true);
                }
            }
        }
    });
    submit.add_event_listener_with_callback("click", submit_callback.as_ref().unchecked_ref())?;
    submit_callback.forget();

    Ok(())
}
