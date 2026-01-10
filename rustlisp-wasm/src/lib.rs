use std::sync::LazyLock;

use rustlisp::{
    interpreter::{Interpreter, InterpreterConfig},
    lexer::Lexer,
    parser::Parser,
};
use wasm_bindgen::prelude::*;

const EXAMPLES: LazyLock<Vec<(String, String)>> = LazyLock::new(|| {
    vec![
        (
            "Factorial".into(),
            r#"(defun fact (n)
  (if (<= n 1)
    1
    (* n (fact (- n 1)))))
(print (fact 5))"#
                .into(),
        ),
        (
            "Fibonacci".into(),
            r#"(defun fib (n)
  (if (<= n 1)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))
  (print (fib 7)) ; beware, a bigger number can lock up your browser"#
                .into(),
        ),
        (
            "Object".into(),
            r#"(defun make-account(balance)
  (lambda (operation &rest args)
    (cond ((= operation (quote deposit)) (setq balance (+ balance (car args))))
          ((= operation (quote withdraw)) (setq balance (- balance (car args))))
          ((= operation (quote check)) balance)
          (t (error "unexpected operation")))))

(setq account (make-account 0))

(print (account (quote deposit) 100))
(print (account (quote withdraw) 30))
(print (account (quote check)))
"#
            .into(),
        ),
        (
            "Stateful counter".into(),
            r#"(defun make-counter (acc)
  (lambda ()
    (setq acc (+ acc 1))))

(setq count (make-counter 0))

(print (count))
(print (count))
(print (count))"#
                .into(),
        ),
        (
            "Traces".into(),
            r#"; Errors show traces for context. Aliased native functions will also show their
; original name. This specific view of the interpreter shows the result of the
; last successful expression, which is the `(setq multiply *)` expression.

(setq multiply *)
(setq x
  (+ (multiply 10 (error "boom!"))))"#
                .into(),
        ),
        ("Empty".into(), "".into()),
    ]
});

#[wasm_bindgen(start)]
fn main() -> Result<(), JsValue> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");

    let select = document
        .get_element_by_id("select")
        .expect("Expected a select element")
        .dyn_into::<web_sys::HtmlSelectElement>()?;
    let input = document
        .get_element_by_id("input")
        .expect("Expected an input element")
        .dyn_into::<web_sys::HtmlTextAreaElement>()?;
    let submit = document
        .get_element_by_id("submit")
        .expect("Expected a submit element");
    let expression = document
        .get_element_by_id("expression")
        .expect("Expected an expression element")
        .dyn_into::<web_sys::HtmlElement>()?;
    let error = document
        .get_element_by_id("error")
        .expect("Expected an error element")
        .dyn_into::<web_sys::HtmlElement>()?;
    let output = document
        .get_element_by_id("output")
        .expect("Expected an output element")
        .dyn_into::<web_sys::HtmlElement>()?;

    for (key, value) in EXAMPLES.iter() {
        let option = document
            .create_element("option")?
            .dyn_into::<web_sys::HtmlOptionElement>()?;
        option.set_label(key);
        option.set_value(value);
        select.append_child(&option)?;
    }

    let select_option = {
        let input = input.clone();
        let select = select.clone();
        move || {
            let index = select
                .options()
                .selected_index()
                .expect("Expected selected index") as u32;
            let option = select
                .options()
                .get_with_index(index)
                .expect("Expected option for selected index")
                .dyn_into::<web_sys::HtmlOptionElement>()
                .unwrap();
            input.set_value(&option.value());
        }
    };

    select_option();

    let select_option_callback = Closure::<dyn FnMut(_)>::new(move |_: web_sys::Event| {
        select_option();
    });

    select.add_event_listener_with_callback(
        "change",
        select_option_callback.as_ref().unchecked_ref(),
    )?;
    select_option_callback.forget();

    let append_error = {
        let error = error.clone();
        move |text: String| {
            if let Some(original) = error.text_content()
                && !original.is_empty()
            {
                error.set_text_content(Some(&format!("{}{}", original, text)));
            } else {
                error.set_text_content(Some(&text));
            }
        }
    };

    let mut interpreter = Interpreter::new(InterpreterConfig {
        stdout: {
            let output = output.clone();
            Box::new(move |str: String| {
                if let Some(original) = output.text_content()
                    && !original.is_empty()
                {
                    output.set_text_content(Some(&format!("{}{}", original, str)));
                } else {
                    output.set_text_content(Some(&str));
                }
            })
        },
        ..Default::default()
    });
    interpreter.load_std();

    let submit_callback = Closure::<dyn FnMut(_)>::new(move |_: web_sys::MouseEvent| {
        // Clear previous outputs
        error.set_text_content(None);
        output.set_text_content(None);
        expression.set_text_content(None);

        let text = input.value();
        let lexer = Lexer::new(&text);
        let tokens = match lexer.lex() {
            Ok(s) => s,
            Err(e) => {
                append_error(e.to_string());
                return;
            }
        };
        let parser = Parser::new(tokens);
        let statements = match parser.parse() {
            Ok(s) => s,
            Err(e) => {
                append_error(e.to_string());
                return;
            }
        };
        for value in statements {
            match interpreter.evaluate(value) {
                Ok(result) => {
                    expression.set_text_content(Some(&result.to_string()));
                }
                Err(e) => {
                    append_error(e.to_string());
                }
            }
        }
    });
    submit.add_event_listener_with_callback("click", submit_callback.as_ref().unchecked_ref())?;
    submit_callback.forget();

    Ok(())
}
