use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{FunctionValue, Link, List, MacroValue, Scope, Symbol, Value},
    interpreter::Interpreter,
};

/// Signature:
///
/// (+ (&rest operands))
///
/// add always returns the value of 0 (the identity) multiplied by all operands.
pub fn add(interpreter: &mut Interpreter, mut link: Option<Box<Link>>) -> Value {
    let mut total = 0;

    loop {
        let Some(current) = link else {
            break;
        };

        let result = interpreter.evaluate(current.value);
        let result = match result {
            Value::Integer(value) => value,
            _ => {
                return Value::Error(format!("Expected integer, received: {result}"));
            }
        };

        total += result;

        link = current.next;
    }

    return Value::Integer(total);
}

/// Signature:
///
/// (* (&rest operands))
///
/// Mul always returns the value of 1 (the identity) multiplied by all operands.
pub fn mul(interpreter: &mut Interpreter, mut link: Option<Box<Link>>) -> Value {
    let mut total = 1;

    loop {
        let Some(current) = link else {
            break;
        };

        let result = interpreter.evaluate(current.value);
        let result = match result {
            Value::Integer(value) => value,
            _ => {
                return Value::Error(format!("Expected integer, received: {result}"));
            }
        };

        total *= result;

        link = current.next;
    }

    return Value::Integer(total);
}

/// Signature:
///
/// (/ (base &rest operands))
///
/// Sub functions as a unary negation with one parameter. If there are any other operands, it will
/// instead perform consecutive subtraction on the first parameter.
pub fn sub(interpreter: &mut Interpreter, mut link: Option<Box<Link>>) -> Value {
    let Some(first) = link else {
        return Value::Error("- expects at least 1 argument".into());
    };

    let value = interpreter.evaluate(first.value);
    let mut total = match value {
        Value::Integer(value) => value,
        _ => {
            return Value::Error(format!("Expected integer, received: {}", value));
        }
    };

    // Single argument is a unary negation operation.
    if first.next.is_none() {
        return Value::Integer(-total);
    }

    link = first.next;

    loop {
        let Some(current) = link else {
            break;
        };

        let result = interpreter.evaluate(current.value);
        let result = match result {
            Value::Integer(value) => value,
            _ => {
                return Value::Error(format!("Expected integer, received: {result}"));
            }
        };

        total -= result;

        link = current.next;
    }

    return Value::Integer(total);
}

/// Signature:
///
/// (/ (base &rest operands))
///
/// Example:
///
/// (/ 5 2)
///
/// Div functions as a reciprocal with one parameter. If there are any other operands, it will
/// instead perform consecutive division on the first parameter.
pub fn div(interpreter: &mut Interpreter, mut link: Option<Box<Link>>) -> Value {
    let Some(first) = link else {
        return Value::Error("/ expects at least 1 argument".into());
    };

    let value = interpreter.evaluate(first.value);
    let mut total: f64 = match value {
        Value::Integer(value) => value as f64,
        Value::Float(value) => value as f64,
        _ => {
            return Value::Error(format!("Expected number, received: {}", value));
        }
    };

    // Single argument is a unary division operation.
    if first.next.is_none() {
        return Value::Float(1.0 / total);
    }

    link = first.next;

    loop {
        let Some(current) = link else {
            break;
        };

        let result = interpreter.evaluate(current.value);
        let result = match result {
            Value::Integer(value) => value as f64,
            Value::Float(value) => value as f64,
            _ => {
                return Value::Error(format!("Expected number, received: {result}"));
            }
        };

        total /= result;

        link = current.next;
    }

    return Value::Float(total);
}

// Example:
//
// (error (wow we really don't support strings, huh?))
pub fn error(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("error expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("error expects 1 argument".into());
    }

    let value = interpreter.evaluate(link.value);

    Value::Error(value.to_string())
}

// Signature:
//
// (lambda (PARAM_LIST &rest body)) -> FUNCTION
//
// Example:
//
// (lambda (a b) (+ a b))
// > <FUNCTION>
//
// Lambda is special in a few ways, firstly it returns a special (opaque) function value. Secondly,
// its first parameter, the parameter list, is never evaluated as code. It is treated as a list,
// always. The body is only evaluated when called, otherwise it is treated as a quoted list.
pub fn lambda(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("missing parameter list".into());
    };

    let params = match link.value {
        Value::List(list) => list,
        _ => {
            return Value::Error("missing parameter list".into());
        }
    };

    fn params_to_symbols(params: Box<List>) -> Result<Vec<Symbol>, Value> {
        params
            .into_iter()
            .map(|value| match value {
                Value::Symbol(symbol) => Ok(symbol),
                _ => Err(Value::Error(
                    "parameter list may only contain symbols".into(),
                )),
            })
            .collect::<Result<Vec<Symbol>, Value>>()
    }

    let params = match params_to_symbols(params) {
        Ok(p) => p,
        Err(err) => return err,
    };

    let Some(body) = link.next else {
        return Value::Error("lambda is missing body".into());
    };

    let scope = interpreter.active_scope();
    Value::Function(FunctionValue::new(scope, params, Box::new(List::new(body))))
}

/// Signature:
///
/// (macroexpand LIST) -> NIL
///
/// Example:
///
/// (macroexpand
///   (defun x ()
///     (print amazing)))
///
/// Expands a given macro into the form(s) that it would produce before evaluation. No need to
/// quote, as the input is treated as quoted by default.
pub fn macroexpand(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("macroexpand expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("macroexpand expects 1 argument".into());
    }

    let link = match link.value {
        Value::List(list) => list.head,
        _ => {
            return Value::Error(format!(
                "macro expects argument to be a list, got: {}",
                link.value
            ));
        }
    };

    let Some(link) = link else {
        return Value::Error("missing macro name in parameter list".into());
    };

    let macro_name = match link.value {
        Value::Symbol(ref symbol) => symbol,
        _ => {
            return Value::Error(format!(
                "macro name in parameter list must be a symbol, got: {}",
                link.value
            ));
        }
    };

    let macro_value = interpreter.read_value(&macro_name);

    let Some(macro_value) = macro_value else {
        return Value::Error(format!("no macro named '{}' found", macro_name));
    };

    let MacroValue { params, body } = match macro_value {
        Value::Macro(macro_value) => macro_value,
        _ => {
            return Value::Error(format!("macro expected, got: {}", link.value));
        }
    };

    let arguments = if let Some(next) = link.next {
        next.into_iter().map(|value| value).collect::<Vec<_>>()
    } else {
        Vec::default()
    };

    if arguments.len() != params.len() {
        return Value::Error(format!(
            "macro param count {} does not match argument count {}",
            params.len(),
            arguments.len()
        ));
    }

    let mut param_map = HashMap::new();
    for (param, argument) in params.into_iter().zip(arguments) {
        param_map.insert(param, argument);
    }

    let expanded = interpreter.expand_macro(&param_map, Value::List(body));

    println!("{expanded}");

    Value::default()
}

/// Defines a macro, which just expands the provided symbols as-is into other values which are then
/// evaluated. See `macroexpand` to see this expansion as it is before evaluation.
pub fn defmacro(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("defmacro expects 2 arguments".into());
    };

    let symbol = match link.value {
        Value::Symbol(symbol) => symbol,
        _ => {
            return Value::Error(format!(
                "defmacro expects first argument to be a symbol, got: {}",
                link.value
            ));
        }
    };

    let Some(link) = link.next else {
        return Value::Error("missing parameter list".into());
    };

    let params = match link.value {
        Value::List(list) => list,
        _ => {
            return Value::Error("missing parameter list".into());
        }
    };

    fn params_to_symbols(params: Box<List>) -> Result<Vec<Symbol>, Value> {
        params
            .into_iter()
            .map(|value| match value {
                Value::Symbol(symbol) => Ok(symbol),
                _ => Err(Value::Error(
                    "parameter list may only contain symbols".into(),
                )),
            })
            .collect::<Result<Vec<Symbol>, Value>>()
    }

    let params = match params_to_symbols(params) {
        Ok(p) => p,
        Err(err) => return err,
    };

    let Some(body) = link.next else {
        return Value::Error("missing body".into());
    };

    if body.next.is_some() {
        return Value::Error("trailing arguments after body found".into());
    }

    let value = Value::Macro(MacroValue::new(params, Box::new(List::new(body))));

    interpreter.set_value(symbol.clone(), value);

    Value::Symbol(symbol)
}

/// Signature:
///
/// (apply FUNC ARGUMENT_LIST) -> VALUE
///
/// Example:
///
/// (apply (lambda (a b) (+ a b)) (5 2))
/// > 7
///
/// Apply is at the heart of the interpreter. It takes in a list with an expected format and
/// executes the function.
pub fn apply(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        // TODO: Support providing a symbol which represents the native function as input
        return Value::Error("missing function reference".into());
    };

    let value = interpreter.evaluate(link.value);

    let FunctionValue {
        parent_scope,
        params,
        body,
    } = match value {
        Value::Function(function) => function,
        _ => {
            return Value::Error(format!("missing function reference, got: {}", value));
        }
    };

    let Some(arguments) = link.next else {
        return Value::Error("apply is missing list arguments".into());
    };

    let arguments = interpreter.evaluate(arguments.value);
    let arguments = match arguments {
        Value::List(list) => list,
        _ => {
            return Value::Error(format!(
                "apply is missing list arguments, got: {}",
                arguments
            ));
        }
    };

    let arguments = arguments.into_iter().collect::<Vec<_>>();
    if arguments.len() != params.len() {
        return Value::Error(format!(
            "function param count {} does not match argument count {}",
            params.len(),
            arguments.len()
        ));
    }

    let scope = Rc::new(RefCell::new(Box::new(Scope::new(Some(parent_scope)))));
    {
        let mut scope = scope.borrow_mut();
        for (key, value) in params.into_iter().zip(arguments) {
            scope.insert(key, value);
        }
    }
    interpreter.push_active_scope(scope);
    let result = progn(interpreter, body.head);
    interpreter.pop_active_scope();

    result
}

/// Signature
/// (progn (&rest EXPRS)) -> VALUE
///
/// Evaluates each expression and returns the value returned by the last expression.
pub fn progn(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::default();
    };

    let mut result = Value::default();
    for value in Link::iter(&link) {
        result = interpreter.evaluate(value.clone());
    }

    result
}

/// Signature:
///
/// (setq (&rest pairs)) -> VALUE
///
/// Example
///
/// (setq a 25 b 20)
/// > 20
///
/// Setq (or "set quoted") sets the result of the expression for each pair to the symbol preceding
/// it. in each pair. It is a special operator in that it does not evaluate the first parameter of
/// each pair as an expression.
pub fn setq(interpreter: &mut Interpreter, mut link: Option<Box<Link>>) -> Value {
    let mut last_value = None;
    loop {
        let Some(current_link) = link else {
            break;
        };

        let symbol = match current_link.value {
            Value::Symbol(symbol) => symbol,
            _ => {
                return Value::Error(format!(
                    "setq expects first argument of pair to be a symbol, got: {}",
                    current_link.value
                ));
            }
        };

        let Some(current_link) = current_link.next else {
            return Value::Error("setq expects pairs".into());
        };

        let value = interpreter.evaluate(current_link.value);

        interpreter.set_value(symbol, value.clone());

        last_value = Some(value);
        link = current_link.next;
    }

    last_value.unwrap_or_else(Value::default)
}

/// Signature:
///
/// (eval LIST) -> VALUE
///
/// Eval evaluates a provided list as an expression. This allows data to be evaluated as code.
pub fn eval(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("eval expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("eval expects 1 argument".into());
    }

    let value = interpreter.evaluate(link.value);

    let list = match value {
        Value::List(list) => list,
        _ => {
            return Value::Error(format!("eval expects argument to be a list, got: {value}"));
        }
    };

    interpreter.evaluate_list(list)
}

pub fn equal(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("equal expects 2 arguments".into());
    };

    let a = interpreter.evaluate(link.value);

    let Some(link) = link.next else {
        return Value::Error("equal expects 2 arguments".into());
    };

    let b = interpreter.evaluate(link.value);

    // TODO:
    // 1. Cast integers into floats if types are cross-compared.
    // 2. Generally improve this check. Equals on function definitions overflows the stack.
    Value::cond(a.eq(&b))
}

pub fn less(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("equal expects 2 arguments".into());
    };

    let a = interpreter.evaluate(link.value);

    let Some(link) = link.next else {
        return Value::Error("equal expects 2 arguments".into());
    };

    let b = interpreter.evaluate(link.value);

    let result = match (a, b) {
        (Value::Integer(a), Value::Integer(b)) => a < b,
        (Value::Float(a), Value::Float(b)) => a < b,
        (Value::Integer(a), Value::Float(b)) => (a as f64) < b,
        (Value::Float(a), Value::Integer(b)) => a < (b as f64),
        _ => {
            return Value::Error("Can't compare these two values".into());
        }
    };

    Value::cond(result)
}

/// Signature:
///
/// (or (&rest exprs)) -> t | ()
///
/// Both `and` and `or` are also short-circuiting native methods. It might be possible to implement
/// these without a native implementation for both, but leaving for now.
pub fn or(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::t();
    };

    let mut result = false;
    for value in link.into_iter() {
        let value = interpreter.evaluate(value);
        result |= value.is_true();

        if result {
            break;
        }
    }

    Value::cond(result)
}

/// Signature:
///
/// (and (&rest exprs)) -> t | ()
///
/// Both `and` and `or` are also short-circuiting native methods. It might be possible to implement
/// these without a native implementation for both, but leaving for now.
pub fn and(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::t();
    };

    let mut result = true;
    for value in link.into_iter() {
        let value = interpreter.evaluate(value);
        result &= value.is_true();

        if !result {
            break;
        }
    }

    Value::cond(result)
}

/// Signature:
///
/// (if <COND_EXPR> <TRUE_EXPR> <FALSE_EXPR>) -> <TRUE_EXPR>
///
/// If is special in that it "short-circuits" the evaluation only to the branch that is followed.
pub fn if_native(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("if expects 3 arguments".into());
    };

    let cond = interpreter.evaluate(link.value);

    // Errors and empty list are false, everything else is true.
    let cond = match cond {
        Value::List(list) => list.head.is_some(),
        Value::Error(_) => false,
        _ => true,
    };

    let Some(pass) = link.next else {
        return Value::Error("if expects 3 arguments".into());
    };

    let Some(fail) = pass.next else {
        return Value::Error("if expects 3 arguments".into());
    };

    interpreter.evaluate(if cond { pass.value } else { fail.value })
}

/// Signature:
///
/// (quote (value)) -> value
///
/// Quote is special in that does not evaluate its argument. It returns it as-is, as data.
pub fn quote(_interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("quote expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("quote expects 1 argument".into());
    }

    link.value
}

/// Signature:
///
/// (cdr <EXPR>) -> LIST
pub fn list(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::default();
    };

    let values = link.into_iter().map(|value| interpreter.evaluate(value));

    let Some(link) = Value::join(values) else {
        return Value::default();
    };

    Value::list(List::new(link))
}

/// Signature:
///
/// (cdr <EXPR>) -> LIST
pub fn car(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("car expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("car expects 1 argument".into());
    }

    let list = match link.value {
        Value::List(list) => list,
        _ => return Value::Error("car expects a list argument".into()),
    };

    let value = interpreter.evaluate_list(list);

    let list = match value {
        Value::List(list) => list,
        _ => return Value::Error("car expects a list argument".into()),
    };

    let Some(head) = list.head else {
        return Value::List(Box::new(List::default()));
    };

    head.value
}

/// Signature
///
/// (cdr <EXPR>) -> <VALUE>
pub fn cdr(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("cdr expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("cdr expects 1 argument".into());
    }

    let value = interpreter.evaluate(link.value);
    let list = match value {
        Value::List(list) => list,
        _ => return Value::Error("cdr expects a list argument".into()),
    };

    let Some(head) = list.head else {
        return Value::List(Box::new(List::default()));
    };

    let Some(rest) = head.next else {
        return Value::List(Box::new(List::default()));
    };

    Value::list(List::new(rest))
}

/// Signature:
///
/// (print <EXPR>) -> <VALUE>
pub fn print(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("message expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("message expects 1 argument".into());
    }

    let value = interpreter.evaluate(link.value);

    println!("{value}");

    value
}

pub fn let_native(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("let expects 2 arguments".into());
    };

    let variable_list = match link.value {
        Value::List(list) => list,
        _ => {
            return Value::Error("first argument of let 2 must be a list".into());
        }
    };

    let rest = link.next;

    let scope = Rc::new(RefCell::new(Box::new(Scope::new(Some(
        interpreter.active_scope(),
    )))));
    {
        let mut scope = scope.borrow_mut();
        let mut link = variable_list.head;
        loop {
            let Some(current_link) = link else {
                break;
            };

            let pair = match current_link.value {
                Value::List(list) => list.head,
                _ => {
                    return Value::Error(format!(
                        "let expects variables specified as a list of lists pairs, got: {}",
                        current_link.value
                    ));
                }
            };

            let Some(pair) = pair else {
                return Value::Error(
                    "let encountered empty list for variable specification".into(),
                );
            };

            let symbol = match pair.value {
                Value::Symbol(symbol) => symbol,
                _ => {
                    return Value::Error(format!(
                        "let expects first argument of pair to be a symbol, got: {}",
                        pair.value
                    ));
                }
            };

            let Some(pair) = pair.next else {
                return Value::Error("let expects pairs".into());
            };

            let value = interpreter.evaluate(pair.value);

            scope.insert(symbol, value);

            link = current_link.next;
        }
    }

    interpreter.push_active_scope(scope);
    let result = progn(interpreter, rest);
    interpreter.pop_active_scope();

    result
}

/// Signature:
///
/// (boundp (SYMBOL)) -> t | ()
pub fn boundp(interpreter: &mut Interpreter, link: Option<Box<Link>>) -> Value {
    let Some(link) = link else {
        return Value::Error("boundp expects 1 argument".into());
    };

    if link.next.is_some() {
        return Value::Error("boundp expects 1 argument".into());
    }

    let symbol = match link.value {
        Value::Symbol(symbol) => symbol,
        _ => {
            return Value::Error(format!(
                "boundp expects argument to be a symbol, got: {}",
                link.value
            ));
        }
    };

    Value::cond(interpreter.read_value(&symbol).is_some())
}
