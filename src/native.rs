use std::{borrow::Cow, cell::RefCell, collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::{
    interpreter::Interpreter,
    values::{
        ConsCell, FunctionValue, HashTableValue, MacroValue, NumberValue, Scope, Symbol, Value,
        ValueExpectError, ValueExpectResult,
    },
};

#[derive(Clone, Debug, Error)]
pub enum NativeError {
    #[error(transparent)]
    ValueExpectError(#[from] ValueExpectError),
    #[error("Expected number, received: {0}")]
    NumberExpected(Value),
    #[error("Unresolved function: {0}")]
    UnresolvedFunction(Symbol),
    #[error("Unresolved macro: {0}")]
    UnresolvedMacro(Symbol),
    #[error("Undefined function: {0}")]
    UndefinedVariable(Symbol),
    #[error("Invalid function or macro expression, received: {0}")]
    InvalidFunctionExpression(Value),
    #[error("Invalid argument count for {name}, expected {expected}")]
    InvalidExactArgumentCount {
        name: Cow<'static, str>,
        expected: usize,
    },
    #[error("Mismatched parameter ({params}) and argument ({arguments}) count for {name}")]
    MismatchedArgumentAndParameterCount {
        name: Cow<'static, str>,
        arguments: usize,
        params: usize,
    },
    #[error("{0}")]
    Generic(Cow<'static, str>),
}

pub type NativeResult<T> = Result<T, NativeError>;

/// Signature:
///
/// (+ (&rest operands))
///
/// add always returns the value of 0 (the identity) multiplied by all operands.
pub fn add(interpreter: &mut Interpreter, mut rest: Value) -> NativeResult<Value> {
    let mut total = NumberValue::default();

    loop {
        if rest.is_nil() {
            break;
        }

        let ConsCell { value, rest: next } = rest.expect_cons_cell()?;

        let result = interpreter.evaluate(*value)?;
        let result = match result {
            Value::Number(value) => value,
            result => {
                return Err(NativeError::NumberExpected(result));
            }
        };
        total = total.add(&result);

        rest = *next;
    }

    Ok(Value::Number(total))
}

/// Signature:
///
/// (* (&rest operands))
///
/// Mul always returns the value of 1 (the identity) multiplied by all operands.
pub fn mul(interpreter: &mut Interpreter, mut rest: Value) -> NativeResult<Value> {
    let mut total = NumberValue::Integer(1);

    loop {
        if rest.is_nil() {
            break;
        }

        let ConsCell { value, rest: next } = rest.expect_cons_cell()?;

        let result = interpreter.evaluate(*value)?;
        let result = match result {
            Value::Number(value) => value,
            result => Err(NativeError::NumberExpected(result))?,
        };

        total = total.mul(&result);

        rest = *next;
    }

    Ok(Value::Number(total))
}

/// Signature:
///
/// (/ (base &rest operands))
///
/// Sub functions as a unary negation with one parameter. If there are any other operands, it will
/// instead perform consecutive subtraction on the first parameter.
pub fn sub(interpreter: &mut Interpreter, mut rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest: next } = rest.expect_cons_cell()?;

    let value = interpreter.evaluate(*value)?;

    let mut total = match value {
        Value::Number(value) => value,
        result => Err(NativeError::NumberExpected(result))?,
    };

    // Single argument is a unary negation operation.
    if next.is_nil() {
        return Ok(Value::Number(total.neg()));
    }

    rest = *next;
    loop {
        if rest.is_nil() {
            break;
        };

        let ConsCell { value, rest: next } = rest.expect_cons_cell()?;

        let result = interpreter.evaluate(*value)?;
        let result = match result {
            Value::Number(value) => value,
            result => Err(NativeError::NumberExpected(result))?,
        };

        total = total.sub(&result);

        rest = *next;
    }

    Ok(Value::Number(total))
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
pub fn div(interpreter: &mut Interpreter, mut rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest: next } = rest.expect_cons_cell()?;

    let value = interpreter.evaluate(*value)?;

    let mut total = match value {
        Value::Number(value) => value,
        result => Err(NativeError::NumberExpected(result))?,
    };

    // Single argument is a unary negation operation.
    if next.is_nil() {
        return Ok(Value::Number(NumberValue::Float(1.0).div(&total)));
    }

    rest = *next;
    loop {
        if rest.is_nil() {
            break;
        }

        let ConsCell { value, rest: next } = rest.expect_cons_cell()?;

        let result = interpreter.evaluate(*value)?;
        let result = match result {
            Value::Number(value) => value,
            result => return Err(NativeError::NumberExpected(result))?,
        };

        total = total.div(&result);

        rest = *next;
    }

    Ok(Value::Number(total))
}

// Example:
//
// (error (list wow we really don't support strings, huh?))
pub fn error(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "error".into(),
            expected: 1,
        });
    }

    let value = interpreter.evaluate(*value)?;

    Err(NativeError::Generic(Cow::Owned(value.to_string())))
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
pub fn lambda(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    let params = if value.is_nil() {
        None
    } else {
        Some(value.expect_cons_cell()?)
    };
    let body = rest.expect_cons_cell()?;

    fn params_to_symbols(params: ConsCell) -> ValueExpectResult<Vec<Symbol>> {
        params
            .into_iter()
            .map(|value| value.expect_symbol())
            .collect::<ValueExpectResult<Vec<Symbol>>>()
    }

    let mut params = params
        .map(|params| params_to_symbols(params))
        .transpose()?
        .unwrap_or_default();

    let rest_argument = {
        if let Some(rest_index) = params
            .iter()
            .enumerate()
            .find(|(_, symbol)| Symbol::new("&rest").eq(symbol))
            .map(|(i, _)| i)
        {
            if rest_index + 2 != params.len() {
                return Err(NativeError::Generic(
                    "rest symbol and argument must be the last symbols in the parameter list"
                        .into(),
                ));
            }

            let rest_argument = params.pop();
            params.pop(); // and the rest argument indicator
            rest_argument
        } else {
            None
        }
    };

    let scope = interpreter.active_scope();

    Ok(Value::Function(FunctionValue::new(
        scope,
        params,
        body,
        rest_argument,
    )))
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
pub fn macroexpand(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "macroexpand".into(),
            expected: 1,
        });
    }

    let ConsCell { value, rest } = value.expect_cons_cell()?;

    let macro_name = value.expect_symbol()?;

    let macro_value = interpreter.read_value(&macro_name);

    let Some(macro_value) = macro_value else {
        return Err(NativeError::UnresolvedMacro(macro_name));
    };

    let MacroValue { params, body } = macro_value.expect_macro()?;

    let arguments = rest.into_iter().map(|value| value).collect::<Vec<_>>();

    if arguments.len() != params.len() {
        return Err(NativeError::MismatchedArgumentAndParameterCount {
            name: "macroexpand".into(),
            params: params.len(),
            arguments: arguments.len(),
        });
    }

    let mut param_map = HashMap::new();
    for (param, argument) in params.into_iter().zip(arguments) {
        param_map.insert(param, argument);
    }

    let expanded = interpreter.expand_macro(&param_map, Value::ConsCell(body));

    println!("{expanded}");

    Ok(Value::nil())
}

/// Defines a macro, which just expands the provided symbols as-is into other values which are then
/// evaluated. See `macroexpand` to see this expansion as it is before evaluation.
pub fn defmacro(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    let symbol = value.expect_symbol()?;
    let ConsCell {
        value: params,
        rest,
    } = rest.expect_cons_cell()?;
    let ConsCell { value: body, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "defmacro".into(),
            expected: 3,
        });
    }

    let body = body.expect_cons_cell()?;

    fn params_to_symbols(params: Value) -> ValueExpectResult<Vec<Symbol>> {
        params
            .into_iter()
            .map(|value| value.expect_symbol())
            .collect::<ValueExpectResult<Vec<Symbol>>>()
    }

    let params = params_to_symbols(*params)?;
    let value = Value::Macro(MacroValue::new(params, body));

    interpreter.set_value(symbol.clone(), value)?;

    Ok(Value::Symbol(symbol))
}

/// Signature:
///
/// (apply FUNC ARGUMENT_LIST) -> VALUE
///
/// Example:
///
/// (apply (lambda (a b) (+ a b)) (list 5 2))
/// > 7
///
/// Apply is at the heart of the interpreter. It takes in a list with an expected format and
/// executes the function.
pub fn apply(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value: func, rest } = rest.expect_cons_cell()?;
    let ConsCell { value: args, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "apply".into(),
            expected: 2,
        });
    }

    let func = interpreter.evaluate(*func)?;

    let FunctionValue {
        parent_scope,
        params,
        body,
        rest_argument,
    } = func.expect_function()?;

    let args = interpreter.evaluate(*args)?;
    let args = if args.is_nil() {
        Vec::default()
    } else {
        args.expect_cons_cell()?.into_iter().collect::<Vec<_>>()
    };

    if rest_argument.is_some() && args.len() < params.len() {
        return Err(NativeError::MismatchedArgumentAndParameterCount {
            name: "apply".into(),
            params: params.len(),
            arguments: args.len(),
        });
    }

    if rest_argument.is_none() && args.len() != params.len() {
        return Err(NativeError::MismatchedArgumentAndParameterCount {
            name: "apply".into(),
            params: params.len(),
            arguments: args.len(),
        });
    }

    let scope = Rc::new(RefCell::new(Box::new(Scope::new(Some(parent_scope)))));
    {
        // TODO: Something less allocation heavy...
        let (args, rest_arg_values): (Vec<_>, Vec<_>) = args
            .into_iter()
            .enumerate()
            .partition(|(i, _)| *i < params.len());

        let mut scope = scope.borrow_mut();
        if let Some(rest_argument) = rest_argument {
            let rest_arg_list = Value::join(rest_arg_values.into_iter().map(|(_, v)| v));
            scope.insert(rest_argument, rest_arg_list);
        }

        for (key, (_, value)) in params.into_iter().zip(args) {
            scope.insert(key, value);
        }
    }
    interpreter.push_active_scope(scope);
    let result = progn(interpreter, Value::ConsCell(body));
    interpreter.pop_active_scope();

    result
}

/// Signature
/// (progn (&rest EXPRS)) -> VALUE
///
/// Evaluates each expression and returns the value returned by the last expression.
pub fn progn(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let mut result = Value::default();
    for value in Value::iter(&rest) {
        result = interpreter.evaluate(value.clone())?;
    }

    Ok(result)
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
/// it in each pair. It is a special operator in that it does not evaluate the first parameter of
/// each pair as an expression.
pub fn setq(interpreter: &mut Interpreter, mut rest: Value) -> NativeResult<Value> {
    let mut last_value = None;
    loop {
        let ConsCell { value, rest: next } = match rest {
            Value::ConsCell(cell) => cell,
            _ => break,
        };
        let symbol = value.expect_symbol()?;
        let ConsCell {
            value: next_value,
            rest: next_rest,
        } = next.expect_cons_cell()?;

        let value = interpreter.evaluate(*next_value)?;

        interpreter.set_value(symbol, value.clone())?;

        last_value = Some(value);
        rest = *next_rest;
    }

    Ok(last_value.unwrap_or_else(Value::nil))
}

/// Signature:
///
/// (eval LIST) -> VALUE
///
/// Eval evaluates a provided list as an expression. This allows data to be evaluated as code.
pub fn eval(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "eval".into(),
            expected: 1,
        });
    }

    let result = interpreter.evaluate(*value)?;
    let result = result.expect_cons_cell()?;

    interpreter.evaluate_list(result)
}

pub fn equal(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value: a, rest } = rest.expect_cons_cell()?;
    let ConsCell { value: b, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "=".into(),
            expected: 2,
        });
    }

    let a = interpreter.evaluate(*a)?;
    let b = interpreter.evaluate(*b)?;

    // TODO:
    // 1. Cast integers into floats if types are cross-compared.
    // 2. Generally improve this check. Equals on function definitions overflows the stack.
    Ok(Value::cond(a.eq(&b)))
}

pub fn less(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value: a, rest } = rest.expect_cons_cell()?;
    let ConsCell { value: b, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "<".into(),
            expected: 2,
        });
    }

    let a = interpreter.evaluate(*a)?;
    let b = interpreter.evaluate(*b)?;

    let result = match (a, b) {
        (Value::Number(a), Value::Number(b)) => a.less(&b),
        (x, _) => {
            // TODO: Correctly model the wrong type, here.
            return Err(NativeError::NumberExpected(x));
        }
    };

    Ok(Value::cond(result))
}

/// Signature:
///
/// (or (&rest exprs)) -> t | ()
///
/// Both `and` and `or` are short-circuiting native methods. It might be possible to implement
/// these without a native implementation for both, but leaving for now.
pub fn or(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let mut result = false;
    for value in rest.into_iter() {
        let value = interpreter.evaluate(value)?;
        result |= value.is_true();

        if result {
            break;
        }
    }

    Ok(Value::cond(result))
}

/// Signature:
///
/// (and (&rest exprs)) -> t | ()
///
/// Both `and` and `or` are short-circuiting native methods. It might be possible to implement
/// these without a native implementation for both, but leaving for now.
pub fn and(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let mut result = true;
    for value in rest.into_iter() {
        let value = interpreter.evaluate(value)?;
        result &= value.is_true();

        if !result {
            break;
        }
    }

    Ok(Value::cond(result))
}

/// Signature:
///
/// (if <COND_EXPR> <TRUE_EXPR> <FALSE_EXPR>) -> <TRUE_EXPR>
///
/// If is special in that it "short-circuits" the evaluation only to the branch that is followed.
pub fn if_native(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value: test, rest } = rest.expect_cons_cell()?;
    let ConsCell { value: pass, rest } = rest.expect_cons_cell()?;
    let ConsCell { value: fail, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "if".into(),
            expected: 3,
        });
    }

    let test = interpreter.evaluate(*test)?;
    let test = test.is_true();

    interpreter.evaluate(if test { *pass } else { *fail })
}

/// Signature:
///
/// (cond (&args pairs)) -> Value
///
/// If is special in that it "short-circuits" the evaluation to the first branch that evaluates to
/// a non-nil result.
pub fn cond(interpreter: &mut Interpreter, mut rest: Value) -> NativeResult<Value> {
    loop {
        if rest.is_nil() {
            break;
        }

        let ConsCell {
            value: pair,
            rest: trail,
        } = rest.expect_cons_cell()?;

        let ConsCell {
            value: test,
            rest: pair_rest,
        } = pair.expect_cons_cell()?;
        let ConsCell {
            value: action,
            rest: pair_rest,
        } = pair_rest.expect_cons_cell()?;

        if !pair_rest.is_nil() {
            return Err(NativeError::InvalidExactArgumentCount {
                name: "cond pair".into(),
                expected: 2,
            });
        }

        let test = interpreter.evaluate(*test)?;

        if test.is_true() {
            return interpreter.evaluate(*action);
        }

        rest = *trail;
    }

    Ok(Value::nil())
}

/// Signature:
///
/// (quote value) -> value
///
/// Quote is special in that does not evaluate its argument. It returns it as-is, as data.
pub fn quote(_interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "quote".into(),
            expected: 1,
        });
    }

    Ok(*value)
}

/// Signature:
///
/// (list (&rest args)) -> LIST
pub fn list(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    // TODO: Avoid this allocation
    let values = rest
        .into_iter()
        .map(|value| interpreter.evaluate(value))
        .collect::<NativeResult<Vec<_>>>()?;

    Ok(Value::join(values))
}

/// Signature:
///
/// (car <EXPR>) -> LIST
pub fn car(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "car".into(),
            expected: 1,
        });
    }

    let value = interpreter.evaluate(*value)?;
    let ConsCell { value, .. } = value.expect_cons_cell()?;

    Ok(*value)
}

/// Signature:
///
/// (cdr <EXPR>) -> LIST
pub fn cdr(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    if rest.is_nil() {
        return Ok(Value::nil());
    }

    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "cdr".into(),
            expected: 1,
        });
    }

    let value = interpreter.evaluate(*value)?;
    let ConsCell { rest, .. } = value.expect_cons_cell()?;

    Ok(*rest)
}

/// Signature:
///
/// (print <EXPR>) -> <VALUE>
pub fn print(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "print".into(),
            expected: 1,
        });
    }

    let value = interpreter.evaluate(*value)?;

    println!("{value}");

    Ok(value)
}

/// Signature:
///
/// (let (LIST &args rest)) -> VALUE
pub fn let_native(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell {
        value: mut variable_list,
        rest,
    } = rest.expect_cons_cell()?;

    let scope = Rc::new(RefCell::new(Box::new(Scope::new(Some(
        interpreter.active_scope(),
    )))));
    {
        let mut scope = scope.borrow_mut();
        loop {
            if variable_list.is_nil() {
                break;
            }

            let ConsCell { value: pair, rest } = variable_list.expect_cons_cell()?;

            let ConsCell {
                value: symbol,
                rest: pair_rest,
            } = pair.expect_cons_cell()?;
            let ConsCell {
                value,
                rest: pair_rest,
            } = pair_rest.expect_cons_cell()?;

            if !pair_rest.is_nil() {
                return Err(NativeError::InvalidExactArgumentCount {
                    name: "let pair".into(),
                    expected: 2,
                });
            }

            let symbol = symbol.expect_symbol()?;
            let value = interpreter.evaluate(*value)?;

            scope.insert(symbol, value);

            variable_list = rest
        }
    }

    interpreter.push_active_scope(scope);
    let result = progn(interpreter, *rest);
    interpreter.pop_active_scope();

    result
}

/// Signature:
///
/// (boundp (SYMBOL)) -> t | ()
pub fn boundp(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "boundp".into(),
            expected: 1,
        });
    }

    let symbol = value.expect_symbol()?;

    Ok(Value::cond(interpreter.read_value(&symbol).is_some()))
}

/// Signature:
///
/// (cons (a b)) -> CONS_CELL
pub fn cons(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value: a, rest } = rest.expect_cons_cell()?;
    let ConsCell { value: b, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "cons".into(),
            expected: 2,
        });
    }

    let a = interpreter.evaluate(*a)?;
    let b = interpreter.evaluate(*b)?;

    Ok(Value::ConsCell(ConsCell::new(a).with_rest(b)))
}

pub fn make_hash_table(_interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    rest.expect_nil()?;

    Ok(Value::HashTable(HashTableValue::default()))
}

pub fn gethash(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value: table, rest } = rest.expect_cons_cell()?;

    let table = interpreter.evaluate(*table)?;
    let table = table.expect_hash_table()?;

    let key = rest.expect_cons_cell()?;
    let key = interpreter.evaluate(*key.value)?;

    let Some(value) = table.get(&key) else {
        return Err(NativeError::Generic(
            format!("Failed to retrieve value from hash map for key {key}").into(),
        ));
    };

    Ok(value.clone())
}

pub fn sethash(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value: table, rest } = rest.expect_cons_cell()?;

    let table = interpreter.evaluate(*table)?;
    let mut table = table.expect_hash_table()?;

    let ConsCell { value: key, rest } = rest.expect_cons_cell()?;
    let key = interpreter.evaluate(*key)?;

    let ConsCell { value, rest } = rest.expect_cons_cell()?;
    let value = interpreter.evaluate(*value)?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "sethash".into(),
            expected: 3,
        });
    }

    table.insert(key, value);

    Ok(Value::Nil)
}

pub fn type_of(interpreter: &mut Interpreter, rest: Value) -> NativeResult<Value> {
    let ConsCell { value, rest } = rest.expect_cons_cell()?;

    if !rest.is_nil() {
        return Err(NativeError::InvalidExactArgumentCount {
            name: "typep".into(),
            expected: 1,
        });
    }
    let value = interpreter.evaluate(*value)?;

    Ok(Value::Symbol(value.type_of()))
}
