/// It can perform actions with all native functions
use assert_cmd::cargo::*;

#[test]
fn add_can_add_two_values() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (+ 5 2))
    "#,
    );
    cmd.assert().success().stdout("7\n");
}

#[test]
fn sub_can_negate_a_value() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (- 10))
    "#,
    );
    cmd.assert().success().stdout("-10\n");
}

#[test]
fn sub_can_subtract_two_values() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (- 10 8))
    "#,
    );
    cmd.assert().success().stdout("2\n");
}

#[test]
fn mul_can_multiply_two_values() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (* 5 2))
    "#,
    );
    cmd.assert().success().stdout("10\n");
}

#[test]
fn mul_can_multiply_one_value() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (* 5))
    "#,
    );
    cmd.assert().success().stdout("5\n");
}

#[test]
fn list_can_make_a_list() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (list 10 20 30))
    "#,
    );
    cmd.assert().success().stdout("(10 20 30)\n");
}

#[test]
fn progn_can_evaluate_multiple_commands() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (progn (setq x 5) (print x))
    "#,
    );
    cmd.assert().success().stdout("5\n");
}

#[test]
fn setq_can_set_a_value() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq x 5)
    (print x)
    "#,
    );
    cmd.assert().success().stdout("5\n");
}

#[test]
fn setq_can_set_a_value_twice() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq x 5)
    (print x)
    (setq x 10)
    (print x)
    "#,
    );
    cmd.assert().success().stdout("5\n10\n");
}

#[test]
fn car_returns_head_of_list() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (car (list 10 20 30)))
    "#,
    );
    cmd.assert().success().stdout("10\n");
}

#[test]
fn cdr_returns_tail_of_list() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (cdr (list 10 20 30)))
    "#,
    );
    cmd.assert().success().stdout("(20 30)\n");
}

#[test]
fn error_constructs_a_raw_error() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (error failure))
    "#,
    );
    cmd.assert().success().stdout("<ERROR:\nfailure>\n");
}

#[test]
fn quote_prevents_evaluation_of_arguments() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (quote (lambda () (+ 2 2))))
    "#,
    );
    cmd.assert().success().stdout("(lambda () (+ 2 2))\n");
}

#[test]
fn eval_works() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (eval (quote (+ 2 2))))
    "#,
    );
    cmd.assert().success().stdout("4\n");
}

#[test]
fn less_than() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (< 5 6))
    (print (< 6 6))
    (print (< 7 6))
    "#,
    );
    cmd.assert().success().stdout("t\n()\n()\n");
}

#[test]
fn it_can_define_and_apply_a_function() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq double (lambda (a) (+ a a)))
    (print (apply double (list 10)))
    "#,
    );
    cmd.assert().success().stdout("20\n");
}

#[test]
fn it_can_define_and_call_a_function_directly() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq double (lambda (a) (+ a a)))
    (print (double 10))
    "#,
    );
    cmd.assert().success().stdout("20\n");
}

#[test]
fn macro_can_create_and_expand_a_macro() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (defmacro printplustwo (a) (print (+ a 2)))
    (printplustwo 5)
    "#,
    );
    cmd.assert().success().stdout("7\n");
}
