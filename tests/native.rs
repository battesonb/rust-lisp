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
fn setq_can_set_pairs() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq x 5 y 10)
    (print x)
    (print y)
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
    (print (error (quote failure)))
    "#,
    );
    cmd.assert().success().stderr("failure\n");
}

#[test]
fn quote_prevents_evaluation_of_arguments() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (quote (lambda () (+ 2 2))))
    "#,
    );
    cmd.assert().success().stdout("(lambda nil (+ 2 2))\n");
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
    cmd.assert().success().stdout("t\nnil\nnil\n");
}

#[test]
fn lambda_can_be_defined_and_applied() {
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
fn lambda_can_be_defined_and_called_directly() {
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
fn lambda_can_be_called_directly() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print ((lambda (a) (+ a a)) 10))
    "#,
    );
    cmd.assert().success().stdout("20\n");
}

#[test]
fn lambda_body_is_rest_arguments() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq print_both (lambda (a b) (print a) (print b)))
    (print_both 5 10)
    "#,
    );
    cmd.assert().success().stdout("5\n10\n");
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

#[test]
fn macroexpand_can_expand_a_macro_and_print_it() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (macroexpand (defun x () (print wow)))
    "#,
    );
    cmd.assert()
        .success()
        .stdout("(setq x (lambda nil (print wow)))\n");
}

#[test]
fn let_can_bind_a_variable() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (let ((a 1) (b 2)) (print (+ a b)))
    (print (boundp a)) ; confirm global a is untouched
    (print (boundp b)) ; confirm global b is untouched
    "#,
    );
    cmd.assert().success().stdout("3\nnil\nnil\n");
}

#[test]
fn boundp_can_verify_whether_a_variable_is_bound() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (boundp a)) ; For basic values
    (setq a 1)
    (print (boundp a))
    (print (boundp b)) ; and same for Lambdas
    (setq b (lambda () (print (quote test))))
    (print (boundp b))
    "#,
    );
    cmd.assert().success().stdout("nil\nt\nnil\nt\n");
}

#[test]
fn cons_can_build_a_list() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (cons 3 (cons 2 ())))
    "#,
    );
    cmd.assert().success().stdout("(3 2)\n");
}

#[test]
fn cons_can_build_a_pair() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (cons 2 3))
    "#,
    );
    cmd.assert().success().stdout("(2 . 3)\n");
}

#[test]
fn cond_only_runs_one_branch() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (let ((a 5) (acc 0))
      (cond ((> a 5) (setq acc (+ acc 1)))
            ((= a 5) (setq acc (+ acc 1)))
            (t (setq acc (+ acc 1))))
      (print acc))
    "#,
    );
    cmd.assert().success().stdout("1\n");
}

#[test]
fn cond_nil_on_no_match() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (cond (nil 10)))
    "#,
    );
    cmd.assert().success().stdout("nil\n");
}

#[test]
fn make_hash_table_get_and_set() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
        (setq x (make-hash-table))
        (sethash x (quote foo) (quote bar))
        (print (gethash x (quote foo)))
    "#,
    );
    cmd.assert().success().stdout("bar\n");
}
