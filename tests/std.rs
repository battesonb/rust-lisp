/// All std functions work
use assert_cmd::cargo::*;

#[test]
fn less_than_or_equal_to() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (<= 5 6))
    (print (<= 6 6))
    (print (<= 7 6))
    "#,
    );
    cmd.assert().success().stdout("t\nt\nnil\n");
}

#[test]
fn greater_than_or_equal_to() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (>= 5 6))
    (print (>= 6 6))
    (print (>= 7 6))
    "#,
    );
    cmd.assert().success().stdout("nil\nt\nt\n");
}

#[test]
fn greater_than() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (> 5 6))
    (print (> 6 6))
    (print (> 7 6))
    "#,
    );
    cmd.assert().success().stdout("nil\nnil\nt\n");
}

#[test]
fn defun_can_define_a_function() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (defun double (x) (+ x x))
    (print (double 5))
    "#,
    );
    cmd.assert().success().stdout("10\n");
}
