/// All std functions work
use assert_cmd::cargo::*;

#[test]
fn not() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (not t))  ; false
    (print (not 1))  ; false
    (print (not ())) ; true
    "#,
    );
    cmd.assert().success().stdout("nil\nnil\nt\n");
}

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
fn null() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (null t))  ; false
    (print (null 1))  ; false
    (print (null ())) ; true
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

#[test]
fn assoc() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq x (list (cons 1 2) (cons 3 4) (cons 5 6)))
    (print (assoc x 3)) ; can fetch a value from an associative array
    (print (assoc x 7)) ; returns nil if not found
    "#,
    );
    cmd.assert().success().stdout("(3 . 4)\nnil\n");
}

#[test]
fn mapcar() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq x (list 1 2 3 4 5))
    (defun double (a) (+ a a))
    (print (mapcar (quote double) x))
    "#,
    );
    cmd.assert().success().stdout("(2 4 6 8 10)\n");
}

#[test]
fn reduce() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (reduce (quote +) (list 1 2 3 4 5))) ; sums all values
    (print (reduce (quote +) (list 3))) ; identity for singleton list
    (print (reduce (quote +) nil)) ; nil for empty list
    "#,
    );
    cmd.assert().success().stdout("15\n3\nnil\n");
}
