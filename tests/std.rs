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
    (print (reduce (lambda (a b) (+ a b)) (list 1 2 3 4 5))) ; works with direct lambdas
    (print (reduce (quote +) (list 3))) ; identity for singleton list
    (print (reduce (quote +) nil)) ; nil for empty list
    "#,
    );
    cmd.assert().success().stdout("15\n15\n3\nnil\n");
}

#[test]
fn nth() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq x (list 1 2 3 4 5))
    (print (nth 0 x)) ; first element
    (print (nth 3 x)) ; third element
    (print (nth -1 x)) ; out of bounds
    (print (nth 6 x)) ; out of bounds
    "#,
    );
    cmd.assert().success().stdout("1\n4\nnil\nnil\n");
}

#[test]
fn filter() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq x (list 1 2 3 4 5))
    (print (filter (lambda (x) (<= x 3)) x)) ; can filter elements
    (print (filter (lambda (x) (<= x 3)) nil)) ; returns nil on empty list
    "#,
    );
    cmd.assert().success().stdout("(1 2 3)\nnil\n");
}

#[test]
fn typep_returns_t_if_correct() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
        (print (typep nil (quote null)))
        (print (typep (list 1 2 3) (quote cons)))
        (print (typep "foo" (quote string)))
        (print (typep (quote foo) (quote symbol)))
        (print (typep 1 (quote number)))
        (print (typep (lambda () nil) (quote function)))
        (print (typep setq (quote native-function)))
        (print (typep defun (quote macro)))
        (print (typep (make-hash-table) (quote hash-table)))
    "#,
    );
    cmd.assert().success().stdout("t\nt\nt\nt\nt\nt\nt\nt\nt\n");
}
#[test]
fn typep_returns_nil_if_incorrect() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
        (print (typep nil (quote string)))
        (print (typep (list 1 2 3) (quote null)))
        (print (typep "foo" (quote null)))
        (print (typep (quote null) (quote null)))
        (print (typep 1 (quote null)))
        (print (typep (lambda () nil) (quote null)))
        (print (typep setq (quote null)))
        (print (typep defun (quote null)))
        (print (typep (make-hash-table) (quote null)))
    "#,
    );
    cmd.assert().success().stdout("nil\nnil\nnil\nnil\nnil\nnil\nnil\nnil\nnil\n");
}
