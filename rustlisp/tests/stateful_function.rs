use assert_cmd::cargo::*;

#[test]
fn it_can_generate_stateful_functions() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (defun make-accumulator ()
        (let ((acc 0))
             (lambda (inc)
               (setq acc (+ acc inc))
               acc)))
    (setq accumulator (make-accumulator))
    (print (accumulator 5))
    (print (accumulator 10))
    (print (accumulator 15))
    (print (boundp acc)) ; verify that acc is still a symbol in the global scope
    "#,
    );
    cmd.assert().success().stdout("5\n15\n30\nnil\n");
}

#[test]
fn it_can_use_stateful_functions_to_simulate_objects() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
(defun make-account(balance)
    (lambda (operation &rest args)
        (cond ((= operation (quote deposit)) (setq balance (+ balance (car args))))
              ((= operation (quote withdraw)) (setq balance (- balance (car args))))
              ((= operation (quote check)) balance)
              (t (error "unexpected operation")))))
(setq account (make-account 0))
(print (account (quote deposit) 100))
(print (account (quote withdraw) 30))
(print (account (quote check)))
(print (account (quote invalid)))
    "#,
    );
    cmd.assert().success().stdout("100\n70\n70\n").stderr(
        r#"ERROR: "unexpected operation"
TRACE:
1. print <NATIVE-FUNCTION print>
2. account <FUNCTION (operation)>
3. cond <NATIVE-FUNCTION cond>
4. error <NATIVE-FUNCTION error>
"#,
    );
}

#[test]
fn it_can_use_stateful_functions_with_hash_tables_to_simulate_objects() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
(defun make-account(balance) ; balance is a single private member
    (progn
        ; The hash table provides the equivalent of public methods
        (setq methods (make-hash-table))
        (sethash methods (quote deposit) (lambda (amount) (setq balance (+ balance amount))))
        (sethash methods (quote withdraw) (lambda (amount) (setq balance (- balance amount))))
        (sethash methods (quote check) (lambda () balance))
        (lambda (operation &rest args)
            (let ((func (gethash methods operation)))
                (if func (apply func args) (error "unexpected operation"))))))
(setq account (make-account 0))
(print (account (quote deposit) 100))
(print (account (quote withdraw) 30))
(print (account (quote check)))
(print (account (quote invalid)))
    "#,
    );
    cmd.assert().success().stdout("100\n70\n70\n").stderr(
        r#"ERROR: "unexpected operation"
TRACE:
1. print <NATIVE-FUNCTION print>
2. account <FUNCTION (operation)>
3. let <NATIVE-FUNCTION let>
4. if <NATIVE-FUNCTION if>
5. error <NATIVE-FUNCTION error>
"#,
    );
}
