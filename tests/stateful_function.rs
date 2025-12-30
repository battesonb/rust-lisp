use assert_cmd::cargo::*;

#[test]
fn it_can_generate_stateful_functions() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (defun make-accumulator ()
        (let ((acc 0)) (lambda (inc)
            (progn
                (setq acc (+ acc inc))
                acc))))
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
fn it_use_stateful_functions_to_simulate_objects() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
(defun make-account(balance)
    (lambda (operation amount)
        (if (= operation deposit)
            (setq balance (+ balance amount))
            (if (= operation withdraw) (setq balance (- balance amount)) balance))))
(setq account (make-account 0))
(print (account deposit 100))
(print (account withdraw 30))
    "#,
    );
    cmd.assert().success().stdout("100\n70\n");
}
