use assert_cmd::cargo::*;

#[test]
fn it_can_generate_stateful_functions() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (defun make-accumulator ()
        (progn
            (setq acc 0)
            (lambda (inc)
                (progn
                    (setq acc (+ acc inc))
                    acc))))
    (setq accumulator (make-accumulator))
    (print (accumulator 5))
    (print (accumulator 10))
    (print (accumulator 15))
    "#,
    );
    cmd.assert().success().stdout("5\n15\n30\n");
}
