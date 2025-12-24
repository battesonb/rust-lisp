use assert_cmd::cargo::*;

#[test]
#[ignore = "not implemeneted yet"]
fn it_can_calculate_a_fibonacci_sequence() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq fib (lambda (n)
        (if (<= n 1) 1 (fib (- n 1)))))
    (print (fib 6))
    "#,
    );
    cmd.assert().success().stdout("8\n");
}
