use assert_cmd::cargo::*;

#[test]
fn it_can_calculate_a_fibonacci_number() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq fib (lambda (n)
        (if (<= n 1)
            n
            (+ (fib (- n 1))
               (fib (- n 2))))))
    (print (fib 7))
    "#,
    );
    cmd.assert().success().stdout("13\n");
}
