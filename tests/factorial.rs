use assert_cmd::cargo::*;

#[test]
fn it_can_calculate_a_factorial() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq fact (lambda (n)
        (if (<= n 1)
            1
            (* n (fact (- n 1))))))
    (print (fact 5))
    "#,
    );
    cmd.assert().success().stdout("120\n");
}
