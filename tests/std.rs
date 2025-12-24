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
    cmd.assert().success().stdout("t\nt\n()\n");
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
    cmd.assert().success().stdout("()\nt\nt\n");
}
