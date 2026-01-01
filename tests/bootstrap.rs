/// Tests which must pass for the interpret to do basic things. It's sort of a "bootstrap" test to
/// ensure we can assert anything else, at all.
use assert_cmd::cargo::*;

#[test]
fn message_can_print() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (print (quote success))
    "#,
    );
    cmd.assert().success().stdout("success\n");
}

#[test]
fn native_functions_are_values() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (setq p print)
    (p (quote native))
    "#,
    );
    cmd.assert().success().stdout("native\n");
}

#[test]
fn function_rest_arguments_supported() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(
        r#"
    (defun printrest (prefix &rest args) (print (cons prefix args)))
    (printrest (quote prefix) (quote a) (quote b) (quote c))
    "#,
    );
    cmd.assert().success().stdout("(prefix a b c)\n");
}
