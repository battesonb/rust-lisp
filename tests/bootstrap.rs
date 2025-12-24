/// Tests which must pass for the interpret to do basic things. It's sort of a "bootstrap" test to
/// ensure we can assert anything else, at all.
use assert_cmd::cargo::*;

#[test]
fn message_can_print() {
    let mut cmd = cargo_bin_cmd!("rustlisp");

    cmd.write_stdin(r#"
    (print success)
    "#);
    cmd.assert().success().stdout("success\n");
}
