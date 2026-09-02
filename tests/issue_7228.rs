//! Regression test for issue #7228: a `-pe` Rot-13 one-liner using the topic
//! `.=` method-call form with colon arguments must parse and produce the same
//! output as Rakudo.

use std::io::Write;
use std::process::{Command, Stdio};

const ROT13: &str = r#".=trans: {$_ => $_».rotate(13)}({[$_».uc, @$_]}("a".."z"))"#;

#[test]
fn rot13_one_liner_with_topic_dotassign_colon_args() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .args(["-pe", ROT13])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn mutsu");

    child
        .stdin
        .take()
        .expect("missing mutsu stdin")
        .write_all(b"abc\nxyz\n")
        .expect("failed to write mutsu stdin");

    let output = child.wait_with_output().expect("failed to wait for mutsu");
    assert!(
        output.status.success(),
        "mutsu -pe failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout), "nop\nklm\n");
}
