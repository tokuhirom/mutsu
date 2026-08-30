use std::io::Write;
use std::process::{Command, Stdio};

#[test]
fn repl_reads_piped_input_in_the_callers_lexical_scope() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .args([
            "-e",
            r#"my $name = "Alice"; say "Hello, $name"; repl(); say "Goodbye, $name""#,
        ])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn mutsu");

    child
        .stdin
        .take()
        .expect("missing child stdin")
        .write_all(b"$name = \"Bob\"; 1 + 2\n")
        .expect("failed to feed repl input");
    let output = child.wait_with_output().expect("failed to wait for mutsu");

    assert!(
        output.status.success(),
        "mutsu failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "Hello, Alice\n3\nGoodbye, Bob\n"
    );
}
