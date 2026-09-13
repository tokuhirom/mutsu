use std::process::Command;

#[test]
fn boolean_match_tests_do_not_materialize_capture_maps() {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.args([
        "-e",
        r#"
my $subject = "a";
for ^16 {
    if $subject ~~ /a/ {
        Nil;
    }
    if $subject ~~ /(a)/ {
        Nil;
    }
}
"#,
    ]);
    cmd.env("MUTSU_VM_STATS", "1");
    let output = cmd.output().expect("failed to spawn mutsu");
    assert!(
        output.status.success(),
        "program failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    let stats_line = stderr
        .lines()
        .find(|line| line.contains("regex-captures:"))
        .unwrap_or_else(|| panic!("no regex-captures stats line in stderr: {stderr}"));
    let materializations = stats_line
        .split_whitespace()
        .find_map(|word| word.strip_prefix("match_materializations="))
        .and_then(|value| value.parse::<u64>().ok())
        .unwrap_or_else(|| panic!("missing match_materializations= in: {stats_line}"));
    assert_eq!(
        materializations, 0,
        "boolean-only Match use materialized capture maps: {stats_line}"
    );
}
