use std::process::Command;

#[test]
fn interpolated_regex_patterns_are_cached_by_concrete_text() {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.args([
        "-e",
        r#"
my $needle = "a";
for ^16 {
    die unless "a" ~~ /$needle/;
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
        .find(|line| line.contains("regex-parse-cache:"))
        .unwrap_or_else(|| panic!("no regex-parse-cache stats line in stderr: {stderr}"));
    let counter = |name: &str| {
        stats_line
            .split_whitespace()
            .find_map(|word| word.strip_prefix(name))
            .and_then(|value| value.parse::<u64>().ok())
            .unwrap_or_else(|| panic!("missing {name} in: {stats_line}"))
    };

    assert_eq!(counter("hits="), 15, "unexpected cache hits: {stats_line}");
    assert_eq!(
        counter("misses="),
        1,
        "the unchanged interpolated pattern was parsed more than once: {stats_line}"
    );
}
