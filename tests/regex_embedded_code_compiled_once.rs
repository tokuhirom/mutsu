//! Pins #10121: code embedded in a regex is compiled once per fragment, not
//! once per match attempt.
//!
//! `{ … }` blocks and `<?{ … }>` assertions already reused a compiled chunk
//! keyed by their parse-cache id (ADR-0009). Three other constructs did not:
//!
//! - a `<{ … }>` interpolation and a `** { … }` count ran their body through
//!   the uncached `eval_block_value`;
//! - a leading `:my $x = …` declaration used the cached path, but the match
//!   then restored the grammar token table on its way out. That took a
//!   registry write guard and bumped `TOKEN_DEFS_GEN` on every match, so
//!   the regex-code parse cache (keyed on the registry write generation)
//!   handed out a fresh id each time. The compile cache could never hit, and
//!   every generation-keyed regex memo was invalidated per match as well.
//!
//! A regression shows up as the `carrier-compile:` line's `misses=` or
//! `uncached=` growing with the number of iterations.

use std::process::Command;

/// `(printed count, compiles)` for `program` with `N` replaced by `n`, where
/// compiles = `misses + uncached` from the `carrier-compile:` stats line.
fn run(program: &str, n: u32) -> (u32, u64) {
    let src = program.replace('N', &n.to_string());
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(&src);
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(out.status.success(), "run failed: {stderr}");
    let count = stdout
        .lines()
        .last()
        .and_then(|l| l.trim().parse().ok())
        .unwrap_or_else(|| panic!("no count on stdout: {stdout}"));
    let line = stderr
        .lines()
        .find_map(|l| l.split("carrier-compile: ").nth(1))
        .unwrap_or_else(|| panic!("no carrier-compile line in stats: {stderr}"));
    let field = |name: &str| -> u64 {
        line.split_whitespace()
            .find_map(|w| w.strip_prefix(name))
            .and_then(|v| v.parse().ok())
            .unwrap_or_else(|| panic!("no {name} in: {line}"))
    };
    (count, field("misses=") + field("uncached="))
}

fn assert_compiles_once(program: &str) {
    let (few, few_compiles) = run(program, 10);
    let (many, many_compiles) = run(program, 60);
    assert_eq!((few, many), (10, 60), "the match stopped succeeding");
    assert_eq!(
        few_compiles, many_compiles,
        "compiles grew with iterations ({few_compiles} at 10, {many_compiles} at 60): \
         the embedded code is compiled per match again (#10121)"
    );
}

#[test]
fn closure_interpolation_compiles_once() {
    assert_compiles_once(
        r#"my $c = 0; my $p = "b"; for ^N -> $i { $c++ if "abc" ~~ /a <{ $p }> c/ }; say $c"#,
    );
}

#[test]
fn computed_repeat_count_compiles_once() {
    assert_compiles_once(
        r#"my $c = 0; my $n = 2; for ^N -> $i { $c++ if "abbc" ~~ /a b ** {$n} c/ }; say $c"#,
    );
}

#[test]
fn leading_my_declaration_compiles_once() {
    assert_compiles_once(
        r#"my $c = 0; for ^N -> $i { $c++ if "abc" ~~ /:my $z = 1; a b c/ }; say $c"#,
    );
}
