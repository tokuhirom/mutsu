//! Pins #10266: the body of a registered `token`/`rule` declaration is compiled
//! once, not once per evaluation. `Grammar.parse` evaluates the start rule's
//! body and a parameterized token's body for every call's arguments; both used
//! to go through the uncached `eval_block_value`, so the `carrier-compile:`
//! vm-stats line grew by one `uncached` compile per parse.

use std::process::Command;

/// Run `src` through the built `mutsu` with `MUTSU_VM_STATS=1` and return the
/// `misses + uncached` of its `carrier-compile:` line.
fn carrier_compiles(src: &str) -> u64 {
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(src)
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "run failed: {stderr}");
    let line = stderr
        .lines()
        .find(|l| l.contains("carrier-compile:"))
        .unwrap_or_else(|| panic!("no carrier-compile vm-stats line in: {stderr}"));
    ["misses=", "uncached="]
        .iter()
        .map(|key| {
            line.split_whitespace()
                .find_map(|w| w.strip_prefix(key))
                .and_then(|v| v.parse::<u64>().ok())
                .unwrap_or_else(|| panic!("missing {key} in: {line}"))
        })
        .sum()
}

fn parses(n: u32) -> String {
    format!(
        "grammar G {{ token TOP {{ :my $*d = 1; a <x(2)> }} token x($k) {{ b }} }}\n\
         my $c = 0; for ^{n} {{ $c++ if G.parse('ab') }}; say $c"
    )
}

#[test]
fn token_body_compiles_do_not_grow_with_the_number_of_parses() {
    let few = carrier_compiles(&parses(10));
    let many = carrier_compiles(&parses(60));
    assert_eq!(
        few, many,
        "the carrier compiles of a token body grow with the number of parses \
         (10 parses: {few}, 60 parses: {many}); see #10266"
    );
}
