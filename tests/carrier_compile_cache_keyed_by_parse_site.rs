//! Pins the carrier compile cache's key to the **parse site** (#7667).
//!
//! `eval_block_value_inner` caches the bytecode it compiles for a carrier block.
//! The key used to be `SubData.id` — which is `next_instance_id()`, a fresh
//! number for every `Sub` *value*. A block instantiated more than once from one
//! literal therefore got a new key every time and could never hit, while leaving
//! a permanently unreachable entry behind in an unbounded map.
//!
//! `Cro::MessageWithBody.body-blob` is `Promise(supply { whenever … })`, so it
//! builds a fresh supply block on every call: it re-compiled three chunks per
//! call and grew the cache by three entries per call.
//!
//! The key is the pool-owned body `Arc` now (`CompiledCode::closure_body_arc`
//! hands every instantiation of one literal the same `Arc`), so the compiled
//! chunk is shared across instantiations. The `whenever` body's phaser split is
//! memoized on the same identity — without that the callback `Sub` was handed a
//! freshly cloned `Vec<Stmt>` per registration and the cache still could not see
//! it as the same block.
//!
//! A regression shows up as `carrier-compile: misses=` growing with the number
//! of *instantiations*.

use std::process::Command;

/// Instantiates one `supply { whenever … }` literal `n` times, taps each.
const PROGRAM: &str = r#"
sub mk($sup) { supply { whenever $sup -> $v { emit $v } } }
my $n = +@*ARGS[0];
my $seen = 0;
for ^$n {
    my $sup = Supplier::Preserving.new;
    $sup.emit(1);
    $sup.done;
    mk($sup).tap({ $seen++ });
}
say $seen;
"#;

/// `(values seen by the taps, carrier-compile misses)`.
fn run(instantiations: u32) -> (u32, u64) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(PROGRAM).arg(instantiations.to_string());
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        out.status.success(),
        "run failed\nstdout: {stdout}\nstderr: {stderr}"
    );
    let seen = stdout
        .lines()
        .last()
        .and_then(|l| l.trim().parse().ok())
        .unwrap_or_else(|| panic!("no count on stdout: {stdout}"));
    let misses = stderr
        .lines()
        .find_map(|l| l.split("carrier-compile: ").nth(1))
        .and_then(|rest| rest.split("misses=").nth(1))
        .and_then(|v| v.split_whitespace().next())
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("no carrier-compile line in stats: {stderr}"));
    (seen, misses)
}

#[test]
fn one_supply_literal_compiles_once_however_often_it_is_instantiated() {
    let (few_seen, few_misses) = run(10);
    let (many_seen, many_misses) = run(40);
    assert_eq!(few_seen, 10, "a tap missed its value");
    assert_eq!(many_seen, 40, "a tap missed its value");
    // Before the fix these were 30 and 120 — three compiles per instantiation.
    assert!(
        many_misses <= few_misses + 2,
        "carrier compiles grew with the instantiation count: {few_misses} for 10, \
         {many_misses} for 40 — the cache is keyed by something per-instance again \
         (see CarrierCacheKey)"
    );
}
