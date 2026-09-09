//! Pins the carrier-compile cache against `whenever` callback bodies (#7667).
//!
//! `eval_block_value_inner` caches the bytecode it compiles for a carrier block,
//! keyed by the code object's `SubData.id` plus the ambient compile context. But
//! it used to *bypass* the cache whenever any of four per-`SubData` mutations
//! applied to the freshly compiled chunk -- the supply-body mark, the emitter
//! name, the vouched capture set, the inherited owned-lexical set -- on the
//! grounds that a cached `Arc` must stay byte-identical to a fresh compile.
//!
//! A `whenever` callback's `authoritative_captures` is never empty, so it took
//! that branch on *every emitted value*: one full `Compiler::compile` over the
//! callback's AST per item through the supply, and with it a full Debug
//! traversal of that AST for each `function_body_fingerprint` the compiler
//! computes. On Cro's HTTP/2 request parser that was 38% of a DATA frame.
//!
//! The four values are derived from the code object, not the ambient frame, so
//! they are the same on every call for one cache id -- which makes them a *key*,
//! not a reason to bypass. They are part of `CarrierCompileCtxKey` now and
//! applied before the chunk is shared, so the byte-identity argument still holds
//! and the cache serves.
//!
//! A regression shows up as `carrier-compile: misses=` growing with the number
//! of values pushed through the supply instead of staying constant.

use std::process::Command;

/// A supply whose `whenever` callback runs once per emitted value.
const PROGRAM: &str = r#"
my $sup = Supplier.new;
my $out = supply { whenever $sup -> $v { emit $v * 2 } };
my $n = +@*ARGS[0];
my $sum = 0;
$out.tap({ $sum += $_ });
for ^$n { $sup.emit($_) }
say $sum;
"#;

/// `(sum the tap accumulated, carrier-compile misses)`.
fn run(emits: u32) -> (u64, u64) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(PROGRAM).arg(emits.to_string());
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        out.status.success(),
        "run failed\nstdout: {stdout}\nstderr: {stderr}"
    );
    let sum = stdout
        .lines()
        .last()
        .and_then(|l| l.trim().parse().ok())
        .unwrap_or_else(|| panic!("no sum on stdout: {stdout}"));
    let misses = stderr
        .lines()
        .find_map(|l| l.split("carrier-compile: ").nth(1))
        .and_then(|rest| rest.split("misses=").nth(1))
        .and_then(|v| v.split_whitespace().next())
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("no carrier-compile line in stats: {stderr}"));
    (sum, misses)
}

#[test]
fn a_whenever_callback_body_is_compiled_once_not_once_per_emitted_value() {
    let (few_sum, few_misses) = run(10);
    let (many_sum, many_misses) = run(40);
    // sum of 2*i for i in 0..n
    assert_eq!(few_sum, 90, "the tap did not see every emitted value");
    assert_eq!(many_sum, 1560, "the tap did not see every emitted value");
    // Before the fix these were 12 and 42 -- exactly `emits + 2`.
    assert!(
        many_misses <= few_misses + 2,
        "carrier compiles grew with the number of emitted values: {few_misses} for 10 \
         emits, {many_misses} for 40 -- the whenever callback body is being re-compiled \
         from AST per item again (see CarrierCompileCtxKey)"
    );
}
