//! Pins the scoped overlay `method_args_match_for_invocant` binds into (#7667).
//!
//! Candidate matching is a speculative window: it binds role type parameters,
//! the candidate's captured lexical scope, `self` and type captures, runs the
//! `where` constraints, and then rolls the whole lot back. It used to bind them
//! into the running frame's own env -- and since the rollback needs
//! `saved_env = self.env.clone()`, which shares the env's `Arc`, the *first*
//! such bind hit `Arc::make_mut` and deep-copied the entire map. Every candidate
//! of every multi call paid one O(env) copy.
//!
//! It binds into an `Env::scoped_child` overlay now: an empty tier that reads
//! through to the frame env, so the same writes are O(1) and the rollback is
//! dropping the tier.
//!
//! Measured as the *slope* of `env_deep_copy_entries` against the call count, so
//! the program's fixed setup cost cancels out and only the per-call cost is
//! asserted. Before the change the slope was ~121 entries per call (the size of
//! the frame env being copied); it is 1 now.

use std::process::Command;

/// One multi with a `where` constraint, called `n` times. Every call runs
/// candidate matching against both candidates.
const PROGRAM: &str = r#"
class C {
    multi method m(Int $x where { $x > 100 }) { 'big' }
    multi method m(Int $x) { 'small' }
}
my $c = C.new;
my $n = +@*ARGS[0];
my $r = '';
for ^$n { $r = $c.m(5) }
say $r;
"#;

fn env_deep_copy_entries(calls: u32) -> u64 {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(PROGRAM).arg(calls.to_string());
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        out.status.success(),
        "run failed\nstdout: {stdout}\nstderr: {stderr}"
    );
    assert_eq!(
        stdout.trim(),
        "small",
        "the narrower candidate should not have won"
    );
    stderr
        .lines()
        .find_map(|l| l.split("env_deep_copy_entries=").nth(1))
        .and_then(|v| v.split_whitespace().next())
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("no env_deep_copy_entries in stats: {stderr}"))
}

#[test]
fn a_multi_call_does_not_deep_copy_the_frame_env_per_candidate() {
    let few = env_deep_copy_entries(50);
    let many = env_deep_copy_entries(250);
    assert!(
        many >= few,
        "counter went backwards: {few} for 50 calls, {many} for 250"
    );
    let per_call = (many - few) as f64 / 200.0;
    assert!(
        per_call < 10.0,
        "each multi call deep-copies {per_call:.1} env entries ({few} for 50 calls, \
         {many} for 250) -- candidate matching is binding into the frame env again \
         instead of an Env::scoped_child overlay. It was ~121 before the fix, 1 after."
    );
}
