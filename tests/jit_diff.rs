//! Differential coverage for the Cranelift JIT (ADR-0004 J1): the same
//! program run with `MUTSU_JIT=off` and `MUTSU_JIT=on` must produce
//! byte-identical output (gc-stress methodology, ADR-0004 §2.6), and a hot
//! Tier A-compilable function must actually enter native code.

use std::process::Command;

/// Run a Raku snippet through the built `mutsu` with the given extra
/// environment. Returns (stdout, stderr, success).
fn run(src: &str, envs: &[(&str, &str)]) -> (String, String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    for k in ["MUTSU_JIT", "MUTSU_JIT_THRESHOLD", "MUTSU_VM_STATS"] {
        cmd.env_remove(k);
    }
    for (k, v) in envs {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to spawn mutsu");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

const FIB: &str =
    "sub fib($n) { if $n < 2 { return $n } return fib($n - 1) + fib($n - 2); }\nsay fib(18);";

/// Recursive fib: the J1 Tier A target shape (LoadConst / GetLocal / NumLt /
/// JumpIfFalse / Sub / Add / CallFunc / Return). Output must not depend on
/// whether the body runs interpreted or native.
#[test]
fn fib_output_is_identical_with_jit_on() {
    let (off_out, off_err, off_ok) = run(FIB, &[("MUTSU_JIT", "off")]);
    let (on_out, on_err, on_ok) = run(FIB, &[("MUTSU_JIT", "on"), ("MUTSU_JIT_THRESHOLD", "1")]);
    assert!(off_ok, "JIT-off run failed: {off_err}");
    assert!(on_ok, "JIT-on run failed: {on_err}");
    assert_eq!(off_out, on_out, "JIT on/off outputs diverge");
    assert_eq!(off_out, "2584\n");
}

/// Explicit `return` inside the native body must unwind exactly like the
/// interpreter's return signal (the `Err(return_value)` arm), including from
/// the recursive fall-through-plus-return mix in `fib`.
#[test]
fn jit_actually_compiles_and_enters() {
    let (out, err, ok) = run(
        FIB,
        &[
            ("MUTSU_JIT", "on"),
            ("MUTSU_JIT_THRESHOLD", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );
    assert!(ok, "JIT-on run failed: {err}");
    assert_eq!(out, "2584\n");
    if cfg!(feature = "jit") {
        let jit_line = err
            .lines()
            .find(|l| l.contains("jit: compiles="))
            .unwrap_or_else(|| panic!("no jit stats line in stderr: {err}"));
        let field = |key: &str| -> u64 {
            jit_line
                .split_whitespace()
                .find_map(|w| w.strip_prefix(key))
                .and_then(|v| v.parse().ok())
                .unwrap_or_else(|| panic!("missing {key} in: {jit_line}"))
        };
        assert!(field("compiles=") >= 1, "hot fib chunk was not compiled");
        assert!(field("entries=") >= 1, "compiled fib chunk never entered");
    }
}

/// J2 coverage: a hot METHOD body (the `call_compiled_method*` entry hooks)
/// built from J2 Tier A opcodes — comparisons (`NumLe`), arith (`Mul`),
/// string concat (`Concat`), and attribute access via the generic `step` shim
/// (`CallMethod`/`GetGlobal`) — must compile, enter, and produce
/// interpreter-identical output.
#[test]
fn hot_method_body_compiles_and_matches() {
    let src = "class Counter { has $.n is rw; method bump() { $.n = $.n * 2 <= 64 ?? $.n * 2 !! $.n + 1; } method label() { \"n=\" ~ $.n } }\nmy $c = Counter.new(n => 1);\nfor ^30 { $c.bump() }\nsay $c.label();";
    let (off_out, off_err, off_ok) = run(src, &[("MUTSU_JIT", "off")]);
    let (on_out, on_err, on_ok) = run(
        src,
        &[
            ("MUTSU_JIT", "on"),
            ("MUTSU_JIT_THRESHOLD", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );
    assert!(off_ok, "JIT-off run failed: {off_err}");
    assert!(on_ok, "JIT-on run failed: {on_err}");
    assert_eq!(off_out, on_out, "JIT on/off outputs diverge");
    if cfg!(feature = "jit") {
        let jit_line = on_err
            .lines()
            .find(|l| l.contains("jit: compiles="))
            .expect("no jit stats line");
        let compiles: u64 = jit_line
            .split_whitespace()
            .find_map(|w| w.strip_prefix("compiles="))
            .and_then(|v| v.parse().ok())
            .expect("missing compiles=");
        assert!(
            compiles >= 1,
            "hot method chunk was not compiled: {jit_line}"
        );
    }
}

/// J5 pin (ADR-0004 §2.5): with `MUTSU_JIT` unset, the JIT is ON by default —
/// a hot Tier A chunk compiles and enters without any opt-in flag.
#[test]
fn jit_is_on_by_default() {
    let (out, err, ok) = run(
        FIB,
        &[("MUTSU_JIT_THRESHOLD", "1"), ("MUTSU_VM_STATS", "1")],
    );
    assert!(ok, "default run failed: {err}");
    assert_eq!(out, "2584\n");
    if cfg!(feature = "jit") {
        let jit_line = err
            .lines()
            .find(|l| l.contains("jit: compiles="))
            .unwrap_or_else(|| panic!("no jit stats line in stderr: {err}"));
        let compiles: u64 = jit_line
            .split_whitespace()
            .find_map(|w| w.strip_prefix("compiles="))
            .and_then(|v| v.parse().ok())
            .expect("missing compiles=");
        assert!(
            compiles >= 1,
            "MUTSU_JIT unset must default to on: {jit_line}"
        );
    }
}

/// A function whose chunk contains an unsupported opcode must be counted as a
/// bailout and keep producing interpreter-identical output.
#[test]
fn unsupported_opcode_bails_out_cleanly() {
    // A `while` loop compiles to the compound `WhileLoop` opcode, which is
    // outside the Tier A set (loop bodies run through the interpreter until
    // J4 hot-loop entry).
    let src = "sub count($n) { my $i = 0; while $i < $n { $i = $i + 1 } return $i }\nmy $s = 0;\nfor ^50 { $s = count(3) }\nsay $s;";
    let (off_out, _, off_ok) = run(src, &[("MUTSU_JIT", "off")]);
    let (on_out, on_err, on_ok) = run(
        src,
        &[
            ("MUTSU_JIT", "on"),
            ("MUTSU_JIT_THRESHOLD", "1"),
            ("MUTSU_VM_STATS", "1"),
        ],
    );
    assert!(off_ok && on_ok, "runs failed: {on_err}");
    assert_eq!(off_out, on_out);
    if cfg!(feature = "jit") {
        let jit_line = on_err
            .lines()
            .find(|l| l.contains("jit: compiles="))
            .expect("no jit stats line");
        assert!(
            jit_line.contains("bailouts=1"),
            "expected exactly one bailout chunk: {jit_line}"
        );
    }
}
