//! Pins that a `CArray` element store or read runs through the straight-line
//! role-method lane (#16215), not through a full method frame.
//!
//! `CArray[num64]`'s `ASSIGN-POS` is `nqp::bindpos_n(self, nqp::unbox_i($pos),
//! nqp::unbox_n($value))`. The lane is gated on
//! `CompiledCode::ignores_role_context`, an allow-list of the ops such a body
//! compiles to. When the compiler started ending a routine's tail call with
//! `DerefContainer`, every store fell back to the method frame: ~140k instead
//! of ~25k instructions per element, and Sort::Fast's 1M-element fill went
//! from ~6 s to ~24 s. Nothing failed; only the `straight_line` vm-stats count
//! shows it.

use std::process::Command;

fn straight_line_calls(src: &str) -> u64 {
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(src)
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "mutsu failed:\n{stdout}\n{stderr}");
    assert!(stdout.contains("done"), "unexpected output:\n{stdout}\n{stderr}");
    stderr
        .lines()
        .find_map(|l| l.split_once("] method-lanes: straight_line="))
        .and_then(|(_, n)| n.split_whitespace().next()?.parse().ok())
        .unwrap_or_else(|| panic!("no method-lanes line in:\n{stderr}"))
}

#[test]
fn carray_num_store_takes_the_straight_line_lane() {
    let n = straight_line_calls(
        "use NativeCall; my @c := CArray[num64].new; \
         @c[$_] = 1.5e0 for ^200; say @c[199] == 1.5e0 ?? 'done' !! 'bad'",
    );
    assert!(n >= 200, "only {n} of 200 stores took the straight-line lane");
}

#[test]
fn carray_int_store_from_a_sub_takes_the_straight_line_lane() {
    let n = straight_line_calls(
        "use NativeCall; sub fill(@a) { my @c := CArray[int32].new; \
         @c[$_] = @a[$_] for ^@a.elems; @c } \
         my @c := fill([^200]); say @c[199] == 199 ?? 'done' !! 'bad'",
    );
    assert!(n >= 200, "only {n} of 200 stores took the straight-line lane");
}
