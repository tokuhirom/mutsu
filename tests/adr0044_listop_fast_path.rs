//! ADR-0044 D1/D2 pin: giving the seven core listops (`push`, `pop`,
//! `shift`, `unshift`, `append`, `prepend`, `splice`) a native function-form
//! implementation (D1, `src/runtime/listop_functions.rs`) must not regress
//! the compiler's dedicated fast-path opcode (D2) for the dominant case —
//! a call site with no competing user/imported candidate for the name.
//! `push(@a, 1)` must keep compiling to `ArrayPush`, not fall to the
//! generic `CallFunc` name dispatch that only fires once a competing
//! candidate is visible (see docs/adr/0044-listops-are-routines-not-a-syntactic-rewrite.md).

use std::process::Command;

fn dump_bytecode(src: &str) -> String {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("--dump-bytecode").arg("-e").arg(src);
    let out = cmd.output().expect("failed to spawn mutsu");
    assert!(
        out.status.success(),
        "mutsu --dump-bytecode failed for {src:?}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

#[test]
fn plain_push_with_no_competing_candidate_stays_on_the_fast_path() {
    let bytecode = dump_bytecode("my @a; push(@a, 1);");
    assert!(
        bytecode.contains("ArrayPush"),
        "push(@a, 1) with no competing candidate did not compile to \
         ArrayPush (D2's fast path regressed):\n{bytecode}"
    );
    assert!(
        !bytecode.contains("CallFunc"),
        "push(@a, 1) with no competing candidate should not fall to the \
         generic CallFunc name dispatch:\n{bytecode}"
    );
}

#[test]
fn push_falls_to_callfunc_only_once_a_competing_multi_is_declared() {
    // The opposite pin: once a `multi push` is visible, the compiler's
    // `user_listop_shadows` veto (D2) must still suppress the ArrayPush
    // fast path so the call reaches the D1 native routine (which then
    // dispatches to the array form itself, since the multi doesn't match).
    let bytecode = dump_bytecode("multi push(Str $s, Int $i) { }; my @a; push(@a, 1);");
    assert!(
        bytecode.contains("CallFunc"),
        "push(@a, 1) with a competing `multi push` in scope should reach \
         the generic CallFunc dispatch (so D1's native fallback can run):\n{bytecode}"
    );
}
