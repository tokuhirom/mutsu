//! Pins that calling the callable a method's `nextcallee` returns runs its
//! chain directly (#12875).
//!
//! The continuation is a compiled template, `sub (|c) is raw {
//! __mutsu_nextcallee_continue(&?ROUTINE, |c) }`. Called through it,
//! `original(self, |c)` is three generic calls: the template with a `|c` bind,
//! the `CallFunc` of the builtin through the interpreter (a function fallback
//! named `__mutsu_nextcallee_continue`), then the candidate. The value-call
//! funnel now runs the builtin itself, so the fallback count stays at zero.
//! Method::Protected wraps every protected method this way.

use std::process::Command;

#[test]
fn a_wrapper_calling_nextcallee_takes_no_function_fallback() {
    let src = "my &w = my method (|c) { my &original = nextcallee; original(self, |c) }; \
               class D { method m($x) { $x + 1 } }; \
               D.^find_method('m').wrap(&w.clone); \
               my $d = D.new; my $s = 0; $s += $d.m($_) for ^50; say $s";
    let out = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg("-e")
        .arg(src)
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "mutsu failed:\n{stdout}\n{stderr}");
    assert_eq!(stdout.trim(), "1275", "stderr:\n{stderr}");
    assert!(
        !stderr.contains("__mutsu_nextcallee_continue="),
        "the continuation went through its template:\n{stderr}"
    );
}
