//! Pins that a `my enum` in a routine or supply body is registered once, when
//! the unit loads, so running the body on a thread clone writes no table the
//! clone shares with its parent (#13010).
//!
//! The registration used to happen only when the body ran. A thread clone's
//! parent had usually never run it, so the first run on each clone found the
//! enum new and copied the whole shared `Registry` (and the type tables) into
//! the clone. A regression shows up as `registry-cow: clones=` climbing back
//! to the number of threads.

use std::process::Command;

fn run(src: &str) -> (String, String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

/// The `clones=N` count of the vm-stats line named `stat`.
fn clones(stderr: &str, stat: &str) -> u64 {
    let prefix = format!("] {stat}: clones=");
    stderr
        .lines()
        .find_map(|l| {
            l.split_once(prefix.as_str())
                .map(|(_, n)| n.trim().to_string())
        })
        .and_then(|n| n.parse().ok())
        .unwrap_or_else(|| panic!("no `{stat}` line in: {stderr}"))
}

#[test]
fn a_routine_my_enum_run_on_threads_copies_no_registry() {
    let src = "sub f() { my enum Expecting <RequestLine Header Body>; RequestLine }\n\
               my @p = (^20).map: { start { f() } };\n\
               say (await @p).unique;";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "(RequestLine)\n");
    assert_eq!(clones(&err, "registry-cow"), 1, "{err}");
    assert_eq!(clones(&err, "program-table-cow"), 2, "{err}");
}

#[test]
fn a_class_supply_my_enum_run_on_threads_copies_no_table() {
    let src = "class P { method parse() { supply { my enum E <RequestLine Header>; emit Header } } }\n\
               my @p = (^20).map: { start { my $r; P.new.parse.tap({ $r = $_ }); $r } };\n\
               say (await @p).unique;";
    let (out, err, ok) = run(src);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "(Header)\n");
    assert_eq!(clones(&err, "registry-cow"), 1, "{err}");
    assert_eq!(clones(&err, "program-table-cow"), 0, "{err}");
}
