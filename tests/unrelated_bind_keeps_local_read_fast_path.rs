//! #9914 (ADR-0097 §15): a `:=`, a closure-captured cell or `use Test`
//! anywhere in a program must not trip the process-global `GetLocal` spoiler
//! latch (`vm_jit::LOCAL_READ_SPOILERS`). A tripped latch routes every local
//! read in every frame through the full guard chain for the rest of the run;
//! the #8748 repro measured +21.7% Ir for one unrelated `:=` with the JIT on.

use std::process::Command;

fn local_read_spoilers(code: &str) -> u64 {
    let output = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .args(["-e", code])
        .env("MUTSU_VM_STATS", "1")
        .output()
        .expect("failed to spawn mutsu");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "program failed: {stderr}");
    stderr
        .lines()
        .find(|line| line.contains("[mutsu vm-stats] jit:"))
        .and_then(|line| {
            line.split_whitespace()
                .find_map(|word| word.strip_prefix("local_read_spoilers="))
        })
        .and_then(|value| value.parse().ok())
        .unwrap_or_else(|| panic!("no local_read_spoilers counter in stderr: {stderr}"))
}

#[test]
fn unrelated_array_bind_does_not_spoil_local_reads() {
    // The #8748 repro: the hot loop never reads `@unused`.
    let code = r#"
my @data = ^256;
my @unused := @data;
my @plain = ^256;
my $seed = 7;
my $s = 0;
for ^300 -> $i { $s += @plain[$i +& 255] + $seed; }
die "wrong sum $s" unless $s == 35686;
"#;
    assert_eq!(local_read_spoilers(code), 0);
}

#[test]
fn scalar_bind_and_rebind_do_not_spoil_local_reads() {
    let code = r#"
my $y = 1;
my $x := $y;
my $z = 2;
$x := $z;
$x = 5;
die "bind did not alias" unless $z == 5 && $y == 1;
"#;
    assert_eq!(local_read_spoilers(code), 0);
}

#[test]
fn closure_capture_cell_does_not_spoil_local_reads() {
    let code = r#"
sub f { my $c = 0; return { $c++ } }
my &g = f();
g(); g();
die "closure lost its cell" unless g() == 2;
"#;
    assert_eq!(local_read_spoilers(code), 0);
}

#[test]
fn use_test_does_not_spoil_local_reads() {
    // Loading Test.pm6 creates ~20 container cells; none may trip the latch,
    // or every test file reads its locals through the slow chain.
    assert_eq!(local_read_spoilers("use Test; ok 1; done-testing"), 0);
}

#[test]
fn atomic_variable_still_spoils_local_reads() {
    // The latch's remaining sources stay wired: an atomic variable makes the
    // atomic-read branch of the slow chain live, so the fast read must yield.
    assert!(local_read_spoilers("my atomicint $a = 0; $a⚛++;") > 0);
}
