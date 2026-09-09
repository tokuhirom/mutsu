//! Pins the copy-on-write sharing of the interpreter's program-global symbol
//! tables across thread clones (#7667).
//!
//! `clone_for_thread_excluding` used to deep-copy ~50 of `Interpreter`'s symbol
//! tables -- loaded modules, exported names, per-package lexicals, class and
//! distribution bookkeeping -- on every thread clone. A thread clone is not a
//! rare event: `start`, `.then`, a `Promise` chained onto a supply and every
//! `whenever <Promise>` registration make one. So the cost of spawning grew with
//! the size of the *program* rather than with the work, which is why
//! `Cro::MessageWithBody.body-blob` -- a `Promise(supply { whenever ... })` --
//! cost several milliseconds per call once Cro's module stack was loaded.
//!
//! Those fields are `std::sync::Arc<...>` now, written through
//! `runtime::cow_table_mut`, so a spawn shares them and only a *write* taken
//! while a clone still holds the share copies the one table it touches. That
//! makes the copies countable: `program-table-cow: clones=` under
//! `MUTSU_VM_STATS`.
//!
//! A regression shows up as that count growing with the frame count -- either
//! because a table went back to being owned, or because a hot path took a
//! mutable borrow of one for what is really a read (which is what
//! `writeback_package_scope_var` did: one whole-table copy per HEADERS frame,
//! for a lookup that found nothing).

use std::process::Command;

/// Feeds `frames` HTTP/2 HEADERS frames through the real bundled
/// `Cro::HTTP2::RequestParser`. Each frame opens a stream, and the parser
/// registers a `whenever $cancellation` per stream -- so this is a loop that
/// interleaves thread clones with ordinary interpreter work, exactly the shape
/// the sharing is for.
const PROGRAM: &str = r#"
use Cro::HTTP2::Frame;
use Cro::HTTP2::RequestParser;
use Cro::HTTP2::ConnectionState;
use HTTP::HPACK;

my $enc = HTTP::HPACK::Encoder.new;
my $blob = $enc.encode-headers([
    HTTP::HPACK::Header.new(name => ':method',    value => 'GET'),
    HTTP::HPACK::Header.new(name => ':path',      value => '/'),
    HTTP::HPACK::Header.new(name => ':scheme',    value => 'https'),
    HTTP::HPACK::Header.new(name => ':authority', value => 'example.com'),
]);
my $n = +@*ARGS[0];
my $cs = Cro::HTTP2::ConnectionState.new;
my $in = Supplier.new;
my $out = Cro::HTTP2::RequestParser.new.transformer($in.Supply, connection-state => $cs);
my $seen = 0;
$out.tap({ $seen++ });
for ^$n -> $i {
    $in.emit(Cro::HTTP2::Frame::Headers.new(
        flags => 5, stream-identifier => 1 + 2 * $i, headers => $blob));
}
say $seen;
"#;

/// `(requests parsed, program-table COW clones)`.
fn run(frames: u32) -> (u32, u64) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(PROGRAM).arg(frames.to_string());
    cmd.env("MUTSU_VM_STATS", "1");
    let out = cmd.output().expect("failed to spawn mutsu");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        out.status.success(),
        "run failed\nstdout: {stdout}\nstderr: {stderr}"
    );
    let parsed = stdout
        .lines()
        .last()
        .and_then(|l| l.trim().parse().ok())
        .unwrap_or_else(|| panic!("no frame count on stdout: {stdout}"));
    let clones = stderr
        .lines()
        .find_map(|l| l.split("program-table-cow: clones=").nth(1))
        .and_then(|v| v.trim().parse().ok())
        .unwrap_or_else(|| panic!("no program-table-cow line in stats: {stderr}"));
    (parsed, clones)
}

#[test]
fn spawning_more_thread_clones_does_not_copy_the_program_tables_again() {
    let (few_parsed, few_clones) = run(4);
    let (many_parsed, many_clones) = run(24);
    assert_eq!(few_parsed, 4, "parser did not emit one request per frame");
    assert_eq!(many_parsed, 24, "parser did not emit one request per frame");
    // Both are 0 today. The assertion is deliberately about *growth*, not about
    // zero: a table copy that happens a bounded number of times during module
    // loading is fine, one per frame is the bug.
    assert!(
        many_clones <= few_clones + 2,
        "program-table COW clones grew with the frame count: {few_clones} for 4 frames, \
         {many_clones} for 24 -- a per-frame path is taking a mutable borrow of a shared \
         program table (see runtime::cow_table_mut)"
    );
}
