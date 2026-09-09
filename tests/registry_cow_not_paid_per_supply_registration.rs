//! Pins the read-guard fast path in `Interpreter::class_has_method` /
//! `class_has_user_method` (#7667).
//!
//! The declaration registry is copy-on-write behind an `Arc`
//! (`RegistryWriteGuard::deref_mut`): the first *mutable* deref after any other
//! holder shares the `Arc` deep-clones the whole `Registry` -- every class,
//! role, method entry and function key, with all their `String` keys. A
//! `supply` block that registers a `whenever` clones the interpreter for the
//! callback (`clone_for_thread_excluding`), and that clone holds a share, so
//! from then on *any* registry write pays a full copy.
//!
//! `class_has_method` / `class_has_user_method` ask a pure question, but used
//! to ask it through `registry_mut()`. Cro's HTTP/2 request parser reaches
//! `class_has_user_method` once per HEADERS frame (the `self!set-headers`
//! private dispatch) and registers one `whenever` per stream, so the two
//! interleaved: one full registry deep clone per frame, 8.7% of the frame's
//! instructions. Both now consult `class_mro_readonly` under a read guard and
//! only fall back to the write side when that declines.
//!
//! A regression shows up as `registry-cow: clones=` growing with the frame
//! count instead of staying flat.

use std::process::Command;

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

/// `(requests parsed, registry COW clones)` for a run that feeds `frames`
/// HEADERS frames through the HTTP/2 request parser.
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
        .find_map(|l| l.split("registry-cow: clones=").nth(1))
        .and_then(|v| v.trim().parse().ok())
        .unwrap_or_else(|| panic!("no registry-cow line in stats: {stderr}"));
    (parsed, clones)
}

#[test]
fn parsing_more_http2_headers_frames_does_not_clone_the_registry_more() {
    let (few_parsed, few_clones) = run(4);
    let (many_parsed, many_clones) = run(24);
    assert_eq!(few_parsed, 4, "parser did not emit one request per frame");
    assert_eq!(many_parsed, 24, "parser did not emit one request per frame");
    // Before the fix this was `frames + 1` on both runs (5 and 25). The bound
    // is deliberately loose: what must not happen is *growth with the frame
    // count*, not any particular small constant.
    assert!(
        many_clones <= few_clones + 2,
        "registry COW clones grew with the frame count: {few_clones} for 4 frames, \
         {many_clones} for 24 -- a pure-read dispatch probe is taking registry_mut() again"
    );
    assert!(
        many_clones < 10,
        "registry COW clones should stay a small constant, got {many_clones}"
    );
}
