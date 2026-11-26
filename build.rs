//! Build script: the startup text layout of the `mutsu` binary (#19197).
//!
//! Interpreter start-up executes ~1,800 functions spread over the whole
//! ~50 MB text segment, so a `say "hello"` used to fault in ~400 distinct
//! 64 KiB windows of mutsu's own code — kernel time that Raku++'s small static
//! binary never pays. `linker/startup-text.ld` lists those functions as
//! input-section patterns and gathers them into one output section in front
//! of `.text`, so they share a few dozen windows instead.
//!
//! The script only changes *where* code is placed, never what is linked: a
//! pattern that matches nothing (a renamed function, an older layout file) is
//! simply ignored, so a stale file degrades to the old layout and never breaks
//! the build. `scripts/gen-startup-layout.py` regenerates it.
//!
//! Applied only where the layout is known to work: an optimized build of the
//! `x86_64-unknown-linux-gnu` target with its default linker (rustc's bundled
//! `rust-lld`, which supports `INSERT BEFORE`). A custom linker (mold rejects
//! `SECTIONS`) or `MUTSU_STARTUP_LAYOUT=0` turns it off.

use std::env;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-env-changed=MUTSU_STARTUP_LAYOUT");
    println!("cargo:rerun-if-env-changed=CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER");
    // The layout file itself is tracked by `src/main.rs` (`include_bytes!`),
    // so editing it relinks the binary without rebuilding the library.

    if env::var("MUTSU_STARTUP_LAYOUT").as_deref() == Ok("0") {
        return;
    }
    if env::var("TARGET").as_deref() != Ok("x86_64-unknown-linux-gnu") {
        return;
    }
    // A debug build runs different (unoptimized, uninlined) code; the layout
    // is generated from, and only meaningful for, an optimized build.
    if env::var("OPT_LEVEL").as_deref() == Ok("0") {
        return;
    }
    if env::var_os("CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER").is_some() {
        return;
    }
    let rustflags = env::var("CARGO_ENCODED_RUSTFLAGS").unwrap_or_default();
    if rustflags.contains("fuse-ld") || rustflags.contains("linker") {
        return;
    }
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR");
    // `-Xlinker` rather than `-Wl,` so a comma in the checkout path is not
    // split into separate linker arguments.
    for arg in [
        "-Xlinker".to_string(),
        "-T".to_string(),
        "-Xlinker".to_string(),
        format!("{manifest_dir}/linker/startup-text.ld"),
    ] {
        println!("cargo:rustc-link-arg-bin=mutsu={arg}");
    }
}
