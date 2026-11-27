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
//!
//! It also packs the binaries' relative relocations (`-z pack-relative-relocs`,
//! DT_RELR): see [`pack_relative_relocs`].

use std::env;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    pack_relative_relocs();
    startup_layout();
}

/// Link the binaries with their relative relocations packed into DT_RELR.
///
/// A PIE `mutsu` carries ~80,000 `R_X86_64_RELATIVE` relocations (vtables,
/// string tables, static slices), a 1.9 MB `.rela.dyn` the dynamic loader
/// reads at every start, about half of a `say "hello"`'s instructions. DT_RELR
/// stores the same relocations as a bitmap of a few dozen KB. The binary stays
/// a PIE with full RELRO and `BIND_NOW`: only the encoding of the table
/// changes, not what is relocated or protected (#19197).
///
/// The loader must understand DT_RELR, which glibc does from 2.36 (the
/// `GLIBC_ABI_DT_RELR` symbol version). A linker does not check that the libc
/// it links against has it, so this is applied only to a native Linux/glibc
/// build on a host whose glibc is 2.36 or newer: the binary then needs no newer
/// glibc than the symbols it already links. `MUTSU_RELR=0` turns it off.
fn pack_relative_relocs() {
    println!("cargo:rerun-if-env-changed=MUTSU_RELR");
    if env::var("MUTSU_RELR").as_deref() == Ok("0") {
        return;
    }
    let target = env::var("TARGET").unwrap_or_default();
    if !target.ends_with("-linux-gnu") || env::var("HOST").as_deref() != Ok(target.as_str()) {
        return;
    }
    if !host_glibc_has_dt_relr() {
        return;
    }
    println!("cargo:rustc-link-arg-bins=-Wl,-z,pack-relative-relocs");
}

/// Whether the glibc this build script runs on is 2.36 or newer.
#[cfg(all(target_os = "linux", target_env = "gnu"))]
fn host_glibc_has_dt_relr() -> bool {
    unsafe extern "C" {
        fn gnu_get_libc_version() -> *const std::ffi::c_char;
    }
    // SAFETY: `gnu_get_libc_version` takes no arguments and returns a pointer
    // to a static NUL-terminated string that glibc owns for the whole process.
    let version = unsafe { std::ffi::CStr::from_ptr(gnu_get_libc_version()) };
    let mut parts = version.to_str().unwrap_or("").split('.');
    let major: u32 = parts.next().and_then(|p| p.parse().ok()).unwrap_or(0);
    let minor: u32 = parts.next().and_then(|p| p.parse().ok()).unwrap_or(0);
    (major, minor) >= (2, 36)
}

#[cfg(not(all(target_os = "linux", target_env = "gnu")))]
fn host_glibc_has_dt_relr() -> bool {
    false
}

/// The startup text layout (see the module docs).
fn startup_layout() {
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
