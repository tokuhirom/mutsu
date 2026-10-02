//! Link the `mutsu` executable as a non-PIE binary on Linux/glibc.
//!
//! A PIE executable is relocated by the dynamic loader on every start: mutsu's
//! static tables (vtables, `&'static str` slices, function-pointer tables) hold
//! ~49,000 absolute addresses, so the loader rewrote ~930 KiB of
//! `.data.rel.ro` before `main`, and each of those ~230 pages took a
//! copy-on-write fault. Linked at a fixed address the loader has nothing to
//! relocate and those pages stay clean, file-backed mappings. Measured on a
//! `say 1` script that removed ~230 of ~930 page faults per process and
//! roughly 0.5-1 ms of the ~6-7 ms startup (see
//! `news/2026-10/startup-skips-qualified-index-and-relocations.md`).
//!
//! Scope, deliberately narrow:
//! - only the `mutsu` binary (`rustc-link-arg-bin`): the library is also built
//!   as a `cdylib`, which must stay position-independent, and `mzef` only
//!   re-execs `mutsu`;
//! - only Linux with glibc, whose `cc` driver accepts `-no-pie` after rustc's
//!   own `-pie` (the last one wins). macOS requires PIE executables (arm64
//!   refuses anything else), and wasm has no such link step.
//!
//! The object code is still compiled position-independent; only the final link
//! fixes the load address, so the shared libraries mutsu loads keep their ASLR.
fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    let os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    let env = std::env::var("CARGO_CFG_TARGET_ENV").unwrap_or_default();
    if os == "linux" && env == "gnu" {
        println!("cargo:rustc-link-arg-bin=mutsu=-no-pie");
    }
}
