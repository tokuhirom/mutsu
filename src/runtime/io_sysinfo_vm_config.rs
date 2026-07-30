//! `$*VM.config` — the build-configuration hash MoarVM exposes.
//!
//! Raku modules read this hash for two very different kinds of information:
//!
//! 1. *Facts about the running VM* (`osname`, `be`, `nativecall_backend`) used
//!    to branch platform-specific code. `NativeLibs` switches its whole
//!    library-naming scheme on `config<osname>`, so a missing key silently
//!    disables every platform branch rather than failing loudly.
//! 2. *The C toolchain the VM itself was built with* (`cc`, `cflags`, `ld`, …).
//!    `NativeLibs::Compile` joins these into a `shell()` command line to build
//!    a companion `.so` for a NativeCall binding. mutsu is not built by a C
//!    compiler in the way MoarVM is, so there is no recorded build config to
//!    echo back; report a working *host* toolchain instead — that is what the
//!    consumers actually need the keys for, and `CC`/`LD`/`CFLAGS` from the
//!    environment still win so a cross/alternate toolchain can be selected.
//!
//! Everything is a `Str`, matching MoarVM (even `be`, which is `"0"`/`"1"`).

use crate::value::Value;
use std::collections::HashMap;

/// The `osname` MoarVM reports: its own build-time OS name, not uname's.
pub(crate) fn osname() -> &'static str {
    match std::env::consts::OS {
        "macos" => "darwin",
        "windows" => "mswin32",
        other => other,
    }
}

fn env_or(key: &str, fallback: &str) -> String {
    std::env::var(key)
        .ok()
        .filter(|v| !v.trim().is_empty())
        .unwrap_or_else(|| fallback.to_string())
}

/// Build the `$*VM.config` hash.
pub(crate) fn vm_config() -> HashMap<String, Value> {
    let os = std::env::consts::OS;
    let is_darwin = os == "macos";
    let is_windows = os == "windows";

    // Toolchain defaults per platform. `cc`/`ld` are the same driver on unix;
    // MoarVM records the shared-library flags separately because linking a
    // loadable module needs `-shared` (`-dynamiclib` on darwin) on top of the
    // position-independent compile flags.
    let (cc, ccshared, ldshared, obj, out) = if is_windows {
        ("cl", "", "/LD", ".obj", "/Fe")
    } else if is_darwin {
        (
            "cc",
            "-fPIC",
            "-dynamiclib -undefined dynamic_lookup",
            ".o",
            "-o ",
        )
    } else {
        ("cc", "-fPIC", "-shared -fPIC", ".o", "-o ")
    };

    let mut config = HashMap::new();
    let mut set = |k: &str, v: String| {
        config.insert(k.to_string(), Value::str(v));
    };

    set("name", "mutsu".to_string());
    set("osname", osname().to_string());
    // be: 0 for little-endian, 1 for big-endian.
    set(
        "be",
        if cfg!(target_endian = "big") {
            "1"
        } else {
            "0"
        }
        .to_string(),
    );
    // nativecall_backend names the FFI implementation behind NativeCall.
    // Modules branch on it to decide whether the dyncall-only extensions are
    // available (`NativeLibs` does `$*VM.config<nativecall_backend> eq
    // 'dyncall'`); mutsu's is libffi, which is also what a modern MoarVM
    // reports, and reading it must not warn about an undefined value.
    set("nativecall_backend", "libffi".to_string());

    // --- C toolchain (see the module docs for why these are host values) ---
    set("cc", env_or("CC", cc));
    set("ccswitch", "-c".to_string());
    set("ccshared", env_or("CFLAGS_SHARED", ccshared));
    set("ccout", out.to_string());
    set("cflags", env_or("CFLAGS", "-O2"));
    set("obj", obj.to_string());
    set("exe", if is_windows { ".exe" } else { "" }.to_string());
    set("ld", env_or("LD", cc));
    set("ldshared", env_or("LDFLAGS_SHARED", ldshared));
    set("ldflags", env_or("LDFLAGS", ""));
    set(
        "ldlibs",
        env_or("LIBS", if is_windows { "" } else { "-lm" }),
    );
    set("ldout", out.to_string());
    let load_ext = if is_windows {
        ".dll"
    } else if is_darwin {
        ".dylib"
    } else {
        ".so"
    };
    // `dll` is a *sprintf pattern* mapping a bare library name to its shared
    // object filename (MoarVM records `lib%s.so`); `$*VM.platform-library-name`
    // is defined in terms of it.
    set(
        "dll",
        if is_windows {
            "%s.dll".to_string()
        } else {
            format!("lib%s{load_ext}")
        },
    );

    config
}
