//! `mzef` — the bundled package manager shim.
//!
//! mutsu ships the real [Zef](https://github.com/ugexe/zef) (vendored under
//! `vendor/zef/`, Artistic-2.0) as its package manager. `mzef` is a thin shim:
//! it locates the `mutsu` interpreter and the vendored zef tree, then runs
//!
//! ```text
//! mutsu -I <zef>/lib <zef>/bin/zef <args...>
//! ```
//!
//! so that `mzef install <dist>`, `mzef info <dist>`, etc. behave exactly like
//! invoking upstream zef under mutsu. Config resolves from the vendored
//! `resources/config.json` via `%?RESOURCES<config.json>` (or the usual
//! `$XDG_CONFIG_HOME/zef/config.json` override); installs land in mutsu's
//! default site repository under `$HOME`.
//!
//! Resolution can be overridden for testing/packaging with the environment
//! variables `MZEF_MUTSU_BIN` (path to the interpreter) and `MZEF_ZEF_HOME`
//! (path to the vendored zef tree containing `bin/zef` and `lib/`).

use std::path::{Path, PathBuf};
use std::process::Command;

/// Locate the vendored zef tree. Returns a directory containing `bin/zef` and
/// `lib/`. Tries, in order: the `MZEF_ZEF_HOME` override, then a set of
/// candidate layouts relative to this executable (installed FHS-style layout
/// first, then the in-repo `vendor/zef` dev layout).
fn find_zef_home(exe_dir: &Path) -> Option<PathBuf> {
    if let Ok(env_home) = std::env::var("MZEF_ZEF_HOME") {
        let p = PathBuf::from(env_home);
        if p.join("bin").join("zef").is_file() {
            return Some(p);
        }
    }
    // Candidate layouts relative to the mzef executable's directory:
    //   installed:  <prefix>/bin/mzef  -> <prefix>/share/mutsu/zef
    //               <prefix>/bin/mzef  -> <prefix>/lib/mutsu/zef
    //   dev build:  target/<profile>/mzef -> <repo>/vendor/zef
    let candidates = [
        "../share/mutsu/zef",
        "../lib/mutsu/zef",
        "../../vendor/zef",
        "../../../vendor/zef",
        "vendor/zef",
    ];
    for rel in candidates {
        let p = exe_dir.join(rel);
        if p.join("bin").join("zef").is_file() {
            // Canonicalize so the `-I` / script paths handed to mutsu are
            // clean absolute paths regardless of the `..` traversal above.
            return Some(p.canonicalize().unwrap_or(p));
        }
    }
    None
}

/// Locate the `mutsu` interpreter binary. Tries the `MZEF_MUTSU_BIN` override,
/// then a sibling `mutsu` next to this `mzef` executable, then falls back to
/// `mutsu` on `PATH`.
fn find_mutsu_bin(exe_dir: &Path) -> PathBuf {
    if let Ok(env_bin) = std::env::var("MZEF_MUTSU_BIN") {
        return PathBuf::from(env_bin);
    }
    let sibling = exe_dir.join(if cfg!(windows) { "mutsu.exe" } else { "mutsu" });
    if sibling.is_file() {
        return sibling;
    }
    PathBuf::from("mutsu")
}

fn main() {
    let exe = std::env::current_exe().unwrap_or_else(|e| {
        eprintln!("mzef: cannot determine own executable path: {e}");
        std::process::exit(2);
    });
    let exe_dir = exe.parent().unwrap_or_else(|| Path::new(".")).to_path_buf();

    let zef_home = find_zef_home(&exe_dir).unwrap_or_else(|| {
        eprintln!(
            "mzef: could not locate the vendored zef tree.\n\
             Looked relative to {} and in $MZEF_ZEF_HOME.\n\
             Expected a directory containing bin/zef and lib/ (e.g. <prefix>/share/mutsu/zef).",
            exe_dir.display()
        );
        std::process::exit(2);
    });

    let mutsu_bin = find_mutsu_bin(&exe_dir);
    let zef_lib = zef_home.join("lib");
    let zef_script = zef_home.join("bin").join("zef");

    let mut cmd = Command::new(&mutsu_bin);
    cmd.arg("-I")
        .arg(&zef_lib)
        .arg(&zef_script)
        .args(std::env::args_os().skip(1));

    // Replace this process with the interpreter so signals, exit codes, and
    // stdio pass through transparently (a package manager runs long shell-outs
    // and must forward Ctrl-C cleanly).
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        let err = cmd.exec(); // only returns on failure
        eprintln!(
            "mzef: failed to exec interpreter '{}': {err}",
            mutsu_bin.display()
        );
        std::process::exit(2);
    }
    #[cfg(not(unix))]
    {
        match cmd.status() {
            Ok(status) => std::process::exit(status.code().unwrap_or(1)),
            Err(err) => {
                eprintln!(
                    "mzef: failed to run interpreter '{}': {err}",
                    mutsu_bin.display()
                );
                std::process::exit(2);
            }
        }
    }
}
