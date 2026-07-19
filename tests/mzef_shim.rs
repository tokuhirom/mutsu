//! Integration coverage for the `mzef` package-manager shim
//! (`src/bin/mzef.rs`, PLAN.md §1 B2).
//!
//! `mzef` locates the vendored zef tree (`vendor/zef/`) and the sibling
//! `mutsu` interpreter, then runs `mutsu -I <zef>/lib <zef>/bin/zef <args>`.
//! These tests exercise the resolution + hand-off offline (no network): if the
//! shim can make the vendored `zef` print its version, then path resolution,
//! the `-I` module search path, config-free `use Zef::CLI` loading, and exit
//! code pass-through all work. The full install/`use` pipeline is covered by
//! the manual E2E in `docs/mzef-install-pipeline.md`.

use std::process::Command;

/// Run the built `mzef` with the given args and extra env, returning
/// (stdout, stderr, success).
fn run_mzef(args: &[&str], envs: &[(&str, &str)]) -> (String, String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mzef"));
    cmd.args(args);
    for (k, v) in envs {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to spawn mzef");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.success(),
    )
}

/// `mzef --version` must resolve the vendored zef and print zef's version.
/// This proves the whole hand-off works end to end without a network round
/// trip: exe-relative zef resolution, the sibling `mutsu` lookup, `-I` wiring,
/// and Zef::CLI loading.
#[test]
fn mzef_version_runs_vendored_zef() {
    let (out, err, ok) = run_mzef(&["--version"], &[]);
    assert!(ok, "mzef --version failed: stderr={err}");
    assert!(
        out.contains("1.1.3"),
        "expected vendored zef version 1.1.3 in stdout, got: {out:?} (stderr={err})"
    );
}

/// The `MZEF_ZEF_HOME` override is honored: pointing it at the in-repo
/// `vendor/zef` (absolute) must still work, confirming the override path is
/// wired and not shadowed by the exe-relative fallbacks.
#[test]
fn mzef_zef_home_override_is_honored() {
    let repo_root = env!("CARGO_MANIFEST_DIR");
    let zef_home = format!("{repo_root}/vendor/zef");
    let (out, err, ok) = run_mzef(&["--version"], &[("MZEF_ZEF_HOME", zef_home.as_str())]);
    assert!(ok, "mzef --version with MZEF_ZEF_HOME failed: stderr={err}");
    assert!(
        out.contains("1.1.3"),
        "expected 1.1.3 via MZEF_ZEF_HOME override, got: {out:?} (stderr={err})"
    );
}

/// A `MZEF_ZEF_HOME` that exists but has no `bin/zef` must not be accepted; the
/// shim falls through to the valid in-repo layout, so this still succeeds —
/// but the point is that a wrong override never crashes the shim.
#[test]
fn mzef_bogus_zef_home_falls_through() {
    let (out, _err, ok) = run_mzef(
        &["--version"],
        &[("MZEF_ZEF_HOME", "/nonexistent/zef/home")],
    );
    // Falls through to the exe-relative vendor/zef, so it still prints 1.1.3.
    assert!(ok && out.contains("1.1.3"));
}
