//! Offline end-to-end coverage of the `mzef` install pipeline
//! (`docs/mzef-install-pipeline.md`).
//!
//! `tests/mzef_shim.rs` only proves the shim reaches zef (`--version`). This
//! drives the real vendored zef through a whole package lifecycle against a
//! local fixture distribution (`tests/fixtures/mzef-dist`), whose own test
//! file `use`s the distribution itself:
//!
//! 1. `mzef install <path>` with tests ON — stage, run the dist's tests against
//!    the staged repository (`-I CompUnit::Repository::Staging#...`, #19246),
//!    install into the site repository of a scratch `HOME`;
//! 2. `use` the installed module from a fresh `mutsu`;
//! 3. `mzef list --installed` names it;
//! 4. `mzef uninstall` takes it out, and `use` then fails (#19248).
//!
//! Nothing here needs the network: a path install resolves no ecosystem, and
//! the proxy variables point at a closed port so an accidental fetch fails
//! instead of reaching out.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

const FIXTURE_IDENTITY: &str = "Mzef::E2E::Fixture:ver<0.0.1>:auth<zef:mutsu>";

/// A scratch directory under cargo's per-target tmp dir, emptied first.
fn scratch_dir(name: &str) -> PathBuf {
    let dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join(name);
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("create scratch dir");
    dir
}

/// Copy the fixture distribution, so nothing zef writes lands in the source tree.
fn copy_tree(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).expect("create fixture copy dir");
    for entry in std::fs::read_dir(from).expect("read fixture dir") {
        let entry = entry.expect("fixture dir entry");
        let target = to.join(entry.file_name());
        if entry.file_type().expect("fixture file type").is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), &target).expect("copy fixture file");
        }
    }
}

/// Run `exe args` with a scratch `HOME`/`TMPDIR` and no usable network.
fn run_in(exe: &str, args: &[&str], home: &Path, cwd: &Path) -> Output {
    let tmp = home.join("tmp");
    std::fs::create_dir_all(&tmp).expect("create scratch TMPDIR");
    let mut cmd = Command::new(exe);
    cmd.args(args)
        .current_dir(cwd)
        .env("HOME", home)
        .env("TMPDIR", &tmp)
        .env_remove("MUTSULIB")
        .env_remove("RAKULIB");
    for var in [
        "http_proxy",
        "https_proxy",
        "HTTP_PROXY",
        "HTTPS_PROXY",
        "all_proxy",
    ] {
        cmd.env(var, "http://127.0.0.1:9");
    }
    cmd.output().expect("failed to spawn")
}

fn text(out: &Output) -> String {
    format!(
        "status={:?}\n--- stdout\n{}\n--- stderr\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

#[test]
fn mzef_installs_tests_lists_and_uninstalls_a_local_dist() {
    let mzef = env!("CARGO_BIN_EXE_mzef");
    let mutsu = env!("CARGO_BIN_EXE_mutsu");
    let root = scratch_dir("mzef-e2e");
    let home = root.join("home");
    std::fs::create_dir_all(&home).expect("create scratch HOME");
    let dist = root.join("dist");
    copy_tree(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/mzef-dist"),
        &dist,
    );
    let dist_arg = dist.to_str().expect("utf-8 scratch path");
    let use_it = [
        "-e",
        "use Mzef::E2E::Fixture; print fixture-greeting('e2e')",
    ];

    // Not installed yet: `use` fails.
    let before = run_in(mutsu, &use_it, &home, &root);
    assert!(
        !before.status.success(),
        "module loads before install:\n{}",
        text(&before)
    );

    // 1. Install with tests ON.
    let install = run_in(mzef, &["install", dist_arg], &home, &root);
    let log = text(&install);
    assert!(install.status.success(), "mzef install failed:\n{log}");
    assert!(
        log.contains(&format!("Testing [OK] for {FIXTURE_IDENTITY}")),
        "the dist's own tests did not pass:\n{log}"
    );
    assert!(
        log.contains(&format!("Installing: {FIXTURE_IDENTITY}")),
        "no install step:\n{log}"
    );

    // 2. The installed module loads.
    let loaded = run_in(mutsu, &use_it, &home, &root);
    assert!(
        loaded.status.success(),
        "installed module does not load:\n{}",
        text(&loaded)
    );
    assert_eq!(String::from_utf8_lossy(&loaded.stdout), "hello, e2e");

    // 3. It is listed.
    let list = run_in(mzef, &["list", "--installed"], &home, &root);
    assert!(
        list.status.success(),
        "mzef list --installed failed:\n{}",
        text(&list)
    );
    assert!(
        String::from_utf8_lossy(&list.stdout).contains(FIXTURE_IDENTITY),
        "installed dist not listed:\n{}",
        text(&list)
    );

    // 4. Uninstall, and it no longer loads.
    let uninstall = run_in(mzef, &["uninstall", "Mzef::E2E::Fixture"], &home, &root);
    assert!(
        uninstall.status.success(),
        "mzef uninstall failed:\n{}",
        text(&uninstall)
    );
    let after = run_in(mutsu, &use_it, &home, &root);
    assert!(
        !after.status.success(),
        "module still loads after uninstall:\n{}",
        text(&after)
    );
    let after_err = String::from_utf8_lossy(&after.stderr);
    assert!(
        after_err.contains("Could not find Mzef::E2E::Fixture"),
        "unexpected failure after uninstall:\n{}",
        text(&after)
    );
    let list = run_in(mzef, &["list", "--installed"], &home, &root);
    assert!(
        !String::from_utf8_lossy(&list.stdout).contains(FIXTURE_IDENTITY),
        "uninstalled dist still listed:\n{}",
        text(&list)
    );

    let _ = std::fs::remove_dir_all(&root);
}

/// A dist whose tests fail aborts the install with a non-zero status through
/// zef's `MAIN` `CATCH` → `&*EXIT(1)` (#19247), and installs nothing.
#[test]
fn mzef_install_with_failing_tests_exits_nonzero() {
    let mzef = env!("CARGO_BIN_EXE_mzef");
    let mutsu = env!("CARGO_BIN_EXE_mutsu");
    let root = scratch_dir("mzef-e2e-failing");
    let home = root.join("home");
    std::fs::create_dir_all(&home).expect("create scratch HOME");
    let dist = root.join("dist");
    copy_tree(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/mzef-dist"),
        &dist,
    );
    std::fs::write(
        dist.join("t/02-fails.rakutest"),
        "use Test;\nplan 1;\nok False, 'this distribution fails its tests';\n",
    )
    .expect("write failing test");

    let install = run_in(mzef, &["install", dist.to_str().unwrap()], &home, &root);
    let log = text(&install);
    assert_eq!(install.status.code(), Some(1), "expected exit 1:\n{log}");
    assert!(
        log.contains("Testing [FAIL]"),
        "no test failure reported:\n{log}"
    );
    assert!(
        !log.contains("Unknown function"),
        "&*EXIT did not resolve:\n{log}"
    );

    let loaded = run_in(mutsu, &["-e", "use Mzef::E2E::Fixture"], &home, &root);
    assert!(
        !loaded.status.success(),
        "a dist that failed its tests was installed"
    );

    let _ = std::fs::remove_dir_all(&root);
}
