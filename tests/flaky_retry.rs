//! Integration coverage for the flaky-test quarantine engine
//! (`scripts/flaky-retry.sh`, `scripts/check-flaky-list.sh`).
//!
//! The engine is what keeps a known-random test from blocking a merge, so its
//! two boundaries have to hold exactly: a quarantined file gets re-rolled, and
//! everything else -- including a quarantined file that fails every attempt --
//! still fails the build. Getting that wrong in either direction is expensive:
//! too little and CI stays noisy, too much and CI stops being a gate at all.
//!
//! Policy: docs/flaky-test-policy.md.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// A fake test command that fails its first `fail_first` invocations and passes
/// afterwards, counting invocations in `counter`.
fn write_fake_test(dir: &Path, fail_first: u32) -> PathBuf {
    let counter = dir.join("count");
    let script = dir.join("fake-test.sh");
    fs::write(
        &script,
        format!(
            "#!/bin/bash\n\
             n=$(cat '{c}' 2>/dev/null || echo 0)\n\
             n=$((n+1)); echo $n > '{c}'\n\
             echo '1..1'\n\
             if [ \"$n\" -le {f} ]; then echo \"not ok 1 - attempt $n\"; exit 1; fi\n\
             echo \"ok 1 - attempt $n\"\n",
            c = counter.display(),
            f = fail_first
        ),
    )
    .expect("write fake test");
    let mut perms = fs::metadata(&script).unwrap().permissions();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        perms.set_mode(0o755);
    }
    fs::set_permissions(&script, perms).unwrap();
    script
}

fn attempts(dir: &Path) -> u32 {
    fs::read_to_string(dir.join("count"))
        .map(|s| s.trim().parse().unwrap_or(0))
        .unwrap_or(0)
}

/// Run flaky-retry.sh for `test_path`, with `list_body` as the ledger.
fn run_retry(dir: &Path, test_path: &str, list_body: &str, fail_first: u32) -> (String, bool, u32) {
    let script = write_fake_test(dir, fail_first);
    let list = dir.join("flaky-tests.txt");
    fs::write(&list, list_body).expect("write ledger");

    let out = Command::new(repo_root().join("scripts/flaky-retry.sh"))
        .current_dir(repo_root())
        .arg(test_path)
        .arg(&script)
        .env("FLAKY_LIST", &list)
        .env("FLAKY_RETRY_LOG", dir.join("retries.log"))
        .output()
        .expect("failed to spawn flaky-retry.sh");

    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        out.status.success(),
        attempts(dir),
    )
}

fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("mutsu-flaky-retry-{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("create scratch dir");
    dir
}

/// A file that is NOT in the ledger runs exactly once. Quarantine has to be an
/// explicit decision; an unlisted regression must fail on its first attempt.
#[test]
fn unlisted_test_is_never_retried() {
    let dir = scratch("unlisted");
    let (stdout, ok, attempts) = run_retry(
        &dir,
        "t/not-listed.t",
        "roast/S02-types/baghash.t 2026-07-24 2026-10-22 unrelated\n",
        1,
    );
    assert!(!ok, "an unlisted failing test must fail the run");
    assert_eq!(attempts, 1, "unlisted tests get exactly one attempt");
    assert!(
        stdout.contains("not ok 1"),
        "the failing TAP must be streamed through, got: {stdout}"
    );
}

/// A quarantined file that fails once and then passes reports success, and says
/// so in the TAP stream rather than hiding the re-roll.
#[test]
fn quarantined_test_is_retried_until_it_passes() {
    let dir = scratch("retried");
    let (stdout, ok, attempts) = run_retry(
        &dir,
        "t/known-flaky.t",
        "t/known-flaky.t 2026-07-24 2026-10-22 synthetic entry for the test suite\n",
        2,
    );
    assert!(ok, "a retry that passes must succeed; got: {stdout}");
    assert_eq!(attempts, 3, "should have taken all three attempts");
    assert!(
        stdout.contains("# flaky-retry: t/known-flaky.t passed on attempt 3/3"),
        "the retry must be announced in the output, got: {stdout}"
    );
    assert!(
        stdout.contains("ok 1 - attempt 3") && !stdout.contains("not ok"),
        "only the passing attempt's TAP may reach prove, got: {stdout}"
    );

    let log = fs::read_to_string(dir.join("retries.log")).expect("retry log");
    assert_eq!(
        log.lines().count(),
        2,
        "both failed attempts must be logged for the CI summary, got: {log}"
    );
}

/// Quarantine is a re-roll, not immunity: a test that fails every attempt still
/// fails the build. This is the property that keeps the ledger from turning CI
/// advisory.
#[test]
fn quarantined_test_that_always_fails_still_fails() {
    let dir = scratch("always-fails");
    let (stdout, ok, attempts) = run_retry(
        &dir,
        "t/known-flaky.t",
        "t/known-flaky.t 2026-07-24 2026-10-22 synthetic entry for the test suite\n",
        99,
    );
    assert!(!ok, "exhausting the attempts must fail the run");
    assert_eq!(attempts, 3, "should stop at FLAKY_MAX_ATTEMPTS");
    assert!(
        stdout.contains("FAILED all 3 attempts"),
        "the exhaustion must be explicit in the log, got: {stdout}"
    );
    assert!(
        stdout.contains("not ok 1"),
        "the last attempt's diagnostic output must survive, got: {stdout}"
    );
}

/// The ledger checker rejects an entry whose review date has passed, so a
/// quarantine cannot become permanent by inattention.
#[test]
fn check_flaky_list_rejects_an_expired_entry() {
    let dir = scratch("expired");
    let list = dir.join("flaky-tests.txt");
    fs::write(
        &list,
        "roast/S02-types/baghash.t 2026-01-01 2026-03-01 expired on purpose\n",
    )
    .unwrap();

    let out = Command::new(repo_root().join("scripts/check-flaky-list.sh"))
        .current_dir(repo_root())
        .arg(&list)
        .env("FLAKY_TODAY", "2026-07-24")
        .output()
        .expect("failed to spawn check-flaky-list.sh");

    assert!(
        !out.status.success(),
        "an expired entry must fail the check"
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("expired on 2026-03-01"),
        "the error must name the expiry, got: {stderr}"
    );
}

/// The repo's real ledger must always be valid: parseable, sorted, pointing at
/// files that exist and (for roast) are actually on the whitelist, unexpired.
#[test]
fn repo_flaky_list_is_valid() {
    let out = Command::new(repo_root().join("scripts/check-flaky-list.sh"))
        .current_dir(repo_root())
        .output()
        .expect("failed to spawn check-flaky-list.sh");
    assert!(
        out.status.success(),
        "flaky-tests.txt is invalid:\n{}",
        String::from_utf8_lossy(&out.stderr)
    );
}
