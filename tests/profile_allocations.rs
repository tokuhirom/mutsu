//! Source-line allocation attribution for the opt-in `alloc-stats` build.
//!
//! The test is feature-gated because the normal binary deliberately has no
//! counting allocator. It checks the published profile document rather than
//! the allocator's internal tables.

#![cfg(feature = "alloc-stats")]

use std::path::Path;
use std::process::Command;

#[test]
fn allocation_profile_attributes_totals_to_source_lines() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let pid = std::process::id();
    let source = root.join("tmp").join(format!("profile-alloc-{pid}.raku"));
    let output = root.join("tmp").join(format!("profile-alloc-{pid}.json"));
    std::fs::write(
        &source,
        "my $s = '';\nfor ^20 {\n    $s ~= 'x';\n}\nsay $s.chars;\n",
    )
    .expect("cannot write allocation fixture");

    let result = Command::new(env!("CARGO_BIN_EXE_mutsu"))
        .arg(format!("--profile={}", output.display()))
        .arg("--profile-report=json")
        .arg(&source)
        .env("MUTSU_ALLOC_STATS", "1")
        .env("MUTSU_PROFILE", "1")
        .env("MUTSU_JIT", "off")
        .output()
        .expect("failed to spawn mutsu");
    assert!(
        result.status.success(),
        "allocation profile failed: {}",
        String::from_utf8_lossy(&result.stderr)
    );

    let document: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&output).expect("allocation profile was not written"),
    )
    .expect("allocation profile is not JSON");
    assert_eq!(document["header"]["allocation_stats"], true);
    assert!(document["header"]["sampling"].is_null());

    let lines = document["files"][0]["lines"]
        .as_array()
        .expect("profile has no line rows");
    let body = lines
        .iter()
        .find(|line| line["line"] == 3)
        .expect("the allocation-producing body line is absent");
    assert!(body["allocations"]["count"].as_u64().unwrap_or(0) > 0);
    assert!(body["allocations"]["bytes"].as_u64().unwrap_or(0) > 0);
    assert!(body["self_us"].is_null());

    let _ = std::fs::remove_file(source);
    let _ = std::fs::remove_file(output);
}
