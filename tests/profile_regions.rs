//! ADR-0106 Slice 4 — the subsystem split, asserted the only way D5 permits.
//!
//! A region tag answers "which part of *mutsu* consumed this Raku line's
//! time": call resolution, the regex walk, a native builtin, an `nqp::` op, the
//! parser. The tag is carried by a sample, so its **nanoseconds are sampled**
//! and nothing here asserts one — the same rule the rest of the profiler's
//! tests follow, and for the same reason: a duration assertion is a flaky test
//! by construction.
//!
//! What *is* assertable is the structure, and `MUTSU_PROFILE_TICK=every-poll`
//! is what makes it so. In that mode a tick is pending in every gap between
//! polls, so the region that claims it is a function of the executed bytecode
//! alone — which region rows a run prints, and how many samples each carries,
//! are then reproducible run to run, under load, and across build profiles.
//! [`region_rows_are_a_function_of_the_bytecode`] pins exactly that property,
//! because it is the one that makes every other assertion here legitimate.

mod profile_doc;

use profile_doc::{Profiled, fixture_path, profile};

/// The closed set of tags (`src/profile/region.rs`). A row naming anything
/// else — an `unknown` above all — means the coverage claim in ADR-0106
/// Slice 4's acceptance has a hole in it.
const KNOWN_REGIONS: &[&str] = &[
    "interp",
    "call-resolve",
    "method-dispatch",
    "native-builtin",
    "nqp",
    "regex",
    "parse",
    "gc",
];

/// Run `path` under the deterministic tick and return its document.
fn profile_regions(path: &std::path::Path) -> Profiled {
    let run = profile(path, &[("MUTSU_PROFILE_TICK", "every-poll")]);
    assert!(run.samples() > 0, "the fixture took no samples at all");
    run
}

/// Seven matches of a pattern with no embedded code, so the whole walk is
/// native and the count of regex regions is the loop's trip count.
const REGEX_LOOP: &str = "\
my $text = 'alpha beta gamma delta';
my $hits = 0;
for ^7 {
    $hits++ if $text ~~ / \\w+ ' ' \\w+ /;
}
say $hits;
";

/// The same shape with no regex anywhere, so `regex` must be absent rather
/// than present-and-zero.
const ARITHMETIC_LOOP: &str = "\
my $total = 0;
for ^7 {
    $total = $total + $_ * 3 - 1;
}
say $total;
";

const NQP_LOOP: &str = "\
my $n = 0;
for ^7 {
    $n = $n + nqp::elems(nqp::list(1, 2, 3));
}
say $n;
";

#[test]
fn a_regex_fixture_names_the_regex_walk_on_the_line_that_ran_it() {
    let path = fixture_path("regions-regex", REGEX_LOOP);
    let run = profile_regions(&path);
    let _ = std::fs::remove_file(&path);

    assert_eq!(
        run.region_samples().get("regex"),
        Some(&7),
        "one regex walk per trip through the loop, and each claims its own \
         tick under the deterministic tick source: {:?}",
        run.region_samples()
    );
    // The point of the slice: the split is attached to the Raku line, so a
    // report can say "line 4, of which N% regex" rather than leaving the next
    // step to callgrind.
    let regex_lines: Vec<u32> = run
        .line_regions()
        .iter()
        .filter(|(_, _, region)| region == "regex")
        .map(|(_, line, _)| *line)
        .collect();
    assert_eq!(
        regex_lines,
        vec![4],
        "the regex time belongs to the line holding the match: {:?}",
        run.line_regions()
    );
}

#[test]
fn a_fixture_without_a_subsystem_does_not_name_it() {
    let path = fixture_path("regions-arith", ARITHMETIC_LOOP);
    let run = profile_regions(&path);
    let _ = std::fs::remove_file(&path);

    assert!(
        !run.region_samples().contains_key("regex"),
        "a fixture with no regex reported regex time: {:?}",
        run.region_samples()
    );
    // Plain bytecode is `interp`, which is an answer rather than a residue
    // bucket -- there is no `unknown` tag for a sample nothing claimed.
    assert!(
        run.region_samples().get("interp").is_some_and(|n| *n > 0),
        "arithmetic in a loop is bytecode, and nothing tagged it: {:?}",
        run.region_samples()
    );
    assert_eq!(run.top_region(), "interp");
}

#[test]
fn an_nqp_fixture_names_the_nqp_dispatcher() {
    let path = fixture_path("regions-nqp", NQP_LOOP);
    let run = profile_regions(&path);
    let _ = std::fs::remove_file(&path);

    // Two `nqp::` ops per trip, each its own dispatch.
    assert_eq!(
        run.region_samples().get("nqp"),
        Some(&14),
        "{:?}",
        run.region_samples()
    );
}

#[test]
fn every_region_row_names_a_known_tag_and_they_account_for_every_sample() {
    let path = fixture_path("regions-coverage", REGEX_LOOP);
    let run = profile_regions(&path);
    let _ = std::fs::remove_file(&path);

    for name in run.region_samples().keys() {
        assert!(
            KNOWN_REGIONS.contains(&name.as_str()),
            "unknown region tag {name:?}"
        );
    }
    for name in &run.excluded_regions() {
        assert!(
            KNOWN_REGIONS.contains(&name.as_str()),
            "unknown excluded-region tag {name:?}"
        );
    }
    // Not a timing assertion: every sample is tagged with exactly one region,
    // so the split is a partition of the samples the header counted. A hole
    // here would mean a sample carried no tag at all.
    let tagged: u64 = run.region_samples().values().sum();
    assert_eq!(
        tagged,
        run.samples(),
        "the region split does not account for every sample: {:?}",
        run.region_samples()
    );
}

#[test]
fn region_rows_are_a_function_of_the_bytecode() {
    let path = fixture_path("regions-determinism", REGEX_LOOP);
    let first = profile_regions(&path);
    let second = profile_regions(&path);
    let _ = std::fs::remove_file(&path);

    // The property every other assertion in this file rests on. Sample *times*
    // differ run to run; which subsystem each sample was charged to does not.
    assert_eq!(first.region_samples(), second.region_samples());
    assert_eq!(first.line_regions(), second.line_regions());
    assert_eq!(first.samples(), second.samples());
}
