//! The at-exit scaffolding report.
//!
//! **This is not the profile document.** Slice 5 ([#8705]) owns that — its
//! JSON schema, its file, its CLI, its text renderer. What this prints is the
//! minimum that makes the collected data *observable from outside the
//! process*, because until it is, ADR-0106 §8's gates cannot be asserted at
//! all: the counters and the sampled tables would be folded into statics
//! nothing reads. `tests/profile_counts.rs` and `tests/profile_samples.rs`
//! are the consumers. Slice 5 replaces these bodies with its document
//! builder; neither the counters nor the sampler change.
//!
//! [#8705]: https://github.com/tokuhirom/mutsu/issues/8705

use super::aggregate;
use super::counts;
use super::sampler;

/// How many rows of each table the scaffolding report prints. A fixture small
/// enough to reason about fits well inside this; a real program does not, and
/// is Slice 5's problem.
const REPORT_ROWS: usize = 20;

/// Fold every thread at process shutdown and print what was collected.
pub(crate) fn flush_at_exit() {
    if !crate::vm::vm_poll::profiler_armed() {
        return;
    }
    report_counts();
    report_samples();
}

fn report_counts() {
    let snapshot = counts::take_counts();
    // Hottest first, so "the top self line" is the first row; the location
    // breaks ties, so two lines with equal hits print in a stable order.
    let mut line_hits = snapshot.line_hits;
    line_hits.sort_by_key(|(location, hits)| {
        (
            std::cmp::Reverse(*hits),
            location.file.as_str(),
            location.line,
        )
    });
    for (location, hits) in line_hits.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: line {}:{} hits={hits}",
            location.file, location.line
        );
    }
    let mut routine_entries = snapshot.routine_entries;
    routine_entries.sort_by_key(|(location, entries)| {
        (
            std::cmp::Reverse(*entries),
            location.package.as_str(),
            location.name.as_str(),
        )
    });
    for (location, entries) in routine_entries.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: routine {}::{} entries={entries}",
            location.package, location.name
        );
    }
    let mut callsite_calls = snapshot.callsite_calls;
    callsite_calls.sort_by_key(|(location, calls)| {
        (
            std::cmp::Reverse(*calls),
            location.caller_file.as_str(),
            location.caller_line,
        )
    });
    for (location, calls) in callsite_calls.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: callsite {}:{} -> {}::{} calls={calls}",
            location.caller_file, location.caller_line, location.package, location.name
        );
    }
}

fn report_samples() {
    let Some(config) = sampler::config() else {
        return;
    };
    let snapshot = aggregate::take_samples();
    let tick = match config.tick {
        sampler::Tick::Timer => "timer",
        sampler::Tick::EveryPoll => "every-poll",
    };
    // `time_is_sampled` is deliberately shouted (ADR-0106 §7): a pasted
    // profile must not be mistaken for a bench-CI measurement. `blocked
    // threads contribute nothing` is the documented property of a poll-based
    // sampler — a thread in `sleep`/IO/`await`/a GC park does not poll, so it
    // is absent rather than idle.
    eprintln!(
        "profile: samples n={} sampled_ns={} truncated={} wall_ns={} rate_hz={} tick={tick} threads={} time_is_sampled=1 blocked_threads_absent=1",
        snapshot.samples,
        snapshot.sampled_ns,
        snapshot.truncated,
        config.started_at.elapsed().as_nanos(),
        config.rate_hz,
        sampler::sampled_threads(),
    );
    print_line_rows("self-line", snapshot.line_self_ns);
    print_line_rows("incl-line", snapshot.line_incl_ns);
    print_routine_rows("self-routine", snapshot.routine_self_ns);
    print_routine_rows("incl-routine", snapshot.routine_incl_ns);

    let mut rows = snapshot.callsite_incl_ns;
    rows.sort_by_key(|(location, ns)| {
        (
            std::cmp::Reverse(*ns),
            location.caller_file.as_str(),
            location.caller_line,
            location.name.as_str(),
        )
    });
    for (location, ns) in rows.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: incl-callsite {}:{} -> {}::{} ns={ns}",
            location.caller_file, location.caller_line, location.package, location.name
        );
    }
}

fn print_line_rows(tag: &str, mut rows: Vec<(super::LineLocation, u64)>) {
    rows.sort_by_key(|(location, ns)| {
        (
            std::cmp::Reverse(*ns),
            location.file.as_str(),
            location.line,
        )
    });
    for (location, ns) in rows.iter().take(REPORT_ROWS) {
        eprintln!("profile: {tag} {}:{} ns={ns}", location.file, location.line);
    }
}

fn print_routine_rows(tag: &str, mut rows: Vec<(super::RoutineLocation, u64)>) {
    rows.sort_by_key(|(location, ns)| {
        (
            std::cmp::Reverse(*ns),
            location.package.as_str(),
            location.name.as_str(),
        )
    });
    for (location, ns) in rows.iter().take(REPORT_ROWS) {
        eprintln!(
            "profile: {tag} {}::{} ns={ns}",
            location.package, location.name
        );
    }
}
