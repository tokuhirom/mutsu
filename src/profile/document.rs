//! The profile document: one serializable value both renderers read
//! (ADR-0106 D7, Slice 5).
//!
//! # Why there is a document at all
//!
//! The JSON artifact and the text summary must never disagree, and the way to
//! guarantee that is not discipline but structure: both are renderings of
//! *this* value, built once from the two snapshots. A second renderer (HTML, a
//! MoarVM-shaped export — ADR-0106 Slice 6) is a third reader of the same
//! thing, not a third traversal of the raw tables.
//!
//! # Absent is not zero
//!
//! Every measured field is an [`Option`] that is **omitted** when this run did
//! not measure it, rather than serialized as `0`. A zero that means "not
//! measured" is a lie a tool reads as data: `"hits": 0` on a line the counters
//! never saw would make a consumer conclude the line never ran, when what
//! happened is that only the sampler reached it. The same rule governs
//! `--profile-kind`: the half that was not asked for is absent, not empty.
//!
//! # Time
//!
//! Every `*_us` field is **sampled** time in microseconds, derived from the
//! sampler's nanosecond clock and floating-point precisely so a sub-microsecond
//! total does not round down into the "not measured" spelling. `time_is_sampled`
//! is in the header for the reason ADR-0106 §7 gives: a pasted profile must not
//! be mistaken for a bench-CI measurement.

use super::counts::CountsSnapshot;
use super::options::ProfileOptions;
use super::region::Region;
use super::sampler;
use super::snapshot::SampledSnapshot;
use super::{CallsiteLocation, LineLocation, RoutineLocation};
use rustc_hash::FxHashMap;
use serde::Serialize;

/// Bumped when a field changes meaning or leaves, never when one is added: a
/// consumer that reads what it knows and ignores the rest keeps working.
pub(crate) const SCHEMA_VERSION: u32 = 1;

#[derive(Serialize)]
pub(crate) struct Profile {
    pub(crate) mutsu_prof_version: u32,
    pub(crate) header: Header,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) files: Option<Vec<FileRows>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) routines: Option<Vec<RoutineRow>>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) regions: Vec<RegionRow>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) excluded_regions: Vec<ExcludedRow>,
}

#[derive(Serialize)]
pub(crate) struct Header {
    pub(crate) mutsu_version: &'static str,
    pub(crate) argv: Vec<String>,
    pub(crate) kind: &'static str,
    pub(crate) report: &'static str,
    /// The configuration the profiled program actually ran in (ADR-0106 D6:
    /// the profiled program is the program).
    pub(crate) jit: &'static str,
    pub(crate) gc: &'static str,
    /// Always `true`. Kept as a field rather than implied by the `_us` suffix
    /// so that a consumer, or a person pasting a row into a document, cannot
    /// miss it.
    pub(crate) time_is_sampled: bool,
    /// Always `true`, and a property of a poll-based sampler rather than a
    /// defect: a thread parked in `sleep`/IO/`await`/a GC stop-the-world park
    /// does not poll, so it contributes nothing and is absent from the tables
    /// instead of showing as idle.
    pub(crate) blocked_threads_absent: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) sampling: Option<Sampling>,
}

/// The statistical half of the header, grouped so that which numbers are
/// sampled is a structural fact and not a convention to remember.
#[derive(Serialize)]
pub(crate) struct Sampling {
    pub(crate) rate_hz: u64,
    pub(crate) tick: &'static str,
    pub(crate) wall_us: f64,
    pub(crate) samples: u64,
    /// The sampled time the tables account for, and the denominator every
    /// percentage in the text report is taken against.
    pub(crate) sampled_us: f64,
    /// Samples whose stack was deeper than the walk limit. Their self time is
    /// still exact; only inclusive credit below the cut is missing.
    pub(crate) truncated_samples: u64,
    pub(crate) threads: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) top_region: Option<&'static str>,
}

#[derive(Serialize)]
pub(crate) struct FileRows {
    pub(crate) path: String,
    pub(crate) lines: Vec<LineRow>,
}

#[derive(Serialize)]
pub(crate) struct LineRow {
    pub(crate) line: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) hits: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) self_us: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) incl_us: Option<f64>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) regions: Vec<LineRegionRow>,
}

#[derive(Serialize, Clone)]
pub(crate) struct LineRegionRow {
    pub(crate) region: &'static str,
    pub(crate) self_us: f64,
}

#[derive(Serialize)]
pub(crate) struct RoutineRow {
    pub(crate) package: String,
    pub(crate) name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) file: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) entries: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) self_us: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) incl_us: Option<f64>,
    /// The per-caller breakdown — NYTProf's most useful column (ADR-0106 D3),
    /// and the one a flat line table cannot produce.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub(crate) callers: Vec<CallerRow>,
}

#[derive(Serialize, Clone)]
pub(crate) struct CallerRow {
    pub(crate) file: String,
    pub(crate) line: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) calls: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) incl_us: Option<f64>,
}

#[derive(Serialize)]
pub(crate) struct RegionRow {
    pub(crate) region: &'static str,
    pub(crate) self_us: f64,
    pub(crate) samples: u64,
}

/// Time **measured** out of the line table rather than sampled into it: a GC
/// collect, a stop-the-world park. A separate row type because it is time the
/// line table above deliberately does not contain — named rather than left as
/// a silent hole.
#[derive(Serialize)]
pub(crate) struct ExcludedRow {
    pub(crate) region: &'static str,
    pub(crate) us: f64,
}

fn on_off(state: bool) -> &'static str {
    if state { "on" } else { "off" }
}

fn us(ns: u64) -> f64 {
    ns as f64 / 1000.0
}

/// Build the document from the two snapshots. Both are consumed: taking a
/// snapshot drains the tables, and a second report would otherwise be a report
/// of nothing.
pub(crate) fn build(
    options: &ProfileOptions,
    counts: CountsSnapshot,
    samples: SampledSnapshot,
) -> Profile {
    let header = build_header(options, &samples);
    let files = options.kind.lines().then(|| build_files(&counts, &samples));
    let routines = options
        .kind
        .routines()
        .then(|| build_routines(&counts, &samples));
    let mut regions: Vec<RegionRow> = (0..Region::COUNT)
        // A region that claimed a sample is a row even if its nanoseconds
        // rounded to nothing: the region split is a partition of the *samples*,
        // and dropping one would leave a hole in it.
        .filter(|index| samples.region_ns[*index] > 0 || samples.region_samples[*index] > 0)
        .map(|index| RegionRow {
            region: Region::from_index(index).name(),
            self_us: us(samples.region_ns[index]),
            samples: samples.region_samples[index],
        })
        .collect();
    regions.sort_by(|a, b| b.self_us.total_cmp(&a.self_us).then(a.region.cmp(b.region)));
    let mut excluded_regions: Vec<ExcludedRow> = samples
        .excluded_region_ns
        .iter()
        .map(|(region, ns)| ExcludedRow {
            region: region.name(),
            us: us(*ns),
        })
        .collect();
    excluded_regions.sort_by(|a, b| b.us.total_cmp(&a.us).then(a.region.cmp(b.region)));
    Profile {
        mutsu_prof_version: SCHEMA_VERSION,
        header,
        files,
        routines,
        regions,
        excluded_regions,
    }
}

fn build_header(options: &ProfileOptions, samples: &SampledSnapshot) -> Header {
    Header {
        mutsu_version: env!("CARGO_PKG_VERSION"),
        argv: std::env::args().collect(),
        kind: options.kind.name(),
        report: options.report.name(),
        jit: on_off(crate::vm::vm_jit::jit_enabled()),
        gc: on_off(crate::gc::gc_enabled()),
        time_is_sampled: true,
        blocked_threads_absent: true,
        // Absent when the sampler never armed — an embedder that collected
        // counts only. The counts half of the document stands on its own.
        sampling: sampler::config().map(|config| Sampling {
            rate_hz: config.rate_hz,
            tick: match config.tick {
                sampler::Tick::Timer => "timer",
                sampler::Tick::EveryPoll => "every-poll",
            },
            wall_us: us(config.started_at.elapsed().as_nanos() as u64),
            samples: samples.samples,
            sampled_us: us(samples.sampled_ns),
            truncated_samples: samples.truncated,
            threads: sampler::sampled_threads(),
            top_region: samples.top_region().map(|(region, _)| region.name()),
        }),
    }
}

/// The per-file, per-line table: the union of the three per-line measurements,
/// so a line the counters saw and the sampler did not (and the reverse) is
/// present with the other field absent.
fn build_files(counts: &CountsSnapshot, samples: &SampledSnapshot) -> Vec<FileRows> {
    let hits: FxHashMap<LineLocation, u64> = counts.line_hits.iter().copied().collect();
    let self_ns: FxHashMap<LineLocation, u64> = samples.line_self_ns.iter().copied().collect();
    let incl_ns: FxHashMap<LineLocation, u64> = samples.line_incl_ns.iter().copied().collect();
    let mut line_regions: FxHashMap<LineLocation, Vec<LineRegionRow>> = FxHashMap::default();
    for (key, ns) in &samples.line_region_ns {
        line_regions
            .entry(key.location)
            .or_default()
            .push(LineRegionRow {
                region: key.region.name(),
                self_us: us(*ns),
            });
    }
    for rows in line_regions.values_mut() {
        rows.sort_by(|a, b| b.self_us.total_cmp(&a.self_us).then(a.region.cmp(b.region)));
    }

    let mut locations: Vec<LineLocation> = Vec::new();
    let mut seen: FxHashMap<LineLocation, ()> = FxHashMap::default();
    for location in hits
        .keys()
        .chain(self_ns.keys())
        .chain(incl_ns.keys())
        .chain(line_regions.keys())
    {
        if seen.insert(*location, ()).is_none() {
            locations.push(*location);
        }
    }
    // Grouped by walking a sorted list rather than by a map of maps: the file
    // order and the line order inside it are part of the document's contract
    // (a second run of the same program differs only in its timing fields).
    locations.sort_by(|a, b| {
        a.file
            .as_str()
            .cmp(b.file.as_str())
            .then(a.line.cmp(&b.line))
    });

    let mut files: Vec<FileRows> = Vec::new();
    for location in locations {
        let row = LineRow {
            line: location.line,
            hits: hits.get(&location).copied(),
            self_us: self_ns.get(&location).copied().map(us),
            incl_us: incl_ns.get(&location).copied().map(us),
            regions: line_regions.get(&location).cloned().unwrap_or_default(),
        };
        match files.last_mut() {
            Some(file) if file.path == location.file.as_str() => file.lines.push(row),
            _ => files.push(FileRows {
                path: location.file.as_str().to_string(),
                lines: vec![row],
            }),
        }
    }
    files
}

/// The routine table, with each routine's callers attached.
///
/// Callers are keyed by `(package, name)` rather than by the callee's file: a
/// callsite records who it called, not which file the body came from, so two
/// same-named routines in one package share a caller table. Documented in
/// `docs/profiler.md` rather than papered over.
fn build_routines(counts: &CountsSnapshot, samples: &SampledSnapshot) -> Vec<RoutineRow> {
    let entries: FxHashMap<RoutineLocation, u64> = counts.routine_entries.iter().copied().collect();
    let self_ns: FxHashMap<RoutineLocation, u64> =
        samples.routine_self_ns.iter().copied().collect();
    let incl_ns: FxHashMap<RoutineLocation, u64> =
        samples.routine_incl_ns.iter().copied().collect();
    let calls: FxHashMap<CallsiteLocation, u64> = counts.callsite_calls.iter().copied().collect();
    let callsite_ns: FxHashMap<CallsiteLocation, u64> =
        samples.callsite_incl_ns.iter().copied().collect();

    let mut callers: FxHashMap<(&'static str, &'static str), Vec<CallerRow>> = FxHashMap::default();
    let mut seen_callsites: FxHashMap<CallsiteLocation, ()> = FxHashMap::default();
    for location in calls.keys().chain(callsite_ns.keys()) {
        if seen_callsites.insert(*location, ()).is_some() {
            continue;
        }
        callers
            .entry((location.package.as_str(), location.name.as_str()))
            .or_default()
            .push(CallerRow {
                file: location.caller_file.to_string(),
                line: location.caller_line,
                calls: calls.get(location).copied(),
                incl_us: callsite_ns.get(location).copied().map(us),
            });
    }
    for rows in callers.values_mut() {
        rows.sort_by(|a, b| {
            b.calls
                .unwrap_or(0)
                .cmp(&a.calls.unwrap_or(0))
                .then(a.file.cmp(&b.file))
                .then(a.line.cmp(&b.line))
        });
    }

    let mut seen: FxHashMap<RoutineLocation, ()> = FxHashMap::default();
    let mut locations: Vec<RoutineLocation> = Vec::new();
    for location in entries.keys().chain(self_ns.keys()).chain(incl_ns.keys()) {
        if seen.insert(*location, ()).is_none() {
            locations.push(*location);
        }
    }
    let mut rows: Vec<RoutineRow> = locations
        .into_iter()
        .map(|location| RoutineRow {
            package: location.package.to_string(),
            name: location.name.to_string(),
            file: location.file.map(|file| file.to_string()),
            entries: entries.get(&location).copied(),
            self_us: self_ns.get(&location).copied().map(us),
            incl_us: incl_ns.get(&location).copied().map(us),
            callers: callers
                .get(&(location.package.as_str(), location.name.as_str()))
                .cloned()
                .unwrap_or_default(),
        })
        .collect();
    rows.sort_by(|a, b| {
        a.package
            .cmp(&b.package)
            .then(a.name.cmp(&b.name))
            .then(a.file.cmp(&b.file))
    });
    rows
}

impl Profile {
    /// The denominator every percentage is taken against: the sampled time the
    /// tables account for. `None` when nothing was sampled, in which case a
    /// renderer prints counts and no percentages rather than dividing by zero.
    pub(crate) fn sampled_us(&self) -> Option<f64> {
        self.header
            .sampling
            .as_ref()
            .map(|sampling| sampling.sampled_us)
            .filter(|total| *total > 0.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn microseconds_keep_a_sub_microsecond_total_visible() {
        // Why the times are floating point: a 400ns total is measured, and an
        // integer microsecond field would round it into 0 -- which this
        // document spells "not measured".
        assert_eq!(us(400), 0.4);
        assert_eq!(us(1_500_000), 1500.0);
    }
}
