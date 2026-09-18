//! The built-in text report (ADR-0106 D7).
//!
//! HTML is out of scope for the first implementation, which puts a hard
//! requirement on this renderer: it has to be good enough that not having HTML
//! is not a gap. Concretely, ADR-0106 §5 asks for the four things a triage
//! session actually needs on screen at once — the top self lines with their
//! exact hit counts, the top routines by inclusive time, who called them, and
//! which interpreter subsystem the time went to.
//!
//! Everything printed here is read off the [`Profile`] document, never off the
//! raw tables, so the text and the JSON cannot drift apart.

use super::document::{Profile, RoutineRow};

/// How many rows each table shows. Enough to see the shape of a real program's
/// profile in one screen; the JSON document carries everything.
const TOP_LINES: usize = 15;
const TOP_ROUTINES: usize = 10;
/// Callers shown per routine. The distribution is usually one dominant caller
/// and a tail, and the tail is what the JSON is for.
const TOP_CALLERS: usize = 3;

/// Render the whole report. Returns a `String` rather than printing, so the
/// text is testable without capturing a stream.
pub(crate) fn render(profile: &Profile) -> String {
    let mut out = String::new();
    render_header(profile, &mut out);
    render_lines(profile, &mut out);
    render_routines(profile, &mut out);
    render_regions(profile, &mut out);
    out
}

fn render_header(profile: &Profile, out: &mut String) {
    let header = &profile.header;
    match &header.sampling {
        Some(sampling) => out.push_str(&format!(
            "mutsu-prof: {} wall, {} samples @{}Hz ({}) on {} thread{}, jit={} gc={}, kind={}\n",
            fmt_us(sampling.wall_us),
            fmt_count(sampling.samples),
            sampling.rate_hz,
            sampling.tick,
            sampling.threads,
            if sampling.threads == 1 { "" } else { "s" },
            header.jit,
            header.gc,
            header.kind,
        )),
        None => out.push_str(&format!(
            "mutsu-prof: counts only (the sampler did not run), jit={} gc={}, kind={}\n",
            header.jit, header.gc, header.kind,
        )),
    }
    // Stated on every report, in the report itself, because the number most
    // likely to be pasted into a document is the one on the next line
    // (ADR-0106 §7): PERFORMANCE.md / news numbers come from the bench CI.
    out.push_str(
        "            hits are exact; every time below is SAMPLED -- never quote it as a measurement\n",
    );
    if header.allocation_stats {
        out.push_str(
            "            allocations are exact; this alloc-stats profile omits sampled time\n",
        );
    }
    if let Some(sampling) = &header.sampling
        && sampling.truncated_samples > 0
    {
        out.push_str(&format!(
            "            {} sample(s) hit the stack-walk limit: self time exact, inclusive time short\n",
            fmt_count(sampling.truncated_samples),
        ));
    }
}

fn render_lines(profile: &Profile, out: &mut String) {
    let Some(files) = &profile.files else {
        return;
    };
    let total = profile.sampled_us();
    // Ranked by sampled self time when there is any, and by exact hits when
    // there is not: a run too short to sample still has a hottest line, and
    // printing the table in file order would hide it.
    let mut rows: Vec<(&str, &super::document::LineRow)> = files
        .iter()
        .flat_map(|file| {
            file.lines
                .iter()
                .map(move |line| (file.path.as_str(), line))
        })
        .collect();
    if total.is_some() {
        rows.sort_by(|a, b| {
            b.1.self_us
                .unwrap_or(0.0)
                .total_cmp(&a.1.self_us.unwrap_or(0.0))
                .then(b.1.hits.unwrap_or(0).cmp(&a.1.hits.unwrap_or(0)))
                .then(a.0.cmp(b.0))
                .then(a.1.line.cmp(&b.1.line))
        });
    } else {
        rows.sort_by(|a, b| {
            b.1.hits
                .unwrap_or(0)
                .cmp(&a.1.hits.unwrap_or(0))
                .then(a.0.cmp(b.0))
                .then(a.1.line.cmp(&b.1.line))
        });
    }
    out.push_str(if total.is_some() {
        "TOP SELF LINES\n"
    } else {
        "TOP LINES BY HITS (nothing was sampled)\n"
    });
    if rows.is_empty() {
        out.push_str("  (no line ran under the profiler)\n");
        return;
    }
    for (rank, (path, row)) in rows.iter().take(TOP_LINES).enumerate() {
        out.push_str(&format!(
            "  {:>2}  {}:{}  {}  {}{}{}\n",
            rank + 1,
            path,
            row.line,
            share(row.self_us, total, "self"),
            match row.hits {
                Some(hits) => format!("hits {}", fmt_count(hits)),
                // Absent, not zero: the sampler saw this line, the
                // line-transition counter did not (an inclusive-only frame).
                None => "hits n/a".to_string(),
            },
            fmt_allocations(row),
            fmt_line_regions(row, total),
        ));
    }
}

fn fmt_allocations(row: &super::document::LineRow) -> String {
    row.allocations
        .as_ref()
        .map(|allocations| {
            format!(
                "  allocs {} ({} bytes)",
                fmt_count(allocations.count),
                fmt_count(allocations.bytes),
            )
        })
        .unwrap_or_default()
}

fn fmt_line_regions(row: &super::document::LineRow, total: Option<f64>) -> String {
    if row.regions.is_empty() {
        return String::new();
    }
    // Per-line region shares are of *this line's* sampled self time, not of the
    // run: "of which" is the question a reader is asking here.
    let line_total = row
        .self_us
        .or(total)
        .filter(|t| *t > 0.0)
        .unwrap_or(f64::NAN);
    let parts: Vec<String> = row
        .regions
        .iter()
        .take(3)
        .map(|region| {
            if line_total.is_nan() {
                region.region.to_string()
            } else {
                format!(
                    "{} {:.0}%",
                    region.region,
                    region.self_us / line_total * 100.0
                )
            }
        })
        .collect();
    format!("  [{}]", parts.join(" | "))
}

fn render_routines(profile: &Profile, out: &mut String) {
    let Some(routines) = &profile.routines else {
        return;
    };
    let total = profile.sampled_us();
    let mut rows: Vec<&RoutineRow> = routines.iter().collect();
    rows.sort_by(|a, b| {
        b.incl_us
            .unwrap_or(0.0)
            .total_cmp(&a.incl_us.unwrap_or(0.0))
            .then(b.entries.unwrap_or(0).cmp(&a.entries.unwrap_or(0)))
            .then(a.package.cmp(&b.package))
            .then(a.name.cmp(&b.name))
    });
    out.push_str("TOP ROUTINES (inclusive)\n");
    if rows.is_empty() {
        out.push_str("  (no routine ran under the profiler)\n");
        return;
    }
    for (rank, row) in rows.iter().take(TOP_ROUTINES).enumerate() {
        out.push_str(&format!(
            "  {:>2}  {}  {}  {}\n",
            rank + 1,
            qualified(row),
            share(row.incl_us, total, "incl"),
            match row.entries {
                Some(entries) => format!("entries {}", fmt_count(entries)),
                None => "entries n/a".to_string(),
            },
        ));
        let calls: u64 = row.callers.iter().filter_map(|caller| caller.calls).sum();
        for caller in row.callers.iter().take(TOP_CALLERS) {
            out.push_str(&format!(
                "        <- {}:{}{}\n",
                caller.file,
                caller.line,
                match (caller.calls, calls) {
                    (Some(n), total) if total > 0 => format!(
                        "  {} call{} ({:.0}% of the calls seen)",
                        fmt_count(n),
                        if n == 1 { "" } else { "s" },
                        n as f64 / total as f64 * 100.0
                    ),
                    (Some(n), _) => format!("  {} calls", fmt_count(n)),
                    (None, _) => String::new(),
                },
            ));
        }
    }
}

fn render_regions(profile: &Profile, out: &mut String) {
    let total = profile.sampled_us();
    if !profile.regions.is_empty() {
        out.push_str("REGIONS (sampled self time)\n ");
        for region in &profile.regions {
            out.push_str(&format!(
                " {} {}",
                region.region,
                match total {
                    Some(total) => format!("{:.1}%", region.self_us / total * 100.0),
                    None => fmt_us(region.self_us),
                }
            ));
        }
        out.push('\n');
    }
    if !profile.excluded_regions.is_empty() {
        // Not part of the line table above, on purpose: this is time measured
        // *out* of it, so a reader can see where the wall clock went without
        // it landing on whichever line happened to be on top.
        out.push_str("EXCLUDED (measured, not sampled; not in the tables above)\n ");
        for region in &profile.excluded_regions {
            out.push_str(&format!(" {} {}", region.region, fmt_us(region.us)));
        }
        out.push('\n');
    }
}

fn qualified(row: &RoutineRow) -> String {
    // A frame with no name is a real thing (a synthetic or anonymous body), and
    // printing nothing for it leaves a row that looks like a rendering bug.
    let name = if row.name.is_empty() {
        "<anon>"
    } else {
        row.name.as_str()
    };
    if row.package == "GLOBAL" || row.package.is_empty() {
        name.to_string()
    } else {
        format!("{}::{}", row.package, name)
    }
}

/// A percentage of the run's sampled time, or the plain duration when there is
/// no denominator to take it against. Never a `0.0%` standing in for "not
/// sampled" — that column reads `-` instead.
fn share(us: Option<f64>, total: Option<f64>, tag: &str) -> String {
    match (us, total) {
        (Some(us), Some(total)) => format!("{:>5.1}% {tag}", us / total * 100.0),
        (Some(us), None) => format!("{} {tag}", fmt_us(us)),
        (None, _) => format!("    - {tag}"),
    }
}

/// Microseconds in the largest unit that keeps three significant figures.
fn fmt_us(us: f64) -> String {
    if us >= 1_000_000.0 {
        format!("{:.2}s", us / 1_000_000.0)
    } else if us >= 1_000.0 {
        format!("{:.1}ms", us / 1_000.0)
    } else {
        format!("{:.0}us", us)
    }
}

/// Thousands separators, because the number that matters most in this report is
/// often an order of magnitude larger than the reader expects — the quadratic
/// scan in ADR-0106 §5's worked example is spotted by the *width* of the hit
/// count before it is read.
fn fmt_count(n: u64) -> String {
    let digits = n.to_string();
    let mut out = String::with_capacity(digits.len() + digits.len() / 3);
    for (index, ch) in digits.chars().enumerate() {
        if index > 0 && (digits.len() - index).is_multiple_of(3) {
            out.push(',');
        }
        out.push(ch);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_get_thousands_separators() {
        assert_eq!(fmt_count(0), "0");
        assert_eq!(fmt_count(999), "999");
        assert_eq!(fmt_count(1_000), "1,000");
        assert_eq!(fmt_count(1_904_000), "1,904,000");
    }

    #[test]
    fn durations_keep_three_significant_figures() {
        assert_eq!(fmt_us(412.0), "412us");
        assert_eq!(fmt_us(12_300.0), "12.3ms");
        assert_eq!(fmt_us(6_200_000.0), "6.20s");
    }

    #[test]
    fn an_unsampled_row_is_a_dash_and_never_a_zero_percent() {
        assert_eq!(share(None, Some(100.0), "self"), "    - self");
        assert_eq!(share(Some(41.3), Some(100.0), "self"), " 41.3% self");
    }
}
