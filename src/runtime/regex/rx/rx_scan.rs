//! The unanchored scan loop inside the compiled engine (#10315): one entry for
//! all the candidate start positions of a scan, instead of one engine entry
//! (guards, program lookup, scratch state) per position.

use super::rx_entry::{Goal, SCRATCH, program_or_raise};
use super::super::regex_prefilter_scan::ScanPositions;
use super::{RxOp, RxProgram};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{RegexCaptures, RegexPattern};
use crate::symbol::Symbol;

/// The leading atom of a program shaped `X{min,} tail` with `min >= 1`, no
/// upper bound, and a `tail` that only tests one-grapheme atoms (possibly
/// repeated) and the subject's end, then matches.
///
/// For such a program a start `p` that fails makes every start `q` that is
/// the cursor after some iteration of `p`'s `X` run fail too: from `q` the
/// run reaches the same end, so it offers `tail` a subset of the positions
/// `p` offered (it has at least one iteration fewer, and `min >= 1` means the
/// positions `p` offered include every one `q` could), and `tail` holds
/// nothing that reads the start, a capture or the code's result, so it
/// answers the same at the same position.
// Cost: O(t), t = the ops, bounded by `LEAD_RUN_MAX_OPS`.
fn leading_run(program: &RxProgram) -> Option<u32> {
    /// A longer program is not worth the analysis for a prune that only helps
    /// short scans' constant factor; it also bounds the analysis itself.
    const LEAD_RUN_MAX_OPS: usize = 16;
    if program.ops.len() > LEAD_RUN_MAX_OPS || program.has_call || program.has_code {
        return None;
    }
    let RxOp::AtomRun {
        atom,
        min,
        max: u32::MAX,
        ..
    } = program.ops[0]
    else {
        return None;
    };
    if min == 0 {
        return None;
    }
    program.ops[1..]
        .iter()
        .all(|op| {
            matches!(
                op,
                RxOp::Atom(_) | RxOp::AtomRun { .. } | RxOp::AssertEnd | RxOp::Match
            )
        })
        .then_some(atom)
}

impl Interpreter {
    /// The first start in `starts` (an unanchored scan's candidate positions,
    /// in order) at which `pattern` matches, with the match's end and
    /// captures: [`Interpreter::rx_match_first`] at each start, but the
    /// pattern's guards, program and scratch state are looked up once for the
    /// scan rather than once per position, and (see [`leading_run`]) the
    /// starts inside a failed start's leading run are not tried.
    // Cost: O(1) per start that fails at once, plus the matches attempted.
    pub(in crate::runtime::regex) fn rx_scan_first(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        starts: ScanPositions<'_>,
        pkg: Symbol,
    ) -> Option<(usize, usize, RegexCaptures)> {
        if super::super::regex_helpers::LTM_DECLARATIVE_MODE.with(std::cell::Cell::get)
            || pattern.ignore_mark
        {
            for start in starts {
                if let Some((end, caps)) = self.rx_match_first(pattern, chars, start, pkg) {
                    return Some((start, end, caps));
                }
            }
            return None;
        }
        let program = program_or_raise(pattern)?;
        let mut found = None;
        self.rx_scan_loop(&program, chars, starts, pkg, false, |start, end, caps| {
            found = Some((start, end, caps));
            None
        });
        found
    }

    /// Every leftmost, non-overlapping match of `pattern` among `starts`, as
    /// `(start, end)` char spans, at most `limit` of them: `.comb(rx)` and the
    /// other walks that want only the spans. One engine entry for the whole
    /// walk (the scratch state, guards and program are taken once, the scan's
    /// candidate iterator is advanced past each match rather than rebuilt), and
    /// no match snapshots its captures. Code blocks are inert, as in
    /// `rx_match_first_no_code`. `None` when the pattern needs the
    /// per-start entry instead (LTM measurement, `:ignoremark`).
    // Cost: O(n) positions scanned plus the engine's work at each admitted
    // start; O(1) per match beyond that, n = chars of the subject.
    pub(in crate::runtime::regex) fn rx_scan_all_spans(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        starts: ScanPositions<'_>,
        pkg: Symbol,
        limit: usize,
    ) -> Option<Vec<(usize, usize)>> {
        if super::super::regex_helpers::LTM_DECLARATIVE_MODE.with(std::cell::Cell::get)
            || pattern.ignore_mark
        {
            return None;
        }
        let program = program_or_raise(pattern)?;
        let mut spans = Vec::new();
        if limit == 0 {
            return Some(spans);
        }
        // Like `rx_match_first_no_code`: a span walk runs no code block.
        let saved = program
            .has_code
            .then(|| super::super::regex_helpers::CODE_ATOMS_INERT.with(|f| f.replace(true)));
        self.rx_scan_loop(&program, chars, starts, pkg, true, |start, end, _| {
            spans.push((start, end));
            // Resume after the match, one char further after an empty one.
            (spans.len() < limit).then_some(if end > start { end } else { start + 1 })
        });
        if let Some(saved) = saved {
            super::super::regex_helpers::CODE_ATOMS_INERT.with(|f| f.set(saved));
        }
        Some(spans)
    }

    /// The scan loop both entries share: try each start of `starts` (pruning
    /// the futile ones a failed start's leading run proves, see
    /// [`leading_run`]) and hand every match to `on_match`, which answers the
    /// position the scan resumes from, or `None` to stop. With `ends_only` a
    /// match's captures are not snapshotted (the callback receives empty ones).
    // Cost: as `rx_scan_first`, per match handed out.
    fn rx_scan_loop(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        mut starts: ScanPositions<'_>,
        pkg: Symbol,
        ends_only: bool,
        mut on_match: impl FnMut(usize, usize, RegexCaptures) -> Option<usize>,
    ) {
        let lead = leading_run(program);
        let _region = crate::profile::enter(crate::profile::Region::Regex);
        let mut scratch = SCRATCH.with(|s| s.borrow_mut().pop()).unwrap_or_default();
        // Where the cursor stood after each iteration of the last failed
        // start's leading run (ascending), and how far the starts have got.
        let mut skip: Vec<usize> = Vec::new();
        let mut skip_at = 0usize;
        while let Some(start) = starts.next() {
            if lead.is_some() {
                while skip.get(skip_at).is_some_and(|&q| q < start) {
                    skip_at += 1;
                }
                if skip.get(skip_at) == Some(&start) {
                    continue;
                }
            }
            crate::vm::vm_stats_regex_vm::record_regex_vm_run();
            let goal = if ends_only { Goal::FirstEnd } else { Goal::First };
            let result = if program.has_call {
                self.rx_run_in::<true>(program, chars, start, pkg, goal, None, &mut scratch)
            } else {
                self.rx_run_in::<false>(program, chars, start, pkg, goal, None, &mut scratch)
            };
            if let Some((end, caps)) = result {
                match on_match(start, end, caps) {
                    Some(resume) => {
                        starts.advance_to(resume);
                        skip.clear();
                        skip_at = 0;
                        continue;
                    }
                    None => break,
                }
            }
            if let Some(atom) = lead {
                // The engine's own run of the leading atom left its end and
                // length behind: iterations of one character each leave every
                // start up to the end futile, so the iterator skips them
                // rather than yielding each one only to discard it.
                let (end, n) = scratch.first_run;
                if end == start + n as usize {
                    starts.advance_to(end + 1);
                    continue;
                }
                skip.clear();
                skip_at = 0;
                self.rx_atom_run(program, atom as usize, chars, start, u32::MAX, pkg, &mut skip);
            }
        }
        SCRATCH.with(|s| s.borrow_mut().push(scratch));
    }
}
