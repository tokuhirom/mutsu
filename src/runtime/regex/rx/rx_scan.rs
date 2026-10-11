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
        mut starts: ScanPositions<'_>,
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
        let lead = leading_run(&program);
        let _region = crate::profile::enter(crate::profile::Region::Regex);
        let mut scratch = SCRATCH.with(|s| s.borrow_mut().pop()).unwrap_or_default();
        // Where the cursor stood after each iteration of the last failed
        // start's leading run (ascending), and how far the starts have got.
        let mut skip: Vec<usize> = Vec::new();
        let mut skip_at = 0usize;
        let mut found = None;
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
            let goal = Goal::First;
            let result = if program.has_call {
                self.rx_run_in::<true>(&program, chars, start, pkg, goal, None, &mut scratch)
            } else {
                self.rx_run_in::<false>(&program, chars, start, pkg, goal, None, &mut scratch)
            };
            if let Some((end, caps)) = result {
                found = Some((start, end, caps));
                break;
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
                self.rx_atom_run(&program, atom as usize, chars, start, u32::MAX, pkg, &mut skip);
            }
        }
        SCRATCH.with(|s| s.borrow_mut().push(scratch));
        found
    }
}
