//! The regex engine's entry points (ADR-0135), the per-pattern program memo,
//! and the pool of VM scratch state a run borrows.

use std::sync::Arc;

use super::rx_frame::{Choice, FMark, Frame};
use super::rx_levels::Levels;
use super::{RxProgram, rx_compile};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{RegexCaptures, RegexPattern};
use crate::symbol::Symbol;

/// What a run is looking for at the pattern's own `Match`.
pub(super) enum Goal<'a> {
    /// The first match in priority order.
    First,
    /// The first match that ends exactly at this position, as the walk's
    /// `regex_match_branch_ending_at` picks one from the full end list.
    End(usize),
    /// Every match in priority order: all of them, or (`stop_at_full`, for
    /// `Grammar.parse`) up to and including the first that covers the whole
    /// subject.
    Ends {
        out: &'a mut Vec<(usize, RegexCaptures)>,
        stop_at_full: bool,
    },
}

/// The VM's growable state, reused across engine entries instead of being
/// reallocated per start position. Taken out of the thread-local pool for
/// one run and put back after.
#[derive(Default)]
pub(super) struct Scratch {
    /// The register arena: one window per active frame (`rx_frame`).
    pub(super) regs: Vec<usize>,
    /// Register writes to undo on backtrack: (index into `regs`, old value).
    pub(super) reg_trail: Vec<(usize, usize)>,
    pub(super) stack: Vec<Choice>,
    /// The frame state of the choice points pushed inside a callee (`FMark`).
    pub(super) fmarks: Vec<FMark>,
    /// The frame arena: every call frame that may still run or be resumed in.
    pub(super) frames: Vec<Frame>,
    /// A proto call's ranking, before it is known to need a choice point.
    pub(super) proto_rank: Vec<usize>,
    pub(super) ends: Vec<usize>,
    pub(super) levels: Levels,
    pub(super) ltm_order: Vec<(usize, (usize, usize))>,
}

thread_local! {
    // Boxed, so taking one out for a run moves a pointer rather than the
    // whole struct: a run happens once per unanchored start position. A pool,
    // because a run can nest (a lookaround's pattern runs inside the run that
    // tests it), and each level keeps its own warm scratch. The boxes are
    // the point: popping one out moves a pointer, not the struct.
    #[allow(clippy::vec_box)]
    static SCRATCH: std::cell::RefCell<Vec<Box<Scratch>>> = const { std::cell::RefCell::new(Vec::new()) };
}

/// The pattern's compiled program, compiled at most once per pattern.
// Cost: O(1) after the first call per pattern; O(t) on it, t = tokens.
pub(super) fn program_for(pattern: &RegexPattern) -> Option<&Arc<RxProgram>> {
    pattern
        .derived
        .rx_program
        .get_or_init(|| {
            let compiled = rx_compile::compile(pattern);
            crate::vm::vm_stats_regex_vm::record_regex_vm_compile(compiled.as_ref().err().copied());
            compiled.ok().map(Arc::new)
        })
        .as_ref()
}

/// The subject a `:m` match maps its stripped positions over: the published
/// match target, or, for a match outside any (an internal caller's), one built
/// from `chars`.
// Cost: O(1) with a published target; else O(n), n = the chars.
fn ignoremark_target(chars: &[char]) -> crate::value::regex_caps::MatchTarget {
    super::super::regex_helpers::current_match_target()
        .unwrap_or_else(|| crate::value::regex_caps::MatchTarget::from_chars(chars))
}

/// `pattern`'s program, or — for a pattern the compiler declines, which no
/// pattern of the suites does (ADR-0135 D7) — an error raised for the match,
/// naming why (`X::NYI`-style: the construct is not implemented).
// Cost: O(1) after the pattern's first compile.
fn program_or_raise(pattern: &RegexPattern) -> Option<Arc<RxProgram>> {
    if let Some(program) = program_for(pattern) {
        return Some(Arc::clone(program));
    }
    let why = rx_compile::compile(pattern).err().unwrap_or("declined");
    let err = crate::value::RuntimeError::new(format!(
        "This regex construct is not implemented by the regex engine ({why})"
    ));
    crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| {
        e.borrow_mut().get_or_insert(err);
    });
    None
}

impl Interpreter {
    /// The first (highest-priority) match of `pattern` at `start`.
    // Cost: the match itself.
    pub(in crate::runtime::regex) fn rx_match_first(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<(usize, RegexCaptures)> {
        self.rx_match_first_in(pattern, chars, start, pkg, true)
    }

    /// [`Self::rx_match_first`] for the position-only matcher
    /// (`regex_match_end_from_in_pkg`), which treats a code atom as an inert
    /// zero-width pass: it probes a pattern without running the user's code.
    // Cost: the match itself.
    pub(in crate::runtime::regex) fn rx_match_first_no_code(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<(usize, RegexCaptures)> {
        self.rx_match_first_in(pattern, chars, start, pkg, false)
    }

    fn rx_match_first_in(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        allow_code: bool,
    ) -> Option<(usize, RegexCaptures)> {
        if super::super::regex_helpers::LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
            return self
                .rx_ltm_measured_ends(pattern, chars, start, pkg)
                .into_iter()
                .next();
        }
        if pattern.ignore_mark {
            return self.rx_match_ignoremark(pattern, chars, start, pkg, allow_code);
        }
        let program = program_or_raise(pattern)?;
        crate::vm::vm_stats_regex_vm::record_regex_vm_run();
        let inert = !allow_code && program.has_code;
        let saved =
            inert.then(|| super::super::regex_helpers::CODE_ATOMS_INERT.with(|f| f.replace(true)));
        let result = self.rx_run(&program, chars, start, pkg, None);
        if let Some(saved) = saved {
            super::super::regex_helpers::CODE_ATOMS_INERT.with(|f| f.set(saved));
        }
        result
    }

    /// A match made while an LTM prefix is measured (ADR-0125): no code runs
    /// and nothing is captured, so the pattern is measured by its own NFA,
    /// whose fate is the enclosing measurement's. Its ends, longest first.
    // Cost: the NFA run, O(n * t), n = positions reached, t = threads.
    fn rx_ltm_measured_ends(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Vec<(usize, RegexCaptures)> {
        let nfa = self.ltm_nfa_for(pattern, pkg, false);
        let run = nfa.run(self, chars, start, &[]);
        if let Some(fate) = run.fate {
            super::super::regex_ltm_fate::ltm_record_fate(fate);
        }
        let mut ends = run.ends;
        ends.sort_unstable_by(|a, b| b.cmp(a));
        ends.dedup();
        ends.into_iter()
            .map(|end| (end, RegexCaptures::default()))
            .collect()
    }

    /// A whole-pattern `:m`: the mark-stripped pattern's program over the
    /// subject's stripped view, mapped back (`ignoremark_on_target`).
    // Cost: the stripped match, plus O(c) to map c capture spans back; O(n)
    // more to build the subject of `chars` when none is published, n = its
    // chars.
    fn rx_match_ignoremark(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        allow_code: bool,
    ) -> Option<(usize, RegexCaptures)> {
        let target = ignoremark_target(chars);
        let mut run = |interp: &mut Interpreter, stripped: &RegexPattern, chars: &[char]| {
            interp
                .rx_match_first_in(stripped, chars, 0, pkg, allow_code)
                .into_iter()
                .collect()
        };
        self.ignoremark_on_target(pattern, &target, start, &mut run)
            .pop()
    }

    /// [`Self::rx_match_ends`] for a `:m` pattern: the mark-stripped pattern's
    /// ends over the subject's stripped view, mapped back
    /// (`ignoremark_on_target`).
    // Cost: the stripped run, plus O(c) per end to map its c capture spans
    // back; O(n) more to build the subject of `chars` when none is published.
    fn rx_match_ignoremark_ends(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        stop_at_full: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        let target = ignoremark_target(chars);
        let mut run = |interp: &mut Interpreter, stripped: &RegexPattern, chars: &[char]| {
            interp.rx_match_ends(stripped, chars, 0, pkg, stop_at_full)
        };
        self.ignoremark_on_target(pattern, &target, start, &mut run)
    }

    /// Run `program` at `start`.
    // Cost: O(s) in the steps the backtracking search takes; each op below
    // states its own cost.
    ///
    /// With `end`, only a match ending exactly there counts: the first one in
    /// priority order, as the walk's `regex_match_branch_ending_at` picks it
    /// from the full end list.
    pub(super) fn rx_run(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        end: Option<usize>,
    ) -> Option<(usize, RegexCaptures)> {
        self.rx_run_seeded(program, chars, start, pkg, end, None)
    }

    /// [`Self::rx_run`], with the pattern level starting from `seed` (an
    /// inline level's captures, `rx_levels::inline_level_caps`) instead of
    /// empty: a nested run that is part of the enclosing regex.
    // Cost: as `rx_run`.
    pub(super) fn rx_run_seeded(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        end: Option<usize>,
        seed: Option<RegexCaptures>,
    ) -> Option<(usize, RegexCaptures)> {
        let goal = end.map_or(Goal::First, Goal::End);
        self.rx_run_goal_seeded(program, chars, start, pkg, goal, seed)
    }

    fn rx_run_goal(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        goal: Goal<'_>,
    ) -> Option<(usize, RegexCaptures)> {
        self.rx_run_goal_seeded(program, chars, start, pkg, goal, None)
    }

    fn rx_run_goal_seeded(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        goal: Goal<'_>,
        seed: Option<RegexCaptures>,
    ) -> Option<(usize, RegexCaptures)> {
        let _region = crate::profile::enter(crate::profile::Region::Regex);
        let mut scratch = SCRATCH.with(|s| s.borrow_mut().pop()).unwrap_or_default();
        // A program with no call never builds a frame: its loop is compiled
        // without the frame machinery.
        let result = if program.has_call {
            self.rx_run_in::<true>(program, chars, start, pkg, goal, seed, &mut scratch)
        } else {
            self.rx_run_in::<false>(program, chars, start, pkg, goal, seed, &mut scratch)
        };
        SCRATCH.with(|s| s.borrow_mut().push(scratch));
        result
    }

    /// Every end of `pattern` at `start`, highest priority first: all of them
    /// (`:ov`/`:ex`, an alternation branch, a cursor token method), or with
    /// `stop_at_full` (`Grammar.parse`) up to and including the first that
    /// covers the whole subject.
    // Cost: O(s) in the steps of the backtracking search, as `rx_run` run to
    // exhaustion (or to the first full match); plus O(c) per end collected,
    // c = its captures.
    pub(in crate::runtime::regex) fn rx_match_ends(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        stop_at_full: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        if super::super::regex_helpers::LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
            return self.rx_ltm_measured_ends(pattern, chars, start, pkg);
        }
        if pattern.ignore_mark {
            return self.rx_match_ignoremark_ends(pattern, chars, start, pkg, stop_at_full);
        }
        let Some(program) = program_or_raise(pattern) else {
            return Vec::new();
        };
        crate::vm::vm_stats_regex_vm::record_regex_vm_run();
        let mut ends = Vec::new();
        let goal = Goal::Ends {
            out: &mut ends,
            stop_at_full,
        };
        self.rx_run_goal(&program, chars, start, pkg, goal);
        ends
    }
}
