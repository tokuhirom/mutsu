//! The entry points the walk's chokepoints consult to reach the compiled
//! engine (ADR-0135 D5), the per-pattern program memo, and the pool of VM
//! scratch state a run borrows.

use std::sync::Arc;

use super::rx_frame::{Choice, FMark};
use super::rx_levels::Levels;
use super::{RxProgram, rx_compile, rx_diff_enabled, rx_vm_enabled};
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
    /// Every match in priority order up to and including the first that covers
    /// the whole subject (`Grammar.parse`).
    UntilFull(&'a mut Vec<(usize, RegexCaptures)>),
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

/// `pattern`'s program, unless it runs code and the caller cannot allow that.
/// `Err` names why the match takes the walk.
fn rx_program_for_run(
    pattern: &RegexPattern,
    allow_code: bool,
) -> Result<Arc<RxProgram>, &'static str> {
    let program = program_for(pattern).ok_or("declined")?;
    if !allow_code && program.has_code {
        return Err("position-only-code");
    }
    Ok(Arc::clone(program))
}

/// Count one match the walk answers instead of the compiled engine.
#[inline]
fn walked<T>(reason: &'static str) -> Option<T> {
    crate::vm::vm_stats_regex_vm::record_regex_walk(
        crate::vm::vm_stats_regex_vm::WalkUse::Walked,
        reason,
    );
    None
}

impl Interpreter {
    /// The first (highest-priority) match of `pattern` at `start`, by the
    /// compiled engine — or `None` when this match must take the walk: the
    /// pattern is outside the compiled language, or the dynamic context carries
    /// state the VM does not model (an enclosing regex's `:my` lexicals or
    /// backreference captures, a grammar rule's dynamic declarations, LTM
    /// measurement).
    // Cost: O(1) to decline; otherwise the match itself.
    pub(in crate::runtime::regex) fn rx_try_match(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<Option<(usize, RegexCaptures)>> {
        self.rx_try_match_in(pattern, chars, start, pkg, true)
    }

    /// [`Self::rx_try_match`] for the position-only matcher
    /// (`regex_match_end_from_in_pkg`). That matcher treats a code atom as an
    /// inert zero-width pass — it is how the walk probes a group without running
    /// the user's code — so a pattern with any code atom declines here and keeps
    /// it.
    // Cost: O(1) to decline; otherwise the match itself.
    pub(in crate::runtime::regex) fn rx_try_match_no_code(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<Option<(usize, RegexCaptures)>> {
        self.rx_try_match_in(pattern, chars, start, pkg, false)
    }

    /// Does the dynamic context let the compiled engine run at all? `Err`
    /// names what keeps it out (`MUTSU_VM_STATS`'s `regex-walk:` line).
    // Cost: O(1).
    #[inline]
    fn rx_context_allows(&mut self) -> Result<(), &'static str> {
        use super::super::regex_helpers as h;
        if !rx_vm_enabled() {
            return Err("context:vm-off");
        }
        if h::LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
            return Err("context:ltm-declarative");
        }
        if !self.grammar_rule_dynvar_decls.is_empty() {
            return Err("context:rule-dynvar-decls");
        }
        if h::inline_regex_vars_active() {
            return Err("context:inline-regex-vars");
        }
        if h::INLINE_CAPTURE_SCOPE.with(std::cell::Cell::get).is_some() {
            return Err("context:inline-capture-scope");
        }
        if h::take_inline_outer_caps_seed().is_some() {
            return Err("context:inline-outer-seed");
        }
        if rx_diff_enabled() {
            // D6: the walk's replay of a compiled run is the walk alone, so a
            // nested pattern it matches is compared too instead of answering
            // from the compiled engine on both sides.
            if super::rx_diff::replaying() {
                return Err("context:diff-replay");
            }
            // D6 can record and replay a code atom, but not user code reached
            // through a wrapped token, a custom HOW or a reduce-time action:
            // the walk's replay would run it a second time. Differential mode
            // declines those matches.
            if self.has_any_wrap_chains()
                || !self.registry().grammar_custom_how.is_empty()
                || super::super::regex_helpers::dynvar_overlay_active()
            {
                return Err("context:diff-user-code");
            }
        }
        Ok(())
    }

    fn rx_try_match_in(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        allow_code: bool,
    ) -> Option<Option<(usize, RegexCaptures)>> {
        if let Err(why) = self.rx_context_allows() {
            return walked(why);
        }
        if pattern.ignore_mark {
            return self.rx_try_ignoremark(pattern, start, pkg, allow_code);
        }
        let program = match rx_program_for_run(pattern, allow_code) {
            Ok(program) => program,
            Err(why) => return walked(why),
        };
        crate::vm::vm_stats_regex_vm::record_regex_vm_run();
        // D6: the compiled run records the code atoms it invokes and the walk
        // replays them (`rx_diff`).
        let diffing = rx_diff_enabled();
        let mark = diffing.then(super::rx_diff::begin_record);
        let result = self.rx_run(&program, chars, start, pkg, None);
        if let Some(mark) = mark {
            super::rx_diff::begin_replay(mark);
            let walked = super::super::regex_helpers::isolate_reduced_log(|| {
                self.regex_walk_first_for_diff(pattern, chars, start, pkg)
            });
            let replay = super::rx_diff::end_replay();
            let same = super::rx_diff::same_match(&result, &walked);
            if let Err(why) = replay.and(same) {
                super::rx_diff::disagreement(format!(
                    "at start {start} of a {}-char subject: {why}\nprogram: {:?}",
                    chars.len(),
                    program.ops
                ));
            }
        }
        Some(result)
    }

    /// A whole-pattern `:m`: the mark-stripped pattern's compiled program
    /// over the subject's stripped view, mapped back by the walk's own
    /// `ignoremark_on_target`. `None` (take the walk) without a published
    /// subject or when the stripped pattern does not compile.
    // Cost: the stripped match, plus O(c) to map c capture spans back.
    fn rx_try_ignoremark(
        &mut self,
        pattern: &RegexPattern,
        start: usize,
        pkg: Symbol,
        allow_code: bool,
    ) -> Option<Option<(usize, RegexCaptures)>> {
        let Some(target) = super::super::regex_helpers::current_match_target() else {
            return walked("ignoremark-no-target");
        };
        let stripped = super::super::regex_helpers::strip_marks_pattern(pattern);
        if let Err(why) = rx_program_for_run(&stripped, allow_code) {
            return walked(why);
        }
        let mut run = |interp: &mut Interpreter, stripped: &RegexPattern, chars: &[char]| {
            interp
                .rx_try_match_in(stripped, chars, 0, pkg, allow_code)
                .flatten()
                .into_iter()
                .collect()
        };
        let mut found = self.ignoremark_on_target(pattern, &target, start, &mut run);
        Some(found.pop())
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

    /// `Grammar.parse`'s entry (`regex_match_ends_stop_at_full`): every end of
    /// `pattern` at `start`, highest priority first, up to and including the
    /// first one that covers the whole subject — or `None` when the match must
    /// take the walk.
    // Cost: O(s) in the steps the backtracking search takes to the first full
    // match, as `rx_run`; plus O(c) per end collected, c = its captures.
    pub(in crate::runtime::regex) fn rx_try_ends_until_full(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<Vec<(usize, RegexCaptures)>> {
        if let Err(why) = self.rx_context_allows() {
            return walked(why);
        }
        if pattern.ignore_mark {
            return walked("ignoremark-parse");
        }
        let program = match rx_program_for_run(pattern, true) {
            Ok(program) => program,
            Err(why) => return walked(why),
        };
        crate::vm::vm_stats_regex_vm::record_regex_vm_run();
        let diffing = rx_diff_enabled();
        let mark = diffing.then(super::rx_diff::begin_record);
        let mut ends = Vec::new();
        self.rx_run_goal(&program, chars, start, pkg, Goal::UntilFull(&mut ends));
        if let Some(mark) = mark {
            super::rx_diff::begin_replay(mark);
            let walked = super::super::regex_helpers::isolate_reduced_log(|| {
                self.regex_walk_ends_until_full_for_diff(pattern, chars, start, pkg)
            });
            let replay = super::rx_diff::end_replay();
            let same = super::rx_diff::same_ends(&ends, &walked);
            if let Err(why) = replay.and(same) {
                super::rx_diff::disagreement(format!(
                    "on the ends from {start} of a {}-char subject: {why}\nprogram: {:?}",
                    chars.len(),
                    program.ops
                ));
            }
        }
        Some(ends)
    }
}
