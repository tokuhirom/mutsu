//! Simulating an [`LtmNfa`] over the subject (ADR-0125).
//!
//! A thread of the simulation is a node plus a call stack (an id into a
//! [`Stacks`](super::regex_ltm_nfa_scratch::Stacks) table: the return nodes of the rules being called, innermost
//! last). Positions are processed in increasing order. Each position keeps
//! the set of threads reached there, and a thread is expanded at most once per
//! position, so a run costs O(positions × threads) plus the leaves' own
//! matching. A leaf may jump more than one character (a grapheme, a builtin
//! `<ident>`), so besides the next position's frontier the run keeps a
//! min-heap of further ones.
//!
//! The run happens under `LTM_DECLARATIVE_MODE`, inside a fate frame of its
//! own: a leaf's matcher never runs user code, and a fate it records (a code
//! block inside a token a `<+name>` class calls, say) records into this run.

use super::super::*;
use super::regex_helpers::LTM_DECLARATIVE_MODE;
use super::regex_ltm_fate::{ltm_fate_frame_close, ltm_fate_frame_open};
use super::regex_ltm_nfa::{LeafKind, LtmNfa, NfaNode, SubKind};
use super::regex_ltm_nfa_scratch::{Scratch, Thread};
use std::cmp::Reverse;

/// What a run found.
pub(super) struct NfaRun {
    /// Every position an accept was reached at.
    pub(super) ends: Vec<usize>,
    /// The furthest fate.
    pub(super) fate: Option<usize>,
    /// Some path went through a `||` (see `LtmMeasure::stopped`).
    pub(super) seqalt: bool,
    /// Where each `_LL` literal a path crossed ended (see
    /// `LtmMeasure::litlen`), without repeating the previous entry.
    pub(super) ll_ends: Vec<usize>,
}

thread_local! {
    /// The vectors of finished runs, for the next run to fill: a grammar
    /// parse measures tens of thousands of proto candidates, and each run
    /// allocated both (#10488). Refilled by [`NfaRun::recycle`].
    static SPARE_RUNS: std::cell::RefCell<Vec<(Vec<usize>, Vec<usize>)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Spare vector pairs kept past this many are dropped.
const SPARE_RUNS_MAX: usize = 16;

impl NfaRun {
    fn empty() -> Self {
        let (ends, ll_ends) = SPARE_RUNS
            .with(|spare| spare.borrow_mut().pop())
            .unwrap_or_default();
        NfaRun {
            ends,
            fate: None,
            seqalt: false,
            ll_ends,
        }
    }

    /// Hand this run's vectors back for the next run to reuse.
    // Cost: O(1) (the vectors are cleared, not freed).
    pub(super) fn recycle(self) {
        let (mut ends, mut ll_ends) = (self.ends, self.ll_ends);
        if ends.capacity() == 0 && ll_ends.capacity() == 0 {
            return;
        }
        ends.clear();
        ll_ends.clear();
        SPARE_RUNS.with(|spare| {
            let mut spare = spare.borrow_mut();
            if spare.len() < SPARE_RUNS_MAX {
                spare.push((ends, ll_ends));
            }
        });
    }

    fn cross_ll(&mut self, end: usize) {
        if self.ll_ends.last() != Some(&end) {
            self.ll_ends.push(end);
        }
    }
}

impl LtmNfa {
    /// Run the NFA from `start`. `outer` names the rules an enclosing run is
    /// already inlining, for the recursion cut.
    // Cost: O(n * t) plus leaf matching, n = positions reached past `start`,
    // t = threads live at a position.
    pub(super) fn run(
        &self,
        interp: &mut Interpreter,
        chars: &[char],
        start: usize,
        outer: &[Symbol],
    ) -> NfaRun {
        let saved_mode = LTM_DECLARATIVE_MODE.with(|f| f.replace(true));
        let enclosing_fate = ltm_fate_frame_open();
        let mut run = self.walk(interp, chars, start, outer);
        let leaf_fate = ltm_fate_frame_close(enclosing_fate);
        LTM_DECLARATIVE_MODE.with(|f| f.set(saved_mode));
        run.fate = run.fate.max(leaf_fate);
        run
    }

    fn walk(
        &self,
        interp: &mut Interpreter,
        chars: &[char],
        start: usize,
        outer: &[Symbol],
    ) -> NfaRun {
        let mut out = NfaRun::empty();
        let mut scratch = Scratch::take(self.nodes.len(), outer);
        let Scratch {
            stacks,
            seen,
            work,
            step,
            far,
        } = &mut scratch;
        work.push((self.start, 0));
        let mut pos = start;
        loop {
            while let Some((node, stack)) = work.pop() {
                if !seen.insert(node, stack) {
                    continue;
                }
                let mut reach = |end: usize, next: u32, stack: u32, work: &mut Vec<Thread>| {
                    if end == pos {
                        work.push((next, stack));
                    } else if end == pos + 1 {
                        step.push((next, stack));
                    } else if end > pos {
                        far.push(Reverse((end, next, stack)));
                    }
                };
                match &self.nodes[node as usize] {
                    NfaNode::Split(targets) => {
                        work.extend(targets.iter().map(|&target| (target, stack)));
                    }
                    NfaNode::SeqAlt(targets) => {
                        out.seqalt = true;
                        work.extend(targets.iter().map(|&target| (target, stack)));
                    }
                    NfaNode::Leaf {
                        atom,
                        pkg,
                        ic,
                        kind,
                        ll,
                        next,
                    } => match kind {
                        LeafKind::Consume => {
                            if let Some(end) =
                                interp.match_consuming_atom(atom, chars, pos, *pkg, *ic)
                            {
                                if *ll {
                                    out.cross_ll(end);
                                }
                                reach(end, *next, stack, work);
                            }
                        }
                        LeafKind::Probe => {
                            if let Some(end) =
                                interp.regex_match_atom_in_pkg(atom, chars, pos, *pkg, *ic)
                            {
                                reach(end, *next, stack, work);
                            }
                        }
                        LeafKind::Plural => {
                            for end in plural_ends(interp, atom, chars, pos, *pkg, *ic) {
                                reach(end, *next, stack, work);
                            }
                        }
                    },
                    NfaNode::WsLead {
                        atom,
                        pkg,
                        ic,
                        next,
                    } => {
                        if pos != 0 {
                            out.fate = out.fate.max(Some(pos));
                        } else if matches!(**atom, RegexAtom::WsRule) {
                            if let Some(end) =
                                interp.regex_match_atom_in_pkg(atom, chars, pos, *pkg, *ic)
                            {
                                reach(end, *next, stack, work);
                            }
                        } else {
                            for end in plural_ends(interp, atom, chars, pos, *pkg, *ic) {
                                reach(end, *next, stack, work);
                            }
                        }
                    }
                    NfaNode::AtStart(next) => {
                        if pos == 0 {
                            work.push((*next, stack));
                        }
                    }
                    NfaNode::AtEnd(next) => {
                        if pos == chars.len() {
                            work.push((*next, stack));
                        }
                    }
                    NfaNode::Call { name, body, ret } => {
                        if stacks.calls(stack, *name) {
                            out.fate = out.fate.max(Some(pos));
                        } else if let Some(seed) =
                            super::regex_lr_state::lr_read_live_seed(*name, chars.len() - pos)
                        {
                            for end in seed {
                                reach(end, *ret, stack, work);
                            }
                        } else if let Some(called) = stacks.push(stack, *ret, *name) {
                            work.push((*body, called));
                        } else {
                            out.fate = out.fate.max(Some(pos));
                        }
                    }
                    NfaNode::Return => {
                        let (parent, ret, _) = stacks.entries[stack as usize];
                        work.push((ret, parent));
                    }
                    NfaNode::DynCall {
                        atom,
                        pkg,
                        ic,
                        next,
                    } => {
                        let names = stacks.names(stack);
                        let region = dyn_call(interp, atom, chars, pos, *pkg, *ic, &names);
                        out.seqalt |= region.seqalt;
                        out.fate = out.fate.max(region.fate);
                        // A subrule's own `_LL` literals count, as they do
                        // for a call compiled into this NFA.
                        for end in region.ll_ends {
                            out.cross_ll(end);
                        }
                        for end in region.ends {
                            reach(end, *next, stack, work);
                        }
                    }
                    NfaNode::Sub { nfa, kind, next } => {
                        let names = stacks.names(stack);
                        let region = run_sub(nfa, kind, interp, chars, pos, &names);
                        out.seqalt |= region.seqalt;
                        out.fate = out.fate.max(region.fate);
                        for end in region.ends {
                            reach(end, *next, stack, work);
                        }
                    }
                    NfaNode::Fate => out.fate = out.fate.max(Some(pos)),
                    NfaNode::Accept => out.ends.push(pos),
                }
            }
            // Advance to the nearest position anything reached.
            pos = if !step.is_empty() {
                std::mem::swap(work, step);
                pos + 1
            } else if let Some(&Reverse((end, _, _))) = far.peek() {
                end
            } else {
                break;
            };
            seen.advance();
            while let Some(&Reverse((end, node, stack))) = far.peek()
                && end == pos
            {
                far.pop();
                work.push((node, stack));
            }
        }
        scratch.give_back();
        out
    }
}

/// A [`NfaNode::Sub`] region at `pos`.
fn run_sub(
    nfa: &LtmNfa,
    kind: &SubKind,
    interp: &mut Interpreter,
    chars: &[char],
    pos: usize,
    names: &[Symbol],
) -> NfaRun {
    match kind {
        SubKind::Scoped(scope) => {
            let saved = interp.install_env_scope(scope);
            let run = nfa.run(interp, chars, pos, names);
            interp.uninstall_regex_closure_scope(Some(saved));
            run
        }
        SubKind::StripMarks => {
            // The stripped subject belongs to the match target; a measurement
            // of anything else has no stripped form, and ends here.
            let target = super::regex_helpers::current_match_target()
                .filter(|target| target.chars().len() == chars.len());
            let Some(target) = target else {
                return NfaRun {
                    fate: Some(pos),
                    ..NfaRun::empty()
                };
            };
            let stripped = target.stripped();
            let from = stripped.original_to_stripped(pos);
            let run = nfa.run(interp, stripped.chars(), from, names);
            let back = |end: usize| stripped.stripped_to_original(end).max(pos);
            // An ignoremark literal has no `_LL` form: nothing in the region
            // counts toward `litlen`.
            NfaRun {
                ends: run.ends.into_iter().map(back).collect(),
                fate: run.fate.map(back),
                seqalt: run.seqalt,
                ll_ends: Vec::new(),
            }
        }
    }
}

/// A [`NfaNode::DynCall`] at `pos`: resolve the call now and run each
/// candidate's NFA, with the called name added to the recursion cut.
fn dyn_call(
    interp: &mut Interpreter,
    atom: &RegexAtom,
    chars: &[char],
    pos: usize,
    pkg: Symbol,
    ic: bool,
    names: &[Symbol],
) -> NfaRun {
    let mut out = NfaRun::empty();
    let RegexAtom::Named(name) = atom else {
        return out;
    };
    let spec = name.spec();
    if names.contains(&spec.lookup_sym) || interp.subrule_has_qq_thunks(&spec.lookup_name, pkg) {
        out.fate = Some(pos);
        return out;
    }
    // ADR-0127 §2.2: a call's arguments are ignored here, so a body that
    // reads its own parameters at parse time (`token v($x) { <$x> }`) cannot
    // be resolved — the unbound parameter is an artifact of the measurement,
    // not an error of the program. Such a call is a fate, as Rakudo's `<$x>`
    // is; the real match binds the arguments and reports any genuine error.
    let ignores_args = !spec.arg_exprs.is_empty();
    let prior_error = ignores_args
        .then(|| crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow_mut().take()))
        .flatten();
    let (candidates, raw_empty) = interp.parsed_subrule_candidates(spec, pkg, &[]);
    if ignores_args {
        let failed = crate::runtime::regex_parse::PENDING_REGEX_ERROR
            .with(|e| std::mem::replace(&mut *e.borrow_mut(), prior_error))
            .is_some();
        if failed {
            out.fate = Some(pos);
            return out;
        }
    }
    if candidates.is_empty() {
        if raw_empty
            && interp
                .registry()
                .user_method_overloads(pkg.as_str(), &spec.lookup_name)
                .is_some()
        {
            out.fate = Some(pos);
        } else {
            out.ends = plural_ends(interp, atom, chars, pos, pkg, ic);
        }
        return out;
    }
    let mut names = names.to_vec();
    names.push(spec.lookup_sym);
    for (parsed, sub_pkg, _) in candidates.iter() {
        let nfa = interp.ltm_nfa_for(parsed, *sub_pkg, ic);
        let run = nfa.run(interp, chars, pos, &names);
        out.ends.extend(run.ends);
        out.fate = out.fate.max(run.fate);
        out.seqalt |= run.seqalt;
        out.ll_ends.extend(run.ll_ends);
    }
    out
}

/// Every end of `atom` at `pos`, from the plural atom matcher.
fn plural_ends(
    interp: &mut Interpreter,
    atom: &RegexAtom,
    chars: &[char],
    pos: usize,
    pkg: Symbol,
    ic: bool,
) -> Vec<usize> {
    interp
        .regex_match_atom_all_with_capture_in_pkg(
            atom,
            chars,
            pos,
            &RegexCaptures::default(),
            pkg,
            ic,
        )
        .into_iter()
        .map(|(end, _)| end)
        .collect()
}
