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
use super::regex_ltm_fate::{ltm_fate_frame_close, ltm_fate_frame_open, ltm_fate_frame_take};
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
    /// What each root of a proto's NFA found, by root. Empty for any other NFA.
    /// The fields above then hold what all the roots found together, which no
    /// caller reads.
    pub(super) origins: Vec<OriginRun>,
    /// The `_LL` literals of a proto's roots: (root, where it ended).
    pub(super) origin_ll: Vec<(u32, usize)>,
}

/// What the paths of one root of a proto's NFA found.
#[derive(Clone, Copy, Default)]
pub(super) struct OriginRun {
    /// The furthest accept.
    pub(super) end: Option<usize>,
    /// The furthest fate.
    pub(super) fate: Option<usize>,
    /// Some path went through a `||`.
    pub(super) seqalt: bool,
}

impl OriginRun {
    /// The furthest place any of the root's paths got, an accept or a fate.
    // Cost: O(1).
    pub(super) fn furthest(&self) -> Option<usize> {
        self.end.max(self.fate)
    }

    /// Whether the root's measurement was cut short (`LtmMeasure::stopped`).
    // Cost: O(1).
    pub(super) fn stopped(&self) -> bool {
        self.seqalt || self.fate.is_some()
    }
}

thread_local! {
    /// Finished runs, emptied, for the next run to fill: a grammar parse
    /// measures tens of thousands of proto candidates, and each run
    /// allocated its vectors (#10488). Refilled by [`NfaRun::recycle`].
    static SPARE_RUNS: std::cell::RefCell<Vec<NfaRun>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Spare runs kept past this many are dropped.
const SPARE_RUNS_MAX: usize = 16;

impl NfaRun {
    /// A run with nothing found, for an NFA of `roots` roots (0 for an NFA
    /// that is not a proto's).
    fn empty(roots: usize) -> Self {
        let mut run = SPARE_RUNS
            .with(|spare| spare.borrow_mut().pop())
            .unwrap_or(NfaRun {
                ends: Vec::new(),
                fate: None,
                seqalt: false,
                ll_ends: Vec::new(),
                origins: Vec::new(),
                origin_ll: Vec::new(),
            });
        run.origins.resize(roots, OriginRun::default());
        run
    }

    /// Hand this run's vectors back for the next run to reuse.
    // Cost: O(1) (the vectors are cleared, not freed).
    pub(super) fn recycle(mut self) {
        if self.ends.capacity() == 0
            && self.ll_ends.capacity() == 0
            && self.origins.capacity() == 0
            && self.origin_ll.capacity() == 0
        {
            return;
        }
        self.ends.clear();
        self.ll_ends.clear();
        self.origins.clear();
        self.origin_ll.clear();
        self.fate = None;
        self.seqalt = false;
        SPARE_RUNS.with(|spare| {
            let mut spare = spare.borrow_mut();
            if spare.len() < SPARE_RUNS_MAX {
                spare.push(self);
            }
        });
    }

    /// An `_LL` literal ended at `end` on a path from root `origin`.
    fn cross_ll(&mut self, origin: u32, end: usize) {
        if self.origins.is_empty() {
            if self.ll_ends.last() != Some(&end) {
                self.ll_ends.push(end);
            }
        } else if self.origin_ll.last() != Some(&(origin, end)) {
            self.origin_ll.push((origin, end));
        }
    }

    /// A path from root `origin` ended in a fate at `pos`.
    fn fate_at(&mut self, origin: u32, pos: usize) {
        self.fate = self.fate.max(Some(pos));
        if let Some(found) = self.origins.get_mut(origin as usize) {
            found.fate = found.fate.max(Some(pos));
        }
    }

    /// A fate no root can be named for: it cuts the paths of all of them.
    fn fate_in_every_root(&mut self, pos: usize) {
        self.fate = self.fate.max(Some(pos));
        for found in &mut self.origins {
            found.fate = found.fate.max(Some(pos));
        }
    }

    /// A path from root `origin` went through a `||`.
    fn seqalt_at(&mut self, origin: u32) {
        self.seqalt = true;
        if let Some(found) = self.origins.get_mut(origin as usize) {
            found.seqalt = true;
        }
    }

    /// What a nested region run (a `Sub`, a `DynCall`'s callee) found, as far as
    /// it ends or cuts paths of root `origin`.
    fn absorb_region(&mut self, origin: u32, region: &NfaRun) {
        if region.seqalt {
            self.seqalt_at(origin);
        }
        if let Some(fate) = region.fate {
            self.fate_at(origin, fate);
        }
    }

    /// A path from root `origin` reached an accept at `pos`.
    fn end_at(&mut self, origin: u32, pos: usize) {
        if let Some(found) = self.origins.get_mut(origin as usize) {
            found.end = found.end.max(Some(pos));
        } else {
            self.ends.push(pos);
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
        if let Some(fate) = leaf_fate {
            run.fate_in_every_root(fate);
        }
        run
    }

    fn walk(
        &self,
        interp: &mut Interpreter,
        chars: &[char],
        start: usize,
        outer: &[Symbol],
    ) -> NfaRun {
        let mut out = NfaRun::empty(self.roots.len());
        let proto = !self.roots.is_empty();
        let mut scratch = Scratch::take(self.nodes.len(), outer);
        let Scratch {
            stacks,
            seen,
            work,
            step,
            far,
        } = &mut scratch;
        if proto {
            for (origin, root) in self.roots.iter().enumerate() {
                if let Some(stack) = stacks.push_root(origin as u32, root.accept) {
                    work.push((root.entry, stack));
                }
            }
        } else {
            work.push((self.start, 0));
        }
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
                        out.seqalt_at(stacks.origin(stack));
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
                                    out.cross_ll(stacks.origin(stack), end);
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
                            out.fate_at(stacks.origin(stack), pos);
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
                            out.fate_at(stacks.origin(stack), pos);
                        } else if let Some(seed) =
                            super::regex_lr_state::lr_read_live_seed(*name, chars.len() - pos)
                        {
                            for end in seed {
                                reach(end, *ret, stack, work);
                            }
                        } else if let Some(called) = stacks.push(stack, *ret, *name) {
                            work.push((*body, called));
                        } else {
                            out.fate_at(stacks.origin(stack), pos);
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
                        let origin = stacks.origin(stack);
                        out.absorb_region(origin, &region);
                        // A subrule's own `_LL` literals count, as they do
                        // for a call compiled into this NFA.
                        for end in region.ll_ends {
                            out.cross_ll(origin, end);
                        }
                        for end in region.ends {
                            reach(end, *next, stack, work);
                        }
                    }
                    NfaNode::Sub { nfa, kind, next } => {
                        let names = stacks.names(stack);
                        let region = run_sub(nfa, kind, interp, chars, pos, &names);
                        out.absorb_region(stacks.origin(stack), &region);
                        for end in region.ends {
                            reach(end, *next, stack, work);
                        }
                    }
                    NfaNode::Fate => out.fate_at(stacks.origin(stack), pos),
                    NfaNode::Accept => out.ends.push(pos),
                    NfaNode::AcceptAt(origin) => out.end_at(*origin, pos),
                }
                // A leaf's matcher records the fates of the user code it
                // refused to run into the frame of the run; in a proto's run
                // the frame is shared by every root, so it is read after each
                // leaf, while the root it belongs to is known.
                if proto
                    && matches!(
                        &self.nodes[node as usize],
                        NfaNode::Leaf { .. } | NfaNode::WsLead { .. } | NfaNode::DynCall { .. }
                    )
                    && let Some(fate) = ltm_fate_frame_take()
                {
                    out.fate_at(stacks.origin(stack), fate);
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
                    ..NfaRun::empty(0)
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
                ..NfaRun::empty(0)
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
    let mut out = NfaRun::empty(0);
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
