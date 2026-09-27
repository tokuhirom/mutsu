//! Simulating an [`LtmNfa`] over the subject (ADR-0125).
//!
//! A thread of the simulation is a node plus a call stack (an id into a
//! [`Stacks`] table: the return nodes of the rules being called, innermost
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
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Reverse;
use std::collections::BinaryHeap;

/// Past this many distinct call stacks in one run, a further call is a fate
/// instead of a new stack. The recursion cut already bounds a stack's depth
/// by the number of rules; this bounds their number for a grammar whose
/// rules call each other along very many distinct paths.
const MAX_STACKS: usize = 1 << 16;

/// What a run found.
pub(super) struct NfaRun {
    /// Every position an accept was reached at.
    pub(super) ends: Vec<usize>,
    /// The furthest fate.
    pub(super) fate: Option<usize>,
    /// Some path went through a `||` (see `LtmMeasure::stopped`).
    pub(super) seqalt: bool,
}

/// The call stacks of one run. Id 0 is the empty stack; every other id is a
/// `(parent id, return node, rule name)` entry, shared by every thread that
/// made the same calls.
struct Stacks {
    entries: Vec<(u32, u32, Symbol)>,
    index: FxHashMap<(u32, u32), u32>,
    /// Rule names already being inlined by the enclosing run, for a
    /// [`NfaNode::Sub`] or [`NfaNode::DynCall`] region's own run.
    outer: Vec<Symbol>,
}

impl Stacks {
    fn new(outer: &[Symbol]) -> Self {
        Stacks {
            entries: vec![(0, 0, Symbol::intern(""))],
            index: FxHashMap::default(),
            outer: outer.to_vec(),
        }
    }

    /// Is `name` being called on `stack`, or by an enclosing run?
    // Cost: O(d), d = the stack's depth (at most the number of rules).
    fn calls(&self, mut stack: u32, name: Symbol) -> bool {
        while stack != 0 {
            let (parent, _, called) = self.entries[stack as usize];
            if called == name {
                return true;
            }
            stack = parent;
        }
        self.outer.contains(&name)
    }

    /// The names on `stack` and the enclosing runs', for a nested run.
    fn names(&self, mut stack: u32) -> Vec<Symbol> {
        let mut names = self.outer.clone();
        while stack != 0 {
            let (parent, _, called) = self.entries[stack as usize];
            names.push(called);
            stack = parent;
        }
        names
    }

    fn push(&mut self, stack: u32, ret: u32, name: Symbol) -> Option<u32> {
        if let Some(&id) = self.index.get(&(stack, ret)) {
            return Some(id);
        }
        if self.entries.len() >= MAX_STACKS {
            return None;
        }
        let id = self.entries.len() as u32;
        self.entries.push((stack, ret, name));
        self.index.insert((stack, ret), id);
        Some(id)
    }
}

/// The threads already expanded at the current position. Almost every node
/// is reached with one stack per position, so the first stack is kept in a
/// flat array and only the rest go to a hash set.
struct Seen {
    pos: Vec<usize>,
    stack: Vec<u32>,
    more: FxHashSet<(u32, u32)>,
}

impl Seen {
    fn new(nodes: usize) -> Self {
        Seen {
            pos: vec![usize::MAX; nodes],
            stack: vec![0; nodes],
            more: FxHashSet::default(),
        }
    }

    /// Record `(node, stack)` at `pos`; `false` when it already was.
    fn insert(&mut self, node: u32, stack: u32, pos: usize) -> bool {
        let n = node as usize;
        if self.pos[n] != pos {
            self.pos[n] = pos;
            self.stack[n] = stack;
            return true;
        }
        self.stack[n] != stack && self.more.insert((node, stack))
    }

    fn advance(&mut self) {
        if !self.more.is_empty() {
            self.more.clear();
        }
    }
}

type Thread = (u32, u32);

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
        let mut out = NfaRun {
            ends: Vec::new(),
            fate: None,
            seqalt: false,
        };
        let mut stacks = Stacks::new(outer);
        let mut seen = Seen::new(self.nodes.len());
        // Threads still to expand at `pos`, threads reached at `pos + 1`
        // (almost every leaf consumes one grapheme of one char), and the rest.
        let mut work: Vec<Thread> = vec![(self.start, 0)];
        let mut step: Vec<Thread> = Vec::new();
        let mut far: BinaryHeap<Reverse<(usize, u32, u32)>> = BinaryHeap::new();
        let mut pos = start;
        loop {
            while let Some((node, stack)) = work.pop() {
                if !seen.insert(node, stack, pos) {
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
                        next,
                    } => match kind {
                        LeafKind::Consume => {
                            if let Some(end) =
                                interp.match_consuming_atom(atom, chars, pos, *pkg, *ic)
                            {
                                reach(end, *next, stack, &mut work);
                            }
                        }
                        LeafKind::Probe => {
                            if let Some(end) =
                                interp.regex_match_atom_in_pkg(atom, chars, pos, *pkg, *ic)
                            {
                                reach(end, *next, stack, &mut work);
                            }
                        }
                        LeafKind::Plural => {
                            for end in plural_ends(interp, atom, chars, pos, *pkg, *ic) {
                                reach(end, *next, stack, &mut work);
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
                                reach(end, *next, stack, &mut work);
                            }
                        } else {
                            for end in plural_ends(interp, atom, chars, pos, *pkg, *ic) {
                                reach(end, *next, stack, &mut work);
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
                                reach(end, *ret, stack, &mut work);
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
                        for end in region.ends {
                            reach(end, *next, stack, &mut work);
                        }
                    }
                    NfaNode::Sub { nfa, kind, next } => {
                        let names = stacks.names(stack);
                        let region = run_sub(nfa, kind, interp, chars, pos, &names);
                        out.seqalt |= region.seqalt;
                        out.fate = out.fate.max(region.fate);
                        for end in region.ends {
                            reach(end, *next, stack, &mut work);
                        }
                    }
                    NfaNode::Fate => out.fate = out.fate.max(Some(pos)),
                    NfaNode::Accept => out.ends.push(pos),
                }
            }
            // Advance to the nearest position anything reached.
            pos = if !step.is_empty() {
                std::mem::swap(&mut work, &mut step);
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
                    ends: Vec::new(),
                    fate: Some(pos),
                    seqalt: false,
                };
            };
            let stripped = target.stripped();
            let from = stripped.original_to_stripped(pos);
            let run = nfa.run(interp, stripped.chars(), from, names);
            let back = |end: usize| stripped.stripped_to_original(end).max(pos);
            NfaRun {
                ends: run.ends.into_iter().map(back).collect(),
                fate: run.fate.map(back),
                seqalt: run.seqalt,
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
    let mut out = NfaRun {
        ends: Vec::new(),
        fate: None,
        seqalt: false,
    };
    let RegexAtom::Named(name) = atom else {
        return out;
    };
    let spec = name.spec();
    if names.contains(&spec.lookup_sym) || interp.subrule_has_qq_thunks(&spec.lookup_name, pkg) {
        out.fate = Some(pos);
        return out;
    }
    let (candidates, raw_empty) = interp.parsed_subrule_candidates(spec, pkg, &[]);
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
