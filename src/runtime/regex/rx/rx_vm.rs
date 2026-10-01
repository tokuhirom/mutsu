//! The backtracking loop that runs an [`RxProgram`] (ADR-0135 D2).
//!
//! State: `pc`, `pos`, the capture levels (`rx_levels`: the walk's own
//! `CapStore`s, whose journal restores captures on backtrack), a register
//! arena whose writes go through a second undo trail, and one explicit stack
//! of choice points. A choice point records the capture-journal and
//! register-trail lengths to rewind to, the arena length and the frame it was
//! pushed in (`rx_frame`); a ratchet cut drops choice points only, never trail
//! entries, so an earlier choice point still rewinds correctly.
//!
//! A `<subrule>` call that resolves to a plain compiled rule (`rx_call`)
//! switches the loop to the callee's program in a new frame and capture level;
//! the callee's `Match` is the return, which files the callee's captures as
//! the subrule's own Match (`build_named_candidates_from_inner`, the walk's
//! own builder) and continues the caller.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use super::super::regex_zero_width_iter::zero_width_iter_counts;
use super::rx_call::CallTarget;
use super::rx_entry::{Goal, Scratch, program_for};
use super::rx_frame::{Choice, FMark, Frame, MAX_FRAME_DEPTH, Mark, ProtoChoice};
use super::{RxOp, RxProgram};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{RegexAtom, RegexCaptures, RegexQuant};
use crate::symbol::Symbol;
use crate::value::Value;
use crate::vm::vm_stats_regex_vm::{WalkUse, record_regex_walk as walk_use};

/// The program the loop is executing: the one the run started with, or the
/// callee of the current frame.
enum Cur<'a> {
    Root(&'a RxProgram),
    Callee(Arc<RxProgram>),
}

impl Cur<'_> {
    #[inline]
    fn get(&self) -> &RxProgram {
        match self {
            Cur::Root(p) => p,
            Cur::Callee(p) => p,
        }
    }
}

impl Interpreter {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn rx_run_in<const FRAMES: bool>(
        &mut self,
        root: &RxProgram,
        chars: &[char],
        start: usize,
        root_pkg: Symbol,
        mut goal: Goal<'_>,
        seed: Option<RegexCaptures>,
        scratch: &mut Scratch,
    ) -> Option<(usize, RegexCaptures)> {
        let Scratch {
            regs,
            reg_trail,
            stack,
            fmarks,
            ends,
            levels,
            ltm_order,
        } = scratch;
        regs.clear();
        regs.resize(root.nregs, 0);
        reg_trail.clear();
        stack.clear();
        fmarks.clear();
        ends.clear();
        levels.reset(start);
        if let Some(seed) = seed {
            levels.seed(seed);
        }
        let mut cur = Cur::Root(root);
        let mut frame: Option<Rc<Frame>> = None;
        // The grammar instance the run's own pattern owns, as `Frame::cursor` is
        // the callee's (#9803). Filed on the result at the pattern's `Match`.
        let root_cursor: RefCell<Option<Value>> = RefCell::new(None);
        // The register window and package of the frame being run.
        let mut base = 0usize;
        let mut pkg = root_pkg;
        let mut pc = 0u32;
        let mut pos = start;
        let mut farthest = start;
        macro_rules! reg {
            ($r:expr) => {
                regs[base + $r as usize]
            };
        }
        macro_rules! set_reg {
            ($r:expr, $v:expr) => {{
                let i = base + $r as usize;
                reg_trail.push((i, regs[i]));
                regs[i] = $v;
            }};
        }
        // The run's own pattern matched: its captures, with the grammar instance
        // it owns when a method it called wrote to one.
        macro_rules! root_snapshot {
            () => {{
                let mut snap = levels.top().snapshot();
                if FRAMES && let Some(cursor) = root_cursor.borrow().as_ref() {
                    snap.set_cursor(cursor.clone());
                }
                snap
            }};
        }
        // What a choice point pushed now rewinds to.
        macro_rules! mark {
            () => {
                Mark {
                    cap: levels.mark(),
                    reg: reg_trail.len(),
                }
            };
        }
        // Push a choice point, with the frame state it restores when it is
        // pushed while a callee (or its register window) is live.
        macro_rules! push_choice {
            ($choice:expr) => {{
                let choice = $choice;
                if FRAMES && (frame.is_some() || regs.len() != root.nregs) {
                    fmarks.push(FMark {
                        at: stack.len(),
                        regs_len: regs.len(),
                        frame: frame.clone(),
                    });
                }
                stack.push(choice);
            }};
        }
        // Drop the choice points above height `$n`, and their frame state.
        macro_rules! truncate_stack {
            ($n:expr) => {{
                let n: usize = $n;
                stack.truncate(n);
                if FRAMES {
                    while fmarks.last().is_some_and(|m| m.at >= n) {
                        fmarks.pop();
                    }
                }
            }};
        }
        // Enter the highest-priority of `cands` (lowest priority first); the
        // rest wait on the stack as one choice point resuming at `pc`.
        macro_rules! enter_cands {
            ($cands:expr) => {{
                let cands: Vec<(usize, RegexCaptures)> = $cands;
                if let Some((end, delta)) = cands.last().cloned() {
                    let left = cands.len() - 1;
                    if left > 0 {
                        push_choice!(Choice::Cands {
                            pc,
                            cands: Rc::new(cands),
                            left,
                            mark: mark!(),
                        });
                    }
                    levels.edit(|s| s.merge_delta(delta));
                    pos = end;
                    farthest = farthest.max(pos);
                    true
                } else {
                    false
                }
            }};
        }
        // Switch the loop to a callee: a new register window and capture level,
        // and a frame that returns to `$ret_pc` in the caller.
        macro_rules! enter_frame {
            ($callee:expr, $callee_pkg:expr, $atom:expr, $entry:expr, $ret_pc:expr,
             $commit:expr, $stack_base:expr, $proto:expr) => {{
                let callee: Arc<RxProgram> = $callee;
                let callee_pkg: Symbol = $callee_pkg;
                let entry: usize = $entry;
                // With no choice point left nothing can rewind, so the undo
                // trails start over: a run of ratcheted calls stays flat.
                if stack.is_empty() {
                    reg_trail.clear();
                    levels.clear_journal();
                }
                let (journal_base, trail_base) = (levels.mark(), reg_trail.len());
                let new_base = regs.len();
                regs.resize(new_base + callee.nregs, 0);
                levels.open(entry, false);
                let depth = frame.as_ref().map_or(0, |f| f.depth) + 1;
                frame = Some(Rc::new(Frame {
                    parent: frame.take(),
                    program: Arc::clone(&callee),
                    pkg: callee_pkg,
                    base: new_base,
                    ret_pc: $ret_pc,
                    entry_pos: entry,
                    site: $atom,
                    commit: $commit,
                    stack_base: $stack_base,
                    journal_base,
                    trail_base,
                    ends_base: ends.len(),
                    proto: $proto,
                    depth,
                    seen: RefCell::new(Vec::new()),
                    cursor: RefCell::new(None),
                }));
                cur = Cur::Callee(callee);
                base = new_base;
                pkg = callee_pkg;
                pc = 0;
                pos = entry;
            }};
        }
        let result = 'run: loop {
            // The program is fixed until the loop switches frames, which
            // every such site does by `continue 'run`: the dispatch below
            // reads `program` directly, as a loop without calls would.
            let program = cur.get();
            loop {
                let ok = match program.ops[pc as usize] {
                    // Cost: O(1) on the ASCII fast path, else O(g), g = the
                    // grapheme's length at `pos`.
                    RxOp::Atom(i) => match self.rx_atom_at(program, i as usize, chars, pos, pkg) {
                        Some(next) => {
                            pos = next;
                            farthest = farthest.max(pos);
                            pc += 1;
                            true
                        }
                        None => false,
                    },
                    // Cost: O(k·g), k = the iterations matched (each given back at
                    // most once, O(1) per give-back).
                    RxOp::AtomRun {
                        atom,
                        min,
                        max,
                        possessive,
                    } => {
                        // `ends[run + c]` is where the cursor stands after `c`
                        // iterations, so count 0 is the run's own start.
                        let run = ends.len();
                        ends.push(pos);
                        let mut n = 0u32;
                        let mut at = pos;
                        while n < max {
                            let Some(next) =
                                self.rx_atom_at(program, atom as usize, chars, at, pkg)
                            else {
                                break;
                            };
                            at = next;
                            ends.push(at);
                            n += 1;
                        }
                        if n < min {
                            ends.truncate(run);
                            false
                        } else {
                            pos = ends[run + n as usize];
                            farthest = farthest.max(pos);
                            if possessive || n == min {
                                ends.truncate(run);
                            } else {
                                // Give back counts n-1 down to min.
                                push_choice!(Choice::Run {
                                    pc: pc + 1,
                                    base: run,
                                    lo: run + min as usize,
                                    hi: run + n as usize,
                                    mark: mark!(),
                                });
                            }
                            pc += 1;
                            true
                        }
                    }
                    // Cost: O(1) for every assertion Slice A compiles.
                    RxOp::Assert(i) => {
                        let hit = self
                            .regex_match_atom_in_pkg(
                                &program.atoms[i as usize],
                                chars,
                                pos,
                                pkg,
                                program.atom_ic[i as usize],
                            )
                            .is_some();
                        pc += 1;
                        hit
                    }
                    // Cost: O(1).
                    RxOp::AssertStart => {
                        pc += 1;
                        pos == 0
                    }
                    // Cost: O(1).
                    RxOp::AssertEnd => {
                        pc += 1;
                        pos == chars.len()
                    }
                    // Cost: O(1) amortized.
                    RxOp::Split { prefer, alt } => {
                        push_choice!(Choice::At {
                            pc: alt,
                            pos,
                            mark: mark!(),
                        });
                        pc = prefer;
                        true
                    }
                    // Cost: O(b·m + b log b), b = the branches, m = one LTM
                    // measurement (`rx_ltm_order`); O(b) choice points pushed.
                    RxOp::LtmAlt(t) => {
                        let table = &program.ltm_alts[t as usize];
                        self.rx_ltm_order(program, table, chars, pos, pkg, ltm_order);
                        // Lower-ranked branches wait on the stack, the next-best
                        // on top; the best one is entered now.
                        for &(i, _) in ltm_order[1..].iter().rev() {
                            push_choice!(Choice::At {
                                pc: table.pcs[i],
                                pos,
                                mark: mark!(),
                            });
                        }
                        pc = table.pcs[ltm_order[0].0];
                        true
                    }
                    // Cost: O(n + r) plus the code's run and the match of the pattern
                    // it yields (`regex_code_interp_ends`), then O(c) per candidate
                    // entered, c = the captures it adds.
                    RxOp::InterpEnds(i) => {
                        let RegexAtom::CodeInterp { code, list } = &program.atoms[i as usize]
                        else {
                            debug_assert!(false, "an InterpEnds op names a CodeInterp atom");
                            break 'run None;
                        };
                        walk_use(WalkUse::Bridged, "code-interp");
                        let cands = self.regex_code_interp_ends(
                            code,
                            *list,
                            chars,
                            pos,
                            levels.top().caps(),
                            pkg,
                            program.atom_ic[i as usize],
                        );
                        pc += 1;
                        enter_cands!(cands)
                    }
                    // Cost: O(1) expected to resolve the callee, then O(1) to enter
                    // its frame; a bridged call is the walk's producer, which
                    // computes the callee's ends (`regex_match_atom_all_with_capture_opts`)
                    // and costs O(c) per candidate entered, c = the captures it adds;
                    // the first call in a frame that runs a grammar method also creates
                    // the frame's cursor, O(a), a = the grammar's attributes.
                    // The callee's own ops state their costs.
                    RxOp::Call { atom, commit } => {
                        if !FRAMES {
                            debug_assert!(false, "a program without frames has no Call op");
                            break 'run None;
                        }
                        let RegexAtom::Named(name) = &program.atoms[atom as usize] else {
                            debug_assert!(false, "a Call op names a `<subrule>` atom");
                            break 'run None;
                        };
                        let ic = program.atom_ic[atom as usize];
                        match self.rx_call_target_checked(name, pkg, ic) {
                            Ok(CallTarget::Plain(callee, callee_pkg)) => {
                                if frame.as_ref().is_some_and(|f| f.depth >= MAX_FRAME_DEPTH) {
                                    false
                                } else {
                                    let stack_base = stack.len();
                                    enter_frame!(
                                        callee,
                                        callee_pkg,
                                        atom,
                                        pos,
                                        pc + 1,
                                        commit,
                                        stack_base,
                                        None
                                    );
                                    continue 'run;
                                }
                            }
                            Ok(CallTarget::Proto(cands)) => {
                                let ranked = self.rx_rank_proto(&cands, chars, pos);
                                match ranked.first().copied() {
                                    // No candidate can match here.
                                    None => false,
                                    Some(_)
                                        if frame
                                            .as_ref()
                                            .is_some_and(|f| f.depth >= MAX_FRAME_DEPTH) =>
                                    {
                                        false
                                    }
                                    Some(first) => {
                                        // The call is committed to the first ranked
                                        // candidate that matches, and to its first
                                        // end, so a cut at its return drops the
                                        // rest of the ranking too.
                                        let stack_base = stack.len();
                                        if ranked.len() > 1 {
                                            push_choice!(Choice::Proto(Box::new(ProtoChoice {
                                                pc: pc + 1,
                                                pos,
                                                atom,
                                                cands: Arc::clone(&cands),
                                                ranked: Rc::new(ranked),
                                                next: 1,
                                                mark: mark!(),
                                            })));
                                        }
                                        let (parsed, sub_pkg, _) = &cands[first];
                                        let Some(callee) = program_for(parsed) else {
                                            debug_assert!(false, "a proto's candidates compile");
                                            break 'run None;
                                        };
                                        enter_frame!(
                                            Arc::clone(callee),
                                            *sub_pkg,
                                            atom,
                                            pos,
                                            pc + 1,
                                            true,
                                            stack_base,
                                            Some((Arc::clone(&cands), first))
                                        );
                                        continue 'run;
                                    }
                                }
                            }
                            Ok(CallTarget::Single) => {
                                walk_use(WalkUse::Leaf, "builtin-call");
                                pc += 1;
                                match self.regex_match_atom_with_capture_in_pkg(
                                    &program.atoms[atom as usize],
                                    chars,
                                    pos,
                                    levels.top().caps(),
                                    pkg,
                                    ic,
                                ) {
                                    Some((end, delta)) => {
                                        levels.edit(|s| s.merge_delta(delta));
                                        pos = end;
                                        farthest = farthest.max(pos);
                                        true
                                    }
                                    None => false,
                                }
                            }
                            Err(why) => {
                                walk_use(WalkUse::Bridged, why);
                                // A grammar method the call runs gets this
                                // invocation's own cursor, not a throwaway one:
                                // what it writes to its attributes is the Match's
                                // (#9803).
                                if self.subrule_names_user_method(name.spec(), pkg) {
                                    let slot = frame.as_ref().map_or(&root_cursor, |f| &f.cursor);
                                    let cursor = self.rx_cursor_of(slot, chars, pos, pkg);
                                    self.rx_cursor = Some(cursor);
                                }
                                let mut cands = self.regex_match_atom_all_with_capture_opts(
                                    &program.atoms[atom as usize],
                                    chars,
                                    pos,
                                    levels.top().caps(),
                                    pkg,
                                    ic,
                                    commit,
                                );
                                self.rx_cursor = None;
                                // Ratchet commits to the highest-priority end, the
                                // last (the producer's order is lowest first).
                                if commit && cands.len() > 1 {
                                    cands.drain(..cands.len() - 1);
                                }
                                pc += 1;
                                enter_cands!(cands)
                            }
                        }
                    }
                    // Cost: O(k·m) for the k iterations the scan matches, m = one
                    // match of the callee (`regex_named_ratchet_run`); O(1) when the
                    // scan does not apply.
                    RxOp::NamedRun { atom, min, skip } => {
                        match self.regex_named_ratchet_run(
                            &program.atoms[atom as usize],
                            chars,
                            pos,
                            min as usize,
                            pkg,
                        ) {
                            None => {
                                pc += 1;
                                true
                            }
                            Some(None) => {
                                walk_use(WalkUse::Bridged, "ratchet-scan");
                                false
                            }
                            Some(Some((end, delta))) => {
                                walk_use(WalkUse::Bridged, "ratchet-scan");
                                levels.edit(|s| s.merge_delta(delta));
                                pos = end;
                                farthest = farthest.max(pos);
                                pc = skip;
                                true
                            }
                        }
                    }
                    // Cost: O(1).
                    RxOp::Jmp(to) => {
                        pc = to;
                        true
                    }
                    // Cost: O(1) amortized.
                    RxOp::Mark(r) => {
                        set_reg!(r, pos);
                        pc += 1;
                        true
                    }
                    // Cost: O(1) amortized.
                    RxOp::PosBase(r) => {
                        set_reg!(r, levels.top().caps().positional.len());
                        pc += 1;
                        true
                    }
                    // Cost: O(1) amortized.
                    RxOp::SepBase(r) => {
                        set_reg!(r, levels.collected_len());
                        pc += 1;
                        true
                    }
                    // Cost: O(1) amortized.
                    RxOp::Height(r) => {
                        set_reg!(r, stack.len());
                        pc += 1;
                        true
                    }
                    // Cost: O(k), k = the choice points dropped (each pushed once).
                    RxOp::Cut(r) => {
                        truncate_stack!(reg!(r));
                        pc += 1;
                        true
                    }
                    // Cost: O(1) amortized.
                    RxOp::CtrZero(r) => {
                        set_reg!(r, 0);
                        pc += 1;
                        true
                    }
                    // Cost: O(1) amortized.
                    RxOp::CtrInc(r) => {
                        set_reg!(r, reg!(r) + 1);
                        pc += 1;
                        true
                    }
                    // Cost: O(1) amortized.
                    RxOp::Repeat {
                        ctr,
                        min,
                        max,
                        body,
                        exit,
                        greedy,
                    } => {
                        let n = reg!(ctr);
                        if n < min as usize {
                            pc = body;
                        } else if max != u32::MAX && n >= max as usize {
                            pc = exit;
                        } else {
                            let (first, second) = if greedy { (body, exit) } else { (exit, body) };
                            push_choice!(Choice::At {
                                pc: second,
                                pos,
                                mark: mark!(),
                            });
                            pc = first;
                        }
                        true
                    }
                    // Cost: one run of the count code (`regex_repeat_count`), then
                    // O(1) amortized.
                    RxOp::RepeatCount { tok, min, max } => {
                        let RegexQuant::RepeatCode(code) = &program.toks[tok as usize].quant else {
                            debug_assert!(false, "a RepeatCount op names a `** {{ … }}` token");
                            break 'run None;
                        };
                        pc += 1;
                        match self.regex_repeat_count(code, pos, levels.top().caps()) {
                            Some((lo, hi)) => {
                                set_reg!(min, lo);
                                set_reg!(max, hi.unwrap_or(usize::MAX));
                                true
                            }
                            None => false,
                        }
                    }
                    // Cost: O(1) amortized.
                    RxOp::RepeatDyn {
                        ctr,
                        min,
                        max,
                        body,
                        exit,
                        greedy,
                    } => {
                        let n = reg!(ctr);
                        let (min, max) = (reg!(min), reg!(max));
                        if n < min {
                            pc = body;
                        } else if max != usize::MAX && n >= max {
                            pc = exit;
                        } else {
                            let (first, second) = if greedy { (body, exit) } else { (exit, body) };
                            push_choice!(Choice::At {
                                pc: second,
                                pos,
                                mark: mark!(),
                            });
                            pc = first;
                        }
                        true
                    }
                    // Cost: O(1).
                    RxOp::ZeroIter {
                        ctr,
                        start,
                        min,
                        max,
                    } => {
                        pc += 1;
                        pos != reg!(start)
                            || zero_width_iter_counts(
                                reg!(ctr),
                                min as usize,
                                (max != u32::MAX).then_some(max as usize),
                            )
                    }
                    // Cost: O(1).
                    RxOp::GoalOk { height } => {
                        let at = reg!(height);
                        if let Some(entry) = stack.get_mut(at) {
                            *entry = Choice::Dead;
                        }
                        pc += 1;
                        true
                    }
                    // Cost: O(1).
                    RxOp::Advanced { start } => {
                        pc += 1;
                        pos > reg!(start)
                    }
                    // Cost: O(1).
                    RxOp::AtLeast { ctr, min } => {
                        pc += 1;
                        reg!(ctr) >= min as usize
                    }
                    // Cost: see `rx_capture_op`.
                    op @ (RxOp::OpenCapture
                    | RxOp::OpenInline
                    | RxOp::OpenSepIter { .. }
                    | RxOp::OpenIsolated
                    | RxOp::DropCapture
                    | RxOp::CloseCapture { .. }
                    | RxOp::CapAtom(_)
                    | RxOp::Code(_)
                    | RxOp::VarDecl(_)
                    | RxOp::Named { .. }
                    | RxOp::ZeroArm { .. }
                    | RxOp::QuantNames { .. }
                    | RxOp::Fold { .. }
                    | RxOp::AltTail { .. }
                    | RxOp::Collect { .. }
                    | RxOp::SepEmit { .. }
                    | RxOp::ReduceAction { .. }
                    | RxOp::GoalEnd { .. }
                    | RxOp::GoalFail { .. }
                    | RxOp::ConjTail { .. }) => {
                        pc += 1;
                        match self.rx_capture_op(
                            program,
                            op,
                            &regs[base..],
                            levels,
                            chars,
                            pos,
                            pkg,
                        ) {
                            Some(next) => {
                                pos = next;
                                farthest = farthest.max(pos);
                                true
                            }
                            None => false,
                        }
                    }
                    // Cost: O(c), c = this level's captures (one snapshot); in a
                    // callee frame also the one `build_named_candidates_from_inner`
                    // call that files them as the subrule's Match, O(1) plus the
                    // callee's own captures moved, not copied.
                    RxOp::Match => match if FRAMES { frame.clone() } else { None } {
                        None => match &mut goal {
                            Goal::First => break 'run Some((pos, root_snapshot!())),
                            Goal::End(end) => {
                                if pos == *end {
                                    break 'run Some((pos, root_snapshot!()));
                                }
                                false
                            }
                            // Every end (up to the first that covers the subject): the
                            // match is kept and the run backtracks for the next one.
                            Goal::Ends { out, stop_at_full } => {
                                out.push((pos, root_snapshot!()));
                                if *stop_at_full && pos == chars.len() {
                                    break 'run None;
                                }
                                false
                            }
                        },
                        Some(f) => {
                            // A second path to an end the call already returned at
                            // is not a new candidate.
                            // A ratcheted call returns once, so it keeps no list.
                            let fresh = f.commit || !f.seen.borrow().contains(&pos);
                            if fresh {
                                if !f.commit {
                                    f.seen.borrow_mut().push(pos);
                                }
                                // A ratcheted call keeps only its first end; a call
                                // that left no choice point in the callee cannot be
                                // resumed either.
                                if f.commit {
                                    truncate_stack!(f.stack_base);
                                }
                                let settled = stack.len() <= f.stack_base;
                                let mut inner = if settled {
                                    levels.close_forget(f.journal_base)
                                } else {
                                    levels.close()
                                };
                                // A proto candidate's Match carries its `:sym<…>`.
                                if let Some((cands, idx)) = &f.proto {
                                    inner.set_sym(cands[*idx].2.clone());
                                }
                                // The grammar instance this invocation owned is
                                // its Match's (#9803).
                                if let Some(cursor) = f.cursor.borrow().as_ref() {
                                    inner.set_cursor(cursor.clone());
                                }
                                let caller: &RxProgram = match &f.parent {
                                    Some(p) => &p.program,
                                    None => root,
                                };
                                let RegexAtom::Named(name) = &caller.atoms[f.site as usize] else {
                                    debug_assert!(false, "a frame's site is a `<subrule>` atom");
                                    break 'run None;
                                };
                                let (_, delta) = self.build_named_candidate_from_inner(
                                    pos,
                                    inner,
                                    f.entry_pos,
                                    name.spec(),
                                    None,
                                );
                                let delta = Some(delta);
                                frame = f.parent.clone();
                                match &frame {
                                    Some(p) => {
                                        cur = Cur::Callee(Arc::clone(&p.program));
                                        base = p.base;
                                        pkg = p.pkg;
                                    }
                                    None => {
                                        cur = Cur::Root(root);
                                        base = 0;
                                        pkg = root_pkg;
                                    }
                                }
                                pc = f.ret_pc;
                                if settled {
                                    // The callee's window, undo entries and run
                                    // ends are unreachable now.
                                    reg_trail.truncate(f.trail_base);
                                    regs.truncate(f.base);
                                    ends.truncate(f.ends_base);
                                }
                                if let Some(delta) = delta {
                                    levels.edit(|s| s.merge_delta(delta));
                                }
                                continue 'run;
                            } else {
                                false
                            }
                        }
                    },
                };
                if !ok {
                    // The newest choice point that is still wanted, with the frame
                    // state it was pushed with (none for a run that never called).
                    let (choice, fm) = loop {
                        let Some(choice) = stack.pop() else {
                            break 'run None;
                        };
                        let fm = if FRAMES && fmarks.last().is_some_and(|m| m.at == stack.len()) {
                            fmarks.pop()
                        } else {
                            None
                        };
                        if !matches!(choice, Choice::Dead) {
                            break (choice, fm);
                        }
                    };
                    // Put a partly consumed choice point back where it was.
                    macro_rules! repush {
                        ($choice:expr) => {{
                            if let Some(m) = &fm {
                                fmarks.push(FMark {
                                    at: stack.len(),
                                    regs_len: m.regs_len,
                                    frame: m.frame.clone(),
                                });
                            }
                            stack.push($choice);
                        }};
                    }
                    let mut cand_delta = None;
                    // A proto call's next candidate, entered once the state is back.
                    let mut enter_proto = None;
                    let (to_pc, to_pos, mark) = match choice {
                        Choice::Dead => {
                            debug_assert!(false, "dead choice points are skipped above");
                            break 'run None;
                        }
                        Choice::At { pc, pos, mark } => (pc, pos, mark),
                        Choice::Proto(proto) => {
                            let ProtoChoice {
                                pc,
                                pos,
                                atom,
                                cands,
                                ranked,
                                next,
                                mark,
                            } = *proto;
                            // The call's own height: its entry is popped.
                            let stack_base = stack.len();
                            let idx = ranked[next];
                            if next + 1 < ranked.len() {
                                repush!(Choice::Proto(Box::new(ProtoChoice {
                                    pc,
                                    pos,
                                    atom,
                                    cands: Arc::clone(&cands),
                                    ranked: Rc::clone(&ranked),
                                    next: next + 1,
                                    mark,
                                })));
                            }
                            enter_proto = Some((atom, pos, pc, cands, idx, stack_base));
                            (pc, pos, mark)
                        }
                        Choice::Run {
                            pc,
                            base: run,
                            lo,
                            hi,
                            mark,
                        } => {
                            let at = ends[hi - 1];
                            if hi - 1 > lo {
                                repush!(Choice::Run {
                                    pc,
                                    base: run,
                                    lo,
                                    hi: hi - 1,
                                    mark,
                                });
                            } else {
                                ends.truncate(run);
                            }
                            (pc, at, mark)
                        }
                        Choice::Cands {
                            pc,
                            cands,
                            left,
                            mark,
                        } => {
                            let (end, delta) = cands[left - 1].clone();
                            if left > 1 {
                                repush!(Choice::Cands {
                                    pc,
                                    cands,
                                    left: left - 1,
                                    mark,
                                });
                            }
                            cand_delta = Some(delta);
                            (pc, end, mark)
                        }
                    };
                    levels.rewind(mark.cap);
                    if let Some(delta) = cand_delta {
                        levels.edit(|s| s.merge_delta(delta));
                    }
                    while reg_trail.len() > mark.reg {
                        let (i, old) = reg_trail.pop().expect("register trail entry");
                        regs[i] = old;
                    }
                    // Windows opened after this choice point are dead.
                    if FRAMES {
                        regs.truncate(fm.as_ref().map_or(root.nregs, |m| m.regs_len));
                    }
                    let target_frame = if FRAMES {
                        fm.and_then(|m| m.frame)
                    } else {
                        None
                    };
                    let same_frame = match (&frame, &target_frame) {
                        (None, None) => true,
                        (Some(a), Some(b)) => Rc::ptr_eq(a, b),
                        _ => false,
                    };
                    // A choice point of another frame: back to that frame. Every
                    // path that switches ends in `continue 'run`.
                    if !same_frame {
                        match &target_frame {
                            Some(f) => {
                                cur = Cur::Callee(Arc::clone(&f.program));
                                base = f.base;
                                pkg = f.pkg;
                            }
                            None => {
                                cur = Cur::Root(root);
                                base = 0;
                                pkg = root_pkg;
                            }
                        }
                        frame = target_frame;
                        match enter_proto {
                            Some((atom, entry, ret_pc, cands, idx, stack_base)) => {
                                let (parsed, sub_pkg, _) = &cands[idx];
                                let Some(callee) = program_for(parsed) else {
                                    debug_assert!(false, "a proto's candidates compile");
                                    break 'run None;
                                };
                                enter_frame!(
                                    Arc::clone(callee),
                                    *sub_pkg,
                                    atom,
                                    entry,
                                    ret_pc,
                                    true,
                                    stack_base,
                                    Some((Arc::clone(&cands), idx))
                                );
                            }
                            None => {
                                pc = to_pc;
                                pos = to_pos;
                            }
                        }
                        continue 'run;
                    }
                    match enter_proto {
                        Some((atom, entry, ret_pc, cands, idx, stack_base)) => {
                            let (parsed, sub_pkg, _) = &cands[idx];
                            let Some(callee) = program_for(parsed) else {
                                debug_assert!(false, "a proto's candidates compile");
                                break 'run None;
                            };
                            enter_frame!(
                                Arc::clone(callee),
                                *sub_pkg,
                                atom,
                                entry,
                                ret_pc,
                                true,
                                stack_base,
                                Some((Arc::clone(&cands), idx))
                            );
                            continue 'run;
                        }
                        None => {
                            pc = to_pc;
                            pos = to_pos;
                        }
                    }
                }
            }
        };
        if FRAMES {
            // Drop the frames the leftover choice points hold.
            stack.clear();
            fmarks.clear();
        }
        super::super::regex_helpers::record_regex_farthest_position(farthest);
        result
    }
}
