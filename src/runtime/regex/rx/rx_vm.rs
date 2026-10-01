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
use super::rx_frame::{Choice, Frame, MAX_FRAME_DEPTH, Mark};
use super::{RxOp, RxProgram};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{RegexAtom, RegexCaptures, RegexQuant};
use crate::symbol::Symbol;

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
    pub(super) fn rx_run_in(
        &mut self,
        root: &RxProgram,
        chars: &[char],
        start: usize,
        root_pkg: Symbol,
        mut goal: Goal<'_>,
        scratch: &mut Scratch,
    ) -> Option<(usize, RegexCaptures)> {
        let Scratch {
            regs,
            reg_trail,
            stack,
            ends,
            levels,
            ltm_order,
        } = scratch;
        regs.clear();
        regs.resize(root.nregs, 0);
        reg_trail.clear();
        stack.clear();
        ends.clear();
        levels.reset(start);
        let mut cur = Cur::Root(root);
        let mut frame: Option<Rc<Frame>> = None;
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
        // What a choice point pushed now restores.
        macro_rules! mark {
            () => {
                Mark {
                    cap: levels.mark(),
                    reg: reg_trail.len(),
                    regs_len: regs.len(),
                    frame: frame.clone(),
                }
            };
        }
        // Enter the highest-priority of `cands` (lowest priority first); the
        // rest wait on the stack as one choice point resuming at `pc`.
        macro_rules! enter_cands {
            ($cands:expr) => {{
                let cands: Vec<(usize, RegexCaptures)> = $cands;
                if let Some((end, delta)) = cands.last().cloned() {
                    let left = cands.len() - 1;
                    if left > 0 {
                        stack.push(Choice::Cands {
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
                    proto: $proto,
                    depth,
                    seen: RefCell::new(Vec::new()),
                }));
                cur = Cur::Callee(callee);
                base = new_base;
                pkg = callee_pkg;
                pc = 0;
                pos = entry;
            }};
        }
        let result = 'run: loop {
            let program = cur.get();
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
                        let Some(next) = self.rx_atom_at(program, atom as usize, chars, at, pkg)
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
                            stack.push(Choice::Run {
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
                    stack.push(Choice::At {
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
                        stack.push(Choice::At {
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
                    let RegexAtom::CodeInterp { code, list } = &program.atoms[i as usize] else {
                        debug_assert!(false, "an InterpEnds op names a CodeInterp atom");
                        break 'run None;
                    };
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
                // and costs O(c) per candidate entered, c = the captures it adds.
                // The callee's own ops state their costs.
                RxOp::Call { atom, commit } => {
                    let RegexAtom::Named(name) = &program.atoms[atom as usize] else {
                        debug_assert!(false, "a Call op names a `<subrule>` atom");
                        break 'run None;
                    };
                    let ic = program.atom_ic[atom as usize];
                    match self.rx_call_target(name, pkg, ic) {
                        Some(CallTarget::Plain(callee, callee_pkg)) => {
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
                                true
                            }
                        }
                        Some(CallTarget::Proto(cands)) => {
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
                                        stack.push(Choice::Proto {
                                            pc: pc + 1,
                                            pos,
                                            atom,
                                            cands: Arc::clone(&cands),
                                            ranked: Rc::new(ranked),
                                            next: 1,
                                            mark: mark!(),
                                        });
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
                                    true
                                }
                            }
                        }
                        None => {
                            let mut cands = self.regex_match_atom_all_with_capture_opts(
                                &program.atoms[atom as usize],
                                chars,
                                pos,
                                levels.top().caps(),
                                pkg,
                                ic,
                                commit,
                            );
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
                        Some(None) => false,
                        Some(Some((end, delta))) => {
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
                    stack.truncate(reg!(r));
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
                        stack.push(Choice::At {
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
                        stack.push(Choice::At {
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
                | RxOp::GoalEnd { .. }
                | RxOp::GoalFail { .. }
                | RxOp::ConjTail { .. }) => {
                    pc += 1;
                    match self.rx_capture_op(program, op, &regs[base..], levels, chars, pos, pkg) {
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
                RxOp::Match => match frame.clone() {
                    None => match &mut goal {
                        Goal::First => break 'run Some((pos, levels.top().snapshot())),
                        Goal::End(end) => {
                            if pos == *end {
                                break 'run Some((pos, levels.top().snapshot()));
                            }
                            false
                        }
                        // Every end up to the first that covers the subject: the
                        // match is kept and the run backtracks for the next one.
                        Goal::UntilFull(out) => {
                            out.push((pos, levels.top().snapshot()));
                            if pos == chars.len() {
                                break 'run None;
                            }
                            false
                        }
                    },
                    Some(f) => {
                        // A second path to an end the call already returned at
                        // is not a new candidate.
                        let fresh = !f.seen.borrow().contains(&pos);
                        if fresh {
                            f.seen.borrow_mut().push(pos);
                            let mut inner = levels.close();
                            // A proto candidate's Match carries its `:sym<…>`.
                            if let Some((cands, idx)) = &f.proto {
                                inner.set_sym(cands[*idx].2.clone());
                            }
                            let caller: &RxProgram = match &f.parent {
                                Some(p) => &p.program,
                                None => root,
                            };
                            let RegexAtom::Named(name) = &caller.atoms[f.site as usize] else {
                                debug_assert!(false, "a frame's site is a `<subrule>` atom");
                                break 'run None;
                            };
                            let wrapped = self.build_named_candidates_from_inner(
                                vec![(pos, inner)],
                                f.entry_pos,
                                name.spec(),
                                None,
                            );
                            let delta = wrapped.into_iter().next().map(|(_, delta)| delta);
                            // A ratcheted call keeps only its first end.
                            if f.commit {
                                stack.truncate(f.stack_base);
                            }
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
                            if let Some(delta) = delta {
                                levels.edit(|s| s.merge_delta(delta));
                            }
                            true
                        } else {
                            false
                        }
                    }
                },
            };
            if !ok {
                let choice = loop {
                    match stack.pop() {
                        None => break 'run None,
                        Some(Choice::Dead) => {}
                        Some(choice) => break choice,
                    }
                };
                let mut cand_delta = None;
                // A proto call's next candidate, entered once the state is back.
                let mut enter_proto = None;
                let (to_pc, to_pos, mark) = match choice {
                    Choice::Dead => {
                        debug_assert!(false, "dead choice points are skipped above");
                        break 'run None;
                    }
                    Choice::At { pc, pos, mark } => (pc, pos, mark),
                    Choice::Proto {
                        pc,
                        pos,
                        atom,
                        cands,
                        ranked,
                        next,
                        mark,
                    } => {
                        // The call's own height: its entry is popped.
                        let stack_base = stack.len();
                        let idx = ranked[next];
                        if next + 1 < ranked.len() {
                            stack.push(Choice::Proto {
                                pc,
                                pos,
                                atom,
                                cands: Arc::clone(&cands),
                                ranked: Rc::clone(&ranked),
                                next: next + 1,
                                mark: mark.clone(),
                            });
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
                            stack.push(Choice::Run {
                                pc,
                                base: run,
                                lo,
                                hi: hi - 1,
                                mark: mark.clone(),
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
                            stack.push(Choice::Cands {
                                pc,
                                cands,
                                left: left - 1,
                                mark: mark.clone(),
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
                regs.truncate(mark.regs_len);
                let same_frame = match (&frame, &mark.frame) {
                    (None, None) => true,
                    (Some(a), Some(b)) => Rc::ptr_eq(a, b),
                    _ => false,
                };
                if !same_frame {
                    match &mark.frame {
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
                    frame = mark.frame;
                }
                if let Some((atom, entry, ret_pc, cands, idx, stack_base)) = enter_proto {
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
                } else {
                    pc = to_pc;
                    pos = to_pos;
                }
            }
        };
        // Drop the frames the leftover choice points hold.
        stack.clear();
        super::super::regex_helpers::record_regex_farthest_position(farthest);
        result
    }
}
