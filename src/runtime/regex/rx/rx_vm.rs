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
//! the subrule's own Match straight into the caller's capture level
//! (`file_named_candidate`, the filing the walk's delta builder makes too) and
//! continues the caller.

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use super::super::regex_match_delta::group_merge_delta;
use super::super::regex_zero_width_iter::zero_width_iter_counts;
use super::rx_call::{CallTarget, ResolvedCall};
use super::rx_entry::{Goal, Scratch, program_for};
use super::rx_frame::{Choice, FMark, Frame, FrameId, MAX_FRAME_DEPTH, Mark, ProtoChoice};
use super::rx_scope::{UNDO_ENTER, UNDO_EXIT};
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
        // The closure scopes this run installs (`rx_scope`).
        let mut scopes = super::rx_scope::Scopes::default();
        let Scratch {
            regs,
            reg_trail,
            stack,
            fmarks,
            frames,
            proto_rank,
            ends,
            levels,
            ltm_order,
        } = scratch;
        regs.clear();
        regs.resize(root.nregs, 0);
        reg_trail.clear();
        stack.clear();
        fmarks.clear();
        frames.clear();
        ends.clear();
        levels.reset(start);
        if let Some(seed) = seed {
            levels.seed(seed);
        }
        let mut cur = Cur::Root(root);
        let mut frame: Option<FrameId> = None;
        // The grammar instance the run's own pattern owns, as `Frame::cursor` is
        // the callee's (#9803). Filed on the result at the pattern's `Match`.
        let root_cursor: RefCell<Option<Value>> = RefCell::new(None);
        // The built invocant of `.parse`'s start rule, when this run is the
        // start rule's own (#10848): its root frame's code blocks run on it.
        let root_invocant = self.take_rx_start_invocant();
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
                // A frame that returned leaving choice points behind is still in
                // the arena; its window may be empty (a callee with no
                // registers), so the arena length is checked, not just the
                // register arena's.
                if FRAMES && (frame.is_some() || regs.len() != root.nregs || !frames.is_empty()) {
                    fmarks.push(FMark {
                        at: stack.len(),
                        regs_len: regs.len(),
                        frames_len: frames.len(),
                        frame,
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
             $commit:expr, $stack_base:expr, $proto:expr, $window:expr, $interp:expr) => {{
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
                let depth = frame.map_or(0, |f| frames[f as usize].depth) + 1;
                frames.push(Frame {
                    parent: frame,
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
                    cursor: RefCell::new(None),
                    window: $window,
                    interp: $interp,
                });
                frame = Some((frames.len() - 1) as FrameId);
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
                    // Cost: O(w), w = the whitespace run at `pos` (`ws_rule_end`).
                    RxOp::Ws => match self.rx_ws_at(chars, pos, pkg) {
                        Some(next) => {
                            pos = next;
                            farthest = farthest.max(pos);
                            pc += 1;
                            true
                        }
                        None => false,
                    },
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
                    // Cost: O(n + r) plus the code's run (`regex_code_interp_parsed`),
                    // n = the subject's length, r = the result's rendered length;
                    // then O(w) to enter the yielded pattern as a frame, w = its
                    // registers. A pattern that declines is matched up front
                    // instead: its all-ends match, then O(c) per candidate entered,
                    // c = the captures it adds.
                    RxOp::InterpEnds(i) => {
                        let RegexAtom::CodeInterp { code, list } = &program.atoms[i as usize]
                        else {
                            debug_assert!(false, "an InterpEnds op names a CodeInterp atom");
                            break 'run None;
                        };
                        let parsed = self.regex_code_interp_parsed(
                            code,
                            *list,
                            chars,
                            pos,
                            levels.top().caps(),
                            program.atom_ic[i as usize],
                        );
                        pc += 1;
                        match parsed {
                            None => false,
                            // The yielded pattern runs as a frame of its own,
                            // resumed on demand as rakudo's interpolated regex
                            // is: code in it runs only on the paths the match
                            // takes. Its return merges its captures here as a
                            // group's (`Frame::interp`).
                            Some(parsed) => match super::rx_entry::program_for(&parsed) {
                                Some(callee) if FRAMES => {
                                    let stack_base = stack.len();
                                    enter_frame!(
                                        Arc::clone(callee),
                                        pkg,
                                        i,
                                        pos,
                                        pc,
                                        false,
                                        stack_base,
                                        None,
                                        None,
                                        true
                                    );
                                    continue 'run;
                                }
                                _ => {
                                    walk_use(WalkUse::Leaf, "code-interp-declined");
                                    let cands = self
                                        .regex_code_interp_pattern_ends(&parsed, chars, pos, pkg);
                                    enter_cands!(cands)
                                }
                            },
                        }
                    }
                    // Cost: O(b), b = the scope's bindings (`rx_scope_enter`).
                    RxOp::ScopeEnter { atom, slot } => {
                        let RegexAtom::CaptureIsolatedGroupScoped(_, scope) =
                            &program.atoms[atom as usize]
                        else {
                            debug_assert!(false, "a ScopeEnter op names a scoped group");
                            break 'run None;
                        };
                        let k = self.rx_scope_enter(&mut scopes, scope);
                        set_reg!(slot, k);
                        reg_trail.push((super::rx_scope::UNDO_ENTER, k));
                        pc += 1;
                        true
                    }
                    // Cost: O(b), b = the scope's bindings (`rx_scope_exit`).
                    RxOp::ScopeExit { slot } => {
                        let k = reg!(slot);
                        self.rx_scope_exit(&mut scopes, k);
                        reg_trail.push((super::rx_scope::UNDO_EXIT, k));
                        pc += 1;
                        true
                    }
                    // Cost: one all-ends run of the body over the stripped subject
                    // (`regex_match_ends_from_caps_in_pkg`), O(e) to map its e ends
                    // back, then O(c) per candidate entered, c = the captures it adds.
                    RxOp::GroupEnds(i) => {
                        let RegexAtom::Group(body) = &program.atoms[i as usize] else {
                            debug_assert!(false, "a GroupEnds op names a Group atom");
                            break 'run None;
                        };
                        let mut cands: Vec<(usize, RegexCaptures)> = self
                            .regex_match_ends_from_caps_in_pkg(body, chars, pos, pkg)
                            .into_iter()
                            .map(|(end, inner)| (end, group_merge_delta(inner)))
                            .collect();
                        // The producer's order: lowest priority first.
                        cands.reverse();
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
                        // `None`: an argument failed to evaluate, so no match.
                        match self.rx_call_resolve(name, pkg, ic, levels.top().caps()) {
                            None => false,
                            Some(ResolvedCall {
                                verdict,
                                args: call_args,
                                window,
                            }) => {
                                // A frame's binding window: rewinding past the
                                // call uninstalls it (`rx_scope`). An eager
                                // evaluation installs its own around itself.
                                let (window, lr_window) =
                                    if matches!(verdict, Ok(CallTarget::Eager(..))) {
                                        (None, window)
                                    } else {
                                        let window = window.map(|window| {
                                            let k = self.rx_window_adopt(&mut scopes, window);
                                            reg_trail.push((UNDO_ENTER, k));
                                            k
                                        });
                                        (window, None)
                                    };
                                match verdict {
                                    Ok(CallTarget::Eager(cands, why)) => {
                                        walk_use(WalkUse::Leaf, why);
                                        let mut ends = self.rx_lr_call_ends(
                                            &program.atoms[atom as usize],
                                            &cands,
                                            lr_window,
                                            call_args.as_deref().unwrap_or(&[]),
                                            chars,
                                            pos,
                                            pkg,
                                            (commit, ic),
                                        );
                                        // Ratchet commits to the highest-priority
                                        // end, the last (lowest priority first).
                                        if commit && ends.len() > 1 {
                                            ends.drain(..ends.len() - 1);
                                        }
                                        pc += 1;
                                        enter_cands!(ends)
                                    }
                                    Ok(CallTarget::Plain(callee, callee_pkg)) => {
                                        if frame.is_some_and(|f| {
                                            frames[f as usize].depth >= MAX_FRAME_DEPTH
                                        }) {
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
                                                None,
                                                window,
                                                false
                                            );
                                            continue 'run;
                                        }
                                    }
                                    Ok(CallTarget::Proto(cands)) => {
                                        self.ltm_rank_proto(
                                            &cands, chars, pos, ltm_order, proto_rank,
                                        );
                                        match proto_rank.first().copied() {
                                            // No candidate can match here.
                                            None => false,
                                            Some(_)
                                                if frame.is_some_and(|f| {
                                                    frames[f as usize].depth >= MAX_FRAME_DEPTH
                                                }) =>
                                            {
                                                false
                                            }
                                            Some(first) => {
                                                // The call is committed to the first ranked
                                                // candidate that matches: its return drops
                                                // the rest of the ranking. Whether the
                                                // candidate keeps only its first end is the
                                                // call site's ratchet, as for any subrule --
                                                // a `regex` caller backtracks into a `regex`
                                                // candidate (`regex TOP { <sep> '9' }` over
                                                // `regex sep:sym<x> { \d* }` matches "129").
                                                let stack_base = stack.len();
                                                if proto_rank.len() > 1 {
                                                    push_choice!(Choice::Proto(Box::new(
                                                        ProtoChoice {
                                                            pc: pc + 1,
                                                            pos,
                                                            atom,
                                                            cands: Arc::clone(&cands),
                                                            ranked: Rc::from(&proto_rank[..]),
                                                            next: 1,
                                                            mark: mark!(),
                                                            window,
                                                            commit,
                                                        }
                                                    )));
                                                }
                                                let (parsed, sub_pkg, _) = &cands[first];
                                                let Some(callee) = program_for(parsed) else {
                                                    debug_assert!(
                                                        false,
                                                        "a proto's candidates compile"
                                                    );
                                                    break 'run None;
                                                };
                                                enter_frame!(
                                                    Arc::clone(callee),
                                                    *sub_pkg,
                                                    atom,
                                                    pos,
                                                    pc + 1,
                                                    commit,
                                                    stack_base,
                                                    Some((Arc::clone(&cands), first)),
                                                    window,
                                                    false
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
                                            let slot = frame.map_or(&root_cursor, |f| {
                                                &frames[f as usize].cursor
                                            });
                                            let cursor = self.rx_cursor_of(slot, chars, pos, pkg);
                                            self.regex_state.rx_cursor = Some(cursor);
                                        }
                                        let mut cands = self.regex_match_atom_all_with_arg_values(
                                            &program.atoms[atom as usize],
                                            chars,
                                            pos,
                                            levels.top().caps(),
                                            pkg,
                                            ic,
                                            commit,
                                            call_args,
                                        );
                                        self.regex_state.rx_cursor = None;
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
                    RxOp::CapMark(r) => {
                        set_reg!(r, levels.top().mark());
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
                    RxOp::ZeroIterDyn {
                        ctr,
                        start,
                        min,
                        max,
                    } => {
                        pc += 1;
                        let max = reg!(max);
                        pos != reg!(start)
                            || zero_width_iter_counts(
                                reg!(ctr),
                                reg!(min),
                                (max != usize::MAX).then_some(max),
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
                    // Cost: O(1).
                    RxOp::AtLeastReg { ctr, min } => {
                        pc += 1;
                        reg!(ctr) >= reg!(min)
                    }
                    // Cost: O(1).
                    RxOp::AtMostReg { ctr, max } => {
                        pc += 1;
                        reg!(ctr) <= reg!(max)
                    }
                    // Cost: see `rx_capture_op`.
                    op @ (RxOp::OpenCapture
                    | RxOp::OpenInline
                    | RxOp::OpenSepIter { .. }
                    | RxOp::OpenPlainIter { .. }
                    | RxOp::ClosePlainIter
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
                    | RxOp::SepNames { .. }
                    | RxOp::ReduceAction { .. }
                    | RxOp::GoalEnd { .. }
                    | RxOp::GoalFail { .. }
                    | RxOp::ConjTail { .. }) => {
                        pc += 1;
                        if let RxOp::Code(_) = op {
                            match if FRAMES { frame } else { None } {
                                Some(_) => self.publish_rx_code_invocant(None),
                                None if root_invocant.is_some() => {
                                    self.publish_rx_code_invocant(root_invocant.clone())
                                }
                                None => {}
                            }
                        }
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
                    // callee frame also the one `file_named_candidate` call that
                    // files them as the subrule's Match, O(n) for the n names the
                    // caller's level has filed, the callee's own captures moved,
                    // not copied.
                    RxOp::Match => match if FRAMES { frame } else { None } {
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
                        Some(fid) => {
                            let fi = fid as usize;
                            // Every path to an end returns, a second path to an
                            // end already returned at included: Rakudo runs the
                            // caller's continuation once per path (#10489).
                            {
                                // A ratcheted call keeps only its first end; a call
                                // that left no choice point in the callee cannot be
                                // resumed either.
                                let (commit, stack_base) =
                                    (frames[fi].commit, frames[fi].stack_base);
                                if commit {
                                    truncate_stack!(stack_base);
                                } else if frames[fi].proto.is_some() {
                                    // A returning proto candidate settles the
                                    // call's choice of candidate: the rest of the
                                    // ranking (its `ProtoChoice`, right at the
                                    // call's height) is given up, while the
                                    // candidate's own choice points above it stay.
                                    let (site, entry_pos) = (frames[fi].site, frames[fi].entry_pos);
                                    if let Some(entry) = stack.get_mut(stack_base)
                                        && matches!(entry, Choice::Proto(p)
                                            if p.atom == site && p.pos == entry_pos)
                                    {
                                        *entry = Choice::Dead;
                                    }
                                }
                                let settled = stack.len() <= stack_base;
                                let f = &frames[fi];
                                let mut inner = if settled {
                                    levels.close_forget(f.journal_base)
                                } else {
                                    levels.close()
                                };
                                // A proto candidate's Match carries its `:sym<…>`.
                                if let Some((cands, idx)) = &f.proto {
                                    inner.set_sym(cands[*idx].2.as_deref().map(Symbol::intern));
                                }
                                // The callee's binding window, for its action.
                                if let Some(k) = f.window {
                                    let vars = inner.regex_vars_mut();
                                    for (key, value) in self.rx_window_values(&scopes, k) {
                                        vars.entry(key).or_insert(value);
                                    }
                                }
                                // The grammar instance this invocation owned is
                                // its Match's (#9803).
                                if let Some(cursor) = f.cursor.borrow().as_ref() {
                                    inner.set_cursor(cursor.clone());
                                }
                                let caller: &RxProgram = match f.parent {
                                    Some(p) => &frames[p as usize].program,
                                    None => root,
                                };
                                if f.interp {
                                    // An interpolated pattern is a regex of its
                                    // own: rakudo keeps none of its captures, so
                                    // its level goes with the frame.
                                    drop(inner);
                                } else {
                                    let RegexAtom::Named(name) = &caller.atoms[f.site as usize]
                                    else {
                                        debug_assert!(
                                            false,
                                            "a frame's site is a `<subrule>` atom"
                                        );
                                        break 'run None;
                                    };
                                    // Filed straight into the caller's level, which
                                    // the close above made the innermost one again.
                                    let entry_pos = f.entry_pos;
                                    levels.edit(|s| {
                                        self.file_named_candidate(
                                            s,
                                            pos,
                                            inner,
                                            entry_pos,
                                            name.spec(),
                                            None,
                                        )
                                    });
                                }
                                let (ret_pc, trail_base, window, ends_base, binding) =
                                    (f.ret_pc, f.trail_base, f.base, f.ends_base, f.window);
                                frame = f.parent;
                                match frame {
                                    Some(p) => {
                                        let p = &frames[p as usize];
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
                                pc = ret_pc;
                                if settled {
                                    // The callee's window, undo entries, run ends
                                    // and frame (with every frame it called, all
                                    // returned the same way) are unreachable now.
                                    reg_trail.truncate(trail_base);
                                    regs.truncate(window);
                                    ends.truncate(ends_base);
                                    frames.truncate(fi);
                                }
                                // The callee's binding window ends with it;
                                // backtracking into the callee installs it again.
                                if let Some(k) = binding {
                                    self.rx_scope_exit(&mut scopes, k);
                                    reg_trail.push((UNDO_EXIT, k));
                                }
                                continue 'run;
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
                                    frames_len: m.frames_len,
                                    frame: m.frame,
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
                                window,
                                commit,
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
                                    window,
                                    commit,
                                })));
                            }
                            enter_proto =
                                Some((atom, pos, pc, cands, idx, stack_base, window, commit));
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
                    while let Some((i, old)) = (reg_trail.len() > mark.reg)
                        .then(|| reg_trail.pop())
                        .flatten()
                    {
                        if i >= super::rx_scope::UNDO_EXIT {
                            self.rx_scope_undo(&mut scopes, i, old);
                        } else {
                            regs[i] = old;
                        }
                    }
                    // Windows and frames opened after this choice point are dead.
                    if FRAMES {
                        regs.truncate(fm.as_ref().map_or(root.nregs, |m| m.regs_len));
                        frames.truncate(fm.as_ref().map_or(0, |m| m.frames_len));
                    }
                    let target_frame = if FRAMES {
                        fm.and_then(|m| m.frame)
                    } else {
                        None
                    };
                    let same_frame = frame == target_frame;
                    // A choice point of another frame: back to that frame. Every
                    // path that switches ends in `continue 'run`.
                    if !same_frame {
                        match target_frame {
                            Some(f) => {
                                let f = &frames[f as usize];
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
                            Some((atom, entry, ret_pc, cands, idx, stack_base, window, commit)) => {
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
                                    commit,
                                    stack_base,
                                    Some((Arc::clone(&cands), idx)),
                                    window,
                                    false
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
                        Some((atom, entry, ret_pc, cands, idx, stack_base, window, commit)) => {
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
                                commit,
                                stack_base,
                                Some((Arc::clone(&cands), idx)),
                                window,
                                false
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
            frames.clear();
        }
        self.rx_scopes_unwind(&mut scopes);
        self.restore_rx_start_invocant(root_invocant);
        super::super::regex_helpers::record_regex_farthest_position(farthest);
        result
    }
}
