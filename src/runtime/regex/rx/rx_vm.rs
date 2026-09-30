//! The backtracking loop that runs an [`RxProgram`] (ADR-0135 D2), and the
//! entry point the walk's chokepoint consults.
//!
//! State: `pc`, `pos`, the walk's own `CapStore` (whose undo trail restores
//! captures on backtrack), a register file whose writes go through a second
//! undo trail, and one explicit stack of choice points. A choice point
//! records the capture-trail and register-trail lengths to rewind to; a
//! ratchet cut drops choice points only, never trail entries, so an earlier
//! choice point still rewinds correctly.

use std::sync::Arc;

use super::super::regex_match_delta::capture_group_delta;
use super::super::regex_trail::CapStore;
use super::{RxOp, RxProgram, rx_compile, rx_diff_enabled, rx_vm_enabled};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{RegexCaptures, RegexPattern};
use crate::symbol::Symbol;

struct Choice {
    pc: u32,
    pos: usize,
    cap_mark: usize,
    reg_mark: usize,
}

/// The pattern's compiled program, compiled at most once per pattern.
// Cost: O(1) after the first call per pattern; O(t) on it, t = tokens.
fn program_for(pattern: &RegexPattern) -> Option<&Arc<RxProgram>> {
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

impl Interpreter {
    /// The first (highest-priority) match of `pattern` at `start`, by the
    /// compiled engine — or `None` when this match must take the walk: the
    /// pattern is outside Slice A, or the dynamic context carries state the
    /// VM does not model (an enclosing regex's `:my` lexicals or backreference
    /// captures, a grammar rule's dynamic declarations, LTM measurement).
    // Cost: O(1) to decline; otherwise the match itself.
    pub(in crate::runtime::regex) fn rx_try_match(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<Option<(usize, RegexCaptures)>> {
        use super::super::regex_helpers as h;
        if !rx_vm_enabled()
            || h::LTM_DECLARATIVE_MODE.with(std::cell::Cell::get)
            || !self.grammar_rule_dynvar_decls.is_empty()
            || h::inline_regex_vars_active()
            || h::INLINE_CAPTURE_SCOPE.with(std::cell::Cell::get).is_some()
            || h::take_inline_outer_caps_seed().is_some()
        {
            return None;
        }
        let program = Arc::clone(program_for(pattern)?);
        crate::vm::vm_stats_regex_vm::record_regex_vm_run();
        let result = self.rx_run(&program, chars, start, pkg);
        if rx_diff_enabled() {
            let walked = self.regex_walk_first_for_diff(pattern, chars, start, pkg);
            if let Err(why) = super::rx_diff::same_match(&result, &walked) {
                panic!(
                    "MUTSU_RX_DIFF: compiled engine and walk disagree at start {start} \
                     of a {}-char subject: {why}\nprogram: {:?}",
                    chars.len(),
                    program.ops
                );
            }
        }
        Some(result)
    }

    /// Run `program` at `start`.
    // Cost: O(s) in the steps the backtracking search takes; each op below is
    // O(1) except `Atom` (O(g), the grapheme's length) and the capture ops
    // (O(1) amortized trail pushes).
    fn rx_run(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<(usize, RegexCaptures)> {
        let _region = crate::profile::enter(crate::profile::Region::Regex);
        let mut store = CapStore::new(RegexCaptures {
            match_from: start,
            ..Default::default()
        });
        let mut regs = vec![0usize; program.nregs];
        let mut reg_trail: Vec<(u16, usize)> = Vec::new();
        let mut stack: Vec<Choice> = Vec::new();
        let mut pc = 0u32;
        let mut pos = start;
        let mut farthest = start;
        macro_rules! set_reg {
            ($r:expr, $v:expr) => {{
                let r = $r;
                reg_trail.push((r, regs[r as usize]));
                regs[r as usize] = $v;
            }};
        }
        let result = 'run: loop {
            let ok = match program.ops[pc as usize] {
                // Cost: O(g), g = the grapheme's length at `pos`.
                RxOp::Atom(i) => {
                    match self.match_consuming_atom(
                        &program.atoms[i as usize],
                        chars,
                        pos,
                        pkg,
                        false,
                    ) {
                        Some(next) => {
                            pos = next;
                            farthest = farthest.max(pos);
                            pc += 1;
                            true
                        }
                        None => false,
                    }
                }
                // Cost: O(1) for every assertion Slice A compiles.
                RxOp::Assert(i) => {
                    let hit = self
                        .regex_match_atom_in_pkg(&program.atoms[i as usize], chars, pos, pkg, false)
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
                    stack.push(Choice {
                        pc: alt,
                        pos,
                        cap_mark: store.mark(),
                        reg_mark: reg_trail.len(),
                    });
                    pc = prefer;
                    true
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
                    set_reg!(r, store.caps().positional.len());
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
                    stack.truncate(regs[r as usize]);
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
                    set_reg!(r, regs[r as usize] + 1);
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
                    let n = regs[ctr as usize];
                    if n < min as usize {
                        pc = body;
                    } else if max != u32::MAX && n >= max as usize {
                        pc = exit;
                    } else {
                        let (first, second) = if greedy { (body, exit) } else { (exit, body) };
                        stack.push(Choice {
                            pc: second,
                            pos,
                            cap_mark: store.mark(),
                            reg_mark: reg_trail.len(),
                        });
                        pc = first;
                    }
                    true
                }
                // Cost: O(1) amortized (one slot, one trail record).
                RxOp::CloseCapture { start } => {
                    let from = regs[start as usize];
                    let inner = RegexCaptures {
                        match_from: from,
                        ..Default::default()
                    };
                    store.merge_delta(capture_group_delta(from, pos, inner));
                    pc += 1;
                    true
                }
                // Cost: O(1) amortized for the aliases Slice A compiles.
                RxOp::Named {
                    tok,
                    start,
                    pos_base,
                } => {
                    Self::store_apply_named_capture(
                        &mut store,
                        &program.toks[tok as usize],
                        regs[start as usize],
                        pos,
                        regs[pos_base as usize],
                    );
                    pc += 1;
                    true
                }
                // Cost: O(c), c = this level's captures (one snapshot).
                RxOp::Match => break 'run Some((pos, store.snapshot())),
            };
            if !ok {
                let Some(choice) = stack.pop() else {
                    break 'run None;
                };
                store.rewind(choice.cap_mark);
                while reg_trail.len() > choice.reg_mark {
                    let (r, old) = reg_trail.pop().expect("register trail entry");
                    regs[r as usize] = old;
                }
                pc = choice.pc;
                pos = choice.pos;
            }
        };
        super::super::regex_helpers::record_regex_farthest_position(farthest);
        result
    }
}
