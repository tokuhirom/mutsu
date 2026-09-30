//! The backtracking loop that runs an [`RxProgram`] (ADR-0135 D2), and the
//! entry point the walk's chokepoint consults.
//!
//! State: `pc`, `pos`, the capture levels (`rx_levels`: the walk's own
//! `CapStore`s, whose journal restores captures on backtrack), a register
//! file whose writes go through a second undo trail, and one explicit stack
//! of choice points. A choice point records the capture-journal and
//! register-trail lengths to rewind to; a
//! ratchet cut drops choice points only, never trail entries, so an earlier
//! choice point still rewinds correctly.

use std::sync::Arc;

use super::super::regex_zero_width_iter::zero_width_iter_counts;
use super::rx_levels::Levels;
use super::{RxOp, RxProgram, rx_compile, rx_diff_enabled, rx_vm_enabled};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{RegexCaptures, RegexPattern};
use crate::symbol::Symbol;

/// A point to resume from on failure. Both kinds record the capture-trail
/// and register-trail lengths to rewind to.
enum Choice {
    /// Resume at `pc` with the cursor at `pos`.
    At {
        pc: u32,
        pos: usize,
        cap_mark: usize,
        reg_mark: usize,
    },
    /// An `AtomRun`'s give-back: resume at `pc` from `ends[hi - 1]`, one
    /// iteration shorter each time, while `hi > lo`; `ends` is truncated back
    /// to `base` once the run is exhausted.
    Run {
        pc: u32,
        base: usize,
        lo: usize,
        hi: usize,
        cap_mark: usize,
        reg_mark: usize,
    },
}

/// The VM's growable state, reused across engine entries instead of being
/// reallocated per start position. Taken out of the thread-local for one run
/// and put back after, so a nested run (none today) would just allocate.
#[derive(Default)]
struct Scratch {
    regs: Vec<usize>,
    reg_trail: Vec<(u16, usize)>,
    stack: Vec<Choice>,
    ends: Vec<usize>,
    levels: Levels,
}

thread_local! {
    // Boxed, so taking it out for a run moves a pointer rather than the whole
    // struct: a run happens once per unanchored start position.
    static SCRATCH: std::cell::Cell<Option<Box<Scratch>>> = const { std::cell::Cell::new(None) };
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
    // Cost: O(s) in the steps the backtracking search takes; each op below
    // states its own cost.
    fn rx_run(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        start: usize,
        pkg: Symbol,
    ) -> Option<(usize, RegexCaptures)> {
        let _region = crate::profile::enter(crate::profile::Region::Regex);
        let mut scratch = SCRATCH.with(std::cell::Cell::take).unwrap_or_default();
        let result = self.rx_run_in(program, chars, start, pkg, &mut scratch);
        SCRATCH.with(|s| s.set(Some(scratch)));
        result
    }

    fn rx_run_in(
        &mut self,
        program: &RxProgram,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        scratch: &mut Scratch,
    ) -> Option<(usize, RegexCaptures)> {
        let Scratch {
            regs,
            reg_trail,
            stack,
            ends,
            levels,
        } = scratch;
        regs.clear();
        regs.resize(program.nregs, 0);
        reg_trail.clear();
        stack.clear();
        ends.clear();
        levels.reset(start);
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
                    // `ends[base + c]` is where the cursor stands after `c`
                    // iterations, so count 0 is the run's own start.
                    let base = ends.len();
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
                        ends.truncate(base);
                        false
                    } else {
                        pos = ends[base + n as usize];
                        farthest = farthest.max(pos);
                        if possessive || n == min {
                            ends.truncate(base);
                        } else {
                            // Give back counts n-1 down to min.
                            stack.push(Choice::Run {
                                pc: pc + 1,
                                base,
                                lo: base + min as usize,
                                hi: base + n as usize,
                                cap_mark: levels.mark(),
                                reg_mark: reg_trail.len(),
                            });
                        }
                        pc += 1;
                        true
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
                    stack.push(Choice::At {
                        pc: alt,
                        pos,
                        cap_mark: levels.mark(),
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
                        stack.push(Choice::At {
                            pc: second,
                            pos,
                            cap_mark: levels.mark(),
                            reg_mark: reg_trail.len(),
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
                    pos != regs[start as usize]
                        || zero_width_iter_counts(
                            regs[ctr as usize],
                            min as usize,
                            (max != u32::MAX).then_some(max as usize),
                        )
                }
                // Cost: O(1).
                RxOp::Advanced { start } => {
                    pc += 1;
                    pos > regs[start as usize]
                }
                // Cost: O(1).
                RxOp::AtLeast { ctr, min } => {
                    pc += 1;
                    regs[ctr as usize] >= min as usize
                }
                // Cost: see `rx_capture_op`.
                op @ (RxOp::OpenCapture
                | RxOp::CloseCapture { .. }
                | RxOp::CapAtom(_)
                | RxOp::Named { .. }
                | RxOp::ZeroArm { .. }
                | RxOp::QuantNames { .. }
                | RxOp::Fold { .. }
                | RxOp::AltTail { .. }
                | RxOp::Collect { .. }
                | RxOp::SepEmit { .. }) => {
                    pc += 1;
                    match self.rx_capture_op(program, op, regs, levels, chars, pos, pkg) {
                        Some(next) => {
                            pos = next;
                            farthest = farthest.max(pos);
                            true
                        }
                        None => false,
                    }
                }
                // Cost: O(c), c = this level's captures (one snapshot).
                RxOp::Match => break 'run Some((pos, levels.top().snapshot())),
            };
            if !ok {
                let Some(choice) = stack.pop() else {
                    break 'run None;
                };
                let (to_pc, to_pos, cap_mark, reg_mark) = match choice {
                    Choice::At {
                        pc,
                        pos,
                        cap_mark,
                        reg_mark,
                    } => (pc, pos, cap_mark, reg_mark),
                    Choice::Run {
                        pc,
                        base,
                        lo,
                        hi,
                        cap_mark,
                        reg_mark,
                    } => {
                        let at = ends[hi - 1];
                        if hi - 1 > lo {
                            stack.push(Choice::Run {
                                pc,
                                base,
                                lo,
                                hi: hi - 1,
                                cap_mark,
                                reg_mark,
                            });
                        } else {
                            ends.truncate(base);
                        }
                        (pc, at, cap_mark, reg_mark)
                    }
                };
                levels.rewind(cap_mark);
                while reg_trail.len() > reg_mark {
                    let (r, old) = reg_trail.pop().expect("register trail entry");
                    regs[r as usize] = old;
                }
                pc = to_pc;
                pos = to_pos;
            }
        };
        super::super::regex_helpers::record_regex_farthest_position(farthest);
        result
    }
}
