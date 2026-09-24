//! The TRIR peephole pass: fewer, fatter ops for the same program.
//!
//! The compiler emits a plain stack form on purpose — each rule in
//! [`super::compile`] reasons about one construct and its operands' kinds,
//! and never about what the next construct will do with the result. That
//! leaves sequences the switch loop executes op by op although the answer is
//! fixed by the sequence as a whole: `LoadI(n) ConstI(c) EqI JumpIfFalseI(t)`
//! is one test of a slot against a constant, dispatched four times with three
//! pushes and three pops on the native bank. The per-character loop of
//! `JSON::Fast`'s `unjsonify-string` was 22 ops, most of them this shape
//! (ADR-0116 §7).
//!
//! This pass runs once, on the finished chunk, and only ever replaces a
//! window of consecutive ops by a shorter window with the same effect on both
//! banks, the slots and the control flow. A window is rewritten only when no
//! jump lands strictly inside it, so every path that reaches the window
//! executes all of it. Jump targets are then renumbered through an
//! old-to-new index map.
//!
//! The rules:
//!
//! - **Jump threading.** A jump whose target is an unconditional `Jump(u)`
//!   jumps to `u` directly. A "keep" jump (`&&`/`||`, which leaves the tested
//!   value on the bank) whose target is a conditional jump on that same value
//!   knows the outcome of that second test; when the fall-through path pops
//!   the value straight away, the pair becomes one popping jump.
//! - **Dead pushes.** A push of a constant, a slot or a reference, followed
//!   by a pop of the same bank, is nothing.
//! - **Compare-and-branch.** An int comparison followed by a conditional jump
//!   on its result is one [`TrOp::JumpCmp`]; with a constant right operand,
//!   [`TrOp::JumpCmpC`]; with a slot left operand too, [`TrOp::JumpCmpLC`],
//!   which touches no bank at all. `ElemsLocal(n); JumpIfFalseI(t)` is
//!   [`TrOp::JumpIfEmptyLocal`].
//! - **Discarded push.** `PushILocal(n); PopObj` is [`TrOp::PushILocalVoid`].

use super::{TrCmp, TrOp};

/// Rewrite `ops` to the fused form. Answers the same program.
// Cost: O(p * n), n = ops in the chunk, p = passes until nothing changes
// (each pass removes at least one op, so p <= n; in practice 2-3).
pub(crate) fn optimize(mut ops: Vec<TrOp>) -> Vec<TrOp> {
    loop {
        thread_jumps(&mut ops);
        let before = ops.len();
        ops = rewrite(ops, thread_keep_jump);
        ops = rewrite(ops, fuse);
        if ops.len() == before {
            return ops;
        }
    }
}

/// Every index some jump lands on.
fn targets(ops: &[TrOp]) -> Vec<bool> {
    let mut t = vec![false; ops.len() + 1];
    for op in ops {
        if let Some(x) = op.target() {
            let x = x as usize;
            if x < t.len() {
                t[x] = true;
            }
        }
    }
    t
}

/// Point every jump whose target is an unconditional `Jump(u)` at `u`.
/// In place: no op moves.
fn thread_jumps(ops: &mut [TrOp]) {
    for i in 0..ops.len() {
        let Some(mut t) = ops[i].target() else {
            continue;
        };
        // Bounded, so a `Jump` cycle (an empty infinite loop) terminates.
        let mut hops = 0;
        while let Some(TrOp::Jump(u)) = ops.get(t as usize) {
            if hops > ops.len() || *u == t {
                break;
            }
            t = *u;
            hops += 1;
        }
        if let Some(x) = ops[i].target_mut() {
            *x = t;
        }
    }
}

/// One rewrite rule: given the ops and the index a window starts at, answer
/// the window's length and its replacement, or `None` to leave it.
type Rule = fn(&[TrOp], usize) -> Option<(usize, Vec<TrOp>)>;

/// Apply `rule` at every index left to right, never inside a window already
/// taken, and never to a window that a jump enters anywhere but its start.
fn rewrite(ops: Vec<TrOp>, rule: Rule) -> Vec<TrOp> {
    let is_target = targets(&ops);
    let mut out: Vec<TrOp> = Vec::with_capacity(ops.len());
    // `map[old]` = the index the op at `old` (or, for a removed op, the op
    // that now follows it) has in `out`.
    let mut map = vec![0u32; ops.len() + 1];
    let mut changed = false;
    let mut i = 0;
    while i < ops.len() {
        if let Some((len, repl)) = rule(&ops, i)
            && len >= 1
            && i + len <= ops.len()
            && !(i + 1..i + len).any(|k| is_target[k])
        {
            for slot in &mut map[i..i + len] {
                *slot = out.len() as u32;
            }
            out.extend(repl);
            i += len;
            changed = true;
            continue;
        }
        map[i] = out.len() as u32;
        out.push(ops[i].clone());
        i += 1;
    }
    map[ops.len()] = out.len() as u32;
    if !changed {
        return out;
    }
    for op in &mut out {
        if let Some(t) = op.target_mut()
            && let Some(&m) = map.get(*t as usize)
        {
            *t = m;
        }
    }
    out
}

/// A keep jump whose target tests the same value again, and whose
/// fall-through drops it: `JumpIfTrueKeepI(t); PopI` where `ops[t]` is
/// `JumpIfFalseI(u)` — taken, the value is non-zero, so the test at `t`
/// falls through and pops it, which is `JumpIfTrueI(t + 1)`.
fn thread_keep_jump(ops: &[TrOp], i: usize) -> Option<(usize, Vec<TrOp>)> {
    let (keep_true, t) = match ops[i] {
        TrOp::JumpIfTrueKeepI(t) => (true, t),
        TrOp::JumpIfFalseKeepI(t) => (false, t),
        _ => return None,
    };
    if !matches!(ops.get(i + 1), Some(TrOp::PopI)) {
        return None;
    }
    let jumped = match ops.get(t as usize)? {
        // Taken with a value of the keep jump's truth: the second test
        // either jumps on, or falls through past itself.
        TrOp::JumpIfFalseI(u) => {
            if keep_true {
                t + 1
            } else {
                *u
            }
        }
        TrOp::JumpIfTrueI(u) => {
            if keep_true {
                *u
            } else {
                t + 1
            }
        }
        _ => return None,
    };
    let op = if keep_true {
        TrOp::JumpIfTrueI(jumped)
    } else {
        TrOp::JumpIfFalseI(jumped)
    };
    Some((2, vec![op]))
}

/// The conditional jump at `i`, as `(on, target)`: jump when the popped
/// value's truth equals `on`.
fn cond_jump(op: Option<&TrOp>) -> Option<(bool, u32)> {
    match op? {
        TrOp::JumpIfTrueI(t) => Some((true, *t)),
        TrOp::JumpIfFalseI(t) => Some((false, *t)),
        _ => None,
    }
}

fn fuse(ops: &[TrOp], i: usize) -> Option<(usize, Vec<TrOp>)> {
    let at = |k: usize| ops.get(i + k);
    match &ops[i] {
        // ---- dead pushes ----
        TrOp::ConstObj(_) | TrOp::LoadObj(_) if matches!(at(1), Some(TrOp::PopObj)) => {
            Some((2, vec![]))
        }
        TrOp::ConstI(_) | TrOp::LoadI(_) | TrOp::GetRefI(_)
            if matches!(at(1), Some(TrOp::PopI)) =>
        {
            Some((2, vec![]))
        }
        TrOp::PushILocal(n) if matches!(at(1), Some(TrOp::PopObj)) => {
            Some((2, vec![TrOp::PushILocalVoid(*n)]))
        }
        // ---- compare-and-branch ----
        TrOp::LoadI(slot) => {
            let Some(TrOp::ConstI(c)) = at(1) else {
                return None;
            };
            let c = i32::try_from(*c).ok()?;
            let cmp = TrCmp::of(at(2)?)?;
            let (on, target) = cond_jump(at(3))?;
            Some((
                4,
                vec![TrOp::JumpCmpLC {
                    cmp,
                    on,
                    slot: *slot,
                    c,
                    target,
                }],
            ))
        }
        TrOp::ConstI(c) => {
            let c = i32::try_from(*c).ok()?;
            let cmp = TrCmp::of(at(1)?)?;
            let (on, target) = cond_jump(at(2))?;
            Some((3, vec![TrOp::JumpCmpC { cmp, on, c, target }]))
        }
        TrOp::ElemsLocal(slot) => {
            let Some(TrOp::JumpIfFalseI(target)) = at(1) else {
                return None;
            };
            Some((
                2,
                vec![TrOp::JumpIfEmptyLocal {
                    slot: *slot,
                    target: *target,
                }],
            ))
        }
        op => {
            let cmp = TrCmp::of(op)?;
            let (on, target) = cond_jump(at(1))?;
            Some((2, vec![TrOp::JumpCmp { cmp, on, target }]))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slot_constant_test_and_branch_is_one_op() {
        let ops = vec![
            TrOp::LoadI(1),
            TrOp::ConstI(92),
            TrOp::EqI,
            TrOp::JumpIfFalseI(5),
            TrOp::IncIVoid(1),
            TrOp::ReturnNil,
        ];
        let out = optimize(ops);
        assert!(matches!(
            out[0],
            TrOp::JumpCmpLC {
                cmp: TrCmp::Eq,
                on: false,
                slot: 1,
                c: 92,
                target: 2
            }
        ));
        assert!(matches!(out[1], TrOp::IncIVoid(1)));
        assert!(matches!(out[2], TrOp::ReturnNil));
    }

    #[test]
    fn a_window_entered_in_the_middle_is_left_alone() {
        // Index 2 (`EqI`) is a jump target, so the four-op window starting
        // at 0 cannot be fused; `EqI; JumpIfFalseI` from 2 still can.
        let ops = vec![
            TrOp::LoadI(0),
            TrOp::ConstI(1),
            TrOp::EqI,
            TrOp::JumpIfFalseI(0),
            TrOp::JumpIfTrueI(2),
            TrOp::ReturnNil,
        ];
        let out = optimize(ops);
        assert!(matches!(out[0], TrOp::LoadI(0)));
        assert!(matches!(out[1], TrOp::ConstI(1)));
        assert!(matches!(
            out[2],
            TrOp::JumpCmp {
                cmp: TrCmp::Eq,
                on: false,
                target: 0
            }
        ));
        assert!(matches!(out[3], TrOp::JumpIfTrueI(2)));
    }

    #[test]
    fn or_chain_threads_its_keep_jump() {
        // `$c == 9 || $c == 10` as a branch condition.
        let ops = vec![
            TrOp::LoadI(0),           // 0
            TrOp::ConstI(9),          // 1
            TrOp::EqI,                // 2
            TrOp::JumpIfTrueKeepI(8), // 3
            TrOp::PopI,               // 4
            TrOp::LoadI(0),           // 5
            TrOp::ConstI(10),         // 6
            TrOp::EqI,                // 7
            TrOp::JumpIfFalseI(10),   // 8
            TrOp::IncIVoid(1),        // 9
            TrOp::ReturnNil,          // 10
        ];
        let out = optimize(ops);
        let want_first = TrOp::JumpCmpLC {
            cmp: TrCmp::Eq,
            on: true,
            slot: 0,
            c: 9,
            target: 2,
        };
        assert_eq!(format!("{:?}", out[0]), format!("{want_first:?}"));
        assert!(matches!(
            out[1],
            TrOp::JumpCmpLC {
                on: false,
                c: 10,
                target: 3,
                ..
            }
        ));
        assert!(matches!(out[2], TrOp::IncIVoid(1)));
        assert!(matches!(out[3], TrOp::ReturnNil));
    }

    #[test]
    fn dead_pushes_vanish_and_their_targets_move_on() {
        let ops = vec![
            TrOp::JumpIfFalseI(2),
            TrOp::Jump(4),
            TrOp::ConstObj(0),
            TrOp::PopObj,
            TrOp::ReturnNil,
        ];
        let out = optimize(ops);
        // Both jumps now land on `ReturnNil`.
        assert!(matches!(out[0], TrOp::JumpIfFalseI(2)));
        assert!(matches!(out[1], TrOp::Jump(2)));
        assert!(matches!(out[2], TrOp::ReturnNil));
        assert_eq!(out.len(), 3);
    }
}
