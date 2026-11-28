//! Loop kernels: the region shape and its typing pass (ADR-19288 D5).
//!
//! A loop kernel's region is a counted `for` over an int `Range` (its body), or
//! a `while` (its condition, then its body). [`type_region`] walks the region
//! once with the kinds its carried slots hold at entry and decides, by a closed
//! op list, whether every value is a machine `Int` or `Num` (or a comparison
//! result). An op not matched declines the loop, so a new opcode can only make
//! loop kernels rarer.
//!
//! The region may branch forward only, inside one segment (a jump to a
//! segment's end is allowed: that is how `&&`/`||` reach a `while`
//! condition's result). A value may sit on the stack across such a join; its
//! kind must agree on every edge.

use super::*;
use crate::opcode::kernel_slot::SlotKind;

/// Which loop the region belongs to.
#[derive(Debug, Clone, Copy)]
pub(super) enum LoopShape {
    /// `for $lo..$hi` / `for ^$n`: the body ops `[body.0, body.1)`, run with
    /// the loop variable set to each integer.
    Range { body: (usize, usize), var: LoopVar },
    /// `while COND { BODY }`: the condition ops `[cond.0, cond.1)` leave one
    /// value, then the body ops `[cond.1, body_end)` run.
    While { cond: (usize, usize), body_end: usize },
}

/// How a `for` body reads its loop variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum LoopVar {
    /// `-> $i`: the parameter's slot.
    Slot(u32),
    /// The topic, read by name (`GetGlobal("_")`): mainline has no `$_` slot.
    Topic,
}

impl LoopShape {
    /// The region's segments `(start, end, values left at the end)`.
    pub(super) fn segments(&self) -> Vec<(usize, usize, usize)> {
        match *self {
            LoopShape::Range { body, .. } => vec![(body.0, body.1, 0)],
            LoopShape::While { cond, body_end } => {
                vec![(cond.0, cond.1, 1), (cond.1, body_end, 0)]
            }
        }
    }

    pub(super) fn loop_var(&self) -> Option<LoopVar> {
        match *self {
            LoopShape::Range { var, .. } => Some(var),
            LoopShape::While { .. } => None,
        }
    }
}

/// The kind of an abstract stack entry.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum K {
    Int,
    Num,
    Bool,
}

impl K {
    pub(super) fn of(kind: SlotKind) -> K {
        match kind {
            SlotKind::Int => K::Int,
            SlotKind::Num => K::Num,
        }
    }

    fn slot(self) -> Option<SlotKind> {
        match self {
            K::Int => Some(SlotKind::Int),
            K::Num => Some(SlotKind::Num),
            K::Bool => None,
        }
    }
}

/// A numeric constant operand.
pub(super) fn num_const(code: &CompiledCode, idx: u32) -> Option<(K, i64, f64)> {
    match code.constants.get(idx as usize)?.view() {
        ValueView::Int(n) => Some((K::Int, n, 0.0)),
        ValueView::Num(f) => Some((K::Num, 0, f)),
        _ => None,
    }
}

/// Whether `op` declares a plain non-dynamic `my` scalar with a slot.
pub(super) fn plain_decl(op: &OpCode) -> Option<u32> {
    match op {
        OpCode::SetVarDynamic {
            dynamic: false,
            local_slot: Some(slot),
            reset: crate::opcode::DeclReset::Fresh | crate::opcode::DeclReset::SeedIfUnbound,
            type_follows: false,
            bind_declaration: false,
            shadows_by_name: false,
            ..
        } => Some(*slot),
        _ => None,
    }
}

/// Whether `name_idx` names the topic.
pub(super) fn is_topic(code: &CompiledCode, name_idx: u32) -> bool {
    matches!(code.constants.get(name_idx as usize).map(|v| v.view()), Some(ValueView::Str(s)) if &**s == "_")
}

/// The slots a region reads or writes that it does not declare and that are
/// not its loop variable: the frame state the kernel carries in and out.
/// `None` when the region contains an op no loop kernel admits, so the loop
/// is never retried.
// Cost: O(n), n = ops in the region.
pub(super) fn carried_slots(code: &CompiledCode, shape: &LoopShape) -> Option<Vec<u32>> {
    let mut declared = Vec::new();
    let mut used = Vec::new();
    for (start, end, _) in shape.segments() {
        for op in code.ops.get(start..end)? {
            match op {
                OpCode::GetLocal(s) | OpCode::SetLocal(s) => used.push(*s),
                OpCode::GetLocalMetaAssign { slot, .. } => used.push(*slot),
                OpCode::SetVarDynamic { .. } => declared.push(plain_decl(op)?),
                OpCode::SetLocalDecl { .. }
                | OpCode::LoadConst(_)
                | OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::IntDiv
                | OpCode::IntMod
                | OpCode::NumLt
                | OpCode::NumLe
                | OpCode::NumGt
                | OpCode::NumGe
                | OpCode::NumEq
                | OpCode::NumNe
                | OpCode::Dup
                | OpCode::Pop
                | OpCode::Jump(_)
                | OpCode::JumpIfFalse(_)
                | OpCode::JumpIfTrue(_)
                | OpCode::CheckReadOnly(_)
                | OpCode::CheckReadOnlyModify(_)
                | OpCode::MarkArrayShareSource(_) => {}
                OpCode::GetGlobal(n)
                    if is_topic(code, *n) && shape.loop_var() == Some(LoopVar::Topic) => {}
                _ => return None,
            }
        }
    }
    let var = match shape.loop_var() {
        Some(LoopVar::Slot(s)) => Some(s),
        _ => None,
    };
    let mut out: Vec<u32> = used
        .into_iter()
        .filter(|s| !declared.contains(s) && Some(*s) != var)
        .collect();
    out.sort_unstable();
    out.dedup();
    // A typed variable's store checks its constraint, which a kernel would
    // skip: only untyped slots are carried or declared.
    let untyped = |s: &u32| {
        code.declared_constraint(*s as usize) == crate::binding_desc::DeclaredConstraint::Untyped
    };
    if !out.iter().all(untyped) || !declared.iter().all(untyped) {
        return None;
    }
    Some(out)
}

/// What the typing pass settled.
pub(super) struct Typed {
    /// The kind of every slot the region touches, by slot.
    pub(super) slot_kind: std::collections::BTreeMap<u32, SlotKind>,
    /// The declarations `(SetVarDynamic ip, slot)` in region order.
    pub(super) decls: Vec<(u32, u32)>,
    /// The readonly checks of the region.
    pub(super) readonly_checks: Vec<(bool, u32)>,
    /// Every join point (a jump target, and each segment's end) with the
    /// stack kinds it is entered with.
    pub(super) joins: std::collections::BTreeMap<usize, Vec<K>>,
}

/// Type the region with the carried slots' kinds. `None` declines.
// Cost: O(n), n = ops in the region.
pub(super) fn type_region(
    code: &CompiledCode,
    shape: &LoopShape,
    carried: &[(u32, SlotKind)],
) -> Option<Typed> {
    let mut slot_kind: std::collections::BTreeMap<u32, SlotKind> =
        carried.iter().copied().collect();
    let var_slot = match shape.loop_var() {
        Some(LoopVar::Slot(s)) => {
            slot_kind.insert(s, SlotKind::Int);
            Some(s)
        }
        _ => None,
    };
    let mut decls = Vec::new();
    let mut readonly_checks = Vec::new();
    let mut joins: std::collections::BTreeMap<usize, Vec<K>> = Default::default();
    // Declared slots that are assigned in the current iteration so far.
    let mut live_decls: Vec<u32> = Vec::new();
    for (start, end, leaves) in shape.segments() {
        let mut stack: Vec<K> = Vec::new();
        let mut pending: Option<u32> = None;
        let mut open = true;
        let mut incoming: std::collections::BTreeMap<usize, Vec<K>> = Default::default();
        let edge = |incoming: &mut std::collections::BTreeMap<usize, Vec<K>>,
                        t: usize,
                        stack: &[K]|
         -> Option<()> {
            if t <= start || t > end {
                return None;
            }
            match incoming.get(&t) {
                Some(k) if k.as_slice() != stack => None,
                Some(_) => Some(()),
                None => {
                    incoming.insert(t, stack.to_vec());
                    Some(())
                }
            }
        };
        for i in start..=end {
            if let Some(k) = incoming.get(&i).cloned() {
                if open && stack != k {
                    return None;
                }
                stack = k.clone();
                open = true;
                if i < end {
                    joins.insert(i, k);
                }
            }
            if i == end {
                break;
            }
            if !open {
                continue;
            }
            let op = &code.ops[i];
            match op {
                OpCode::GetLocal(s) | OpCode::GetLocalMetaAssign { slot: s, .. } => {
                    let k = *slot_kind.get(s)?;
                    // A declared slot is readable only after its declaration
                    // in this iteration.
                    if decls.iter().any(|(_, d)| d == s) && !live_decls.contains(s) {
                        return None;
                    }
                    stack.push(K::of(k));
                }
                OpCode::GetGlobal(n)
                    if is_topic(code, *n) && shape.loop_var() == Some(LoopVar::Topic) =>
                {
                    stack.push(K::Int)
                }
                OpCode::LoadConst(c) => stack.push(num_const(code, *c)?.0),
                OpCode::Add | OpCode::Sub | OpCode::Mul => {
                    let y = stack.pop()?;
                    let x = stack.pop()?;
                    stack.push(match (x, y) {
                        (K::Int, K::Int) => K::Int,
                        (K::Bool, _) | (_, K::Bool) => return None,
                        _ => K::Num,
                    });
                }
                OpCode::IntDiv | OpCode::IntMod => {
                    if stack.pop()? != K::Int || stack.pop()? != K::Int {
                        return None;
                    }
                    stack.push(K::Int);
                }
                OpCode::NumLt
                | OpCode::NumLe
                | OpCode::NumGt
                | OpCode::NumGe
                | OpCode::NumEq
                | OpCode::NumNe => {
                    let y = stack.pop()?;
                    let x = stack.pop()?;
                    // Mixed Int/Num comparisons are left to the generic path.
                    if x != y || x == K::Bool {
                        return None;
                    }
                    stack.push(K::Bool);
                }
                OpCode::Dup => stack.push(*stack.last()?),
                OpCode::Pop => {
                    stack.pop()?;
                }
                OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) => {
                    stack.pop()?;
                    edge(&mut incoming, *t as usize, &stack)?;
                }
                OpCode::Jump(t) => {
                    edge(&mut incoming, *t as usize, &stack)?;
                    open = false;
                }
                OpCode::CheckReadOnly(n) => readonly_checks.push((false, *n)),
                OpCode::CheckReadOnlyModify(n) => readonly_checks.push((true, *n)),
                OpCode::MarkArrayShareSource(_) => {
                    if stack.last()? == &K::Bool {
                        return None;
                    }
                }
                OpCode::SetLocal(s) => {
                    let v = stack.pop()?.slot()?;
                    if Some(*s) == var_slot || slot_kind.get(s) != Some(&v) {
                        return None;
                    }
                    if decls.iter().any(|(_, d)| d == s) && !live_decls.contains(s) {
                        return None;
                    }
                }
                OpCode::SetVarDynamic { .. } => {
                    let slot = plain_decl(op)?;
                    if pending.is_some() || Some(slot) == var_slot || carried.iter().any(|c| c.0 == slot) {
                        return None;
                    }
                    if decls.iter().any(|(_, d)| *d == slot) {
                        return None;
                    }
                    pending = Some(slot);
                    decls.push((i as u32, slot));
                }
                OpCode::SetLocalDecl {
                    slot,
                    explicit_init: true,
                    typechecked: false,
                } => {
                    if pending != Some(*slot) {
                        return None;
                    }
                    pending = None;
                    let v = stack.pop()?.slot()?;
                    if slot_kind.insert(*slot, v).is_some_and(|old| old != v) {
                        return None;
                    }
                    live_decls.push(*slot);
                }
                _ => return None,
            }
        }
        if !open || stack.len() != leaves || pending.is_some() {
            return None;
        }
        // The stack every edge brings to the segment's end.
        joins.insert(end, stack);
    }
    Some(Typed {
        slot_kind,
        decls,
        readonly_checks,
        joins,
    })
}
