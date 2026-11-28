//! Loop kernels: the typing pass (ADR-19288 D5).
//!
//! `type_region` walks a loop region once with the kinds its carried slots
//! hold at entry and decides, op by op, that every value is a machine `Int` or
//! `Num` (or a comparison result). A counted `for` or a `while` nested in the
//! region is walked in place: it becomes a native loop inside the kernel, and a
//! bail inside it reruns the enclosing kernel iteration from its start.
//!
//! Joins are keyed by `(segment start, ip)`: a value may sit on the stack
//! across a forward jump inside one segment, and its kind must agree on every
//! edge.

use super::vm_loop_kernel_types::{
    ElemRead, ElemShape, K, LoopShape, LoopVar, is_topic, nested_range, nested_while, num_const,
    num_method_call, plain_decl,
};
use super::*;
use crate::opcode::kernel_slot::SlotKind;
use std::collections::BTreeMap;

/// What the typing pass settled.
pub(super) struct Typed {
    /// The kind of every slot the region touches, by slot.
    pub(super) slot_kind: BTreeMap<u32, SlotKind>,
    /// The declarations `(SetVarDynamic ip, slot)` of the outermost body, in
    /// region order (a nested loop's declarations are not replayed: they are
    /// out of scope once the region ends).
    pub(super) decls: Vec<(u32, u32)>,
    /// The readonly checks of the region.
    pub(super) readonly_checks: Vec<(bool, u32)>,
    /// `.Num` call sites `(ip, receiver kind)`, vouched for at entry.
    pub(super) num_sites: Vec<(u32, SlotKind)>,
    /// Sub calls `(ip, arity)` in region order; each must reach a sub kernel
    /// (resolved at entry, ADR-19288 D3), and its index here is its slot in
    /// the kernel's callee table.
    pub(super) calls: Vec<(u32, u8)>,
    /// Every join `(segment start, ip)` with the stack kinds it is entered with
    /// (a jump target, and each segment's end).
    pub(super) joins: BTreeMap<(usize, usize), Vec<K>>,
    /// The region stores under a `"prefix$i"` key: the entry checks that the
    /// interpolation is the built-in one (`any_user_find_method`).
    pub(super) str_keys: bool,
}

struct Walk<'c> {
    code: &'c CompiledCode,
    topic: bool,
    /// A `Str` list loop's variable slot (`ListKind::Str`): its reads are the
    /// item's index (`K::StrItem`).
    str_var: Option<u32>,
    t: Typed,
    /// Loop variables: readable, never written.
    loop_vars: Vec<u32>,
    /// Declared slots, and those assigned so far in the current iteration.
    declared: Vec<u32>,
    live: Vec<u32>,
    /// Nesting depth of the walk (0 = the kernel's own body).
    depth: u32,
    /// The shape and element kind of each array or hash the region reads,
    /// settled at entry.
    elem_reads: &'c [ElemRead],
    /// The arrays and hashes the region stores into (`true` = a hash).
    elem_stores: &'c [(u32, bool)],
}

impl Walk<'_> {
    fn readable(&self, s: u32) -> Option<SlotKind> {
        let k = *self.t.slot_kind.get(&s)?;
        if self.declared.contains(&s) && !self.live.contains(&s) {
            return None;
        }
        Some(k)
    }

    /// Walk `[start, end)` leaving `leaves` values. `None` declines.
    fn segment(&mut self, start: usize, end: usize, leaves: usize) -> Option<()> {
        let mut stack: Vec<K> = Vec::new();
        let mut pending: Option<u32> = None;
        let mut open = true;
        let mut incoming: BTreeMap<usize, Vec<K>> = BTreeMap::new();
        let edge = |incoming: &mut BTreeMap<usize, Vec<K>>, t: usize, stack: &[K]| -> Option<()> {
            if t <= start || t > end || stack.iter().any(super::vm_loop_kernel_types::k_is_pending)
            {
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
        let mut i = start;
        loop {
            if let Some(k) = incoming.get(&i).cloned() {
                if open && stack != k {
                    return None;
                }
                stack = k.clone();
                open = true;
                if i < end {
                    self.t.joins.insert((start, i), k);
                }
            }
            if i >= end {
                break;
            }
            if !open {
                i += 1;
                continue;
            }
            let op = &self.code.ops[i];
            if let Some((_, _)) = num_method_call(self.code, op) {
                let k = stack.pop()?.slot_kind()?;
                self.t.num_sites.push((i as u32, k));
                stack.push(K::Num);
                i += 1;
                continue;
            }
            if let Some((_, var, body_start, body_end, resume)) = nested_range(self.code, i) {
                if stack.pop()? != K::Int || stack.pop()? != K::Int || pending.is_some() {
                    return None;
                }
                if self
                    .t
                    .slot_kind
                    .insert(var, SlotKind::Int)
                    .is_some_and(|k| k != SlotKind::Int)
                {
                    return None;
                }
                self.loop_vars.push(var);
                self.depth += 1;
                self.segment(body_start, body_end, 0)?;
                self.depth -= 1;
                i = resume;
                continue;
            }
            if let Some((cond_start, cond_end, body_end)) = nested_while(self.code, i) {
                if !stack.is_empty() || pending.is_some() {
                    return None;
                }
                self.depth += 1;
                self.segment(cond_start, cond_end, 1)?;
                self.segment(cond_end, body_end, 0)?;
                self.depth -= 1;
                i = body_end;
                continue;
            }
            match op {
                OpCode::GetLocal(s) if self.str_var == Some(*s) => stack.push(K::StrItem),
                OpCode::GetLocal(s) if self.elem_reads.iter().any(|e| e.0 == *s) => {
                    stack.push(K::Arr(*s));
                }
                OpCode::GetLocal(s) | OpCode::GetLocalMetaAssign { slot: s, .. } => {
                    stack.push(K::of(self.readable(*s)?));
                }
                // `@a[$i]`: an Int index into a read array; the element is
                // the kind the entry settled (a different one declines there).
                // `@g[$i]` of a grid is a row, which only a second subscript
                // consumes; `%h{$i}` keys a hash by an Int.
                OpCode::Index { is_positional } => {
                    if stack.pop()? != K::Int {
                        return None;
                    }
                    let (s, row) = match stack.pop()? {
                        K::Arr(s) => (s, false),
                        K::Row(s) => (s, true),
                        _ => return None,
                    };
                    let &(_, shape, k) = self.elem_reads.iter().find(|e| e.0 == s)?;
                    stack.push(match (shape, row, *is_positional) {
                        (ElemShape::Array, false, true) | (ElemShape::Hash, false, false) => {
                            K::of(k)
                        }
                        (ElemShape::Grid, false, true) => K::Row(s),
                        (ElemShape::Grid, true, true) => K::of(k),
                        _ => return None,
                    });
                }
                // `@a[$i] = v` / `%h{$i} = v`: the value stays on the stack.
                OpCode::IndexAssignLocal {
                    slot,
                    is_positional,
                    ..
                } if self.elem_stores.contains(&(*slot, !*is_positional)) => {
                    let v = stack.pop()?;
                    let key = stack.pop()?;
                    // A hash takes a `"prefix$i"` key too; an array only an Int.
                    let key_ok = key == K::Int || (!*is_positional && matches!(key, K::IntKey(_)));
                    if !v.numeric() || !key_ok {
                        return None;
                    }
                    if matches!(key, K::IntKey(_)) {
                        self.t.str_keys = true;
                    }
                    stack.push(v);
                }
                // `%h{$k}++` and kin on a stored hash, keyed by a `Str` list
                // item or an Int: the old or new value is never read, only
                // sunk (`K::Sunk`).
                OpCode::PreIncrementIndex(_, Some(slot), false)
                | OpCode::PreDecrementIndex(_, Some(slot), false)
                | OpCode::PostIncrementIndex(_, Some(slot), false)
                | OpCode::PostDecrementIndex(_, Some(slot), false)
                    if self.elem_stores.contains(&(*slot, true)) =>
                {
                    match stack.pop()? {
                        K::StrItem | K::Int => stack.push(K::Sunk),
                        _ => return None,
                    }
                }
                // `@a.push(v)`: the array it leaves is only ever sunk.
                OpCode::ArrayPush {
                    target_slot: Some(slot),
                    value_source_idx: None,
                    ..
                } if self.elem_stores.contains(&(*slot, false)) => {
                    if !stack.pop()?.numeric() {
                        return None;
                    }
                    stack.push(K::Sunk);
                }
                OpCode::SinkPop(..) => {
                    let k = stack.pop()?;
                    if k != K::Sunk && !k.numeric() {
                        return None;
                    }
                }
                // `"prefix$i"`: a `Str` constant and an `Int`, only as a hash
                // key (see `K::IntKey`).
                OpCode::StringConcat(2) => {
                    let y = stack.pop()?;
                    let x = stack.pop()?;
                    match (x, y) {
                        (K::StrConst(c), K::Int) => stack.push(K::IntKey(c)),
                        _ => return None,
                    }
                }
                OpCode::SinkPopAssign => {
                    stack.pop()?.slot_kind()?;
                }
                // `$i++` and kin on a carried or live Int: the readonly check
                // the op makes is made once at entry.
                OpCode::PreIncrement(n, Some(s))
                | OpCode::PreDecrement(n, Some(s))
                | OpCode::PostIncrement(n, Some(s))
                | OpCode::PostDecrement(n, Some(s)) => {
                    if self.readable(*s)? != SlotKind::Int || self.loop_vars.contains(s) {
                        return None;
                    }
                    self.t.readonly_checks.push((true, *n));
                    stack.push(K::Int);
                }
                OpCode::GetGlobal(n) if self.topic && is_topic(self.code, *n) => stack.push(K::Int),
                OpCode::LoadConst(c) if super::vm_loop_kernel_types::str_const(self.code, *c) => {
                    stack.push(K::StrConst(*c))
                }
                OpCode::LoadConst(c) => stack.push(num_const(self.code, *c)?.0),
                OpCode::Add | OpCode::Sub | OpCode::Mul => {
                    let y = stack.pop()?;
                    let x = stack.pop()?;
                    if !x.numeric() || !y.numeric() {
                        return None;
                    }
                    stack.push(if (x, y) == (K::Int, K::Int) {
                        K::Int
                    } else {
                        K::Num
                    });
                }
                // `%` on two Ints is the floored `mod`; on anything else the
                // generic path decides.
                OpCode::IntDiv
                | OpCode::IntMod
                | OpCode::Mod
                | OpCode::BitAnd
                | OpCode::BitOr
                | OpCode::BitXor
                | OpCode::BitShiftRight => {
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
                    if x != y || !x.numeric() {
                        return None;
                    }
                    stack.push(K::Bool);
                }
                OpCode::Dup => stack.push(*stack.last()?),
                OpCode::Pop => {
                    stack.pop()?;
                }
                // Identities on an Int or Num: a pair's containerization and
                // the wrap that lets a callee bind a variable `is rw` (a sub
                // kernel's parameters are readonly).
                OpCode::ContainerizePair | OpCode::WrapVarRef { .. } => {
                    stack.last()?.slot_kind()?;
                }
                OpCode::CallFunc {
                    arity,
                    literal_native_args: 0,
                    ..
                } => {
                    if *arity as usize > super::vm_kernel_compile::MAX_KERNEL_PARAMS
                        || self.t.calls.len() >= super::vm_loop_kernel_compile::MAX_CALLS
                    {
                        return None;
                    }
                    for _ in 0..*arity {
                        if stack.pop()? != K::Int {
                            return None;
                        }
                    }
                    self.t.calls.push((i as u32, *arity as u8));
                    stack.push(K::Int);
                }
                OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) => {
                    stack.pop()?;
                    edge(&mut incoming, *t as usize, &stack)?;
                }
                OpCode::Jump(t) => {
                    edge(&mut incoming, *t as usize, &stack)?;
                    open = false;
                }
                OpCode::CheckReadOnly(n) => self.t.readonly_checks.push((false, *n)),
                OpCode::CheckReadOnlyModify(n) => self.t.readonly_checks.push((true, *n)),
                OpCode::MarkArrayShareSource(_) => {
                    stack.last()?.slot_kind()?;
                }
                OpCode::SetLocal(s) => {
                    let v = stack.pop()?.slot_kind()?;
                    if self.loop_vars.contains(s) || self.t.slot_kind.get(s) != Some(&v) {
                        return None;
                    }
                    if self.declared.contains(s) && !self.live.contains(s) {
                        return None;
                    }
                }
                OpCode::SetVarDynamic { .. } => {
                    let slot = plain_decl(op)?;
                    if pending.is_some() || self.loop_vars.contains(&slot) {
                        return None;
                    }
                    // A declaration is fresh each iteration: it is not live
                    // until its store below.
                    self.live.retain(|s| *s != slot);
                    if !self.declared.contains(&slot) {
                        if self.t.slot_kind.contains_key(&slot) {
                            // A carried slot cannot also be declared here.
                            return None;
                        }
                        self.declared.push(slot);
                        if self.depth == 0 {
                            self.t.decls.push((i as u32, slot));
                        }
                    }
                    pending = Some(slot);
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
                    let v = stack.pop()?.slot_kind()?;
                    if self
                        .t
                        .slot_kind
                        .insert(*slot, v)
                        .is_some_and(|old| old != v)
                    {
                        return None;
                    }
                    self.live.push(*slot);
                }
                _ => return None,
            }
            i += 1;
        }
        if !open
            || stack.len() != leaves
            || pending.is_some()
            || stack.iter().any(super::vm_loop_kernel_types::k_is_pending)
        {
            return None;
        }
        // The stack every edge brings to the segment's end.
        self.t.joins.insert((start, end), stack);
        Some(())
    }
}

/// Type the region with the carried slots' kinds. `None` declines.
// Cost: O(n), n = ops in the region.
pub(super) fn type_region(
    code: &CompiledCode,
    shape: &LoopShape,
    carried: &[(u32, SlotKind)],
    elem_reads: &[ElemRead],
    elem_stores: &[(u32, bool)],
) -> Option<Typed> {
    let mut w = Walk {
        code,
        topic: shape.loop_var() == Some(LoopVar::Topic),
        str_var: match (*shape, shape.loop_var()) {
            (
                LoopShape::Range {
                    list: super::vm_loop_kernel_types::ListKind::Str,
                    ..
                },
                Some(LoopVar::Slot(s)),
            ) => Some(s),
            _ => None,
        },
        t: Typed {
            slot_kind: carried.iter().copied().collect(),
            decls: Vec::new(),
            readonly_checks: Vec::new(),
            num_sites: Vec::new(),
            calls: Vec::new(),
            joins: BTreeMap::new(),
            str_keys: false,
        },
        loop_vars: Vec::new(),
        declared: Vec::new(),
        live: Vec::new(),
        depth: 0,
        elem_reads,
        elem_stores,
    };
    if let Some(LoopVar::Slot(s)) = shape.loop_var() {
        // A `Str` list's variable holds no machine value (`K::StrItem`).
        if w.str_var.is_none() {
            w.t.slot_kind.insert(s, SlotKind::Int);
        }
        w.loop_vars.push(s);
    }
    for (start, end, leaves) in shape.segments() {
        w.segment(start, end, leaves)?;
    }
    Some(w.t)
}
