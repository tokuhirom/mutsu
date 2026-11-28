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
    K, LoopShape, LoopVar, is_topic, nested_range, nested_while, num_const, num_method_call,
    plain_decl,
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
    /// Every join `(segment start, ip)` with the stack kinds it is entered with
    /// (a jump target, and each segment's end).
    pub(super) joins: BTreeMap<(usize, usize), Vec<K>>,
}

struct Walk<'c> {
    code: &'c CompiledCode,
    topic: bool,
    t: Typed,
    /// Loop variables: readable, never written.
    loop_vars: Vec<u32>,
    /// Declared slots, and those assigned so far in the current iteration.
    declared: Vec<u32>,
    live: Vec<u32>,
    /// Nesting depth of the walk (0 = the kernel's own body).
    depth: u32,
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
                let k = match stack.pop()? {
                    K::Int => SlotKind::Int,
                    K::Num => SlotKind::Num,
                    K::Bool => return None,
                };
                self.t.num_sites.push((i as u32, k));
                stack.push(K::Num);
                i += 1;
                continue;
            }
            if let Some((_, var, body_start, body_end, resume)) = nested_range(self.code, i) {
                if stack.pop()? != K::Int || stack.pop()? != K::Int || pending.is_some() {
                    return None;
                }
                if self.t.slot_kind.insert(var, SlotKind::Int).is_some_and(|k| k != SlotKind::Int) {
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
                OpCode::GetLocal(s) | OpCode::GetLocalMetaAssign { slot: s, .. } => {
                    stack.push(K::of(self.readable(*s)?));
                }
                OpCode::GetGlobal(n) if self.topic && is_topic(self.code, *n) => stack.push(K::Int),
                OpCode::LoadConst(c) => stack.push(num_const(self.code, *c)?.0),
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
                OpCode::CheckReadOnly(n) => self.t.readonly_checks.push((false, *n)),
                OpCode::CheckReadOnlyModify(n) => self.t.readonly_checks.push((true, *n)),
                OpCode::MarkArrayShareSource(_) => {
                    if stack.last()? == &K::Bool {
                        return None;
                    }
                }
                OpCode::SetLocal(s) => {
                    let v = match stack.pop()? {
                        K::Int => SlotKind::Int,
                        K::Num => SlotKind::Num,
                        K::Bool => return None,
                    };
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
                    let v = match stack.pop()? {
                        K::Int => SlotKind::Int,
                        K::Num => SlotKind::Num,
                        K::Bool => return None,
                    };
                    if self.t.slot_kind.insert(*slot, v).is_some_and(|old| old != v) {
                        return None;
                    }
                    self.live.push(*slot);
                }
                _ => return None,
            }
            i += 1;
        }
        if !open || stack.len() != leaves || pending.is_some() {
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
) -> Option<Typed> {
    let mut w = Walk {
        code,
        topic: shape.loop_var() == Some(LoopVar::Topic),
        t: Typed {
            slot_kind: carried.iter().copied().collect(),
            decls: Vec::new(),
            readonly_checks: Vec::new(),
            num_sites: Vec::new(),
            joins: BTreeMap::new(),
        },
        loop_vars: Vec::new(),
        declared: Vec::new(),
        live: Vec::new(),
        depth: 0,
    };
    if let Some(LoopVar::Slot(s)) = shape.loop_var() {
        w.t.slot_kind.insert(s, SlotKind::Int);
        w.loop_vars.push(s);
    }
    for (start, end, leaves) in shape.segments() {
        w.segment(start, end, leaves)?;
    }
    Some(w.t)
}
