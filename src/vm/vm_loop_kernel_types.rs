//! Loop kernels: the region shape and its typing pass (ADR-19288 D5).
//!
//! A loop kernel's region is a counted `for` over an int `Range` (its body), or
//! a `while` (its condition, then its body). The typing pass
//! (`vm_loop_kernel_typing`) walks the region once with the kinds its carried
//! slots hold at entry and decides, by a closed op list, whether every value is
//! a machine `Int` or `Num` (or a comparison result). An op not matched declines the loop, so a new opcode can only make
//! loop kernels rarer.
//!
//! The region may branch forward only, inside one segment (a jump to a
//! segment's end is allowed: that is how `&&`/`||` reach a `while`
//! condition's result). A value may sit on the stack across such a join; its
//! kind must agree on every edge.

use super::*;
use crate::opcode::kernel_slot::{ElemSlot, SlotKind};

/// Which loop the region belongs to.
#[derive(Debug, Clone, Copy)]
pub(super) enum LoopShape {
    /// `for $lo..$hi` / `for ^$n`: the body ops `[body.0, body.1)`, run with
    /// the loop variable set to each integer.
    ///
    /// With a `list` other than `ListKind::No`, the loop runs over a
    /// materialized list instead (`for %h.values { ... }`,
    /// `for $line.split(' ') -> $w { ... }`): the counter is the item's index,
    /// from the start index to the list's length (exclusive), and the loop
    /// variable is a slot holding the item at that index (see `ListKind`).
    Range {
        body: (usize, usize),
        var: LoopVar,
        list: ListKind,
    },
    /// `while COND { BODY }`: the condition ops `[cond.0, cond.1)` leave one
    /// value, then the body ops `[cond.1, body_end)` run.
    While {
        cond: (usize, usize),
        body_end: usize,
    },
}

/// What a list loop iterates (`LoopShape::Range::list`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ListKind {
    /// A counted loop over an `Int` range.
    No,
    /// Plain `Int`s, copied into the `LoopCtx::list` buffer: the loop
    /// variable is a machine `Int`.
    Int,
    /// Plain `Str`s, read in place through `LoopCtx::items`: the loop variable
    /// is the item's index (`K::StrItem`), which only a hash element's
    /// increment consumes, through a helper that reads the item.
    Str,
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
    /// An array or hash local about to be subscripted (`@a` before `[$i]`):
    /// only an `Index` consumes it.
    Arr(u32),
    /// A grid's row index already applied (`@g[$i]` before `[$j]`), carrying
    /// `$i`: only a second `Index` consumes it.
    Row(u32),
    /// A `Str` constant (`"key-"` in `"key-$i"`): only an interpolation of it
    /// and one `Int` consumes it.
    StrConst(u32),
    /// The interpolation of the `Str` constant at this index and an `Int`
    /// (`"key-$i"`), carrying the `Int`: only a hash store's key consumes it,
    /// and the store's helper writes the key's text itself.
    IntKey(u32),
    /// A `Str` list loop's variable, carrying the item's index: only a hash
    /// element's `++`/`--` consumes it, as the key.
    StrItem,
    /// The result of a hash element's `++`/`--`, which only a `SinkPop`
    /// consumes: its value is never read.
    Sunk,
}

/// What a read element slot holds, settled at kernel entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ElemShape {
    /// `@a[$i]`: an array of plain `Int`/`Num` elements.
    Array,
    /// `@g[$i][$j]`: an array of plain arrays.
    Grid,
    /// `%h{$i}`: a hash keyed by the decimal strings of `Int`s.
    Hash,
}

/// A read element slot: its local, shape and element kind.
pub(super) type ElemRead = (u32, ElemShape, SlotKind);

/// The variant-key code of a carried slot's kind.
pub(super) fn carried_code(k: SlotKind) -> u8 {
    k as u8
}

/// The variant-key code of a read element slot's shape and kind.
pub(super) fn elem_code(shape: ElemShape, k: SlotKind) -> u8 {
    2 + 2 * shape as u8 + k as u8
}

impl K {
    pub(super) fn of(kind: SlotKind) -> K {
        match kind {
            SlotKind::Int => K::Int,
            SlotKind::Num => K::Num,
        }
    }

    /// The slot kind a value of this kind stores as; `None` for a comparison
    /// result or an array.
    pub(super) fn slot_kind(self) -> Option<SlotKind> {
        match self {
            K::Int => Some(SlotKind::Int),
            K::Num => Some(SlotKind::Num),
            K::Bool
            | K::Arr(_)
            | K::Row(_)
            | K::StrConst(_)
            | K::IntKey(_)
            | K::StrItem
            | K::Sunk => None,
        }
    }

    /// Whether this is an `Int` or a `Num`.
    pub(super) fn numeric(self) -> bool {
        self.slot_kind().is_some()
    }
}

/// Whether this kind may not sit on the stack across a join: an aggregate
/// about to be subscripted, or a string piece about to be joined or keyed.
pub(super) fn k_is_pending(k: &K) -> bool {
    matches!(
        k,
        K::Arr(_) | K::Row(_) | K::StrConst(_) | K::IntKey(_) | K::StrItem | K::Sunk
    )
}

/// Whether constant `idx` is a `Str` (the prefix of a `"key-$i"` key).
// Cost: O(1).
pub(super) fn str_const(code: &CompiledCode, idx: u32) -> bool {
    matches!(
        code.constants.get(idx as usize).map(Value::view),
        Some(ValueView::Str(_))
    )
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

/// Whether an int-range `for`'s spec is one a kernel can serve: one plain
/// readonly scalar parameter (or the topic), nothing the driver must do per
/// iteration beyond binding it.
pub(super) fn plain_range_spec(spec: &crate::opcode::ForLoopSpec) -> bool {
    !(spec.collect
        || spec.threaded
        || spec.is_rw
        || spec.kv_mode
        || spec.arity != 1
        || spec.chunk_local.is_some()
        || spec.source_container_local.is_some()
        || spec.block_callable_local.is_some()
        || spec.body_declares_routines
        || spec.param_type_constraint.is_some()
        || !spec.multi_param_names.is_empty()
        || !spec.source_var_names.is_empty()
        || spec.autothread_junctions
        || spec.values_mode
        || spec.param_sigilless
        || spec.param_raw
        || spec.do_writeback)
}

/// A counted loop nested in a region: `MakeRange`/`MakeRangeExcl` at `i`,
/// then a `ForLoop` over it with a named parameter. Returns its
/// inclusivity, its parameter's slot, its body `[start, end)` and the index
/// after it (past its `RestoreForParam`).
pub(super) fn nested_range(
    code: &CompiledCode,
    i: usize,
) -> Option<(bool, u32, usize, usize, usize)> {
    let inclusive = match code.ops.get(i)? {
        OpCode::MakeRange => true,
        OpCode::MakeRangeExcl => false,
        _ => return None,
    };
    let OpCode::ForLoop(spec) = code.ops.get(i + 1)? else {
        return None;
    };
    if !plain_range_spec(spec) || spec.param_idx.is_none() {
        return None;
    }
    let var = spec.param_local?;
    let end = spec.body_end as usize;
    let resume = if matches!(code.ops.get(end), Some(OpCode::RestoreForParam)) {
        end + 1
    } else {
        end
    };
    Some((inclusive, var, i + 2, end, resume))
}

/// A `while` nested in a region at `i`: its condition `[start, end)` and the
/// end of its body.
pub(super) fn nested_while(code: &CompiledCode, i: usize) -> Option<(usize, usize, usize)> {
    match code.ops.get(i)? {
        OpCode::WhileLoop {
            cond_end,
            body_end,
            collect: false,
            isolate_topic: false,
            ..
        } => Some((i + 1, *cond_end as usize, *body_end as usize)),
        _ => None,
    }
}

/// Whether `op` is a `.Num` call with no arguments (`$i.Num`): a kernel turns
/// an Int into a Num, or keeps a Num, once the method lane vouches at entry
/// that the call reaches the built-in method.
pub(super) fn num_method_call(code: &CompiledCode, op: &OpCode) -> Option<(u32, Option<u32>)> {
    let (name_idx, target) = match op {
        OpCode::CallMethodMut {
            name_idx,
            arity: 0,
            target_name_idx,
            modifier_idx: None,
            quoted: false,
            arg_sources_idx: None,
            ..
        } => (*name_idx, Some(*target_name_idx)),
        OpCode::CallMethod {
            name_idx,
            arity: 0,
            modifier_idx: None,
            quoted: false,
            arg_sources_idx: None,
        } => (*name_idx, None),
        _ => return None,
    };
    matches!(code.constants.get(name_idx as usize).map(|v| v.view()), Some(ValueView::Str(s)) if &**s == "Num")
        .then_some((name_idx, target))
}

/// The slots a region reads or writes that it does not declare and that are
/// not its loop variable: the frame state the kernel carries in and out.
/// `None` when the region contains an op no loop kernel admits, so the loop
/// is never retried. Also returns the array locals whose elements the region
/// reads or stores (`kernel_elem_slot_ok`); a local both read and stored
/// declines, so a replayed iteration never reads its own earlier store.
// Cost: O(n), n = ops in the region.
pub(super) fn carried_slots(
    code: &CompiledCode,
    shape: &LoopShape,
) -> Option<(Vec<u32>, Vec<ElemSlot>)> {
    let mut elems: Vec<ElemSlot> = Vec::new();
    let mut elem = |slot: u32, name_idx: u32, store: bool, hash: bool| -> Option<()> {
        match elems.iter().find(|e| e.slot == slot) {
            Some(e) if e.store != store || e.hash != hash => None,
            Some(_) => Some(()),
            None => {
                elems.push(ElemSlot {
                    slot,
                    name_idx,
                    store,
                    hash,
                });
                Some(())
            }
        }
    };
    let array_slot = |s: u32| super::vm_kernel_elem::kernel_elem_slot_ok(code, s);
    let hash_slot = |s: u32| super::vm_kernel_hash::kernel_hash_slot_ok(code, s);
    let mut declared = Vec::new();
    // Nested loops' parameters: readonly per-iteration bindings the kernel
    // owns, neither carried nor `my` declarations.
    let mut loop_vars = Vec::new();
    let mut used = Vec::new();
    for (start, end, _) in shape.segments() {
        for op in code.ops.get(start..end)? {
            if num_method_call(code, op).is_some() {
                continue;
            }
            match op {
                OpCode::ForLoop(spec) => loop_vars.push(spec.param_local?),
                OpCode::MakeRange
                | OpCode::MakeRangeExcl
                | OpCode::RestoreForParam
                | OpCode::WhileLoop { .. }
                | OpCode::ContainerizePair
                | OpCode::WrapVarRef { .. }
                | OpCode::CallFunc { .. } => {}
                OpCode::GetLocal(s) if array_slot(*s) => elem(*s, 0, false, false)?,
                OpCode::GetLocal(s) if hash_slot(*s) => elem(*s, 0, false, true)?,
                OpCode::IndexAssignLocal {
                    slot,
                    name_idx,
                    is_positional,
                    index_first: true,
                    concat_append: false,
                    element_share: false,
                } if (*is_positional && array_slot(*slot))
                    || (!*is_positional && hash_slot(*slot)) =>
                {
                    elem(*slot, *name_idx, true, !*is_positional)?
                }
                OpCode::PreIncrementIndex(name_idx, Some(slot), false)
                | OpCode::PreDecrementIndex(name_idx, Some(slot), false)
                | OpCode::PostIncrementIndex(name_idx, Some(slot), false)
                | OpCode::PostDecrementIndex(name_idx, Some(slot), false)
                    if hash_slot(*slot) =>
                {
                    elem(*slot, *name_idx, true, true)?
                }
                OpCode::PreIncrement(_, Some(s))
                | OpCode::PreDecrement(_, Some(s))
                | OpCode::PostIncrement(_, Some(s))
                | OpCode::PostDecrement(_, Some(s)) => used.push(*s),
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
                | OpCode::Mod
                | OpCode::BitAnd
                | OpCode::BitOr
                | OpCode::BitXor
                | OpCode::BitShiftRight
                | OpCode::Index { .. }
                | OpCode::StringConcat(2)
                | OpCode::SinkPop(..)
                | OpCode::SinkPopAssign
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
        .filter(|s| !declared.contains(s) && !loop_vars.contains(s) && Some(*s) != var)
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
    Some((out, elems))
}
