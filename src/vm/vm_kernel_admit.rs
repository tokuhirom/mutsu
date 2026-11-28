//! Native int kernels: admission (ADR-19288 D1).
//!
//! Whether a routine may run as a kernel is decided from its signature and its
//! compiled bytecode alone, by a closed op list: an op not matched in [`scan`]
//! declines the routine, so a new opcode can only make kernels rarer, never
//! wrong. The abstract stack holds machine integers and comparison results;
//! control flow is statement-level (nothing on the stack at a join) and
//! forward-only (a kernel has no loop, so it needs no back-edge poll).

use super::vm_kernel_compile::MAX_KERNEL_PARAMS;
use super::*;

/// Whether `cf`'s signature is one a kernel can bind: 1..=6 mandatory
/// positional `$` scalars, untyped or `Int`, with nothing else attached.
// Cost: O(p), p = parameters.
pub(super) fn admit_signature(cf: &CompiledFunction) -> Option<Vec<usize>> {
    let n = cf.param_defs.len();
    if n == 0 || n > MAX_KERNEL_PARAMS {
        return None;
    }
    if cf.is_cached
        || cf.is_rw
        || cf.is_raw
        || cf.uses_return_rw
        || cf.deprecated_info.is_some()
        || cf.has_inner_subs
        || cf.declares_inner_routines
        || cf.has_rw_positional_param
        || cf.has_sigilless_positional_param
        || cf.has_copy_positional_param
        || cf.aggregate_positional_params != 0
        || cf.return_smiley.is_some()
        || !matches!(cf.return_type.as_deref(), None | Some("Int"))
    {
        return None;
    }
    for pd in &cf.param_defs {
        // `required` marks an explicit `!` and `multi_invocant` is the default;
        // neither changes how a positional scalar binds. A scalar's name is
        // stored without its `$`; an aggregate or code parameter keeps its sigil.
        let plain = !pd.named
            && !pd.slurpy
            && !pd.double_slurpy
            && !pd.onearg
            && !pd.sigilless
            && !pd.is_invocant
            && !pd.optional_marker
            && pd.default.is_none()
            && pd.where_constraint.is_none()
            && pd.sub_signature.is_none()
            && pd.outer_sub_signature.is_none()
            && pd.code_signature.is_none()
            && pd.literal_value.is_none()
            && pd.type_capture.is_none()
            && pd.traits.is_empty()
            && !pd.name.starts_with(['@', '%', '&', '$'])
            && matches!(pd.type_constraint.as_deref(), None | Some("Int"));
        if !plain {
            return None;
        }
    }
    let slots = cf.param_local_slots.as_ref()?.clone();
    if slots.len() != n {
        return None;
    }
    Some(slots)
}

/// The kind of an abstract stack entry.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(super) enum K {
    Int,
    Bool,
}

/// One forward pass over the ops: the abstract stack's kinds, the slots that
/// are definitely assigned, and the branch targets. `None` declines.
pub(super) struct Shape {
    pub(super) targets: std::collections::BTreeSet<usize>,
    pub(super) self_name: Option<Symbol>,
}

/// Whether `op` is a `SetVarDynamic` the kernel may treat as a plain slot
/// declaration: an ordinary non-dynamic `my` with a slot and nothing else.
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

/// The integer constant at `idx`, when it is a plain `Int`.
pub(super) fn int_const(code: &CompiledCode, idx: u32) -> Option<i64> {
    match code.constants.get(idx as usize)?.view() {
        ValueView::Int(n) => Some(n),
        _ => None,
    }
}

// Cost: O(n), n = ops.
pub(super) fn scan(code: &CompiledCode, params: &[usize]) -> Option<Shape> {
    let ops = &code.ops;
    let len = ops.len();
    let mut targets = std::collections::BTreeSet::new();
    for (i, op) in ops.iter().enumerate() {
        if let OpCode::Jump(t) | OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) = op {
            let t = *t as usize;
            // Forward only: a kernel has no loops, so it needs no back-edge poll.
            if t <= i || t >= len {
                return None;
            }
            targets.insert(t);
        }
    }
    let nslots = code.locals.len();
    let mut assigned = vec![false; nslots];
    for &p in params {
        *assigned.get_mut(p)? = true;
    }
    // Per target: the assigned set flowing in along jumps (intersection).
    let mut incoming: std::collections::BTreeMap<usize, Vec<bool>> = Default::default();
    let mut stack: Vec<K> = Vec::new();
    let mut live = true;
    let mut self_name: Option<Symbol> = None;
    let mut pending_decl: Option<u32> = None;
    let merge =
        |incoming: &mut std::collections::BTreeMap<usize, Vec<bool>>, t: usize, a: &[bool]| {
            incoming
                .entry(t)
                .and_modify(|cur| {
                    for (c, x) in cur.iter_mut().zip(a) {
                        *c &= *x;
                    }
                })
                .or_insert_with(|| a.to_vec());
        };
    for (i, op) in ops.iter().enumerate() {
        if targets.contains(&i) {
            // Statement-level control flow only: nothing on the stack at a join.
            if live && !stack.is_empty() {
                return None;
            }
            let mut a = incoming.get(&i).cloned();
            if live {
                a = Some(match a {
                    Some(mut x) => {
                        for (c, y) in x.iter_mut().zip(&assigned) {
                            *c &= *y;
                        }
                        x
                    }
                    None => assigned.clone(),
                });
            }
            assigned = a?;
            stack.clear();
            live = true;
        }
        if !live {
            continue;
        }
        match op {
            OpCode::GetLocal(s) => {
                if !*assigned.get(*s as usize)? {
                    return None;
                }
                stack.push(K::Int);
            }
            OpCode::LoadConst(k) => {
                int_const(code, *k)?;
                stack.push(K::Int);
            }
            OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::IntDiv | OpCode::IntMod => {
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
                if stack.pop()? != K::Int || stack.pop()? != K::Int {
                    return None;
                }
                stack.push(K::Bool);
            }
            OpCode::ContainerizePair => {
                if *stack.last()? != K::Int {
                    return None;
                }
            }
            OpCode::JumpIfFalse(t) | OpCode::JumpIfTrue(t) => {
                stack.pop()?;
                if !stack.is_empty() {
                    return None;
                }
                merge(&mut incoming, *t as usize, &assigned);
            }
            OpCode::Jump(t) => {
                if !stack.is_empty() {
                    return None;
                }
                merge(&mut incoming, *t as usize, &assigned);
                live = false;
            }
            OpCode::Return => {
                if stack.pop()? != K::Int || !stack.is_empty() {
                    return None;
                }
                live = false;
            }
            OpCode::SetVarDynamic { .. } => {
                let slot = plain_decl(op)?;
                if pending_decl.is_some()
                    || (slot as usize) >= nslots
                    || params.contains(&(slot as usize))
                {
                    return None;
                }
                pending_decl = Some(slot);
            }
            OpCode::SetLocalDecl {
                slot,
                explicit_init: true,
                typechecked: false,
            } => {
                if pending_decl != Some(*slot) || stack.pop()? != K::Int {
                    return None;
                }
                pending_decl = None;
                assigned[*slot as usize] = true;
            }
            OpCode::CallFunc {
                name_idx,
                arity,
                arg_sources_idx: None,
                literal_native_args: 0,
                ..
            } => {
                let sym = code.const_sym(*name_idx);
                if *arity as usize != params.len()
                    || code.lexical_routine(sym).is_some()
                    || sym.flags() & crate::symbol::flags::NQP_OP != 0
                    || self_name.is_some_and(|s| s != sym)
                {
                    return None;
                }
                self_name = Some(sym);
                for _ in 0..*arity {
                    if stack.pop()? != K::Int {
                        return None;
                    }
                }
                stack.push(K::Int);
            }
            _ => return None,
        }
    }
    if pending_decl.is_some() {
        return None;
    }
    // Falling off the end returns the value on top of the stack.
    if live && (stack.len() != 1 || stack[0] != K::Int) {
        return None;
    }
    Some(Shape { targets, self_name })
}
