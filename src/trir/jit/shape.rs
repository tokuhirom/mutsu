//! The static shape of a chunk's int bank, which is what lets the lowering
//! keep native operands in SSA values (ADR-0116 §2.2).
//!
//! Every [`TrOp`] pops and pushes a fixed number of int-bank entries, settled
//! by the op and, for a call, by its [`super::super::TrInnerCall`]. So the
//! bank's depth at every op is a compile-time fact, and the lowering can name
//! "the value at depth `d`" as one Cranelift variable. A chunk whose depths
//! disagree at a join (which the TRIR compiler never produces) is not lowered
//! at all.

use crate::trir::{TrArg, TrChunk, TrKind, TrOp};

/// How many int-bank entries `op` pops and pushes when it falls through.
///
/// The `Keep` jumps peek rather than pop, so both of their edges carry the
/// operand on; the pop-and-jump forms pop on both edges.
// Cost: O(a), a = the call site's argument count (O(1) for every other op).
pub(super) fn int_effect(chunk: &TrChunk, op: &TrOp) -> (u32, u32) {
    use TrOp::*;
    match op {
        ConstI(_) | LoadI(_) | IncI(_) | DecI(_) | GetRefI(_) | IncRefI(_) | DecRefI(_)
        | CharsLocal(_) => (0, 1),
        StoreI(_) | SetRefI(_) | PopI | JumpIfFalseI(_) | JumpIfTrueI(_) | BoxI | BoxN => (1, 0),
        IncIVoid(_) | DecIVoid(_) | IncRefIVoid(_) | DecRefIVoid(_) | Jump(_)
        | JumpIfFalseKeepI(_) | JumpIfTrueKeepI(_) => (0, 0),
        AddI | SubI | MulI | DivI | ModI | BitAndI | BitOrI | BitXorI | ShlI | ShrI | EqI | NeI
        | LtI | LeI | GtI | GeI | AddN | SubN | MulN | DivN | EqN | LtN | LeN | GtN | GeN => (2, 1),
        NegI | NotI | IntToNum | NumToInt | WrapI { .. } => (1, 1),
        OrdAtLocal(_) | OrdAtOuter(_) | AtPosILocal(_) | AtPosIOuter(_) | OrdAt | AtPosI => (1, 1),
        UnboxI | NarrowStoreI(_) | CharsS | TruthyDefined | TruthyObj | ElemsO | ShiftIO => (0, 1),
        SubstrS => (2, 0),
        EqAtS => (1, 1),
        PushIO => (1, 0),
        ConstObj(_)
        | LoadObj(_)
        | StoreObj(_)
        | PopObj
        | LoadOuter(_)
        | LoadBareWord(_)
        | LoadDynamic(_)
        | ConcatN(_)
        | ConcatBin
        | DupObj
        | NewHash
        | NewArray
        | MakeListN(_)
        | NqpOpGen { .. }
        | MethodGen(_) => (0, 0),
        CallTr(site) | CallGen(site) => {
            let call = &chunk.calls[*site as usize];
            let pops = call
                .args
                .iter()
                .filter(|a| matches!(a, TrArg::Value(TrKind::Int | TrKind::Num)))
                .count() as u32;
            let pushes = u32::from(call.result.is_native());
            (pops, pushes)
        }
        ReturnI | ReturnN => (1, 0),
        ReturnObj | ReturnNil => (0, 0),
    }
}

/// Whether control can fall through `op` to the next op.
pub(super) fn falls_through(op: &TrOp) -> bool {
    !matches!(
        op,
        TrOp::Jump(_) | TrOp::ReturnI | TrOp::ReturnN | TrOp::ReturnObj | TrOp::ReturnNil
    )
}

/// The jump target of `op`, if it has one.
pub(super) fn jump_target(op: &TrOp) -> Option<usize> {
    match op {
        TrOp::Jump(t)
        | TrOp::JumpIfFalseI(t)
        | TrOp::JumpIfTrueI(t)
        | TrOp::JumpIfFalseKeepI(t)
        | TrOp::JumpIfTrueKeepI(t) => Some(*t as usize),
        _ => None,
    }
}

/// The int bank's depth on entry to every op: `None` for an op no path
/// reaches. `Err` when the depths disagree at a join, underflow, or a jump
/// leaves the chunk.
// Cost: O(n), n = the chunk's op count (each op is visited once).
pub(super) fn int_depths(chunk: &TrChunk) -> Result<Vec<Option<u32>>, &'static str> {
    let ops = &chunk.ops;
    let mut depth: Vec<Option<u32>> = vec![None; ops.len()];
    let mut work: Vec<usize> = Vec::new();
    if ops.is_empty() {
        return Ok(depth);
    }
    depth[0] = Some(0);
    work.push(0);
    let reach = |at: usize, d: u32, depth: &mut Vec<Option<u32>>, work: &mut Vec<usize>| {
        if at >= depth.len() {
            return Err("jump out of the chunk");
        }
        match depth[at] {
            Some(seen) if seen != d => Err("int-bank depths disagree at a join"),
            Some(_) => Ok(()),
            None => {
                depth[at] = Some(d);
                work.push(at);
                Ok(())
            }
        }
    };
    while let Some(ip) = work.pop() {
        let d = depth[ip].unwrap_or(0);
        let op = &ops[ip];
        let (pops, pushes) = int_effect(chunk, op);
        let Some(after) = d.checked_sub(pops) else {
            return Err("int-bank underflow");
        };
        let after = after + pushes;
        if let Some(t) = jump_target(op) {
            reach(t, after, &mut depth, &mut work)?;
        }
        if falls_through(op) {
            reach(ip + 1, after, &mut depth, &mut work)?;
        }
    }
    Ok(depth)
}
