//! Renaming one op of an inlined callee into its caller (ADR-8673 D2; the
//! splice itself is `inline_call.rs`).

use super::TrirCompiler;
use super::inline_call::{RefTarget, SlotMap};
use crate::trir::TrOp;

impl TrirCompiler<'_> {
    /// One callee op, renamed into the caller.
    // Cost: O(1).
    pub(super) fn inline_op(
        &self,
        op: &TrOp,
        m: &SlotMap,
        shape: &RegionShape<'_>,
    ) -> Option<Vec<TrOp>> {
        let RegionShape {
            pos,
            region,
            join,
            nil_const,
            reseed,
        } = *shape;
        let to = |t: &u32| pos.get(*t as usize).copied();
        let reference = |s: &u16| m.refs.get(s).copied();
        let op = match op {
            // Exits: the result on the boxed bank, then the region's end.
            TrOp::ReturnI => {
                return Some(vec![TrOp::BoxI, TrOp::InlineReturn { region, join }]);
            }
            TrOp::ReturnN => {
                return Some(vec![TrOp::BoxN, TrOp::InlineReturn { region, join }]);
            }
            TrOp::ReturnNil => {
                return Some(vec![
                    TrOp::ConstObj(nil_const),
                    TrOp::InlineReturn { region, join },
                ]);
            }
            TrOp::ReturnObj => TrOp::InlineReturn { region, join },
            // Calls the callee makes: re-read its free variables after each,
            // as its own frame did.
            TrOp::CallTr(c) | TrOp::CallGen(c) | TrOp::MethodGen(c) => {
                let renamed = match op {
                    TrOp::CallTr(_) => TrOp::CallTr(c + m.calls),
                    TrOp::CallGen(_) => TrOp::CallGen(c + m.calls),
                    _ => TrOp::MethodGen(c + m.methods),
                };
                if reseed {
                    return Some(vec![renamed, TrOp::InlineReseed(region)]);
                }
                renamed
            }
            // `is rw` references: the caller's variable itself, or the
            // caller's own reference.
            TrOp::GetRefI(s) => match reference(s)? {
                RefTarget::Direct(t) => TrOp::LoadI(t),
                RefTarget::Ref(t) => TrOp::GetRefI(t),
            },
            TrOp::OrdAtLocalRef { text, pos_ref } => match reference(pos_ref)? {
                RefTarget::Direct(t) => TrOp::OrdAtLocalSlot {
                    text: m.o(*text)?,
                    pos: t,
                },
                RefTarget::Ref(t) => TrOp::OrdAtLocalRef {
                    text: m.o(*text)?,
                    pos_ref: t,
                },
            },
            TrOp::AddRefIC { pos_ref, c } => match reference(pos_ref)? {
                RefTarget::Direct(t) => TrOp::AddLocalIC { slot: t, c: *c },
                RefTarget::Ref(t) => TrOp::AddRefIC { pos_ref: t, c: *c },
            },
            TrOp::AddRefICVoid { pos_ref, c } => match reference(pos_ref)? {
                RefTarget::Direct(t) => TrOp::AddLocalICVoid { slot: t, c: *c },
                RefTarget::Ref(t) => TrOp::AddRefICVoid { pos_ref: t, c: *c },
            },
            TrOp::SetRefI(s) => match reference(s)? {
                RefTarget::Direct(t) => TrOp::StoreI(t),
                RefTarget::Ref(t) => TrOp::SetRefI(t),
            },
            TrOp::IncRefI(s) => match reference(s)? {
                RefTarget::Direct(t) => TrOp::IncI(t),
                RefTarget::Ref(t) => TrOp::IncRefI(t),
            },
            TrOp::IncRefIVoid(s) => match reference(s)? {
                RefTarget::Direct(t) => TrOp::IncIVoid(t),
                RefTarget::Ref(t) => TrOp::IncRefIVoid(t),
            },
            TrOp::DecRefI(s) => match reference(s)? {
                RefTarget::Direct(t) => TrOp::DecI(t),
                RefTarget::Ref(t) => TrOp::DecRefI(t),
            },
            TrOp::DecRefIVoid(s) => match reference(s)? {
                RefTarget::Direct(t) => TrOp::DecIVoid(t),
                RefTarget::Ref(t) => TrOp::DecRefIVoid(t),
            },
            // Native slots.
            TrOp::LoadI(s) => TrOp::LoadI(m.n(*s)?),
            TrOp::StoreI(s) => TrOp::StoreI(m.n(*s)?),
            TrOp::IncI(s) => TrOp::IncI(m.n(*s)?),
            TrOp::DecI(s) => TrOp::DecI(m.n(*s)?),
            TrOp::IncIVoid(s) => TrOp::IncIVoid(m.n(*s)?),
            TrOp::DecIVoid(s) => TrOp::DecIVoid(m.n(*s)?),
            // Boxed slots.
            TrOp::LoadObj(s) => TrOp::LoadObj(m.o(*s)?),
            TrOp::StoreObj(s) => TrOp::StoreObj(m.o(*s)?),
            TrOp::OrdAtLocal(s) => TrOp::OrdAtLocal(m.o(*s)?),
            TrOp::AtPosILocal(s) => TrOp::AtPosILocal(m.o(*s)?),
            TrOp::CharsLocal(s) => TrOp::CharsLocal(m.o(*s)?),
            TrOp::ElemsLocal(s) => TrOp::ElemsLocal(m.o(*s)?),
            TrOp::ShiftILocal(s) => TrOp::ShiftILocal(m.o(*s)?),
            TrOp::PushILocal(s) => TrOp::PushILocal(m.o(*s)?),
            TrOp::PushILocalVoid(s) => TrOp::PushILocalVoid(m.o(*s)?),
            TrOp::ShiftIStoreLocal {
                list,
                slot,
                bits,
                signed,
            } => TrOp::ShiftIStoreLocal {
                list: m.o(*list)?,
                slot: m.n(*slot)?,
                bits: *bits,
                signed: *signed,
            },
            TrOp::PushISlotLocalVoid { list, slot } => TrOp::PushISlotLocalVoid {
                list: m.o(*list)?,
                slot: m.n(*slot)?,
            },
            TrOp::OrdAtLocalSlot { text, pos } => TrOp::OrdAtLocalSlot {
                text: m.o(*text)?,
                pos: m.n(*pos)?,
            },
            TrOp::AddLocalIC { slot, c } => TrOp::AddLocalIC {
                slot: m.n(*slot)?,
                c: *c,
            },
            TrOp::AddLocalICVoid { slot, c } => TrOp::AddLocalICVoid {
                slot: m.n(*slot)?,
                c: *c,
            },
            // Free variables: after the caller's own (`apply_inline_outer_fixups`).
            TrOp::LoadOuter(n) => TrOp::LoadOuter(*n),
            TrOp::OrdAtOuter(n) => TrOp::OrdAtOuter(*n),
            TrOp::AtPosIOuter(n) => TrOp::AtPosIOuter(*n),
            // Constants.
            TrOp::ConstObj(i) => TrOp::ConstObj(i + m.consts),
            TrOp::NarrowStoreI(i) => TrOp::NarrowStoreI(i + m.consts),
            TrOp::LoadDynamic(i) => TrOp::LoadDynamic(i + m.consts),
            // Jumps.
            TrOp::Jump(t) => TrOp::Jump(to(t)?),
            TrOp::JumpIfFalseI(t) => TrOp::JumpIfFalseI(to(t)?),
            TrOp::JumpIfTrueI(t) => TrOp::JumpIfTrueI(to(t)?),
            TrOp::JumpIfFalseKeepI(t) => TrOp::JumpIfFalseKeepI(to(t)?),
            TrOp::JumpIfTrueKeepI(t) => TrOp::JumpIfTrueKeepI(to(t)?),
            TrOp::JumpCmp { cmp, on, target } => TrOp::JumpCmp {
                cmp: *cmp,
                on: *on,
                target: to(target)?,
            },
            TrOp::JumpCmpC { cmp, on, c, target } => TrOp::JumpCmpC {
                cmp: *cmp,
                on: *on,
                c: *c,
                target: to(target)?,
            },
            TrOp::JumpCmpLC {
                cmp,
                on,
                slot,
                c,
                target,
            } => TrOp::JumpCmpLC {
                cmp: *cmp,
                on: *on,
                slot: m.n(*slot)?,
                c: *c,
                target: to(target)?,
            },
            TrOp::JumpIfEmptyLocal { slot, target } => TrOp::JumpIfEmptyLocal {
                slot: m.o(*slot)?,
                target: to(target)?,
            },
            TrOp::JumpIfNonEmptyLocal { slot, target } => TrOp::JumpIfNonEmptyLocal {
                slot: m.o(*slot)?,
                target: to(target)?,
            },
            // A callee never carries regions of its own (checked above).
            TrOp::InlineEnter { .. } | TrOp::InlineReturn { .. } | TrOp::InlineReseed(_) => {
                return None;
            }
            // Everything else names no slot, constant, call or position.
            TrOp::ConstI(_)
            | TrOp::AddI
            | TrOp::SubI
            | TrOp::MulI
            | TrOp::DivI
            | TrOp::ModI
            | TrOp::NegI
            | TrOp::BitAndI
            | TrOp::BitOrI
            | TrOp::BitXorI
            | TrOp::ShlI
            | TrOp::ShrI
            | TrOp::EqI
            | TrOp::NeI
            | TrOp::LtI
            | TrOp::LeI
            | TrOp::GtI
            | TrOp::GeI
            | TrOp::NotI
            | TrOp::AddN
            | TrOp::SubN
            | TrOp::MulN
            | TrOp::DivN
            | TrOp::EqN
            | TrOp::LtN
            | TrOp::LeN
            | TrOp::GtN
            | TrOp::GeN
            | TrOp::IntToNum
            | TrOp::NumToInt
            | TrOp::WrapI { .. }
            | TrOp::BoxI
            | TrOp::BoxN
            | TrOp::UnboxI
            | TrOp::PopObj
            | TrOp::PopI
            | TrOp::OrdAt
            | TrOp::AtPosI
            | TrOp::CharsS
            | TrOp::SubstrS
            | TrOp::EqAtS
            | TrOp::ConcatN(_)
            | TrOp::ConcatBin
            | TrOp::DupObj
            | TrOp::TruthyDefined
            | TrOp::NewHash
            | TrOp::NewArray
            | TrOp::MakeListN(_)
            | TrOp::TruthyObj
            | TrOp::NqpOpGen { .. }
            | TrOp::GetAttrC(_)
            | TrOp::ClassOperand(_)
            | TrOp::BindAttrC(_)
            | TrOp::ElemsO
            | TrOp::ShiftIO
            | TrOp::PushIO => op.clone(),
        };
        Some(vec![op])
    }
}

/// The native slot an op names directly (not through a reference).
// Cost: O(1).
pub(super) fn native_slot(op: &TrOp) -> Option<u16> {
    match op {
        TrOp::LoadI(s)
        | TrOp::StoreI(s)
        | TrOp::IncI(s)
        | TrOp::DecI(s)
        | TrOp::IncIVoid(s)
        | TrOp::DecIVoid(s)
        | TrOp::JumpCmpLC { slot: s, .. }
        | TrOp::ShiftIStoreLocal { slot: s, .. }
        | TrOp::PushISlotLocalVoid { slot: s, .. }
        | TrOp::OrdAtLocalSlot { pos: s, .. }
        | TrOp::AddLocalIC { slot: s, .. }
        | TrOp::AddLocalICVoid { slot: s, .. } => Some(*s),
        _ => None,
    }
}

/// Where one inlined region sits in its caller: the callee op index → caller
/// op index map (`pos`, one past the end is the join), the region's index,
/// its join, the Nil constant a bare `return` produces, and whether the
/// callee's calls must re-seed its free variables.
#[derive(Clone, Copy)]
pub(super) struct RegionShape<'a> {
    pub(super) pos: &'a [u32],
    pub(super) region: u32,
    pub(super) join: u32,
    pub(super) nil_const: u32,
    pub(super) reseed: bool,
}
