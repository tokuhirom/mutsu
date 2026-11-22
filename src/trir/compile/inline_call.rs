//! ADR-8673 D2: a small statically linked callee is spliced into its caller's
//! op stream instead of being called.
//!
//! A `CallTr` costs ~1,000 instructions over its callee's body (the frame,
//! the argument copies, the outer seeding and the routine boundary), and a
//! JSON::Fast record makes ~85 of them, most to bodies of a dozen ops. An
//! inlined callee pays for none of that. What it keeps is what makes it the
//! same routine to the program:
//!
//! - its guards: [`TrOp::InlineEnter`] bails, exactly as the `CallTr` would,
//!   when the callee has been wrapped or replaced since it was linked;
//! - its identity on the routine stack: `InlineEnter` records the routine
//!   entry the call would have deferred (ADR-8673 D1), so a backtrace or
//!   `callframe` taken inside it shows the callee, and a return pops it;
//! - its package and its free variables, seeded and re-seeded as its frame
//!   did;
//! - its routine boundary: a `fail` raised inside it becomes the callee's
//!   `Failure` value (`exec.rs`), and every return leaves the result on the
//!   boxed bank as the call did.
//!
//! The callee's slots get fresh slots of the caller's frame. A native `is rw`
//! parameter, which held a reference to the caller's variable, becomes that
//! variable itself, so `GetRefI(p)` is a plain `LoadI(s)`. Every other
//! parameter is a copy, marked read-only so a call that passes it on hands
//! over the value as the callee did.
//!
//! Only callees whose inlining is plainly the same program are taken: at
//! most [`INLINE_BUDGET`] ops, no inlined callees of their own (so regions
//! never nest), no declared return type to check, no `use fatal` on either
//! side, and no call that passes a free variable by name.
//! `MUTSU_TRIR_INLINE=0` turns inlining off.

use std::collections::HashMap;

use super::TrirCompiler;
use super::inline_remap::{RegionShape, native_slot};
use crate::trir::{TrArg, TrCallee, TrInlineRegion, TrInnerCall, TrLink, TrOp};
use crate::value::Value;

/// The largest callee inlined, in ops (ADR-8673 D2).
const INLINE_BUDGET: usize = 24;

/// Whether `MUTSU_TRIR_INLINE=0` has turned inlining off.
// Cost: O(1) after the first call.
pub(crate) fn inlining_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("MUTSU_TRIR_INLINE").map_or(true, |v| v != "0"))
}

/// Where a callee's `is rw` native parameter points in the caller.
#[derive(Clone, Copy)]
pub(super) enum RefTarget {
    /// The caller's own native slot.
    Direct(u16),
    /// A reference the caller itself holds (its own `is rw` parameter).
    Ref(u16),
}

/// The slot renaming of one inlined callee.
pub(super) struct SlotMap {
    native: HashMap<u16, u16>,
    obj: HashMap<u16, u16>,
    pub(super) refs: HashMap<u16, RefTarget>,
    pub(super) consts: u32,
    pub(super) calls: u32,
    pub(super) methods: u32,
}

impl SlotMap {
    // Cost: O(1).
    pub(super) fn n(&self, s: u16) -> Option<u16> {
        self.native.get(&s).copied()
    }

    // Cost: O(1).
    pub(super) fn o(&self, s: u16) -> Option<u16> {
        self.obj.get(&s).copied()
    }

    /// A call argument of the callee, in the caller's frame.
    // Cost: O(1).
    pub(super) fn arg(&self, a: &TrArg) -> Option<TrArg> {
        Some(match *a {
            TrArg::Value(k) => TrArg::Value(k),
            TrArg::Native(s, k) => TrArg::Native(self.n(s)?, k),
            TrArg::Obj(s) => TrArg::Obj(self.o(s)?),
            TrArg::Ref(s, k) => match self.refs.get(&s)? {
                RefTarget::Direct(t) => TrArg::Native(*t, k),
                RefTarget::Ref(t) => TrArg::Ref(*t, k),
            },
            TrArg::Outer(_) => return None,
        })
    }
}

impl TrirCompiler<'_> {
    /// Splice `link`'s callee in place of a `CallTr` with argument plan
    /// `plan`, leaving its result on the boxed bank as the call would.
    /// `None` when this callee is not one to inline; nothing has been emitted
    /// then, and the caller compiles the call.
    // Cost: O(c), c = ops, constants and calls of the callee.
    pub(super) fn try_inline_call(
        &mut self,
        link: &TrLink,
        name: crate::symbol::Symbol,
        plan: &[TrArg],
    ) -> Option<()> {
        let callee = &*link.chunk;
        if !inlining_enabled()
            || callee.ops.len() > INLINE_BUDGET
            || !callee.inline_regions.is_empty()
            || callee.return_check.is_some()
            || callee.captured_fatal_mode
            || self.captured_fatal_mode
            || callee.params.len() != plan.len()
            || u16::try_from(self.outers.len() + callee.outers.len()).is_err()
        {
            return None;
        }
        if callee
            .calls
            .iter()
            .any(|c| c.args.iter().any(|a| matches!(a, TrArg::Outer(_))))
        {
            return None;
        }
        // Parameters first, without emitting anything: a shape that is not
        // taken must leave the op stream as it was.
        let mut refs = HashMap::new();
        for (p, arg) in callee.params.iter().zip(plan) {
            match (arg, p.is_rw) {
                (TrArg::Native(s, _), true) if p.kind.is_native() => {
                    refs.insert(p.slot, RefTarget::Direct(*s));
                }
                (TrArg::Ref(s, _), true) if p.kind.is_native() => {
                    refs.insert(p.slot, RefTarget::Ref(*s));
                }
                (TrArg::Native(..) | TrArg::Ref(..) | TrArg::Obj(_) | TrArg::Value(_), _)
                    if !p.is_rw || !p.kind.is_native() => {}
                _ => return None,
            }
        }
        // A reference slot is only ever read and written through the
        // reference ops; anything else would need the index it holds.
        if callee
            .ops
            .iter()
            .any(|op| native_slot(op).is_some_and(|s| refs.contains_key(&s)))
        {
            return None;
        }

        let mut map = SlotMap {
            native: HashMap::new(),
            obj: HashMap::new(),
            refs,
            consts: self.constants.len() as u32,
            calls: self.calls.len() as u32,
            methods: self.methods.len() as u32,
        };
        // Fresh caller slots for every callee slot that is not a reference.
        for s in 0..callee.n_native {
            if !map.refs.contains_key(&s) {
                let q = self.fresh_native_slot();
                map.native.insert(s, q);
            }
        }
        for s in 0..callee.n_obj {
            let q = self.fresh_obj_slot();
            map.obj.insert(s, q);
        }
        // The evaluated arguments are on the banks in argument order: store
        // them last first. Nothing is committed until the whole body is
        // proved, so these are collected first.
        let mut setup = Vec::new();
        let mut readonly = Vec::new();
        for (p, arg) in callee.params.iter().zip(plan).rev() {
            if let TrArg::Value(k) = arg {
                setup.push(if k.is_native() {
                    TrOp::StoreI(map.n(p.slot)?)
                } else {
                    TrOp::StoreObj(map.o(p.slot)?)
                });
            }
        }
        // Then the copies of by-variable arguments.
        for (p, arg) in callee.params.iter().zip(plan) {
            match *arg {
                TrArg::Native(s, _) if !p.is_rw => {
                    setup.push(TrOp::LoadI(s));
                    setup.push(TrOp::StoreI(map.n(p.slot)?));
                }
                TrArg::Ref(s, _) if !p.is_rw => {
                    setup.push(TrOp::GetRefI(s));
                    setup.push(TrOp::StoreI(map.n(p.slot)?));
                }
                TrArg::Obj(s) => {
                    setup.push(TrOp::LoadObj(s));
                    setup.push(TrOp::StoreObj(map.o(p.slot)?));
                }
                _ => {}
            }
            if !p.is_rw {
                let slot = if p.kind.is_native() {
                    map.n(p.slot)?
                } else {
                    map.o(p.slot)?
                };
                readonly.push((slot, p.kind));
            }
        }

        // The body. `start` is where `InlineEnter` goes; each callee op's
        // position in the caller is computed first, since a return and a
        // call grow by one op.
        let region = self.inline_regions.len() as u32;
        let reseed = !callee.outers.is_empty();
        let start = self.ops.len() + setup.len();
        let mut pos = Vec::with_capacity(callee.ops.len() + 1);
        let mut at = start + 1;
        for op in &callee.ops {
            pos.push(at as u32);
            at += match op {
                TrOp::ReturnI | TrOp::ReturnN | TrOp::ReturnNil => 2,
                TrOp::CallTr(_) | TrOp::CallGen(_) | TrOp::MethodGen(_) if reseed => 2,
                _ => 1,
            };
        }
        pos.push(at as u32);
        let join = at as u32;
        let nil_const = self.constants.len() as u32 + callee.constants.len() as u32;
        let mut body = Vec::with_capacity(at - start);
        let shape = RegionShape {
            pos: &pos,
            region,
            join,
            nil_const,
            reseed,
        };
        body.push(TrOp::InlineEnter { region, join });
        for op in &callee.ops {
            body.extend(self.inline_op(op, &map, &shape)?);
        }
        // Everything is proved: commit the tables the body indexes.
        let mut calls = Vec::with_capacity(callee.calls.len());
        for c in &callee.calls {
            let args = c
                .args
                .iter()
                .map(|a| map.arg(a))
                .collect::<Option<Vec<_>>>()?;
            calls.push(TrInnerCall {
                callee: match &c.callee {
                    TrCallee::Trir(l) => TrCallee::Trir(l.clone()),
                    TrCallee::Generic => TrCallee::Generic,
                },
                name: c.name,
                args,
                native_args: c.native_args,
                result: c.result,
            });
        }
        self.calls.extend(calls);
        self.methods.extend(callee.methods.iter().cloned());
        self.constants.extend(callee.constants.iter().cloned());
        self.constants.push(Value::NIL);
        self.inline_regions.push(TrInlineRegion {
            link: link.clone(),
            name,
        });
        self.readonly_slots.extend(readonly);
        self.ops.extend(setup);
        for (k, op) in body.iter().enumerate() {
            if matches!(
                op,
                TrOp::LoadOuter(_) | TrOp::OrdAtOuter(_) | TrOp::AtPosIOuter(_)
            ) {
                self.inline_outer_fixups.push(start + k);
            }
        }
        self.ops.extend(body);
        Some(())
    }

    /// A native slot with no name, for an inlined callee.
    // Cost: O(1).
    pub(super) fn fresh_native_slot(&mut self) -> u16 {
        let s = self.n_native;
        self.n_native += 1;
        s
    }

    /// A boxed slot with no name, for an inlined callee.
    // Cost: O(1).
    pub(super) fn fresh_obj_slot(&mut self) -> u16 {
        let s = self.n_obj;
        self.n_obj += 1;
        self.obj_written.push(true);
        s
    }

    /// Shift the free-variable index of every op that reads an inlined
    /// callee's free variable past this body's own: the region seeds them
    /// right after these (`InlineEnter`).
    // Cost: O(f), f = fixups.
    pub(super) fn apply_inline_outer_fixups(&mut self) {
        let base = self.outers.len() as u16;
        for &i in &self.inline_outer_fixups {
            match self.ops.get_mut(i) {
                Some(TrOp::LoadOuter(n) | TrOp::OrdAtOuter(n) | TrOp::AtPosIOuter(n)) => *n += base,
                _ => {}
            }
        }
        self.inline_outer_fixups.clear();
    }
}
