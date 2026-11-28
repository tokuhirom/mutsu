//! Method kernels (ADR-19315 §2 item 2): a planned slot-frame method whose
//! body is closed arithmetic over its parameters, its receiver's scalar
//! attributes and its parameters' generated accessors runs as a straight-line
//! evaluation over machine numbers, without a frame.
//!
//! `method distance-to(Point $other) { (($!x - $other.x) ** 2 + ...).sqrt }`
//! is the shape: attribute reads, accessor reads on a parameter, `+ - *`,
//! `**` by a small constant, `.sqrt`. Through the slot frame each call costs
//! ~10k instructions of frame, binding and dispatch around a few dozen of
//! arithmetic.
//!
//! # Why a kernel run is the call
//!
//! The kernel is decided once per method-call plan ([`MethodKernel::compile`])
//! from the slot frame's own copy of the body, by a closed op list: an op not
//! listed declines the method, so a new opcode can only make kernels rarer.
//! Every listed op is pure. It reads a parameter, a scalar attribute of the
//! receiver through the body's attribute-site cache, or a generated accessor
//! through the accessor lane (`vm_accessor_lane`), or it computes. So a kernel
//! that cannot finish -- a value that is not a plain `Int`/`Num`, an overflow,
//! a cold attribute site, an accessor the lane does not hold -- *bails*, and
//! the caller runs the call on the slot frame exactly as before, from the same
//! unchanged state (ADR-19288 D4: rerun from the start, no deoptimization).
//!
//! What the frame would have observed is pinned the same way the slot frame
//! pins it. The plan's registry generations pin the resolution, and the body
//! is a slot-frame body (no name reads, no closures, no declarations). On
//! every entry the kernel requires:
//! - no user infix operator declared anywhere;
//! - no wrapped routine;
//! - the kernels' global switch on (`kernels_enabled`: JIT on, no profiler).
//!
//! Each of those is what makes a built-in operator's answer the one the frame
//! would get.
//!
//! The arithmetic is the operators' own on these operands: `Int` `+ - *` with
//! overflow (which the generic path would widen to a BigInt) bailing, `Num`
//! IEEE, a mixed pair as `Num`, `Int ** k` for a constant `0 <= k <= 30` (the
//! generic fast path's range), and `.sqrt` through the shared
//! `arith::sqrt_numeric`.

use super::*;
use crate::vm::vm_method_frame_plan::LocalInit;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering::Relaxed};

/// Consecutive bails after which a kernel stops being tried.
const MAX_BAIL_STREAK: u32 = 8;
/// The largest constant exponent a kernel raises an `Int` to.
const MAX_INT_POW: i64 = 30;
/// The longest body a kernel takes.
const MAX_KERNEL_OPS: usize = 64;
/// The deepest value stack a kernel may need.
const KERNEL_STACK: usize = 16;

/// One kernel op over the value stack.
#[derive(Debug, Clone, Copy)]
enum KOp {
    Int(i64),
    Num(f64),
    /// The positional argument at this index (the invocant not counted).
    Param(u16),
    /// The receiver's scalar attribute behind local slot `idx` of the body.
    Attr(u32),
    /// The generated accessor `name` of the positional argument at this
    /// index (`$other.x`); the last field indexes the kernel's
    /// accessor-slot memo.
    ParamAccessor(u16, Symbol, u16),
    /// The generated accessor `name` of the receiver (`$.x`, `self.x`).
    SelfAccessor(Symbol, u16),
    Add,
    Sub,
    Mul,
    /// `** k`, `k` a constant pushed just before (folded in here).
    PowConst(i64),
    Sqrt,
}

/// A machine number on the kernel's stack.
#[derive(Debug, Clone, Copy)]
enum N {
    I(i64),
    F(f64),
}

/// A compiled method kernel and its bail bookkeeping.
#[derive(Debug)]
pub(crate) struct MethodKernel {
    ops: Box<[KOp]>,
    /// Per accessor op, the accessor lane's `(layout id << 32) | entry` it
    /// last read through, or 0. The lane is cleared with the registry method
    /// generation, which a registry write moves; a write also retires the
    /// plan that owns this kernel, so a memo never outlives its lane entry.
    accessor_slots: Box<[std::sync::atomic::AtomicU64]>,
    bail_streak: AtomicU32,
    disabled: AtomicBool,
}

impl MethodKernel {
    /// The kernel for a slot-frame method whose body is `body` (the slot
    /// frame's copy, `slot_frame_body_code`) and whose frame plan is that of
    /// `def` compiled as `cc`, or `None` when an op is outside the list.
    // Cost: O(n), n = ops, once per plan.
    pub(crate) fn compile(
        def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        body: &CompiledCode,
        owner: Symbol,
    ) -> Option<Self> {
        if body.ops.is_empty() || body.ops.len() > MAX_KERNEL_OPS {
            return None;
        }
        let plan = def.frame_plan(owner, cc);
        let inv_offset = usize::from(def.param_defs.iter().any(|pd| pd.is_invocant));
        let mut ops: Vec<KOp> = Vec::with_capacity(body.ops.len());
        let mut depth: usize = 0;
        let mut max_depth: usize = 0;
        let mut accessors: u16 = 0;
        let n = body.ops.len();
        let mut i = 0usize;
        while i < n {
            let op = &body.ops[i];
            // A 0-arg call right after reading a parameter or the invocant is
            // that value's generated accessor (checked per entry through the
            // accessor lane); `.sqrt` is the numeric one.
            let accessor_next = match body.ops.get(i + 1) {
                Some(OpCode::CallMethod {
                    name_idx,
                    arity: 0,
                    modifier_idx: None,
                    quoted: false,
                    arg_sources_idx: None,
                }) if body.const_sym(*name_idx) != crate::sym!("sqrt") => {
                    Some(body.const_sym(*name_idx))
                }
                _ => None,
            };
            let k = match op {
                OpCode::LoadConst(idx) => match body.constants.get(*idx as usize)?.view() {
                    ValueView::Int(v) => KOp::Int(v),
                    ValueView::Num(f) => KOp::Num(f),
                    _ => return None,
                },
                OpCode::GetLocal(idx) => match (plan.locals.get(*idx as usize)?, accessor_next) {
                    (LocalInit::Param(p), next) => {
                        let arg = u16::try_from((*p as usize).checked_sub(inv_offset)?).ok()?;
                        match next {
                            Some(name) => {
                                i += 1;
                                accessors += 1;
                                KOp::ParamAccessor(arg, name, accessors - 1)
                            }
                            None => KOp::Param(arg),
                        }
                    }
                    (LocalInit::Invocant, Some(name)) => {
                        i += 1;
                        accessors += 1;
                        KOp::SelfAccessor(name, accessors - 1)
                    }
                    _ if super::vm_method_slot_frame::slot_attr_read(cc, *idx as usize) => {
                        KOp::Attr(*idx)
                    }
                    _ => return None,
                },
                OpCode::CallMethod {
                    name_idx,
                    arity: 0,
                    modifier_idx: None,
                    quoted: false,
                    arg_sources_idx: None,
                } if body.const_sym(*name_idx) == crate::sym!("sqrt") => KOp::Sqrt,
                OpCode::Add => KOp::Add,
                OpCode::Sub => KOp::Sub,
                OpCode::Mul => KOp::Mul,
                OpCode::Pow => match ops.pop()? {
                    KOp::Int(k) if (0..=MAX_INT_POW).contains(&k) => {
                        depth -= 1;
                        KOp::PowConst(k)
                    }
                    _ => return None,
                },
                // Unwrapping or item-wrapping a number is the number.
                OpCode::DerefContainer | OpCode::Itemize => {
                    i += 1;
                    continue;
                }
                // Only as the body's last op: its value is the result.
                OpCode::Return if i + 1 == n => {
                    i += 1;
                    continue;
                }
                _ => return None,
            };
            depth = match k {
                KOp::Int(_)
                | KOp::Num(_)
                | KOp::Param(_)
                | KOp::Attr(_)
                | KOp::ParamAccessor(..)
                | KOp::SelfAccessor(..) => depth + 1,
                KOp::Add | KOp::Sub | KOp::Mul => depth.checked_sub(1)?,
                KOp::PowConst(_) | KOp::Sqrt => {
                    if depth == 0 {
                        return None;
                    }
                    depth
                }
            };
            if depth == 0 {
                return None;
            }
            max_depth = max_depth.max(depth);
            ops.push(k);
            i += 1;
        }
        // A kernel computes one number from what it reads; a body that only
        // forwards a parameter or an attribute is not worth one.
        let computes = ops.iter().any(|k| {
            matches!(
                k,
                KOp::Add | KOp::Sub | KOp::Mul | KOp::PowConst(_) | KOp::Sqrt
            )
        });
        (depth == 1 && computes && max_depth <= KERNEL_STACK).then(|| MethodKernel {
            ops: ops.into_boxed_slice(),
            accessor_slots: (0..accessors)
                .map(|_| std::sync::atomic::AtomicU64::new(0))
                .collect(),
            bail_streak: AtomicU32::new(0),
            disabled: AtomicBool::new(false),
        })
    }
}

impl Interpreter {
    /// Run `kernel` for a planned call on `receiver` with `args`, whose slot
    /// body is `body`. `Some(value)`: the call's result. `None`: the kernel
    /// bailed or may not run, nothing was changed, and the caller runs the
    /// call on the slot frame.
    // Cost: O(n), n = kernel ops, each O(1) (an accessor read is one lane
    // probe).
    pub(super) fn try_method_kernel(
        &mut self,
        kernel: &MethodKernel,
        body: &CompiledCode,
        receiver: &Value,
        args: &[Value],
    ) -> Option<Value> {
        if kernel.disabled.load(Relaxed)
            || !kernels_on()
            || super::vm_jit::USER_INFIX_DECLS.load(Relaxed) != 0
            || self.any_routine_wrapped()
        {
            return None;
        }
        match self.run_method_kernel(kernel, body, receiver, args) {
            Some(value) => {
                kernel.bail_streak.store(0, Relaxed);
                Some(value)
            }
            None => {
                if kernel.bail_streak.fetch_add(1, Relaxed) + 1 >= MAX_BAIL_STREAK {
                    kernel.disabled.store(true, Relaxed);
                }
                None
            }
        }
    }

    fn run_method_kernel(
        &mut self,
        kernel: &MethodKernel,
        body: &CompiledCode,
        receiver: &Value,
        args: &[Value],
    ) -> Option<Value> {
        let mut stack: [N; KERNEL_STACK] = [N::I(0); KERNEL_STACK];
        let mut sp = 0usize;
        for op in kernel.ops.iter() {
            match *op {
                KOp::Int(v) => {
                    stack[sp] = N::I(v);
                    sp += 1;
                }
                KOp::Num(f) => {
                    stack[sp] = N::F(f);
                    sp += 1;
                }
                KOp::Param(i) => {
                    stack[sp] = number_of(args.get(i as usize)?)?;
                    sp += 1;
                }
                KOp::Attr(idx) => {
                    stack[sp] = self.kernel_attr_read(body, idx as usize, receiver)?;
                    sp += 1;
                }
                KOp::ParamAccessor(i, name, memo) => {
                    stack[sp] =
                        self.kernel_accessor_read(kernel, memo, args.get(i as usize)?, name)?;
                    sp += 1;
                }
                KOp::SelfAccessor(name, memo) => {
                    stack[sp] = self.kernel_accessor_read(kernel, memo, receiver, name)?;
                    sp += 1;
                }
                KOp::Add | KOp::Sub | KOp::Mul => {
                    sp -= 1;
                    let (a, b) = (stack[sp - 1], stack[sp]);
                    stack[sp - 1] = arith(*op, a, b)?;
                }
                KOp::PowConst(k) => {
                    stack[sp - 1] = match stack[sp - 1] {
                        N::I(v) => N::I(v.checked_pow(k as u32)?),
                        N::F(_) => return None,
                    };
                }
                KOp::Sqrt => {
                    stack[sp - 1] = sqrt(stack[sp - 1])?;
                }
            }
        }
        Some(to_value(stack[0]))
    }

    /// The receiver's scalar attribute behind local slot `idx` of `body` as a
    /// number, from the body's attribute-site cache (filled by the slot
    /// frame's own reads); `None` when the site is cold, holds another layout
    /// or the attribute is not a plain number.
    // Cost: O(1).
    fn kernel_attr_read(&self, body: &CompiledCode, idx: usize, receiver: &Value) -> Option<N> {
        let (layout_id, slot) = body.attr_sites.cached(body.attr_site_words(), idx)?;
        let ValueView::Instance { attributes, .. } = receiver.view() else {
            return None;
        };
        let map = attributes.as_map();
        if map.layout()?.id() != layout_id || map.has_undeclared() {
            return None;
        }
        // Read in place: `number_of` looks through the one container a
        // scalar attribute holds, as `deref_container` would.
        number_of(map.slot_vivify(slot)?)
    }

    /// `target.name` as a number, when that is the generated accessor's plain
    /// read the accessor lane holds for `target`'s layout; `None` otherwise.
    /// The lane entry is memoized in the kernel's accessor-slot `memo`.
    // Cost: O(1); a memo miss costs one lane probe.
    fn kernel_accessor_read(
        &mut self,
        kernel: &MethodKernel,
        memo: u16,
        target: &Value,
        name: Symbol,
    ) -> Option<N> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        let cell = kernel.accessor_slots.get(memo as usize)?;
        let map = attributes.as_map();
        let layout_id = map.layout()?.id();
        let word = cell.load(Relaxed);
        if word != 0 && (word >> 32) as u32 == layout_id {
            if map.has_undeclared() {
                return None;
            }
            let slot = super::vm_accessor_lane::accessor_lane_slot_index(word as u32);
            return number_of(map.slot_vivify(slot)?);
        }
        drop(map);
        if self.caches.accessor_lane.is_empty() {
            return None;
        }
        self.refresh_method_caches_for_generation();
        let entry = *self.caches.accessor_lane.get(&(layout_id, name))?;
        cell.store((u64::from(layout_id) << 32) | u64::from(entry), Relaxed);
        number_of(&self.accessor_lane_read(target, layout_id, entry)?)
    }
}

/// Whether kernels may run at all (`vm_kernel::kernels_enabled`).
// Cost: O(1).
#[inline]
fn kernels_on() -> bool {
    #[cfg(feature = "jit")]
    {
        super::vm_kernel::kernels_enabled()
    }
    #[cfg(not(feature = "jit"))]
    {
        false
    }
}

/// A plain `Int` (one that fits a machine word) or `Num`, through one
/// container.
// Cost: O(1).
fn number_of(v: &Value) -> Option<N> {
    match v.view() {
        ValueView::Int(n) => Some(N::I(n)),
        ValueView::Num(f) => Some(N::F(f)),
        ValueView::ContainerRef(cell) => match cell.load().view() {
            ValueView::Int(n) => Some(N::I(n)),
            ValueView::Num(f) => Some(N::F(f)),
            _ => None,
        },
        _ => None,
    }
}

/// `a op b` as the built-in operator answers it on plain numbers; `None`
/// when an `Int` result would overflow (the generic path widens it).
// Cost: O(1).
fn arith(op: KOp, a: N, b: N) -> Option<N> {
    Some(match (a, b) {
        (N::I(x), N::I(y)) => N::I(match op {
            KOp::Add => x.checked_add(y)?,
            KOp::Sub => x.checked_sub(y)?,
            KOp::Mul => x.checked_mul(y)?,
            _ => return None,
        }),
        (x, y) => {
            let (x, y) = (as_f64(x), as_f64(y));
            N::F(match op {
                KOp::Add => x + y,
                KOp::Sub => x - y,
                KOp::Mul => x * y,
                _ => return None,
            })
        }
    })
}

// Cost: O(1).
fn as_f64(n: N) -> f64 {
    match n {
        N::I(v) => v as f64,
        N::F(f) => f,
    }
}

/// `.sqrt` through the shared numeric routine.
// Cost: O(1).
fn sqrt(n: N) -> Option<N> {
    match crate::builtins::arith::sqrt_numeric(&to_value(n))?.view() {
        ValueView::Num(f) => Some(N::F(f)),
        _ => None,
    }
}

// Cost: O(1).
fn to_value(n: N) -> Value {
    match n {
        N::I(v) => Value::int(v),
        N::F(f) => Value::num(f),
    }
}
