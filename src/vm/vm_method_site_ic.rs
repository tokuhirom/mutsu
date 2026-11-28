//! The method-call site inline cache (#18267, O2 call plan).
//!
//! A planned method call (`vm_method_call_plan`) or a generated-accessor read
//! (`vm_accessor_lane`) on an ordinary instance still walked a dozen layers
//! before reaching its memo: the site lanes, the lazy-named and `.VAR`
//! checks, the instance front, the accessor-lane probe, the plain-lane key
//! (one type-key intern per argument) and two hash probes. On `method-call`
//! that was ~3k instructions of a ~16k call.
//!
//! A site that has been answered by one of those memos records the answer
//! here, keyed by the site (`(chunk address, ip)`). The next call at the site
//! checks the receiver's class and the registry generations and goes straight
//! to the memo's replay, in front of every layer.
//!
//! # Why an entry may answer for the site
//!
//! An entry is written only by the memo that just answered this call, and it
//! holds exactly what that memo is keyed on, so a hit replays what the memo
//! would have replayed:
//!
//! * **the call shape**: the op's method name and arity (re-read from the op
//!   at the hit), no modifier, not quoted, not `.VAR`/`throw`, and a receiver
//!   that is not an attribute aggregate slot;
//! * **the receiver's class** and, for a plan, **the arguments' type keys**
//!   (recomputed per hit and compared, as the plain-method lane does);
//! * **the registry**: the method generation (which clears the accessor and
//!   plain-method lanes) and the registry write generation (which clears the
//!   plan table) at install time; a hit requires both unchanged;
//! * **the process-wide gates** the skipped layers test on every call (an
//!   accessor-ref request, a user `^find_method`, a wrap chain, `use fatal`)
//!   are re-tested on every hit.
//!
//! A chunk's address can be reused by another chunk after it is freed. That
//! cannot make an entry wrong: the entry is a function of the op shape, the
//! class, the argument keys and the generations, all re-checked against the
//! op actually at `ip`, never of which chunk installed it.
//!
//! The table lives in the per-interpreter `caches` subsystem, so it needs no
//! lock.

use super::*;

/// What a site answered the last time.
#[derive(Debug, Clone)]
pub(crate) enum MethodSiteIcAction {
    /// The generated accessor's plain read (`try_accessor_lane`).
    Accessor,
    /// A planned user-method call (`run_method_call_plan`) for these argument
    /// type keys.
    Plan {
        arg_keys: Vec<Symbol>,
        plan: std::sync::Arc<super::vm_method_call_plan::MethodCallPlan>,
    },
    /// A `CallMethod` site whose call was found unplannable: the full path
    /// runs, without deciding the plan again.
    Unplanned,
}

/// One site's recorded answer.
#[derive(Debug, Clone)]
pub(crate) struct MethodSiteIcEntry {
    class: Symbol,
    method: Symbol,
    method_generation: u64,
    write_generation: u64,
    action: MethodSiteIcAction,
}

/// The site the current method dispatch may record its answer for, with the
/// call shape its op had when it was armed.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MethodSiteIcArm {
    key: (usize, u32),
    method: Symbol,
    arity: usize,
    /// A `CallMethod` (not a `CallMethodMut`) site.
    call_method: bool,
}

/// The key of the site at `code.ops[ip]`.
// Cost: O(1).
#[inline]
fn site_key(code: &CompiledCode, ip: usize) -> (usize, u32) {
    (code as *const CompiledCode as usize, ip as u32)
}

/// The call shape a site must have for an entry to answer it: the method
/// name, the arity, and whether it is a `CallMethodMut`.
// Cost: O(1).
fn site_shape(code: &CompiledCode, ip: usize) -> Option<(Symbol, usize, bool)> {
    match code.ops.get(ip)? {
        OpCode::CallMethodMut {
            name_idx,
            arity,
            target_slot,
            modifier_idx: None,
            quoted: false,
            ..
        } => {
            // An attribute receiver slot keeps its prologue/epilogue.
            if target_slot.is_some_and(|slot| code.local_attr_key(slot as usize).is_some()) {
                return None;
            }
            Some((code.const_sym(*name_idx), *arity as usize, true))
        }
        OpCode::CallMethod {
            name_idx,
            arity,
            modifier_idx: None,
            quoted: false,
            ..
        } => Some((code.const_sym(*name_idx), *arity as usize, false)),
        _ => None,
    }
}

/// A method name whose site never takes the cache: `.VAR` and the resumable
/// `throw` have site-level handling of their own.
// Cost: O(1).
fn site_name_excluded(method: Symbol) -> bool {
    crate::sym_in!(method, "VAR" | "throw" | "rethrow" | "new")
}

impl Interpreter {
    /// The process-wide gates every skipped layer tests per call.
    // Cost: O(1).
    #[inline]
    fn method_site_ic_gates_clear(&self) -> bool {
        !self.accessor_ref_pending
            && !self.module.fatal_mode
            && !crate::runtime::find_method_intercept::any_user_find_method()
    }

    /// Record `action` for the armed site, if its op is a call of `method`
    /// with `arity` arguments (the receiver's class is `class`).
    ///
    /// An entry is a function of the op's call shape, the class, the argument
    /// keys and the registry generations only, so it is right for whichever
    /// site of that shape the arm names.
    // Cost: O(a) to clone the argument keys, a = arguments; once per site and
    // generation.
    pub(super) fn install_method_site_ic(
        &mut self,
        class: Symbol,
        method: Symbol,
        arity: usize,
        action: MethodSiteIcAction,
    ) {
        let Some(arm) = self.caches.method_site_ic_site.take() else {
            return;
        };
        if arm.method != method || arm.arity != arity || site_name_excluded(method) {
            return;
        }
        let key = arm.key;
        if let MethodSiteIcAction::Plan { plan, .. } = &action
            && !plan.lean
        {
            // A plan off the lean frame may run a body that rebinds the
            // receiver's variable through `CALLER::`; the site's writeback
            // check stays on the full path for it.
            return;
        }
        let method_generation = self.registry().method_generation;
        let write_generation = self.registry_write_generation();
        if self.caches.method_site_ic_generation != method_generation {
            self.caches.method_site_ic_generation = method_generation;
            self.caches.method_site_ic.clear();
        }
        self.caches.method_site_ic.insert(
            key,
            MethodSiteIcEntry {
                class,
                method,
                method_generation,
                write_generation,
                action,
            },
        );
    }

    /// Answer the call at `code.ops[ip]` from its site's entry, before every
    /// other layer. `None`: no entry applies, nothing was changed, and the
    /// caller runs its full path (having armed the site to record).
    // Cost: O(1) to refuse; a hit costs one hash probe, two generation reads
    // and, for a call with arguments, O(a) type keys, a = arguments.
    pub(super) fn try_method_site_ic(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Option<Result<(), RuntimeError>> {
        let key = site_key(code, ip);
        let Some((method, arity, mutating)) = site_shape(code, ip) else {
            self.caches.method_site_ic_site = None;
            return None;
        };
        self.caches.method_site_ic_site = Some(MethodSiteIcArm {
            key,
            method,
            arity,
            call_method: !mutating,
        });
        if self.caches.method_site_ic.is_empty() {
            return None;
        }
        // Only an instance receiver can hit: refuse every other call before
        // hashing the site.
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        if !self.stack[target_idx].is_instance_value() {
            return None;
        }
        let hit = self.method_site_ic_hit(code, ip, key);
        if hit.is_some() {
            self.caches.method_site_ic_site = None;
        }
        hit
    }

    /// The `CallMethod` dispatch tail (`dispatch_compiled_method`) reached a
    /// compiled user method on an ordinary instance from an armed
    /// `CallMethod` site: every probe of that op's preamble declined it.
    /// Decide the call's plan here, as the `CallMethodMut` tail decides its
    /// own (`install_method_call_plan`), and record it for the site.
    // Cost: O(1) when no `CallMethod` site is armed; otherwise once per site
    // and generation, the plan decision, O(p * a), p = parameters, a =
    // arguments.
    pub(super) fn note_call_method_site_plan(
        &mut self,
        owner: Symbol,
        method: Symbol,
        def: &std::sync::Arc<crate::runtime::MethodDef>,
        target: &Value,
        args: &[Value],
    ) {
        if !self
            .caches
            .method_site_ic_site
            .is_some_and(|arm| arm.call_method && arm.method == method && arm.arity == args.len())
        {
            return;
        }
        let ValueView::Instance { class_name, .. } = target.view() else {
            return;
        };
        if !self.plain_method_lane_class_eligible(class_name)
            || !self.method_site_ic_gates_clear()
            || self.registry().method_name_ever_wrapped(method.as_str())
        {
            self.caches.method_site_ic_site = None;
            return;
        }
        let mut arg_keys = Vec::with_capacity(args.len());
        if !args.is_empty() && !self.plain_lane_arg_keys_into(args, &mut arg_keys) {
            self.caches.method_site_ic_site = None;
            return;
        }
        let action = match self.method_call_plan_for(method, owner, def, target, args) {
            Some(plan) if plan.lean => MethodSiteIcAction::Plan { arg_keys, plan },
            _ => MethodSiteIcAction::Unplanned,
        };
        self.install_method_site_ic(class_name, method, args.len(), action);
    }

    fn method_site_ic_hit(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        key: (usize, u32),
    ) -> Option<Result<(), RuntimeError>> {
        let entry = self.caches.method_site_ic.get(&key)?;
        let (method, arity, mutating) = site_shape(code, ip)?;
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        let ValueView::Instance { class_name, .. } = self.stack[target_idx].view() else {
            return None;
        };
        if entry.class != class_name || entry.method != method {
            return None;
        }
        let action = entry.action.clone();
        let (method_generation, write_generation) =
            (entry.method_generation, entry.write_generation);
        if !self.method_site_ic_gates_clear()
            || self.registry().method_generation != method_generation
        {
            return None;
        }
        match action {
            MethodSiteIcAction::Accessor => {
                if arity != 0 || self.has_any_wrap_chains() {
                    return None;
                }
                self.sync_source_line(code, ip);
                let value = self.try_accessor_lane(method)?;
                self.stack.pop();
                self.stack.push(value);
                self.set_pending_call_arg_sources(None);
                if mutating {
                    self.pending_call_arg_source_slots.clear();
                }
                self.caches.ctor_lane_candidate = None;
                Some(Ok(()))
            }
            MethodSiteIcAction::Unplanned => {
                // Nothing to replay, and nothing to decide again.
                self.caches.method_site_ic_site = None;
                None
            }
            MethodSiteIcAction::Plan { arg_keys, plan } => {
                if self.registry_write_generation() != write_generation
                    || self.registry().method_name_ever_wrapped(method.as_str())
                {
                    return None;
                }
                self.run_method_site_ic_plan(code, ip, method, &arg_keys, &plan)
            }
        }
    }

    /// The plan replay of a site hit: the plain-method lane's per-call effects
    /// (`try_plain_method_early_lane`), then `run_method_call_plan`, inside
    /// the site's own frame of effects (`call_method_mut_site_around`).
    fn run_method_site_ic_plan(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        method: Symbol,
        arg_keys: &[Symbol],
        plan: &std::sync::Arc<super::vm_method_call_plan::MethodCallPlan>,
    ) -> Option<Result<(), RuntimeError>> {
        let (arity, arg_sources_idx, target_name) = match code.ops[ip] {
            OpCode::CallMethodMut {
                arity,
                arg_sources_idx,
                target_name_idx,
                ..
            } => (
                arity,
                arg_sources_idx,
                Self::const_str(code, target_name_idx),
            ),
            // A `CallMethod` receiver is a value, not a variable: no name to
            // arm a raw invocant from, as on its own dispatch tail.
            OpCode::CallMethod {
                arity,
                arg_sources_idx,
                ..
            } => (arity, arg_sources_idx, ""),
            _ => return None,
        };
        let arity = arity as usize;
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        let mut decoded = None;
        let mut args = Vec::new();
        if arity > 0 {
            if self.stack[target_idx + 1..].iter().any(|a| {
                matches!(a.view(), ValueView::VarRef { .. }) || a.needs_hllize_for_binding()
            }) {
                return None;
            }
            decoded = self.decode_arg_sources_shared(code, arg_sources_idx);
            if decoded.as_ref().is_some_and(|(_, spread)| *spread) {
                return None;
            }
            args = self.stack.split_off(target_idx + 1);
            let mut keys = std::mem::take(&mut self.caches.plain_method_lane_scratch);
            keys.clear();
            let same = self.plain_lane_arg_keys_into(&args, &mut keys) && keys == arg_keys;
            keys.clear();
            self.caches.plain_method_lane_scratch = keys;
            if !same {
                self.stack.extend(args);
                return None;
            }
        } else if !arg_keys.is_empty() {
            return None;
        }
        self.sync_source_line(code, ip);
        let Some(target) = self.stack.pop() else {
            self.stack.extend(args);
            return None;
        };
        self.caches.ctor_lane_candidate = None;
        self.caches.plain_method_lane_candidate = None;
        if arity == 0 {
            self.pending_call_arg_source_slots.clear();
            self.set_pending_call_arg_sources(None);
        } else {
            self.set_pending_call_arg_sources_shared(decoded.map(|(names, _)| names));
            if let Err(e) = self.reify_map_grep_seq_method_args(&target, method, &args) {
                return Some(Err(e));
            }
        }
        let result = self.in_method_call(|vm| {
            vm.run_method_call_plan(code, target_name, target, method, plan, args)
        });
        // The instance front's own exit bookkeeping.
        self.set_pending_call_arg_sources(None);
        self.pending_call_arg_source_slots.clear();
        if let Err(e) = result {
            self.record_call_resume_point(code, ip, &e);
            return Some(Err(e));
        }
        self.apply_pending_rw_writeback(code);
        self.drain_pending_local_updates_after_call(code);
        Some(Ok(()))
    }
}
