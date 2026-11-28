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

    /// Record `action` for the armed site, if it is `code`'s and its op is a
    /// call of `method` with `arity` arguments on a receiver of `class`.
    // Cost: O(a) to clone the argument keys, a = arguments; once per site and
    // generation.
    pub(super) fn install_method_site_ic(
        &mut self,
        code: &CompiledCode,
        class: Symbol,
        method: Symbol,
        arity: usize,
        action: MethodSiteIcAction,
    ) {
        let Some(key) = self.caches.method_site_ic_site.take() else {
            return;
        };
        if key.0 != code as *const CompiledCode as usize || site_name_excluded(method) {
            return;
        }
        let Some((site_method, site_arity, _)) = site_shape(code, key.1 as usize) else {
            return;
        };
        if site_method != method || site_arity != arity {
            return;
        }
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
        self.caches.method_site_ic_site = Some(key);
        if self.caches.method_site_ic.is_empty() {
            return None;
        }
        // Only an instance receiver can hit: refuse every other call before
        // hashing the site.
        let arity = match code.ops[ip] {
            OpCode::CallMethodMut { arity, .. } | OpCode::CallMethod { arity, .. } => {
                arity as usize
            }
            _ => return None,
        };
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
            MethodSiteIcAction::Plan { arg_keys, plan } => {
                if !mutating
                    || self.registry_write_generation() != write_generation
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
        let OpCode::CallMethodMut {
            arity,
            arg_sources_idx,
            target_name_idx,
            ..
        } = code.ops[ip]
        else {
            return None;
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
        let target_name = Self::const_str(code, target_name_idx);
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
