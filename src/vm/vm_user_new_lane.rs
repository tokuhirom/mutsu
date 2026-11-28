//! The user-`new` lane: a class's own `method new(*%_)` called on its type
//! object, replayed straight into its frame once the `CallMethodMut` chain
//! has been observed to reach it (#19399).
//!
//! `Dist.new(name => ..., ...)` on a class that declares
//! `method new(*%_) { self.bless(|%_) }` walks the argument stretch, the
//! probe chain, the native default constructor (which declines: the class
//! has a `new` that takes the call), the dispatch tail's accessor and wrap
//! probes, the method resolution and the fast-path gate, before the method's
//! frame is pushed. None of it depends on more than the class, the call shape
//! and the class's one `new`. The plain-method lane and its plan
//! (`vm_call_method_plain_lane`, `vm_method_call_plan`) skip the same walk for
//! an instance receiver; they do not cover a type object, whose probes are the
//! constructor's (`vm_ctor_lane`).
//!
//! # Why the memo cannot certify something that did not happen
//!
//! As for the ctor lane: every probe *returns* when it claims a call, so a
//! dispatch that reaches the user-method tail
//! (`compiled_mut_resolved_dispatch`) for a type object has been declined by
//! every probe in between, the native default constructor included. The
//! memo is written exactly there ([`Interpreter::install_user_new_lane`]), for
//! the class this dispatch's own gate nominated (`ctor_lane_candidate`, method
//! `new`, cleared by every `CallMethodMut` and site gate).
//!
//! # What the key has to hold constant
//!
//! The key is the class alone, as for the ctor lane, and the call shape the
//! ctor lane requires (method `new`, no modifier, not quoted, no container
//! request, every argument a string-keyed `Pair` whose value is not a
//! `Junction`) is required on both sides. The class alone suffices because
//! the lane admits only a `new` whose signature is the invocant plus a named
//! slurpy, and only when it is the class's one visible candidate:
//!
//! * the native default constructor's decline (`user_new_declines`) is then
//!   a function of the class: a single slurpy candidate takes every all-named
//!   call;
//! * the fast-path gate's verdict (`method_fast_gate`) reads, for such a
//!   signature, the arguments only through three per-call questions, which
//!   the replay asks on every call: a `Mu`-only value (the implicit `Any`
//!   check), an internal `__mutsu_*` key, and the explicit `*%h`'s argument
//!   sources (`method_named_slurpy_args_light`);
//! * the registry (methods, wraps, MRO) is pinned by the method generation,
//!   and the class shape by the identity of its `NativeCtorPlan`, as for the
//!   ctor lane.
//!
//! The replay is the dispatch tail's own user-method arm with the
//! resolution and the gate taken from the plan, as the method-call plan's
//! replay (`run_method_call_plan`) runs it for an instance: the raw-invocant
//! arrival, the method-dispatch frame, the named slurpy's promotion, the fast
//! binder, and the commit and Proxy fetch on return. The frame stays the
//! general fast binder's: the lean and slot frames are for instance
//! receivers.
//!
//! The early form runs at the top of `CallMethodMut` and skips the argument
//! stretch too, with the ctor lane's early form's argument
//! (`vm_ctor_early_lane`): a call whose arguments the stretch would change (a
//! `|` spread, a `VarRef`, a value `hllize_for_binding` rewrites, a gather or
//! consumed `Seq`) is refused, the argument sources are published, and a
//! deferred `.map`/`.grep` argument is reified.

use super::*;
use crate::vm::vm_method_call_attrs::CallAttrs;
use crate::vm::vm_method_call_plan::{MethodFastHints, MethodFrameFacts};

/// The replayable outcome of one class's `.new` dispatch tail.
pub(crate) struct UserNewPlan {
    /// The class the plan was decided under (its identity pins the shape).
    ctor_plan: std::sync::Arc<crate::runtime::NativeCtorPlan>,
    /// The class that declares the resolved `new`.
    owner: Symbol,
    def: std::sync::Arc<crate::runtime::MethodDef>,
    can_skip_merge: bool,
    facts: Option<MethodFrameFacts>,
}

/// Whether `def` is a `new` the lane may replay: compiled, not a multi, not
/// deprecated, and taking the invocant plus a named slurpy only.
// Cost: O(p), p = parameters.
fn user_new_shape_ok(def: &crate::runtime::MethodDef) -> bool {
    def.compiled_code.is_some()
        && def.delegation.is_none()
        && !def.is_multi
        && def.deprecated_message.is_none()
        && def.param_defs.iter().any(|pd| pd.slurpy && pd.name.starts_with('%'))
        && def.param_defs.iter().all(|pd| {
            if pd.is_invocant {
                return pd.implicit_invocant
                    && pd.type_constraint.is_none()
                    && pd.type_capture.is_none()
                    && pd.traits.is_empty()
                    && !pd.sigilless
                    && !crate::runtime::raw_invocant::param_is_raw_invocant(pd);
            }
            (pd.slurpy && pd.name == "%_" && pd.type_constraint.is_none())
                || Interpreter::method_named_slurpy_light(pd)
        })
}

impl Interpreter {
    /// Record the plan for the dispatch tail that just resolved `.new` on the
    /// type object `class_sym` to `(owner, def)`, when this dispatch's gate
    /// nominated the class for the ctor lane (whose native default
    /// constructor declined it).
    // Cost: O(m * c) once per class and generation (the candidate count and
    // the gate), m = MRO length, c = `new` candidates; O(1) otherwise.
    pub(super) fn install_user_new_lane(
        &mut self,
        class_sym: Symbol,
        owner: Symbol,
        def: &std::sync::Arc<crate::runtime::MethodDef>,
        target: &Value,
        args: &[Value],
    ) {
        if self.caches.ctor_lane_candidate != Some((class_sym, crate::symbol::wk::new_method()))
            || !matches!(target.view(), ValueView::Package(_))
            || !user_new_shape_ok(def)
        {
            return;
        }
        self.caches.ctor_lane_candidate = None;
        if self.caches.user_new_lane.contains_key(&class_sym)
            || !self.plain_method_lane_class_eligible(class_sym)
            || self.is_role(class_sym.as_str())
            || self.count_visible_method_candidates(class_sym.as_str(), "new") != 1
        {
            return;
        }
        let Some(cc) = def.compiled_code.as_ref() else {
            return;
        };
        let gate = self.method_fast_gate(def, cc, owner.as_str(), target, CallAttrs::Empty, args);
        if !gate.fast || gate.has_rw_params || gate.has_attr_aliases {
            return;
        }
        let ctor_plan = self.native_ctor_plan(class_sym);
        let facts = (!self.types.published_mixins.may_hold(owner.as_str())).then(|| {
            let owner_is_role = self.is_role(owner.as_str());
            MethodFrameFacts {
                owner_is_role,
                has_role_bindings: self.method_has_role_bindings(
                    def,
                    owner.as_str(),
                    class_sym.as_str(),
                ),
                body_package: self.method_body_package(owner.as_str(), owner, def),
                role_context: MethodFrameFacts::role_context_of(owner_is_role, owner, def),
            }
        });
        self.caches.user_new_lane.insert(
            class_sym,
            std::sync::Arc::new(UserNewPlan {
                ctor_plan,
                owner,
                def: std::sync::Arc::clone(def),
                can_skip_merge: gate.can_skip_merge,
                facts,
            }),
        );
    }

    /// The plan for `class_sym` under the current registry generation and
    /// class shape, if one was recorded.
    // Cost: O(1), two hash probes.
    fn user_new_lane_hit(&mut self, class_sym: Symbol) -> Option<std::sync::Arc<UserNewPlan>> {
        if self.caches.user_new_lane.is_empty() {
            return None;
        }
        self.refresh_method_caches_for_generation();
        let plan = self.caches.user_new_lane.get(&class_sym)?;
        let current = self.caches.native_ctor_plan_cache.get(&class_sym)?;
        std::sync::Arc::ptr_eq(&plan.ctor_plan, current).then(|| std::sync::Arc::clone(plan))
    }

    /// Answer `Class.new(named...)` at the top of `CallMethodMut` when the
    /// user-`new` lane holds the class (see the module doc). `None`, with the
    /// stack untouched, on a miss.
    // Cost: O(a), a = arguments, to decide; then the method's frame.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn try_user_new_early_lane(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        method_sym: Symbol,
        arity: usize,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<(), RuntimeError>> {
        if self.caches.user_new_lane.is_empty()
            || method_sym != crate::symbol::wk::new_method()
            || modifier_idx.is_some()
            || quoted
            || self.accessor_ref_pending
            || crate::runtime::find_method_intercept::any_user_find_method()
            || self.registry().method_name_ever_wrapped("new")
        {
            return None;
        }
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        let class_sym = Self::ctor_lane_key(
            &self.stack[target_idx],
            &self.stack[target_idx + 1..],
            None,
            false,
            false,
            method_sym,
        )?;
        let args = &self.stack[target_idx + 1..];
        if args
            .iter()
            .any(|a| matches!(a.view(), ValueView::VarRef { .. }) || a.needs_hllize_for_binding())
            || Self::has_builtin_seq_read(args)
        {
            return None;
        }
        let plan = self.user_new_lane_hit(class_sym)?;
        // The gate's per-call readings (see the module doc).
        for idx in target_idx + 1..self.stack.len() {
            let (internal, value) = match self.stack[idx].view() {
                ValueView::Pair(key, value) => (key.starts_with("__mutsu_"), value.clone()),
                _ => return None,
            };
            if internal || !self.light_arg_is_any(&value) {
                return None;
            }
        }
        if !self.method_named_slurpy_args_light(&plan.def, &self.stack[target_idx + 1..]) {
            return None;
        }
        // The stretch's own decode: `true` means a `|` spread at the call site.
        let decoded = self.decode_arg_sources_shared(code, arg_sources_idx);
        if decoded.as_ref().is_some_and(|(_, spread)| *spread) {
            return None;
        }
        let mut args = self.stack.split_off(target_idx + 1);
        let target = self.stack.pop()?;
        // What the stretch leaves behind for a call with these arguments.
        self.caches.ctor_lane_candidate = None;
        self.caches.plain_method_lane_candidate = None;
        self.set_pending_call_arg_sources_shared(decoded.map(|(names, _)| names));
        if args.iter().any(Self::ctor_arg_may_hold_lazy)
            && let Err(e) = self.reify_map_grep_seq_method_args(&target, method_sym, &args)
        {
            return Some(Err(e));
        }
        // The dispatch tail's user-method arm, replayed from the plan.
        self.dispatch.method_dispatch_pure = false;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user-new-lane");
        let def = &*plan.def;
        let cc = def
            .compiled_code
            .as_ref()
            .expect("a planned new is compiled");
        let armed = self.arm_raw_invocant_arrival(code, target_name, &target, method_sym, &args);
        let pushed_dispatch = match loan_env!(
            self,
            push_method_dispatch_frame_sym(
                class_sym,
                method_sym,
                &args,
                target.clone(),
                Some((plan.owner, def))
            )
        ) {
            Ok(pushed) => pushed,
            Err(e) => {
                self.disarm_raw_invocant_arrival(armed);
                return Some(Err(e));
            }
        };
        // While the caller's frame is still current (`vm_method_named_slurpy`).
        self.promote_method_named_slurpy_args(def, &mut args);
        // `arm_multi_method_winner_trust` for a non-multi winner, and
        // `call_compiled_method_in_unit`'s entry.
        self.pending_skip_constraint_recheck = false;
        self.pending_rw_writeback_sources.clear();
        let empty_fns = CompiledFns::default();
        let fns_ref = def.compiled_fns.as_deref().unwrap_or(&empty_fns);
        let method_result = self.call_compiled_method_fast(
            class_sym.as_str(),
            plan.owner,
            method_sym,
            def,
            cc,
            args,
            target,
            fns_ref,
            MethodFastHints {
                can_skip_merge: plan.can_skip_merge,
                facts: plan.facts,
            },
        );
        if pushed_dispatch {
            self.pop_method_dispatch();
        }
        self.pop_method_samewith_context();
        self.disarm_raw_invocant_arrival(armed);
        let result = self.finish_compiled_method_mut_call(class_sym, None, None, def, method_result);
        Some(result.map(|value| self.stack.push(value)))
    }
}
