//! The method-call plan (ADR-15737 D1): a plain-method-lane hit that replays
//! the call's resolution and binding decision instead of recomputing them.
//!
//! A lane hit (`vm_call_method_plain_lane`) already skips the opcode-level
//! probe stretch, but it then still walks the whole dispatch tail on every
//! call: `try_compiled_method_mut_or_interpret_sym` and
//! `compiled_mut_resolved_dispatch` re-ask the class-scoped-accessor and
//! accessor-wins probes, resolve the method, look for a wrap chain, and
//! `call_compiled_method_in_unit` re-runs the fast-path gate
//! ([`Interpreter::method_fast_gate`]) over the whole signature -- about a
//! quarter of a plain method call, none of it depending on anything but the
//! receiver's class, the method and the argument types.
//!
//! A [`MethodCallPlan`] records the outcome of that tail once per lane key:
//! the resolved `(owner, MethodDef)` and the gate's verdict. A lane hit that
//! finds a plan goes straight to the fast binder
//! (`call_compiled_method_fast`), running only the tail's per-call effects
//! (the raw-invocant arrival channel, the method-dispatch frame, the
//! attribute commit and the Proxy fetch on return).
//!
//! # Why a replayed decision is the decision the tail would make
//!
//! * **Resolution** is a function of the registry for a non-`multi` method
//!   (`resolve_method_cached` serves it from a `(class, method)` memo). A
//!   `multi` winner can depend on argument values (a `where` candidate), so a
//!   multi is planned only when no candidate of its family can
//!   (`multi_dispatch_type_cacheable`): its winner is then a function of the
//!   argument type keys, which the lane key holds (ADR-15737 D1b).
//! * **The accessor and wrap probes** (`winning_class_scoped_accessor`,
//!   `resolve_user_method_or_accessor_sym`, `check_method_wrap_chain`) read
//!   the registry only.
//! * **The gate** reads the signature, the compiled code, the argument list,
//!   the call site's argument sources, the receiver and its attribute cell. A
//!   plan is installed only where the gate's reading is fixed by the argument
//!   type keys: no parameter that may alias the caller's container (sigilless,
//!   `is rw`, `is raw`, a `does` target, which bind by whether the argument
//!   came from a variable); no `Array`/`Hash`/`List`/`Seq` argument or named
//!   argument value (whose container sharing also depends on the argument
//!   sources); no container but a plain shared cell holding a scalar-like
//!   value, whose position the lane key marks
//!   ([`Interpreter::plain_lane_arg_keys_into`]) so a plan decided for a cell
//!   never answers for a plain value. A named argument with a scalar value is
//!   keyed by its name and its value's type. The receiver must be an
//!   ordinary instance (a user class's, or a runtime mixin type's the lane
//!   admits: `plain_method_lane_mixin_eligible`), whose only per-instance
//!   gate input, sigilless-alias metadata on the attribute cell, is
//!   re-checked on every replay.
//! * **Both gates replay it**: the early lane, and the late gate a call the
//!   early lane refuses reaches (a `VarRef` argument, a `|` spread), whose
//!   arguments are exactly the tail's.
//!
//! The whole plan table is keyed on the registry *write* generation, which
//! every registry write moves (`registry_mut`), so no plan outlives a
//! declaration, an `augment`, a wrap or a role composition. The table lives
//! in the per-interpreter `caches` subsystem, so it needs no lock (ADR-15737,
//! *Threads*).

use super::*;
use crate::vm::vm_method_call_attrs::CallAttrs;

/// The replayable outcome of one lane key's dispatch tail.
#[derive(Debug)]
pub(crate) struct MethodCallPlan {
    /// The class that declares the resolved method.
    pub(crate) owner: Symbol,
    /// The resolved (non-`multi`, compiled) method.
    pub(crate) def: std::sync::Arc<crate::runtime::MethodDef>,
    /// The fast-path gate's `can_skip_merge` for this key.
    pub(crate) can_skip_merge: bool,
    /// The method needs none of the general binder's optional machinery
    /// and runs in the lean frame (`vm_method_lean_frame`).
    pub(crate) lean: bool,
    /// The lean frame may run as a slot frame (ADR-18393 stage 1,
    /// `vm_method_slot_frame`): decided once per plan from the body's ops.
    pub(crate) slot_frame: bool,
    /// A parameter may alias the caller's container (sigilless, `is raw`,
    /// `is rw`): whether this call binds it on the fast binder depends on
    /// where each argument came from, so a replay re-checks the argument
    /// sources (`method_container_args_light`) and promotes the caller's
    /// variables to shared cells (`promote_method_container_args`), exactly
    /// as the dispatch tail would.
    pub(crate) container_params: bool,
    /// A private method (`$obj!name`): whether the calling context may
    /// dispatch it depends on the caller, not on the lane key, so a replay
    /// re-asks `can_fast_dispatch_private_method_vm` on every call, as the
    /// private dispatch arm does.
    pub(crate) private: bool,
    /// The method has a `*%h` the fast binder fills
    /// (`method_has_light_named_slurpy`): a replay checks and promotes the
    /// call's leftover named arguments. A function of the `MethodDef` alone,
    /// so it is answered once here rather than by a parameter scan per call.
    pub(crate) named_slurpy: bool,
    /// The body a slot frame runs, when `slot_frame` is set
    /// (`vm_method_slot_frame::slot_frame_body_code`): built once with the
    /// plan, so the per-call path makes no decision about it.
    pub(crate) slot_body: Option<std::sync::Arc<CompiledCode>>,
    /// What the fast binder's prologue asks of the owner and the method on
    /// every call, answered once for this plan (#18267 D1a); `None` when the
    /// answer can change without a registry write (a published mixin owner).
    pub(crate) facts: Option<MethodFrameFacts>,
}

/// The per-call questions `call_compiled_method_fast_in_unit` asks that are
/// a function of the resolved `(owner, MethodDef)` and the receiver class
/// alone, so a [`MethodCallPlan`] answers them once (#18267 D1a). Each reads
/// the registry only, which the plan table's generation key pins.
#[derive(Debug, Clone, Copy)]
pub(crate) struct MethodFrameFacts {
    /// `is_role(owner)`.
    pub(crate) owner_is_role: bool,
    /// `method_has_role_bindings(def, owner, receiver)`.
    pub(crate) has_role_bindings: bool,
    /// `method_body_package(owner, owner_sym, def)`.
    pub(crate) body_package: Option<Symbol>,
    /// The role the body was written in (`?ROLE`), interned.
    pub(crate) role_context: Option<Symbol>,
}

impl MethodFrameFacts {
    /// The role a method body runs for: its owner when the owner is a role,
    /// else the role it was composed from, if any.
    // Cost: O(1) plus one intern for a composed role method.
    pub(crate) fn role_context_of(
        owner_is_role: bool,
        owner_sym: Symbol,
        def: &crate::runtime::MethodDef,
    ) -> Option<Symbol> {
        if owner_is_role {
            Some(owner_sym)
        } else {
            def.original_role
                .as_deref()
                .or(def.role_origin.as_deref())
                .map(Symbol::intern)
        }
    }
}

/// How `call_compiled_method_fast` runs a call: the fast-path gate's
/// `can_skip_merge`, and a plan's precomputed [`MethodFrameFacts`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct MethodFastHints {
    pub(crate) can_skip_merge: bool,
    pub(crate) facts: Option<MethodFrameFacts>,
}

impl MethodFastHints {
    /// A call that was not replayed from a plan: every fact is asked anew.
    // Cost: O(1).
    pub(crate) fn unplanned(can_skip_merge: bool) -> Self {
        MethodFastHints {
            can_skip_merge,
            facts: None,
        }
    }
}

/// An argument whose fast-path gate reading is fixed by its type key.
// Cost: O(1).
fn plan_arg_kind_ok(arg: &Value) -> bool {
    // A named argument: a string-keyed Pair (not an internal `__mutsu_*` one,
    // which the gate refuses) whose value is itself scalar-like. The lane key
    // holds its name and its value's type key; the gate's named-parameter
    // reading (`named_param_fast_eligible`'s type check, `named_args_all_land`)
    // depends on nothing else for such a value, and container sharing into a
    // named parameter only arises for an Array/Hash value.
    if arg.is_string_pair_value()
        && let ValueView::Pair(key, value) = arg.view()
    {
        return !key.starts_with("__mutsu_") && plan_scalar_kind_ok(value);
    }
    // A shared cell (a caller variable an `is raw` / sigilless parameter
    // promoted, relayed onward: AttrX::Mooish's `Mu $instance is raw`). The
    // lane key marks its position (`plain_lane_arg_keys_into`) and keys its
    // contents, so a plan decided for a cell is replayed only for a cell in
    // the same position holding the same kind of value. Only a plain cell
    // holding a scalar-like value: an itemized or native-typed cell carries
    // a reading of its own.
    // A positional `Proxy` (AttrX::Mooish's `AttrProxy` handed to
    // `build-attr` / `store-with-cb`, #18321): the lane key holds a marker in
    // its position, never what it FETCHes. The gate's reading of it is fixed
    // by the parameter it binds -- a `Mu` container parameter keeps the Proxy
    // (`MethodContainerBind::Proxy`, re-classified on every replay of a plan
    // with container parameters), any other parameter FETCHes it on each call
    // in the binder -- and the lean frame refuses it per call
    // (`lean_args_ok`).
    if arg.is_proxy_value() {
        return true;
    }
    if arg.is_container_ref() {
        return !arg.container_ref_is_itemized()
            && arg.native_container_type().is_none()
            && plan_scalar_kind_ok(&arg.deref_container());
    }
    plan_scalar_kind_ok(arg)
}

/// The marker a lane key carries when the call asked for a container
/// (`want_ref`, see `plain_method_lane_key`).
// Cost: O(1) after the first call.
pub(super) fn want_ref_marker() -> Symbol {
    static MARKER: std::sync::OnceLock<Symbol> = std::sync::OnceLock::new();
    // A NUL cannot occur in a Raku type name (see `container_arg_marker`).
    *MARKER.get_or_init(|| Symbol::intern("\u{0}want-ref"))
}

/// The marker a lane key carries for a shared-cell argument, one symbol per
/// argument position (see [`Interpreter::plain_lane_arg_keys_into`]).
// Cost: O(1) after the first call per position.
fn container_arg_marker(position: usize) -> Option<Symbol> {
    const POSITIONS: usize = 8;
    static MARKERS: [std::sync::OnceLock<Symbol>; POSITIONS] =
        [const { std::sync::OnceLock::new() }; POSITIONS];
    let cell = MARKERS.get(position)?;
    // A NUL cannot occur in a Raku type name, so the marker never reads as
    // the key of an argument.
    Some(*cell.get_or_init(|| Symbol::intern(&format!("\u{0}container-arg\u{0}{position}"))))
}

/// A positional value whose gate reading is fixed by its type key.
// Cost: O(1).
fn plan_scalar_kind_ok(arg: &Value) -> bool {
    matches!(
        arg.view(),
        ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Str(_)
            | ValueView::Rat(..)
            | ValueView::Bool(_)
            | ValueView::Nil
            | ValueView::Package(_)
            | ValueView::Instance { .. }
            // A Capture argument is keyed as the class `Capture`
            // (`multi_arg_type_keys_impl`); the fast binder type-checks it
            // per call like any instance, and no gate reading looks inside
            // it (AttrX::Mooish's `store-with-cb(..., \( :builder ))`).
            | ValueView::Capture { .. }
    )
}

impl Interpreter {
    /// The plain-method lane's argument key: the arguments' type keys
    /// ([`Self::multi_arg_type_keys_into_with_named`], which keys a shared
    /// cell by its contents), then one marker per argument that is a shared
    /// cell. `false` when an argument has no key, or a cell sits past the
    /// eighth position.
    ///
    /// The marker is what lets a [`MethodCallPlan`] be decided for a call
    /// that relays a cell: without it a cell and a plain value of the same
    /// type share a key, so the plan one of them installed would replay for
    /// the other, whose fast-path gate reading may differ.
    // Cost: O(a), a = arguments.
    pub(super) fn plain_lane_arg_keys_into(
        &mut self,
        args: &[Value],
        keys: &mut Vec<Symbol>,
    ) -> bool {
        if !self.multi_arg_type_keys_into_with_named(args, keys) {
            return false;
        }
        for (position, arg) in args.iter().enumerate() {
            if arg.is_container_ref() {
                let Some(marker) = container_arg_marker(position) else {
                    return false;
                };
                keys.push(marker);
            }
        }
        true
    }

    /// The plan table entry for `key`: `Some(Some(plan))` to replay,
    /// `Some(None)` when this key was found unplannable under the current
    /// registry, `None` when nothing has been decided yet.
    // Cost: O(a) to hash the key, a = argument type keys.
    pub(super) fn method_call_plan_lookup(
        &mut self,
        key: &crate::runtime::PlainMethodLaneKey,
    ) -> Option<Option<std::sync::Arc<MethodCallPlan>>> {
        let generation = self.registry_write_generation();
        if self.caches.method_call_plans_generation != generation {
            self.caches.method_call_plans_generation = generation;
            self.caches.method_call_plans.clear();
            return None;
        }
        self.caches.method_call_plans.get(key).cloned()
    }

    /// The plan to replay for a lane hit on `key`; when none has been decided
    /// under the current registry, asks the tail this hit is about to run to
    /// decide it.
    // Cost: O(a) to hash the key, a = argument type keys; plus a key clone
    // once per key and registry generation.
    pub(super) fn method_call_plan_for_hit(
        &mut self,
        key: &crate::runtime::PlainMethodLaneKey,
    ) -> Option<std::sync::Arc<MethodCallPlan>> {
        match self.method_call_plan_lookup(key) {
            Some(plan) => plan,
            None => {
                self.want_method_call_plan(key);
                None
            }
        }
    }

    /// Ask the dispatch tail now starting for `key` to record its outcome
    /// (see [`Self::install_method_call_plan`]).
    // Cost: O(a), a = argument type keys (the key is cloned).
    pub(super) fn want_method_call_plan(&mut self, key: &crate::runtime::PlainMethodLaneKey) {
        self.caches.method_call_plan_candidate = Some(key.clone());
    }

    /// Record the plan (or that there is none) for the dispatch tail that just
    /// resolved `class_sym.method_sym` to `(owner, def)`, when a lane gate
    /// asked for one for exactly this call (the candidate's class, method and
    /// argument type keys must all be this call's).
    ///
    /// Runs before the method body, so nothing the body dispatches can
    /// consume or replace the candidate first.
    // Cost: O(p * a) once per key and registry generation (the gate), p =
    // parameters, a = arguments; O(1) when no candidate is pending.
    pub(super) fn install_method_call_plan(
        &mut self,
        class_sym: Symbol,
        method_sym: Symbol,
        owner: Symbol,
        def: &std::sync::Arc<crate::runtime::MethodDef>,
        target: &Value,
        args: &[Value],
    ) {
        let Some(key) = self
            .caches
            .method_call_plan_candidate
            .take_if(|key| key.0 == class_sym && key.1 == method_sym)
        else {
            return;
        };
        let mut arg_keys = Vec::with_capacity(args.len());
        if !args.is_empty() && !self.plain_lane_arg_keys_into(args, &mut arg_keys) {
            return;
        }
        // The container-request marker is the gate's, not the arguments'.
        if key.2.last() == Some(&want_ref_marker()) {
            arg_keys.push(want_ref_marker());
        }
        if arg_keys != key.2 {
            return;
        }
        let plan = self.method_call_plan_for(method_sym, owner, def, target, args);
        let generation = self.registry_write_generation();
        if self.caches.method_call_plans_generation != generation {
            self.caches.method_call_plans_generation = generation;
            self.caches.method_call_plans.clear();
        }
        self.caches.method_call_plans.insert(key, plan);
    }

    /// The plan for this resolved call, or `None` when its shape is outside
    /// what a replay may skip (see the module docs).
    // Cost: O(p * a), p = parameters, a = arguments.
    fn method_call_plan_for(
        &mut self,
        method_sym: Symbol,
        owner: Symbol,
        def: &std::sync::Arc<crate::runtime::MethodDef>,
        target: &Value,
        args: &[Value],
    ) -> Option<std::sync::Arc<MethodCallPlan>> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        let attributes = attributes.clone();
        // The entry of the tail's prefix that a hit skips and that is not a
        // probe: a native array's retyped answer and `.new` on a built-in
        // instance. A private call is planned by its own dispatch arm
        // (`try_private_compiled_mut_dispatch`); its multi resolution reads
        // the arguments by a path of its own, so a private multi is not.
        let method = method_sym.as_str();
        let private = method.starts_with('!');
        // A multi is planned only when its family's winner is a function of the
        // argument type keys the lane key holds (no `where`, literal, subset,
        // smiley or `is rw` candidate; `multi_dispatch_type_cacheable`), which
        // is also the condition under which the dispatch tail memoizes its
        // resolution (ADR-15737 D1b).
        // A container or Proxy argument's lane key does not change with what
        // it holds (a FETCH may answer a different type on every call), so
        // the winner would not be a function of the key.
        let multi_unplannable = def.is_multi
            && (args
                .iter()
                .any(|a| a.is_container_ref() || super::vm_call_helpers::bound_proxy(a).is_some())
                || match target.view() {
                    ValueView::Instance { class_name, .. } => !self.multi_dispatch_type_cacheable(
                        class_name,
                        method_sym,
                        class_name.as_str(),
                        method,
                    ),
                    _ => true,
                });
        if method == "new"
            || (private && def.is_multi)
            || crate::builtins::method_table::native_array_result::retypes(method)
            || multi_unplannable
            || def.deprecated_message.is_some()
            // A `does` target binds through the argument's container by a
            // path the fast binder does not take. A parameter that may alias
            // the caller's container (sigilless, `is rw`, `is raw`) binds by
            // where the argument came from, which the call site's argument
            // sources tell and the type key does not: such a plan re-checks
            // the sources on every replay (`container_params`).
            || def.param_defs.iter().any(|pd| pd.is_does_target())
            || !args.iter().all(plan_arg_kind_ok)
        {
            return None;
        }
        let cc = def.compiled_code.as_ref()?;
        let gate = self.method_fast_gate(
            def,
            cc,
            owner.as_str(),
            target,
            CallAttrs::Cell(&attributes),
            args,
        );
        if !gate.fast || gate.has_rw_params || gate.has_attr_aliases {
            return None;
        }
        let receiver = match target.view() {
            ValueView::Instance { class_name, .. } => class_name,
            _ => return None,
        };
        let lean = super::vm_method_lean_frame::lean_frame_eligible(
            self,
            def,
            cc,
            owner,
            receiver,
            gate.can_skip_merge || gate.merge_is_name_carry_only,
        );
        // A slot frame has no overlay to carry anything out of. A merge the
        // gate keeps only for the runtime-name carry (`merge_is_name_carry_only`:
        // the body calls something) is no reason to refuse one: a callee's
        // writes land in the env handle the slot frame shares with its caller,
        // which is where the lean frame's carry would have put them. The body
        // itself writes no name (`slot_frame_body_ok`).
        let slot_frame = lean
            && (gate.can_skip_merge || gate.merge_is_name_carry_only)
            && super::vm_method_slot_frame::slot_frame_body_ok(def, cc, owner)
            // A CStruct keeps its fields in native memory, which the slot
            // frame's attribute-cell read does not reach.
            && (!super::vm_method_slot_frame::slot_body_reads_attrs(cc)
                || self.cstruct_class_name(owner.as_str()).is_none());
        // A mixin owner another thread may publish is asked anew per call
        // (`method_body_package` reads `published_mixins`, which no registry
        // write of this interpreter moves).
        let facts = (!self.types.published_mixins.may_hold(owner.as_str())).then(|| {
            let owner_is_role = self.is_role(owner.as_str());
            MethodFrameFacts {
                owner_is_role,
                has_role_bindings: self.method_has_role_bindings(
                    def,
                    owner.as_str(),
                    receiver.as_str(),
                ),
                body_package: self.method_body_package(owner.as_str(), owner, def),
                role_context: MethodFrameFacts::role_context_of(owner_is_role, owner, def),
            }
        });
        Some(std::sync::Arc::new(MethodCallPlan {
            owner,
            def: std::sync::Arc::clone(def),
            can_skip_merge: gate.can_skip_merge,
            lean,
            slot_frame,
            container_params: def.has_container_params(),
            private,
            named_slurpy: Self::method_has_light_named_slurpy(def),
            slot_body: slot_frame.then(|| {
                std::sync::Arc::new(super::vm_method_slot_frame::slot_frame_body_code(cc))
            }),
            facts,
        }))
    }

    /// Replay `plan` for a lane hit: the dispatch tail's per-call effects
    /// around the fast binder, with the resolution and the gate taken from
    /// the plan. Pushes the call's result.
    // Cost: O(1) beyond the method body and its binding.
    pub(super) fn run_method_call_plan(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        target: Value,
        method_sym: Symbol,
        plan: &MethodCallPlan,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        let ValueView::Instance {
            class_name: class_sym,
            attributes,
            id: target_id,
        } = target.view()
        else {
            unreachable!("a method-call plan is only looked up for an instance receiver");
        };
        let attrs_cell = attributes.clone();
        // The one per-instance input of the gate (see the module docs), and
        // a private method's per-call permission check: a caller the private
        // dispatch arm would refuse takes the whole path, as it did unplanned.
        if CallAttrs::Cell(&attrs_cell).has_alias_meta()
            || (plan.private && !self.can_fast_dispatch_private_method_vm(plan.owner.as_str()))
        {
            return self.run_plain_method_lane(code, target_name, target, method_sym, args);
        }
        // The per-call half of the gate for a container parameter: an
        // argument this replay cannot bind on the fast binder (a source the
        // promotion cannot serve) takes the dispatch tail, as the first call
        // with that shape did.
        let def = &*plan.def;
        let cc = def
            .compiled_code
            .as_ref()
            .expect("a planned method is compiled");
        let mut args = args;
        if plan.container_params {
            if !self.method_container_args_light(def, cc, &args) {
                return self.run_plain_method_lane(code, target_name, target, method_sym, args);
            }
            // While the caller's frame is still current, as the tail does.
            self.promote_method_container_args(def, &mut args);
        }
        // An explicit `*%h`'s leftover arguments, checked per call like the
        // container parameters above; promoted below, once the deferral
        // frame has recorded the arguments as they arrived.
        if plan.named_slurpy && !self.method_named_slurpy_args_light(def, &args) {
            return self.run_plain_method_lane(code, target_name, target, method_sym, args);
        }
        self.dispatch.method_dispatch_pure = false;
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
        let armed = self.arm_raw_invocant_arrival(code, target_name, &target, method_sym, &args);
        let owner = plan.owner;
        let pushed_dispatch = loan_env!(
            self,
            push_method_dispatch_frame_sym(
                class_sym,
                method_sym,
                &args,
                target.clone(),
                Some((owner, def))
            )
        );
        let pushed_dispatch = match pushed_dispatch {
            Ok(pushed) => pushed,
            Err(e) => {
                self.disarm_raw_invocant_arrival(armed);
                return Err(e);
            }
        };
        // While the caller's frame is still current (`vm_method_named_slurpy`).
        if plan.named_slurpy {
            self.promote_method_named_slurpy_args(def, &mut args);
        }
        // `arm_multi_method_winner_trust` for a non-multi winner, and
        // `call_compiled_method_in_unit`'s entry, which clears the rw
        // writeback list this call's site drains.
        self.pending_skip_constraint_recheck = false;
        self.pending_rw_writeback_sources.clear();
        let empty_fns = CompiledFns::default();
        let fns_ref = def.compiled_fns.as_deref().unwrap_or(&empty_fns);
        let method_result = if plan.lean && self.lean_args_ok(def, &args) {
            let call = super::vm_method_lean_frame::LeanMethodCall {
                receiver_class: class_sym,
                owner_sym: owner,
                method_sym,
                method_def: def,
                cc,
                compiled_fns: fns_ref,
                body_package: plan.facts.map(|facts| facts.body_package),
                can_skip_merge: plan.can_skip_merge,
                slot_body: plan.slot_body.as_deref(),
            };
            if plan.slot_frame && self.slot_frame_entry_ok() {
                self.call_compiled_method_slot(&call, args, target.clone())
            } else {
                self.call_compiled_method_lean(&call, args, target.clone())
            }
        } else {
            self.call_compiled_method_fast(
                class_sym.as_str(),
                owner,
                method_sym,
                def,
                cc,
                args,
                target.clone(),
                fns_ref,
                MethodFastHints {
                    can_skip_merge: plan.can_skip_merge,
                    facts: plan.facts,
                },
            )
        };
        if pushed_dispatch {
            self.pop_method_dispatch();
        }
        self.pop_method_samewith_context();
        self.disarm_raw_invocant_arrival(armed);
        let result = self.finish_compiled_method_mut_call(
            class_sym,
            Some(target_id),
            Some(&attrs_cell),
            def,
            method_result,
        );
        if let Err(e) = &result
            && Self::is_method_not_found_error(e)
        {
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "notfound");
        }
        self.stack.push(result?);
        Ok(())
    }
}
