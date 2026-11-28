//! The lean method frame (ADR-18244 stage D, ADR-15737 D1, #18267): a
//! planned method call whose method needs none of the frame's optional
//! machinery runs through this short frame instead of the general fast
//! binder (`call_compiled_method_fast_in_unit`).
//!
//! The general binder asks, on every call, about every feature a method
//! *might* use: a deprecation notice, `callframe`/`CALLER::`, `samewith`
//! and the deferral context, role composition bindings and a role's
//! `?ROLE`, the method's capture (`nqp::usecapture`), the implicit `*%_`, a
//! lexical capture of an `.^add_method` closure, `state` variables,
//! attribute `is default` registrations, a return type, a lexical routine or
//! class declared in the body, and every parameter shape (named, typed,
//! `is copy`, aggregates, sigilless, defaults). Each answer is a function of
//! the method, its compiled code and the registry -- not of the call -- so
//! [`lean_frame_eligible`] decides once, when the method-call plan is
//! installed (`vm_method_call_plan`), that none of them applies. The plan
//! table is cleared on every registry write, so the decision cannot outlive
//! a declaration, an `augment`, a wrap or a role composition.
//!
//! What is left per call is the frame itself: the light call frame, the
//! method's env overlay (until O1 retires it, ADR-17335), the readonly
//! scope, the parameters, the method-class and routine frames, the body
//! ([`Interpreter::run_compiled_method_body`], shared with the general
//! binder).
//!
//! The one per-call input the plan cannot fix is an argument's value: an
//! untyped parameter is implicitly `Any`, and a `Mu`-only argument must fail
//! that check with the general binder's error. [`Interpreter::lean_args_ok`]
//! checks it before anything is set up; a refusal hands the call to the
//! general binder untouched.

use super::*;
use crate::vm::vm_method_frame_plan::LocalInit;

/// Whether `def` (compiled as `cc`, declared by `owner`, called on
/// `receiver`) can run in the lean frame. Reads the method, its code and the
/// registry only; the caller has already established that the method-call
/// plan replays this call on the fast binder with `can_skip_merge`.
// Cost: O(p + l), p = parameters, l = local slots.
pub(super) fn lean_frame_eligible(
    interp: &Interpreter,
    def: &crate::runtime::MethodDef,
    cc: &CompiledCode,
    owner: Symbol,
    receiver: Symbol,
    can_skip_merge: bool,
) -> bool {
    let owner_name = owner.as_str();
    can_skip_merge
        && cc.closure_compiled_codes.is_empty()
        && def.deprecated_message.is_none()
        && !cc.uses_callframe
        && !cc.uses_dispatcher
        && !cc.uses_capture
        && !cc.may_observe_named_slurpy
        && cc.state_locals.is_empty()
        && cc.class_decl_plans.is_empty()
        && !cc.declares_inner_routines()
        && def.captured_env.is_none()
        // A literal definite return (`--> Nil`) names a value, so its
        // finalize reads no capture or owner scope (#18267).
        && def
            .return_type
            .as_deref()
            .is_none_or(|spec| matches!(spec, "Nil" | "True" | "False" | "Empty"))
        && def.role_param_bindings.is_none()
        && def.original_role.is_none()
        && def.role_origin.is_none()
        && !interp.is_role(owner_name)
        && !interp.method_has_role_bindings(def, owner_name, receiver.as_str())
        && interp.attr_default_classes(owner_name, receiver.as_str()) == (false, false)
        && def.param_defs.iter().all(|pd| {
            lean_param_ok(pd)
                && pd.type_constraint.as_deref().is_none_or(|tc| {
                    // A builtin type, or a user class (not a role, subset or
                    // enum, whose checks read more than the instance's class),
                    // that nothing in scope renames.
                    matches!(
                        interp.resolved_param_constraint_cow(pd, tc),
                        std::borrow::Cow::Borrowed(_)
                    ) && (lean_type_ok(tc) || interp.is_lean_nominal_class(tc))
                })
        })
}

/// Whether a name a lean frame's env holds is the frame's own, kept out of
/// the runtime-name carry: a frame fixture, a parameter, a compiled local or
/// an env-only `my` or a dynamic one (class-body statics and attribute
/// twigil forms are tested by the caller). The lean frame admits no role binding, sub-signature,
/// type capture or sigilless parameter, the fast binder's other own names.
// Cost: O(p + l), p = parameters, l = locals and env-only declarations.
fn lean_frame_owns_name(s: &str, def: &crate::runtime::MethodDef, cc: &CompiledCode) -> bool {
    matches!(s, "self" | "__ANON_STATE#" | "?CLASS" | "?ROLE" | "_")
        || def.params.iter().any(|p| p == s)
        || s.starts_with(crate::runtime::enum_bare_names::ENUM_BARE_PREFIX)
        || cc.locals.iter().any(|l| !l.is_empty() && l == s)
        || cc.env_only_decls.iter().any(|n| n == s)
        || super::vm_method_dispatch::dynamic_decl_local(&cc.locals, s)
}

/// A builtin value type a lean positional parameter may be constrained to,
/// checked per call with `type_matches_value` exactly as the general binder
/// checks it. Native types (coercing binds) and `Positional` (bind
/// failover) keep the general binder.
// Cost: O(k), k = length of `tc`.
fn lean_type_ok(tc: &str) -> bool {
    let (base, smiley) = crate::runtime::types::strip_type_smiley(tc);
    matches!(smiley, None | Some(":D") | Some(":U"))
        && matches!(
            base,
            "Int" | "Str" | "Num" | "Bool" | "Rat" | "Real" | "Numeric" | "Cool" | "Any" | "Mu"
        )
}

/// Whether `tc` is spelled like a plain nominal class name: no smiley, no
/// parameterization, no coercion, no definiteness or native lower-case form.
/// `lean_frame_eligible` then requires it to name a registered user class.
// Cost: O(k), k = length of `tc`.
fn plain_class_name(tc: &str) -> bool {
    tc.chars().next().is_some_and(|c| c.is_ascii_uppercase())
        && tc
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | ':'))
        && !tc.ends_with(':')
        && !tc.contains(":D")
        && !tc.contains(":U")
}

/// A parameter the lean binder binds: the plain invocant, the implicit
/// `*%_`, or an untyped, trait-less, required positional `$` scalar.
// Cost: O(1).
fn lean_param_ok(pd: &crate::ast::ParamDef) -> bool {
    if pd.is_invocant {
        return pd.implicit_invocant
            && pd.type_constraint.is_none()
            && pd.type_capture.is_none()
            && pd.traits.is_empty()
            && !pd.sigilless
            && !crate::runtime::raw_invocant::param_is_raw_invocant(pd);
    }
    if pd.slurpy && pd.name == "%_" {
        return true;
    }
    !pd.name.is_empty()
        && !pd
            .name
            .starts_with(['@', '%', '&', '!', '.', '^', ':', '$'])
        && !pd.declares_self_lexical()
        && !pd.named
        && !pd.slurpy
        && !pd.double_slurpy
        && !pd.sigilless
        && !pd.onearg
        && !pd.optional_marker
        && !pd.does_target
        && !pd.block_param
        && pd.default.is_none()
        && pd
            .type_constraint
            .as_deref()
            .is_none_or(|tc| lean_type_ok(tc) || plain_class_name(tc))
        && !pd.is_type_only()
        && pd.type_capture.is_none()
        && pd.literal_value.is_none()
        && pd.sub_signature.is_none()
        && pd.outer_sub_signature.is_none()
        && pd.code_signature.is_none()
        && pd.where_constraint.is_none()
        && pd.shape_constraints.is_none()
        && pd.traits.is_empty()
        && pd.trait_args.is_empty()
}

/// The resolved, planned call a lean frame runs.
pub(super) struct LeanMethodCall<'a> {
    /// The receiver's class (a metamethod's `state` is keyed by it).
    pub(super) receiver_class: Symbol,
    /// The class that declares the method.
    pub(super) owner_sym: Symbol,
    pub(super) method_sym: Symbol,
    pub(super) method_def: &'a crate::runtime::MethodDef,
    pub(super) cc: &'a CompiledCode,
    pub(super) compiled_fns: &'a CompiledFns,
    /// The plan's `method_body_package` answer (#18267 D1a), or `None` to ask.
    pub(super) body_package: Option<Option<Symbol>>,
    /// The body neither calls nor writes the env by name, so the exit drops
    /// the overlay; otherwise it carries the frame's pending runtime-name
    /// writes out, as the fast binder's non-skip exit does.
    pub(super) can_skip_merge: bool,
    /// The body a slot frame runs (`MethodCallPlan::slot_body`); `None` runs
    /// `cc`. Only the ops run from it: frame plans and facts stay keyed on
    /// `cc`, the method's own code.
    pub(super) slot_body: Option<&'a CompiledCode>,
}

impl Interpreter {
    /// Whether every argument passes the type check of the positional
    /// parameter it binds -- an untyped one's implicit `Any`, a typed one's
    /// builtin type -- and is no `Proxy` (which a typed parameter fetches).
    /// The only per-call check a lean frame makes; a refusal sends the call
    /// to the general binder, which raises the error.
    // Cost: O(a), a = arguments.
    pub(super) fn lean_args_ok(&mut self, def: &crate::runtime::MethodDef, args: &[Value]) -> bool {
        let mut params = def
            .param_defs
            .iter()
            .filter(|pd| !pd.is_invocant && !pd.slurpy);
        args.iter().all(|a| {
            let Some(pd) = params.next() else {
                return false;
            };
            if a.is_proxy_value() {
                return false;
            }
            match pd.type_constraint.as_deref() {
                None => self.light_arg_is_any(a),
                Some("Mu") => true,
                Some(tc) if lean_type_ok(tc) => self.type_matches_value(tc, a),
                // A user class (admitted by `lean_frame_eligible`): an instance
                // of exactly that class passes. Anything else -- a subclass
                // instance, the type object, a junction -- goes to the general
                // binder, which decides it in full.
                Some(tc) => matches!(
                    a.view(),
                    ValueView::Instance { class_name, .. } if class_name.as_str() == tc
                ),
            }
        })
    }

    /// Run a planned call in the lean frame (see the module docs). The
    /// caller checked [`Self::lean_args_ok`].
    // Cost: O(p + l), p = parameters, l = local slots, plus the body.
    pub(super) fn call_compiled_method_lean(
        &mut self,
        call: &LeanMethodCall<'_>,
        args: Vec<Value>,
        base: Value,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        let unit = self.unit_of_source_sym(call.cc.source_file);
        let saved_unit = std::mem::replace(&mut self.current_unit, unit);
        let result = self.call_compiled_method_lean_in_unit(call, args, base);
        self.current_unit = saved_unit;
        result
    }

    fn call_compiled_method_lean_in_unit(
        &mut self,
        call: &LeanMethodCall<'_>,
        args: Vec<Value>,
        base: Value,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        // ADR-0100, as in the general binder.
        crate::guard_native_stack!(
            self,
            self.call_compiled_method_lean_in_unit(call, args, base)
        );
        let &LeanMethodCall {
            receiver_class,
            owner_sym,
            method_sym,
            method_def,
            cc,
            compiled_fns,
            body_package,
            can_skip_merge,
            // The lean frame runs the method's own code.
            slot_body: _,
        } = call;
        // A non-multi winner: nothing to skip (see the general binder).
        self.pending_skip_constraint_recheck = false;
        let owner_class: &str = owner_sym.as_str();
        let (role_attrs_cell, inner_attrs_cell) =
            Self::method_attr_cells_for(&base, owner_class, false);
        let attrs_cell = role_attrs_cell.or_else(|| inner_attrs_cell.clone());
        let fallback_attrs_cell = inner_attrs_cell.filter(|cell| {
            attrs_cell.as_ref().is_some_and(|primary| {
                crate::gc::Gc::as_ptr(primary) != crate::gc::Gc::as_ptr(cell)
            })
        });
        let _mark_context_guard = crate::vm::vm_call_state_guard::MarkContextGuard::new(self);

        self.push_light_call_frame();
        let end_phaser_mark_before = self.end_phaser_capture_mark();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        self.unmark_readonly_topic();
        let links_frame = cc.links_frame_to_unit();
        {
            let parent = self.env().clone();
            let mut scoped = crate::env::Env::scoped_child(parent);
            scoped.mark_owns_routine_state();
            self.set_env(scoped);
        }
        let method_callable_id = crate::value::next_instance_id();
        if !can_skip_merge {
            // As the fast binder's non-skip arm: the rooted frame answers `$_`
            // and `$!` for the callees this body runs.
            self.env_mut().set_callable_id(method_callable_id as i64);
        }
        let saved_var_bindings = self.take_var_bindings();
        self.push_method_class_sym(owner_sym, Some(false));
        let body_package = match body_package {
            Some(package) => package,
            None => self.method_body_package(owner_class, owner_sym, method_def),
        };
        let saved_package: Option<Symbol> = body_package.map(|package| {
            let saved = self.current_package_sym();
            self.set_current_package_sym(package);
            saved
        });

        // Bind: the invocant, then each positional argument in order (every
        // parameter is a plain positional `$` scalar, `lean_param_ok`). The
        // gate admitted only arities that match.
        let mut param_values = super::vm_param_values::ParamValues::new();
        let param_syms = method_def.param_syms();
        let param_storage = method_def.param_storage_names();
        let mut args_iter = args.iter();
        for (idx, param_name) in param_storage.iter().enumerate() {
            let pd = method_def.param_defs.get(idx);
            if pd.is_some_and(|pd| pd.is_invocant) {
                let invocant = self
                    .take_implicit_self_invocant_arrival_sym(method_sym, pd)
                    .unwrap_or_else(|| base.clone());
                param_values.push((param_name.as_str(), param_syms[idx], invocant));
                continue;
            }
            if pd.is_some_and(|pd| pd.slurpy) {
                continue;
            }
            if let Some(val) = args_iter.next() {
                param_values.push((
                    param_name.as_str(),
                    param_syms[idx],
                    Self::itemize_fast_method_arg(pd, val.clone()),
                ));
            }
        }

        let class_val = Value::package(owner_sym);
        self.inject_class_body_statics(owner_class);
        self.insert_fast_param_values_by_name(
            &param_values,
            method_def,
            cc,
            cc.static_name_lookup || cc.needs_reflective_capture,
        );
        self.discard_pending_call_arg_sources();
        self.mark_fast_method_params_readonly(method_def);

        // Seed the slots from the frame plan, as the general binder does.
        let marked_frame = links_frame && self.env().is_frame_root();
        if marked_frame {
            if self.code_is_foreign_unit(cc) {
                self.env_mut().mark_outer_is_foreign_unit();
            } else {
                self.env_mut().mark_outer_is_unit();
            }
        }
        self.locals.refill_slots(cc.locals.len());
        {
            let attrs_guard = attrs_cell.as_ref().map(|c| c.as_map());
            let fallback_attrs_guard = fallback_attrs_cell.as_ref().map(|c| c.as_map());
            let attr_get = |key: Symbol| -> Option<Value> {
                attrs_guard
                    .as_ref()
                    .and_then(|g| g.get(key).cloned())
                    .or_else(|| {
                        fallback_attrs_guard
                            .as_ref()
                            .and_then(|g| g.get(key).cloned())
                    })
            };
            let plan = method_def.frame_plan(owner_sym, cc);
            let params_by_index = param_values.len() == method_def.params.len();
            for (i, init) in plan.locals.iter().enumerate() {
                let init = match *init {
                    LocalInit::Param(idx) => {
                        let bound = if params_by_index {
                            Some(&param_values[idx as usize].2)
                        } else {
                            let name = cc.locals[i].as_str();
                            param_values
                                .iter()
                                .find(|(n, _, _)| *n == name)
                                .map(|(_, _, val)| val)
                        };
                        match bound {
                            Some(val) => {
                                self.locals[i] = val.clone();
                                continue;
                            }
                            None => plan.unbound_param[i],
                        }
                    }
                    other => other,
                };
                self.locals[i] = match init {
                    LocalInit::Invocant => base.clone(),
                    LocalInit::Class => class_val.clone(),
                    LocalInit::Role | LocalInit::Nil | LocalInit::Param(_) => Value::NIL,
                    LocalInit::Topic => Value::package(crate::symbol::wk::any()),
                    LocalInit::AttrPrivate { qualified, bare } => attr_get(qualified)
                        .or_else(|| attr_get(bare))
                        .unwrap_or(Value::NIL),
                    LocalInit::AttrPublic { bare } => attr_get(bare).unwrap_or(Value::NIL),
                    LocalInit::AttrAggregate { sigil_key, bare } => attr_get(sigil_key)
                        .or_else(|| attr_get(bare))
                        .unwrap_or(Value::NIL),
                    LocalInit::Outer => cc
                        .local_sym(i)
                        .and_then(|sym| self.env().get_sym(sym))
                        .cloned()
                        .unwrap_or(Value::NIL),
                };
            }
        }

        // A method's `state` is keyed by its type; the body has none
        // (`lean_frame_eligible`), but a block nested in it may ask.
        let saved_state_scope = self.lexicals.state_scope_id.take();
        self.scope_method_state_sym(method_sym, receiver_class, owner_sym);
        self.reconcile_captured_readonly_ex(
            method_def.captured_readonly.as_ref(),
            cc,
            method_def.is_rw,
        );
        self.push_method_routine_with_location(
            owner_sym,
            method_def.lexical_package,
            method_sym,
            self.current_source_line(),
            self.executing_source_file_sym(),
            method_def.source_file_sym(),
            method_def.is_submethod,
            method_def.is_hidden_from_backtrace,
            method_callable_id,
            crate::runtime::routine_stack::FrameFixtures {
                invocant: Some(base.clone()),
                class: Some(class_val),
                role: None,
            },
            false,
        );

        let let_mark = self.let_saves_len();
        let saved_when_matched = self.when_matched();
        let saved_lexical_fatal_mode =
            std::mem::replace(&mut self.module.lexical_fatal_mode, cc.method_fatal_pragma);
        let super::vm_method_body_run::MethodBodyOutcome {
            result,
            explicit_return,
            fail_bypass,
        } = self.run_compiled_method_body(
            cc,
            compiled_fns,
            super::vm_method_body_run::MethodBodyFrame {
                owner_sym,
                method_sym,
                method_callable_id,
                saved_stack_depth,
                let_mark,
            },
        );
        if marked_frame {
            self.env_mut().unmark_outer_is_unit();
        }
        self.set_when_matched(saved_when_matched);
        self.module.lexical_fatal_mode = saved_lexical_fatal_mode;

        let ret_val = if result.is_ok() && self.stack.len() > saved_stack_depth {
            self.stack.pop().unwrap_or(Value::NIL)
        } else {
            Value::NIL
        };
        if result.is_ok() && explicit_return.is_none() {
            self.resolve_frame_let_saves(let_mark, &ret_val);
        }
        self.stack.truncate(saved_stack_depth);
        self.lexicals.state_scope_id.set(saved_state_scope);

        let end_phaser_env = (self.end_phaser_capture_mark() > end_phaser_mark_before).then(|| {
            self.sync_env_from_locals(cc);
            self.clone_env()
        });

        let method_var_bindings = self.take_var_bindings();
        let mut restored_bindings = saved_var_bindings;
        for (k, v) in method_var_bindings {
            restored_bindings.insert(k, v);
        }
        self.restore_var_bindings(restored_bindings);

        self.pop_routine();
        self.pop_method_class();
        if let Some(pkg) = saved_package {
            self.set_current_package_sym(pkg);
        }
        // Drop the overlay. A body that calls (`lean_frame_eligible`) carries
        // its pending runtime-name writes out first, exactly as the fast
        // binder's non-skip exit does: every other write reached the caller
        // through the variable's own cell (ADR-12529).
        self.dispatch.method_dispatch_pure = true;
        let frame = self.pop_call_frame();
        if can_skip_merge
            || self.pending_runtime_name_writes.is_empty()
            || self.env().ptr_eq(&frame.saved_env)
        {
            self.set_env(frame.saved_env);
        } else {
            let current_env = self.take_env();
            let class_body_static_names = self.lexicals.class_body_static_names.snapshot();
            let carried = self.carried_runtime_name_writes(&current_env, &|s: &str| {
                lean_frame_owns_name(s, method_def, cc)
                    || class_body_static_names
                        .get(owner_class)
                        .is_some_and(|names| names.contains(s))
                    || attrs_cell.as_ref().is_some_and(|c| {
                        super::vm_method_dispatch::attr_twigil_local(&c.as_map(), s)
                    })
            });
            drop(current_env);
            let mut merged = frame.saved_env;
            for (name, v) in carried {
                merged.insert(name, v);
            }
            self.set_env(merged);
        }

        if let Some(current) = end_phaser_env {
            let caller = self.clone_env();
            let dying: crate::runtime::NameSet = current
                .keys()
                .filter(|k| !caller.contains_key_sym(**k))
                .copied()
                .collect();
            self.update_end_phaser_envs(end_phaser_mark_before, &current, &dying);
        }

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };
        let final_result = if fail_bypass {
            final_result
        } else {
            // As the fast binder finalizes: a literal definite return spec
            // is its own effective spec (`method_effective_return_spec`).
            let spec = method_def.return_type.as_deref();
            let return_fast = spec.and_then(|spec| self.method_return_fast(owner_class, spec));
            let effective = spec.filter(|_| return_fast.is_none());
            self.finalize_method_result_fast(cc, final_result, spec, effective, return_fast)
        };
        final_result.map(|v| (Self::method_frame_result_sharing(&base, v), None))
    }

    /// Whether `name` is a registered class that is not a role, subset or
    /// enum: a type a value passes exactly when its class is it or inherits
    /// it.
    // Cost: O(1), four registry probes.
    pub(super) fn is_lean_nominal_class(&self, name: &str) -> bool {
        let registry = self.registry();
        registry.classes.contains_key(name)
            && !registry.roles.contains_key(name)
            && !registry.subsets.contains_key(name)
            && !registry.enum_types.contains_key(name)
    }
}
