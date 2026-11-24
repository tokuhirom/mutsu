use super::vm_method_call_attrs::CallAttrs;
use super::vm_method_call_plan::{MethodFastHints, MethodFrameFacts};
use super::*;
use crate::meta_ns::MetaNs;
use crate::value::AttrMap;
use crate::vm::vm_method_fast_gate::MethodFastGate;
use crate::vm::vm_method_frame_plan::LocalInit;

pub(crate) const ATTR_ALIAS_META_PREFIX: &str = "__mutsu_attr_alias::";

impl Interpreter {
    /// Ordinary methods own state through their declaring/composing type;
    /// inherited methods retain that owner. A metamethod instead belongs to
    /// each receiver's HOW, including dynamically composed receiver types.
    // Cost: O(1), except O(n) to intern a metamethod receiver name of n bytes.
    pub(super) fn scope_method_state(
        &self,
        method_name: &str,
        receiver_class_name: &str,
        owner: Symbol,
    ) {
        let owner = if method_name.starts_with('^') {
            Symbol::intern(receiver_class_name)
        } else {
            owner
        };
        self.lexicals
            .state_scope_id
            .set(Some(crate::state_scope::StateScopeId::Type(owner.id())));
    }

    /// Call a compiled method body (MethodDef with compiled_code).
    /// Mirrors `Interpreter::run_resolved_method_celled` but executes bytecode.
    ///
    /// The body runs in its own compilation unit, as a sub body does
    /// (`enter_compilation_unit`): a routine name it calls is resolved in the
    /// compunit the method was written in, not in whichever unit the caller
    /// happened to be running (ADR-12529 phase 2). Without it a method's call
    /// to a routine its module imported or declared resolved through the
    /// frame-anchored fallback on every call, which no resolution memo can
    /// hold.
    // Cost: O(c) to leave a lexical type scope when this body declares one,
    // c = lexical classes it declared, plus the body.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn call_compiled_method(
        &mut self,
        receiver_class_name: &str,
        owner_sym: Symbol,
        method_sym: Symbol,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        attributes: CallAttrs<'_>,
        args: Vec<Value>,
        invocant: Option<Value>,
        compiled_fns: &CompiledFns,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        let unit = self.unit_of_source_sym(cc.source_file);
        let saved_unit = std::mem::replace(&mut self.current_unit, unit);
        // A method has no surrounding block-scope opcode. Its own `my class`
        // must leave scope when the call returns, including on an exception.
        // Most methods have no class declaration and skip both operations.
        let scopes_lexical_types = !cc.class_decl_plans.is_empty();
        if scopes_lexical_types {
            self.push_lexical_class_scope();
        }
        let result = self.call_compiled_method_in_unit(
            receiver_class_name,
            owner_sym,
            method_sym,
            method_def,
            cc,
            attributes,
            args,
            invocant,
            compiled_fns,
        );
        if scopes_lexical_types {
            self.pop_lexical_class_scope();
        }
        self.current_unit = saved_unit;
        result.map_err(|mut err| {
            // An error the body raised carries its backtrace already; one
            // without is the binder's, raised before the method's frame was
            // pushed. Rakudo reports it inside the method (`in method m`).
            if !err.has_backtrace() && err.control.is_none() && !err.is_leave {
                self.attach_method_frame_backtrace(&mut err, owner_sym, method_sym, method_def, cc);
            }
            err
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn call_compiled_method_in_unit(
        &mut self,
        receiver_class_name: &str,
        owner_sym: Symbol,
        method_sym: Symbol,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        attributes: CallAttrs<'_>,
        args: Vec<Value>,
        invocant: Option<Value>,
        compiled_fns: &CompiledFns,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        // The owner and the method name arrive interned: the callers resolved
        // them as `Symbol`s, and re-interning their text here cost a
        // thread-local string-hash probe per call for a name that never
        // changes (#10961).
        let owner_class: &str = owner_sym.as_str();
        let method_name: &str = method_sym.as_str();
        // ADR-0100: refuse the call while there is still stack left to raise
        // with, so deep recursion becomes a catchable exception instead of a
        // guard-page abort. Same boundary as this path's `Call` GC safepoint.
        // Ahead of the flag taken below, so a call continued on a fresh stack
        // segment (ADR-17392) finds it still set.
        crate::guard_native_stack!(
            self,
            self.call_compiled_method_in_unit(
                receiver_class_name,
                owner_sym,
                method_sym,
                method_def,
                cc,
                attributes,
                args,
                invocant,
                compiled_fns,
            )
        );
        // Taken at entry so it describes only THIS method's own parameter
        // bind below, never a nested call made before it (#10986; see
        // `pending_skip_constraint_recheck`).
        let skip_constraint_recheck = std::mem::take(&mut self.pending_skip_constraint_recheck);
        crate::alloc_scope!("call-compiled-method");
        // Slice F: the rw-writeback source list is drained by the CallMethod /
        // CallMethodMut op right after this dispatch returns, so it must hold
        // only this call's sources. Clear any leftover from a sibling whose call
        // site did not drain; nested calls in the body self-drain via their own
        // call-site ops before this frame records its own sources.
        self.pending_rw_writeback_sources.clear();
        // RAII (`MarkContextGuard`,
        // `todo/deep/mark-context-flags-leak-across-live-call-boundary.md`):
        // isolate the "mark context" one-shot flag family (bind_context et
        // al.) so a caller's pending `:=` mark does not leak into this
        // method's own vardecl/store opcodes -- restored on every exit path,
        // including a Rust panic unwind through the body loop below.
        let _mark_context_guard = crate::vm::vm_call_state_guard::MarkContextGuard::new(self);
        // Check for `is DEPRECATED` trait on the method
        if let Some(ref msg) = method_def.deprecated_message {
            let cl = Some(self.cur_source_line);
            loan_env!(
                self,
                check_deprecation_for_method_with_line(method_name, owner_class, msg, cl,)
            );
        }
        // Build the base (self) value
        let mut base = if let Some(inv) = invocant {
            inv
        } else {
            let attributes = attributes.materialize();
            if attributes.is_empty() {
                Value::package(crate::symbol::Symbol::intern(receiver_class_name))
            } else {
                Value::make_instance(
                    crate::symbol::Symbol::intern(receiver_class_name),
                    attributes.into_owned(),
                )
            }
        };
        // A CStruct/CUnion handle keeps its fields in native memory, not in the
        // attribute cell, so `$!field` inside the class's own method would read
        // nothing. (`$obj.field` works: the generated accessor goes through
        // `cstruct_field_value`.) Materialise the declared fields for the
        // duration of the call — re-read on every entry, so a value C changed
        // in between is picked up. `MoarVM::Guts::REPRs`' `MVMArrayB.realstart`
        // is exactly this shape: `+$!start ?? … !! $!any`.
        // This runs before the fast-path decision below: the fast binder reads
        // the attributes off `self`'s cell and must see the re-read fields too.
        self.seed_cstruct_fields_for_method(receiver_class_name, Some(&base));
        let gate = self.method_fast_gate(method_def, cc, owner_class, &base, attributes, &args);
        if gate.has_rw_params {
            self.publish_rw_arg_cells_by_source();
        }
        let MethodFastGate {
            can_skip_merge,
            has_attr_aliases,
            ..
        } = gate;
        if gate.fast {
            // A container parameter's caller variable becomes its shared cell
            // while the caller's frame is still current
            // (`vm_method_container_params`).
            let mut args = args;
            if method_def.has_container_params() {
                self.promote_method_container_args(method_def, &mut args);
            }
            // An explicit `*%h`'s leftover arguments (`vm_method_named_slurpy`).
            self.promote_method_named_slurpy_args(method_def, &mut args);
            // The fast binder below checks the parameter types itself;
            // hand it this call's #10986 trust.
            self.pending_skip_constraint_recheck = skip_constraint_recheck;
            return self.call_compiled_method_fast(
                receiver_class_name,
                owner_sym,
                method_sym,
                method_def,
                cc,
                args,
                base,
                compiled_fns,
                MethodFastHints::unplanned(can_skip_merge),
            );
        }

        // Only the full binder below reads the attributes as a map (#8880).
        let attributes = attributes.materialize();
        let attributes: &AttrMap = &attributes;
        self.push_call_frame();
        // See `call_compiled_function_named_inner`: an END registered inside
        // this method closes over a frame that dies on return, so the return
        // path freezes its capture against this frame's final env.
        let end_phaser_mark_before = self.end_phaser_capture_mark();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        // A method gets a fresh, writable `$_` (Any) — it does NOT inherit the
        // caller's topic, so a caller `given`/`with`/`for` that marked `_`
        // readonly must not leak that mark into the method (`given 'x' { $o.m }`
        // where `method m { $_ = ... }`). Clear it here, journaled by the
        // frame's readonly scope so it is restored on return; an explicit
        // (readonly) `$_` parameter re-marks it during param binding.
        self.unmark_readonly_topic();

        // ADR-0035 slice 2: a body that reads `CALLER::`/`callframe()` needs a
        // caller-env frame pushed, mirroring `call_compiled_function_named_inner`
        // on the sub side. Gated on `cc.uses_callframe` (already computed
        // correctly for methods by the shared compiler) so the overwhelming
        // majority of method calls, which never observe their caller, pay only
        // this one boolean test. Pushed BEFORE the scoped-overlay install below
        // so the pushed entry captures the caller's (unoverlaid) env.
        let pushed_caller = cc.uses_callframe;
        if pushed_caller {
            self.push_caller_env_in_call_frame();
        }

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller, the method's frame root, so its
        // `self`/`?CLASS`/param/attr/local env setup writes land in a fresh map
        // instead of forking the caller env. `frame.saved_env` holds the caller
        // for restoration; the can_skip_merge path drops the overlay via
        // set_env(saved_env) and the merge path's `merge_method_env` iterates
        // it overlay-only (the method's own writes). A closure the method
        // creates captures up to this root (`layered_capture`), never the
        // caller's env above it (#12882).
        // A method declared at program scope links the root past its callers
        // (ADR-12529).
        let links_frame = cc.links_frame_to_unit();
        {
            let parent = self.env().clone();
            let mut frame = crate::env::Env::scoped_child(parent);
            // A method owns its `$/` (`env::routine_owned`).
            frame.mark_owns_routine_state();
            self.set_env(frame);
        }

        // Clear var_bindings so attribute aliases from outer interpreter-level
        // method calls don't leak into compiled method locals (e.g. `x → !x`
        // from forward_resolved_delegation shadowing a local parameter `x`).
        let saved_var_bindings = self.take_var_bindings();

        // The owner's role-ness is asked here once per call and handed to the
        // frame, so the body's attribute accesses never ask it again.
        let owner_is_role = self.is_role(owner_class);
        self.push_method_class_sym(owner_sym, Some(owner_is_role));
        // Keep the method name and invocant available to native deferral
        // fallbacks when this body can invoke a dispatcher builtin. The outer
        // call path owns the method dispatch frame, including an empty boundary
        // when there is no next candidate.
        if cc.uses_dispatcher {
            self.push_method_samewith_context(
                receiver_class_name,
                method_name,
                &args,
                Some(base.clone()),
            );
        }

        // Detect role context: use the pre-computed role_origin stored on the
        // MethodDef (set during role composition) instead of expensive fingerprint
        // matching on every call.
        let role_context = if owner_is_role {
            Some(owner_class.to_string())
        } else {
            method_def
                .original_role
                .as_ref()
                .or(method_def.role_origin.as_ref())
                .cloned()
        };

        // Set current_package so class-scoped subs are found during method execution.
        // Only change package if the class has subs declared in its body.
        //
        // Keyed on `owner_class` (the class/role that LEXICALLY declares this
        // method body), not `receiver_class_name` (self's dynamic class): a
        // method's lexical scope is fixed by where it is written, not by what
        // it is called on. Using the receiver broke an INHERITED method whose
        // own body declares a recursive nested `sub` (e.g. Tarjan's SCC
        // algorithm): calling it through a subclass that happens to declare
        // its own, unrelated class-body `sub` flipped `current_package` to
        // the subclass for the whole call, and the nested sub's own
        // self-recursive call -- looked up under that now-mismatched package
        // -- died "Unknown function: <name>" (#9008). `owner_class` still
        // equals `receiver_class_name` whenever the method is not inherited,
        // so this is a no-op for the common case and for the scenario
        // `t/oo/method/class-body-use-import-visible-in-method.t` pins
        // (#8883, no inheritance involved there either).
        let saved_package = self.current_package_sym();
        if let Some(package) = self.method_body_package(owner_class, owner_sym, method_def) {
            self.set_current_package_sym(package);
        }

        // In Raku, methods do NOT set $_ to the invocant by default.
        // $_ in a method body is Any unless the invocant is explicitly named $_
        // (e.g. `method foo ($_: ) { ... }`). The invocant binding loop below
        // will set $_ back to self if the invocant param is named "_".
        // The method's frame root answers both (`env::routine_owned`).

        // Assign a unique callable ID for this method invocation so that
        // non-local returns from blocks defined inside this method can target it.
        let method_callable_id = crate::value::next_instance_id();
        self.env_mut().set_callable_id(method_callable_id as i64);

        // Role param bindings (see `install_method_role_bindings`).
        self.install_method_role_bindings(
            receiver_class_name,
            owner_class,
            method_name,
            method_def,
        );
        // A method can carry its defining lexical environment either from an
        // `.^add_method` closure literal or from a class declared in a routine.
        // Install these true lexical captures before parameter binding.
        if let Some(captured) = &method_def.captured_env {
            let dynamic_add_method = captured
                .get("__mutsu_declared_method_capture")
                .is_some_and(|value| value.as_int() == Some(2));
            let env = self.env_mut();
            let authoritative = captured.contains_key_sym(crate::symbol::Symbol::intern(
                "__mutsu_declared_method_capture",
            ));
            for (sym, val) in captured.iter() {
                // A dynamic variable (`$*FLAG`) is the CALLER's, never the
                // declaring scope's: a snapshot of it taken when the method
                // was defined must not hide the binding the call runs under.
                if !sym.with_str(|name| {
                    matches!(
                        name,
                        "self" | "?CLASS" | "?ROLE" | "__mutsu_declared_method_capture"
                    ) || name.starts_with('*')
                        || name.get(1..).is_some_and(|rest| rest.starts_with('*'))
                }) && (authoritative || !env.contains_key_sym(*sym))
                    && captured_name_reachable(cc, method_def, *sym)
                {
                    env.insert_sym(*sym, val.clone());
                }
            }
            if dynamic_add_method {
                env.insert("__mutsu_declared_method_capture".to_string(), Value::int(2));
            }
        }
        // A composed role's type parameters are more specific than a lexical
        // capture with the same spelling. Re-apply them after the captured
        // environment: `role R[&limit]` must keep its `&limit` callable even
        // when the callable argument closes over an outer `$limit`, whose
        // capture metadata otherwise supplies a same-named nil entry.
        self.apply_method_role_bindings(method_def);

        // Skip invocant param, bind remaining
        let mut skipped_invocant = false;
        for (idx, param_name) in method_def.params.iter().enumerate() {
            let is_invocant = method_def
                .param_defs
                .get(idx)
                .map(|pd| pd.is_invocant)
                .unwrap_or(false);
            if is_invocant {
                skipped_invocant = true;
                if let Some(pd) = method_def.param_defs.get(idx) {
                    // `method m(::T Foo:D: ...)` captures the invocant's type
                    // AND constrains it, so the capture is bound first and the
                    // nominal check below still runs (#7984).
                    if let Some(captured_name) = pd.captured_type_name() {
                        loan_env!(self, bind_type_capture(captured_name, &base));
                    }
                }
                if let Some(pd) = method_def.param_defs.get(idx)
                    && let Some(constraint) = &pd.type_constraint
                {
                    if crate::qualified::is_type_capture(constraint) {
                        // `::?CLASS` / `::?ROLE` / `::(expr)`: bound above, not a
                        // nominal constraint.
                    } else {
                        let coercion_target = if let Some(open) = constraint.find('(') {
                            if constraint.ends_with(')') && open > 0 {
                                Some(&constraint[..open])
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        let expected = coercion_target.unwrap_or(constraint.as_str());
                        if coercion_target.is_some() {
                            let mut candidate = loan_env!(
                                self,
                                try_coerce_value_for_constraint(constraint, base.clone())
                            )
                            .unwrap_or_else(|_| base.clone());
                            if !self.type_matches_value(expected, &candidate)
                                && let Ok(coerced) = self.try_compiled_method_or_interpret(
                                    base.clone(),
                                    expected,
                                    vec![],
                                )
                            {
                                candidate = coerced;
                            }
                            if self.type_matches_value(expected, &candidate) {
                                base = candidate;
                            }
                        } else if !self.type_matches_value(constraint, &base)
                            && let Ok(coerced) = loan_env!(
                                self,
                                try_coerce_value_for_constraint(constraint, base.clone())
                            )
                        {
                            base = coerced;
                        }
                        if !self.type_matches_value(expected, &base) {
                            self.restore_var_bindings(saved_var_bindings);
                            if cc.uses_dispatcher {
                                self.pop_method_samewith_context();
                            }
                            self.pop_method_class();
                            self.set_current_package_sym(saved_package);
                            self.stack.truncate(saved_stack_depth);
                            if pushed_caller {
                                self.pop_caller_env();
                            }
                            let frame = self.pop_call_frame();
                            *self.env_mut() = frame.saved_env;
                            // :D/:U smiley mismatch → X::Parameter::InvalidConcreteness
                            if constraint.ends_with(":D") || constraint.ends_with(":U") {
                                let (base_type, _) =
                                    crate::runtime::types::strip_type_smiley(constraint);
                                let should_be_concrete = constraint.ends_with(":D");
                                return Err(RuntimeError::parameter_invalid_concreteness(
                                    base_type,
                                    base_type,
                                    method_name,
                                    &format!("${}", param_name),
                                    should_be_concrete,
                                    pd.is_invocant,
                                ));
                            }
                            return Err(RuntimeError::typecheck_binding_parameter(
                                param_name,
                                constraint,
                                crate::runtime::value_type_name(&base),
                                None,
                            ));
                        }
                    }
                }
                // ADR-0067 slice 3b: a raw invocant parameter binds the
                // caller's container, not a copy of its contents, so
                // `method m($s is raw:) { $s = 7 }` writes the caller's
                // variable. `base` itself stays the plain value — it is what
                // `self`, the attribute seeding and the dispatch frame use.
                let arrival = self
                    .take_raw_invocant_arrival(method_name, method_def.param_defs.get(idx))
                    .or_else(|| {
                        self.take_implicit_self_invocant_arrival(
                            method_name,
                            method_def.param_defs.get(idx),
                        )
                    });
                // A location either arrived boxed from the VM's gate, or `base`
                // is itself the caller's container (the `$a.m = v` lvalue path
                // hands one straight in).
                let bound_to_container = arrival.is_some() || base.is_container_ref();
                let invocant_value = arrival.unwrap_or_else(|| base.clone());
                // A sigilless invocant (`sub (\obj)` installed by `^add_method`)
                // binds its term key (ADR-12441).
                let storage_name = method_def
                    .param_defs
                    .get(idx)
                    .filter(|pd| pd.name == *param_name)
                    .map_or_else(|| param_name.clone(), |pd| pd.storage_name().into_owned());
                if !is_fixed_frame_name(&storage_name) {
                    self.env_mut().insert(storage_name.clone(), invocant_value);
                }
                // (A raw `$self` invocant already bound the caller's container
                // under that key above; rebinding the plain value here would
                // cut `$self = ...` off from the caller's variable.)
                if !bound_to_container
                    && method_def
                        .param_defs
                        .get(idx)
                        .is_some_and(|pd| pd.declares_self_lexical())
                {
                    self.env_mut()
                        .insert(crate::env::LEX_SELF.to_string(), base.clone());
                }
                // No container arrived: the invocant was an immutable value with
                // no location (`(1, 2)[0]`, `42`, `$a + 1`). raku refuses the
                // body's write to it -- "Cannot modify an immutable Int (1)" --
                // where mutsu bound it by value and dropped the write silently.
                // The same readonly marker a raw PARAMETER gets
                // (`bind_function_args_values`, which already produces exactly
                // that diagnostic for `sub g(\S) { S = 7 }; g(5)`) makes the
                // invocant refuse identically.
                if let Some(pd) = method_def.param_defs.get(idx)
                    && crate::runtime::raw_invocant::param_is_raw_invocant(pd)
                    && pd.sigilless
                {
                    let key = crate::runtime::sigilless_readonly_key(&storage_name);
                    if bound_to_container {
                        self.env_mut().remove_sym(key);
                    } else {
                        self.env_mut().insert_sym_noting(key, Value::TRUE);
                    }
                }
                continue;
            }
        }
        // Without an explicit invocant parameter (the common case) the
        // signature is bound as declared: borrow it instead of cloning every
        // parameter name and `ParamDef` on each call.
        let (bind_params, bind_param_defs): (
            std::borrow::Cow<'_, [String]>,
            std::borrow::Cow<'_, [crate::ast::ParamDef]>,
        ) = if skipped_invocant {
            let is_invocant = |idx: usize| {
                method_def
                    .param_defs
                    .get(idx)
                    .is_some_and(|pd| pd.is_invocant)
            };
            // The invocant is the leading parameter (`::?CLASS:D:` of every
            // role method of a CArray): bind the rest as a borrowed slice
            // instead of cloning each `ParamDef` per call (#16215).
            let lead = (0..method_def.params.len())
                .take_while(|idx| is_invocant(*idx))
                .count();
            let all_leading = (lead..method_def.params.len()).all(|idx| !is_invocant(idx))
                && method_def.param_defs.len() >= method_def.params.len();
            if all_leading {
                (
                    std::borrow::Cow::Borrowed(&method_def.params[lead..]),
                    std::borrow::Cow::Borrowed(
                        &method_def.param_defs[lead..method_def.params.len()],
                    ),
                )
            } else {
                let params: Vec<String> = method_def
                    .params
                    .iter()
                    .enumerate()
                    .filter(|(idx, _)| !is_invocant(*idx))
                    .map(|(_, name)| name.clone())
                    .collect();
                let defs: Vec<crate::ast::ParamDef> = (0..method_def.params.len())
                    .filter(|idx| !is_invocant(*idx))
                    .filter_map(|idx| method_def.param_defs.get(idx).cloned())
                    .collect();
                (params.into(), defs.into())
            }
        } else {
            (
                std::borrow::Cow::Borrowed(&method_def.params[..]),
                std::borrow::Cow::Borrowed(
                    &method_def.param_defs
                        [..method_def.params.len().min(method_def.param_defs.len())],
                ),
            )
        };

        // Bind attributes
        for (attr_name, attr_val) in attributes {
            let attr_name = attr_name.as_str();
            // Skip class-qualified private attribute entries (ClassName\0attrName)
            // — these are handled below when binding $!attr for the owner class.
            if attr_name.contains('\0') {
                continue;
            }
            if let Some(actual_attr) = attr_name.strip_prefix(ATTR_ALIAS_META_PREFIX) {
                if let ValueView::Str(source_name) = attr_val.view() {
                    // A sigilless attribute is in play — enable the cell-direct
                    // routing's alias-table lookup (Phase 3 Stage 2c (ii)), and
                    // spoil the JIT's inline local read, which skips exactly
                    // that lookup (`vm_jit::LOCAL_READ_SPOILERS`).
                    self.threads.sigilless_attrs_active = true;
                    crate::vm::vm_jit::note_local_read_spoiler();
                    // Set up bidirectional alias: !x ↔ alias_name
                    let attr_var = format!("!{actual_attr}");
                    self.env_mut().insert_sym_noting(
                        crate::runtime::sigilless_alias_key(&attr_var),
                        Value::str(source_name.to_string()),
                    );
                    self.env_mut().insert_sym_noting(
                        crate::runtime::sigilless_readonly_key(&attr_var),
                        Value::FALSE,
                    );
                    // Reverse alias: alias_name → !attr so writing to $x updates $!x
                    self.env_mut().insert_sym_noting(
                        crate::runtime::sigilless_alias_key(&source_name),
                        Value::str(attr_var),
                    );
                    self.env_mut().insert_sym_noting(
                        crate::runtime::sigilless_readonly_key(&source_name),
                        Value::FALSE,
                    );
                    // Also set up the alias name with the current attribute value
                    if let Some(attr_value) = attributes.get(actual_attr) {
                        self.env_mut()
                            .insert(source_name.to_string(), attr_value.clone());
                    }
                }
                continue;
            }
            // Phase 3 Stage 2c (iii-b): the attribute value env copies (`!attr`,
            // `.attr`, `@!attr`, …) that materialize used to insert here are no
            // longer needed — every read of `$!x`/`$.x`/`@!a`/`%!h`/sigilless `$x`
            // goes cell-direct (Stage 2a/2b/2c (ii)), and every bind seats the
            // cell (ADR-18356). Only the sigilless alias table above is still
            // required.
        }

        // Register `is default(...)` values for attribute variables so that
        // .VAR.default returns the correct value inside methods. Gated on the
        // owner or receiver class declaring ANY attribute default: otherwise
        // every call paid two lookups per attribute of the receiver for
        // nothing, which is what a class with many attributes (#9494) felt.
        let (owner_has_defaults, receiver_has_defaults) =
            self.attr_default_classes(owner_class, receiver_class_name);
        if owner_has_defaults || receiver_has_defaults {
            for attr_name in attributes.keys() {
                let attr_name = attr_name.as_str();
                if attr_name.contains('\0') || attr_name.starts_with(ATTR_ALIAS_META_PREFIX) {
                    continue;
                }
                // Check both the owner class and the receiver class for defaults
                let default_val = self.attr_default_for_call(
                    owner_class,
                    receiver_class_name,
                    (owner_has_defaults, receiver_has_defaults),
                    attr_name,
                );
                if let Some(def) = default_val {
                    self.set_attr_var_defaults(attr_name, def);
                }
            }
        }

        // Make class-body `my` statics visible to every read path in the body.
        // Injected BEFORE parameter binding so a parameter of the same name still
        // wins (it simply overwrites), while an unrelated CALLER lexical that
        // happens to share the name does not: the statics are the class's own
        // lexicals and the method body is inside their scope.
        self.inject_class_body_statics_sym(owner_sym);
        // A role method's own composition bindings (its type parameters and the
        // role-body lexicals persisted with the composition) are closer than the
        // statics of a class the role is declared in: `class C { my \iv = 1;
        // my role R[\n] { my \iv = n * 2; method v { iv } } }` reads the
        // role's `iv`. The enclosing class's statics were just injected over
        // them, so apply the bindings again.
        if owner_is_role {
            self.apply_method_role_bindings(method_def);
        }

        // Push routine_stack so &?ROUTINE can find the current method. This is
        // done BEFORE parameter binding: a parameter default (`:$n = helper()`)
        // is evaluated in the method's lexical scope, so a routine the method's
        // own file imported must resolve through this frame's lexical package.
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
                // A role method composed into a mixin sees the mixin type as
                // `::?CLASS` (the binding `dispatch_mixin_method_call` stages).
                class: Some(
                    method_def
                        .role_param_bindings
                        .as_deref()
                        .and_then(|b| b.iter().find(|(n, _)| n == "?CLASS"))
                        .map_or_else(|| Value::package(owner_sym), |(_, v)| v.clone()),
                ),
                role: role_context.as_deref().map(crate::symbol::Symbol::intern),
            },
            false,
        );

        // Bind method parameters
        self.pending_skip_constraint_recheck = skip_constraint_recheck;
        let rw_bindings = match loan_env!(
            self,
            bind_method_function_args_values(&bind_param_defs, &bind_params, &args)
        ) {
            Ok(bindings) => bindings,
            Err(e) => {
                self.pop_routine();
                self.restore_var_bindings(saved_var_bindings);
                if cc.uses_dispatcher {
                    self.pop_method_samewith_context();
                }
                self.pop_method_class();
                self.set_current_package_sym(saved_package);
                self.stack.truncate(saved_stack_depth);
                if pushed_caller {
                    self.pop_caller_env();
                }
                let frame = self.pop_call_frame();
                *self.env_mut() = frame.saved_env;
                return Err(e);
            }
        };
        self.arm_rw_param_rebinds(cc, &rw_bindings);
        // A method's capture holds its arguments' values: Rakudo's method
        // dispatch hands a `$` parameter's argument over decontainerized.
        self.request_call_capture(cc.uses_capture, Some(&base));
        self.finish_call_capture(&args);

        // Initialize locals from env. The parameters are bound, so a linked
        // frame is marked first: a local is never a caller's variable, and
        // the seed reads through the link (#12478).
        let marked_frame = links_frame && self.env().is_frame_root();
        if marked_frame {
            if self.code_is_foreign_unit(cc) {
                self.env_mut().mark_outer_is_foreign_unit();
            } else {
                self.env_mut().mark_outer_is_unit();
            }
        }
        self.locals.refill_slots(cc.locals.len());
        // The names were interned when the chunk was built (`locals_sym`), so the
        // per-call seed is an array read, not an interner round trip per local.
        for i in 0..cc.locals.len() {
            if let Some(sym) = cc.local_sym(i)
                && let Some(val) = self.env().get_sym(sym)
            {
                self.locals[i] = val.clone();
            }
        }
        // The fixed names are the frame record's, not env keys (ADR-14843).
        for (i, local_name) in cc.locals.iter().enumerate() {
            if matches!(local_name.as_str(), "self" | "?CLASS" | "?ROLE")
                && let Some(val) = self.frame_fixture_value(Symbol::intern(local_name))
            {
                self.locals[i] = val;
            }
        }
        // Phase 3 Stage 2c (i): an attributive parameter (`method m($!x)` /
        // `method m(@!a)`) binds straight to the attribute. The binding above
        // writes only env/locals; mirror it into `self`'s shared cell now so the
        // method body's cell-direct reads of `$!x` see the parameter value (not
        // the stale entry value) and the mutation is visible to every alias.
        self.mirror_attributive_params_to_cell(cc, &method_def.param_defs);
        // Install the declaring/composing type's state scope for initialization,
        // loads and writeback. Inherited methods retain that owner; a separate
        // role composition owns separate state. Restore the caller after sync.
        let saved_state_scope = self.lexicals.state_scope_id.take();
        self.scope_method_state(method_name, receiver_class_name, owner_sym);
        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            if let Some(val) = self.get_state_var(self.scoped_state_key(*key)) {
                self.locals[*slot] = val.clone();
            }
        }

        // The outer variables the body writes take the readonly state of the
        // frame the method was declared in, not of the caller (#11054).
        // Inside this call's readonly frame, so the caller's marks return.
        self.reconcile_captured_readonly_ex(
            method_def.captured_readonly.as_ref(),
            cc,
            method_def.is_rw,
        );

        // Execute bytecode
        let let_mark = self.let_saves_len();
        // A `sub` declared directly in this method's body is lexical to it --
        // see the matching `routine_snapshot` comment in
        // `call_compiled_method_fast`.
        let routine_snapshot = if cc.declares_inner_routines() {
            Some(self.snapshot_routine_registry())
        } else {
            None
        };
        let closures_created_before = self.closures_created;
        // A method body is its own topicalizer for a bare `when`/`default`: a
        // matching `when` sets the global `when_matched` flag, which must not
        // leak to an enclosing `given`/`with` body (see vm_call_light.rs for the
        // full rationale). Reset for the body; restore the caller's value below.
        let saved_when_matched = self.when_matched();
        // `use fatal` -- explicit, or implied by a caller's `try` -- is lexical
        // to where this method was declared, not the caller's state (#11391).
        let saved_lexical_fatal_mode =
            std::mem::replace(&mut self.module.lexical_fatal_mode, cc.method_fatal_pragma);
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        let polls = crate::vm::vm_poll::DispatchPolls::enter(cc);
        let frame_caller_code = self.current_code;
        while ip < cc.ops.len() {
            // JIT entry (ADR-0004 J2): same hook as vm_call_light.rs — at body
            // start, run the whole body natively when the chunk is hot and
            // Tier A-compilable; the native outcome threads through the same
            // match arms below.
            let step = if ip == 0
                && let Some(r) = crate::vm::vm_jit::try_enter(self, cc, compiled_fns, polls)
            {
                ip = cc.ops.len();
                r
            } else {
                self.exec_one_backedge_polled(cc, &mut ip, compiled_fns, polls)
            };
            match step {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = crate::qualified::qualified(owner_sym, method_sym);
                    let matches_frame = if let Some(_target_id) = e.leave_callable_id() {
                        // Methods don't have callable IDs, so this won't match
                        false
                    } else if let Some(target_routine) = e.leave_routine() {
                        target_routine == routine_key.as_str()
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.control = None;
                        let ret_val = e.return_value.unwrap_or(Value::NIL);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val.clone());
                        self.resolve_frame_let_saves(let_mark, &ret_val);
                        result = Ok(());
                        break;
                    }
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
                Err(e)
                    if e.return_value.is_some()
                        && !e.is_yield_signal()
                        && !cc.lets_succeed_through(&e) =>
                {
                    // Non-local return: if the signal targets a specific callable,
                    // only catch it if this method is the target.
                    if let Some(target_id) = e.return_target_callable_id()
                        && target_id != method_callable_id
                    {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val.clone());
                    self.resolve_frame_let_saves(let_mark, &ret_val);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail() => {
                    if e.fail_passes_frame(Some(method_callable_id)) {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    fail_bypass = true;
                    let failure = self.fail_error_to_failure_value(&e);
                    loan_env!(self, restore_let_saves(let_mark));
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }
        self.current_code = frame_caller_code;

        // The return path reads values across frames.
        if marked_frame {
            self.env_mut().unmark_outer_is_unit();
        }
        // Restore the caller's `when_matched` — a bare `when` inside this method
        // body must not leak its match state to an enclosing given/with.
        self.set_when_matched(saved_when_matched);
        self.module.lexical_fatal_mode = saved_lexical_fatal_mode;

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::NIL)
            } else {
                // A body that leaves no value (an empty body) returns Nil, as the
                // fast path does; `$_` is always `Any` on method entry, so reading
                // it here made `method m(D:D:) {}` answer `Any`.
                Value::NIL
            }
        } else {
            Value::NIL
        };

        // Natural fall-through completion: resolve the body's own `let`/`temp`
        // saves against the value it produced. A method body is a block, so it
        // owns the saves its `let`/`temp` recorded; without this they leaked
        // into the caller's scope (#7646).
        if result.is_ok() && explicit_return.is_none() {
            self.resolve_frame_let_saves(let_mark, &ret_val);
        }

        self.stack.truncate(saved_stack_depth);

        // A `--> T` return constraint names a type CAPTURE bound by this call's
        // own signature, so it has to be resolved while the callee env is still
        // live. The sub paths already do this before popping the frame; the
        // method paths resolved it after the env had been restored, which left
        // the literal `T` as the expected type (#7984).
        let effective_return_spec = method_def
            .return_type
            .as_deref()
            .map(|spec| self.method_effective_return_spec(owner_class, spec));

        // Sync state variables back
        for (slot, key) in &cc.state_locals {
            let local_name = &cc.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            let scoped_key = self.scoped_state_key(*key);
            loan_env!(self, set_state_var(scoped_key, val));
        }
        self.lexicals.state_scope_id.set(saved_state_scope);

        // The instance's attribute cell is the only store of its attributes:
        // every write and every `:=` bind reaches it when it runs (ADR-18356),
        // so the exit has nothing to reconcile.
        if can_skip_merge {
            let method_var_bindings = self.take_var_bindings();
            let mut restored_bindings = saved_var_bindings;
            for (k, v) in method_var_bindings {
                restored_bindings.insert(k, v);
            }
            self.restore_var_bindings(restored_bindings);

            self.pop_routine();
            if cc.uses_dispatcher {
                self.pop_method_samewith_context();
            }
            self.pop_method_class();
        } else {
            // Sync locals back to env
            for (i, local_name) in cc.locals.iter().enumerate() {
                // The frame's record answers the fixed names (ADR-14843).
                if !local_name.is_empty()
                    && !is_fixed_frame_name(local_name)
                    && let Some(sym) = cc.local_sym(i)
                {
                    let __v = self.locals[i].clone();
                    self.env_mut().insert_sym_noting(sym, __v);
                }
            }

            let rw_writeback: Vec<(String, Value)> = rw_bindings
                .iter()
                .filter_map(|(param_name, source_name)| {
                    // A `|c` capture element read from a caller scalar
                    // (`capture_param_containers`) writes back the cell's
                    // final value.
                    if let Some((key, name)) =
                        crate::runtime::types::decode_capture_named_rw_param(param_name)
                    {
                        return self
                            .capture_rw_named(key, name)
                            .map(|val| (source_name.clone(), val));
                    }
                    if let Some((key, elem_idx, None)) =
                        crate::runtime::types::decode_slurpy_rw_param(param_name)
                    {
                        return self
                            .slurpy_rw_element(key, elem_idx)
                            .map(|val| (source_name.clone(), val));
                    }
                    // A param the body rebound (`$p := ...`) writes back its
                    // pre-rebind value (#10361).
                    self.rw_param_detached_by_name(cc, param_name)
                        .or_else(|| self.env().get(param_name).cloned())
                        .or_else(|| {
                            let qualified =
                                crate::qualified::qualified(owner_sym, Symbol::intern(param_name));
                            self.env().get(qualified.as_str()).cloned()
                        })
                        .map(|val| {
                            // A SIGILLESS param aliasing an `@`/`%` variable
                            // stores through that variable's own shape
                            // (`\c := @a; c = LIST` is `@a.STORE(LIST)`), so the
                            // writeback must not leave the caller holding the
                            // bare List the param's slot held. Gated on the
                            // parameter being sigilless, as in
                            // `apply_rw_bindings_to_env`: a plain `@`/`%` param
                            // already carries the caller's own container and
                            // re-coercing it would strip its metadata.
                            let val = if crate::runtime::utils::param_is_sigilless(param_name) {
                                crate::runtime::utils::shape_value_for_sigiled_target(
                                    source_name,
                                    &val,
                                )
                            } else {
                                val
                            };
                            (source_name.clone(), val)
                        })
                })
                .collect();

            // Snapshot the callee env for any END phaser registered during this
            // method, before `take_env` hands it to the merge. Gated on the
            // phaser count so an ordinary method call never pays the clone.
            let end_phaser_env =
                (self.end_phaser_capture_mark() > end_phaser_mark_before).then(|| self.clone_env());
            // Take sole ownership of caller + callee envs (pop the frame for the
            // saved caller env, take the live callee env) so merge_method_env can
            // mutate the caller env in place without a deep copy.
            if pushed_caller {
                self.pop_caller_env();
            }
            let frame = self.pop_call_frame();
            let current_env = self.take_env();
            // The frame's writes reach the caller through the variables' own
            // cells (ADR-12529, #18274 slice 3): what is left to carry is a
            // runtime-named write (`CALLER::LEXICAL::<$x> = v`) and a nested
            // method capture's plain value.
            let carried = if self.pending_runtime_name_writes.is_empty() {
                Vec::new()
            } else {
                // Callee-frame key predicate for the runtime-name carry: the method's params and locals,
                // the frame fixtures, the attribute twigil forms and sigilless
                // aliases, and env-only `my` declarations (no compiled slot, e.g. a
                // `my @x` declared in a `next unless my @x = ...` condition) — a
                // self-recursive method must not leak the callee's `my @x` back
                // into the caller's same-named `@x` (zef `!find-prereq-candidates`
                // `@needed`). The light-call merge already excludes these via
                // `declared_locals`.
                let class_body_static_names = self.lexicals.class_body_static_names.snapshot();
                // The type parameters of the roles the receiver class composes are
                // bound while an instance is constructed, so a class's own TWEAK /
                // BUILD frame sees them; they are not the caller's variables
                // (`Pk[Qos $qos]` and a caller's `my $qos` passed as `:$qos`).
                let class_role_param_names: Vec<String> = self
                    .registry()
                    .class_role_param_bindings
                    .get(receiver_class_name)
                    .map(|bindings| bindings.keys().cloned().collect())
                    .unwrap_or_default();
                let is_method_local = |s: &str| -> bool {
                    // `@_`/`%_` are rebound to the callee's own arguments by the
                    // slow bind path, so they are as frame-local as `_`: merging
                    // them back clobbered a caller's explicit `@_` parameter.
                    matches!(
                        s,
                        "self" | "__ANON_STATE#" | "?CLASS" | "?ROLE" | "_" | "@_" | "%_"
                    ) || method_def.params.iter().any(|p| p == s)
                        // An enum the method declares binds its keys in the method's
                        // scope; rakudo installs them in the method's package, which
                        // `record_package_enum_key` mirrors. Merged back they would
                        // shadow a same-named key of the caller's own enum (#16361).
                        || s.starts_with(crate::runtime::enum_bare_names::ENUM_BARE_PREFIX)
                        || bound_by_sub_signature(&method_def.param_defs, s)
                        || bound_by_type_capture(&method_def.param_defs, s)
                        || cc.locals.iter().any(|l| !l.is_empty() && l == s)
                        || cc.env_only_decls.iter().any(|n| n == s)
                        || dynamic_decl_local(&cc.locals, s)
                        // A composed role's type parameter (`T` of `role R[::T]` /
                        // `role R[$T]`) is bound into the callee env for this
                        // call only. Merging it back would clobber the caller's
                        // own `$T` when the caller is another instance of the
                        // same role composed with a different argument and the
                        // call is a recursion into it (HTML::Tag's `render`).
                        || method_def
                            .role_param_bindings
                            .as_deref()
                            .is_some_and(|b| b.iter().any(|(n, _)| n == s))
                        || class_role_param_names.iter().any(|n| n == s)
                        || class_body_static_names
                            .get(owner_class)
                            .is_some_and(|names| names.contains(s))
                        || attr_twigil_local(attributes, s)
                        || (has_attr_aliases && attr_alias_local(attributes, s))
                        // Sigilless alias/readonly markers for a method parameter
                        // are frame-local bookkeeping. A raw parameter may reuse a
                        // caller's name, so merging `__mutsu_sigilless_alias::p`
                        // back would overwrite the caller's alias chain with the
                        // callee's immediate argument name.
                        || s.strip_prefix("__mutsu_sigilless_alias::")
                            .is_some_and(|name| method_def.param_defs.iter().any(|pd| pd.storage_name() == name))
                        || s.strip_prefix("__mutsu_sigilless_readonly::")
                            .is_some_and(|name| method_def.param_defs.iter().any(|pd| pd.storage_name() == name))
                };
                self.carried_runtime_name_writes(&current_env, &is_method_local)
            };
            self.persist_nested_capture_writes(owner_sym, method_def, &current_env);
            drop(current_env);
            let mut merged_env = frame.saved_env;
            for (name, v) in carried {
                merged_env.insert(name, v);
            }
            // The caller only needs an env->locals re-sync when this method
            // wrote back an `is rw` param.
            self.dispatch.method_dispatch_pure = rw_writeback.is_empty();

            for (source_name, val) in &rw_writeback {
                // A named `:$scalar` param bound from an `@`/`%` SOURCE THAT IS
                // ITSELF AN ATTRIBUTE EXPRESSION (`:%!plugin-config`, `:$.foo`)
                // encodes that attribute's twigil form as `source_name` --
                // e.g. `RouteHandler.copy-adding(..., :%!plugin-config, ...)`
                // (Cro::HTTP::Router) makes `source_name` == "%!plugin-config",
                // RouteSet's OWN attribute-twigil key, not a lexical belonging
                // to `copy-adding`'s caller. Writing it into the caller's env
                // verbatim plants a pseudo-key under an attribute's twigil
                // spelling in a caller's env, which an unrelated later method
                // call sharing that env would read as its own attribute. The shared `ContainerRef` cell this
                // writeback exists for already keeps content mutations visible
                // without this insert (see `named_scalar_container_share_eligible`
                // in `bind_function_args_values`); skip it for attribute-shaped
                // sources.
                if is_attr_twigil_shaped(source_name) {
                    continue;
                }
                merged_env.insert(source_name.clone(), val.clone());
                Self::mirror_twigil_alias_writeback(&mut merged_env, source_name, val);
            }
            // Slice F: record the caller-source names this `is rw` method writeback
            // touched (the value now lives in `merged_env`, which becomes
            // `self.env` below) so the CallMethod / CallMethodMut op — which holds
            // the caller's `code` — can write each value straight through to the
            // caller's local slot, dropping the dependency on the reverse
            // `sync_locals_from_env` pull. See `apply_pending_rw_writeback`.
            if !rw_writeback.is_empty() {
                self.pending_rw_writeback_sources
                    .extend(rw_writeback.iter().map(|(source, _)| source.clone()));
            }

            let method_var_bindings = self.take_var_bindings();
            let mut restored_bindings = saved_var_bindings;
            for (k, v) in method_var_bindings {
                restored_bindings.insert(k, v);
            }
            self.restore_var_bindings(restored_bindings);

            self.pop_routine();
            if cc.uses_dispatcher {
                self.pop_method_samewith_context();
            }
            self.pop_method_class();
            self.set_current_package_sym(saved_package);
            // A name the callee env held that the merge did not carry into the
            // caller is one this frame takes with it, so the phaser's captured
            // copy is its last surviving binding; names that merged through stay
            // unfrozen and keep reading live.
            if let Some(current) = end_phaser_env {
                let dying: crate::runtime::NameSet = current
                    .keys()
                    .filter(|k| !merged_env.contains_key_sym(**k))
                    .copied()
                    .collect();
                self.update_end_phaser_envs(end_phaser_mark_before, &current, &dying);
            }
            *self.env_mut() = merged_env;
        }

        if can_skip_merge {
            // No env writes possible -> the caller's slots stay coherent (pure).
            self.dispatch.method_dispatch_pure = true;
            self.set_current_package_sym(saved_package);
            if pushed_caller {
                self.pop_caller_env();
            }
            let frame = self.pop_call_frame();
            *self.env_mut() = frame.saved_env;
        }

        let final_result = match result {
            Ok(()) => Ok(explicit_return.unwrap_or(ret_val)),
            Err(e) => Err(e),
        };

        // Apply return type spec (e.g. `--> 5` returns literal 5 from empty body)
        // A `fail` hands its Failure back as is: a definite `--> True` /
        // `--> 42` return value is what a routine that COMPLETES yields.
        let final_result = if fail_bypass {
            final_result
        } else {
            self.finalize_method_result(cc, final_result, effective_return_spec.as_deref())
        };

        // Restore the routine registry (removing this body's lexical routines)
        // unless an inner routine escaped via the return value, or a closure
        // literal created during the call may have escaped via a side channel
        // (e.g. handed to `.tap`) -- see `closures_created`.
        if let Some(snapshot) = routine_snapshot {
            let closure_escaped = self.closures_created != closures_created_before;
            let skip_restore = closure_escaped
                || matches!(&final_result, Ok(v) if Self::return_value_escapes_routine(v));
            if !skip_restore {
                self.restore_routine_registry(snapshot);
            }
        }

        // A result that is the invocant itself shares its live cell.
        final_result.map(|v| (Self::method_frame_result_sharing(&base, v), None))
    }

    /// Whether `owner_class` and `receiver_class` each declare any attribute
    /// `is default(...)` (evaluated, or deferred from a parametric role).
    // Cost: O(1) expected (four hash probes).
    pub(super) fn attr_default_classes(
        &self,
        owner_class: &str,
        receiver_class: &str,
    ) -> (bool, bool) {
        let registry = self.registry();
        let has = |class: &str| {
            registry.class_attribute_defaults.has_class(class)
                || registry.class_attribute_default_exprs.has_class(class)
        };
        (has(owner_class), has(receiver_class))
    }

    /// `attr_name`'s `is default(...)` for a method call: the owner class's,
    /// else the receiver class's. `has_defaults` is
    /// [`Self::attr_default_classes`] for the same pair, so a class with no
    /// defaults at all is not asked per attribute.
    // Cost: O(1) expected, plus evaluating a deferred role default expression.
    fn attr_default_for_call(
        &mut self,
        owner_class: &str,
        receiver_class: &str,
        has_defaults: (bool, bool),
        attr_name: &str,
    ) -> Option<Value> {
        let from_owner = if has_defaults.0 {
            self.class_attribute_default_with_role_fallback(owner_class, attr_name)
        } else {
            None
        };
        from_owner.or_else(|| {
            if has_defaults.1 {
                self.class_attribute_default_with_role_fallback(receiver_class, attr_name)
            } else {
                None
            }
        })
    }

    /// Phase 3 Stage 2c (i): mirror attributive parameters (`$!x`/`@!a`/`%!h`)
    /// into `self`'s shared attribute cell right after argument binding. The
    /// parameter binding writes the value to env/locals keyed by the attribute
    /// twigil name (`!x`, `@!a`, …); this pushes it through to the cell so that
    /// cell-direct body reads and cross-frame aliases observe it.
    ///
    /// Shared with a plain `sub`'s call path (`call_compiled_function_named_inner`):
    /// a `sub` nested directly in a method body may bind an attributive
    /// parameter too (#8452) — it closes over the same `self` the method's own
    /// attribute cell belongs to, so the mirror is a no-op (`write_self_attr_cell`
    /// finds no `self`) for any other `sub`, whose parameters the parser already
    /// rejects at parse time (`reject_attr_params_in_sub`).
    pub(super) fn mirror_attributive_params_to_cell(
        &self,
        code: &CompiledCode,
        param_defs: &[crate::ast::ParamDef],
    ) {
        for pd in param_defs {
            if pd.is_invocant {
                continue;
            }
            if Self::attr_twigil_base(&pd.name).is_some() {
                self.mirror_attr_value_to_cell_by_name(code, &pd.name);
            }
        }
    }

    /// Mark a compiled method's `$` scalar parameters read-only, mirroring
    /// `bind_function_args_values`. Routine params are read-only by default
    /// unless `is rw`/`is copy`/`is raw`; `@`/`%` params and attribute-binding
    /// params (`$!x`/`$.x`) stay writable. Sigilless (`\x`) params are raw
    /// aliases. The light-frame fast path skips the slow path's readonly
    /// snapshot, so each newly-marked name is recorded on the current call frame
    /// and dropped in `pop_call_frame`.
    pub(super) fn mark_fast_method_params_readonly(
        &mut self,
        method_def: &crate::runtime::MethodDef,
    ) {
        // Interned once per def, not per call (#10961).
        let name_syms = method_def.param_def_name_syms();
        for (pd, &sym) in method_def.param_defs.iter().zip(name_syms) {
            if pd.name.is_empty()
                || pd.is_type_only()
                || pd.is_parens()
                || pd.sigilless
                || pd.is_invocant
                || pd.name.starts_with('!')
                || pd.name.starts_with('.')
                || pd.name.starts_with('@')
                || pd.name.starts_with('%')
            {
                continue;
            }
            if pd
                .traits
                .iter()
                .any(|t| t == "rw" || t == "copy" || t == "raw")
            {
                // A writable param must shadow a caller's same-named read-only
                // variable (the readonly set is keyed by bare name, shared across
                // frames). Drop the mark and record it so `pop_call_frame`
                // restores the caller's state.
                if self.is_readonly_sym(sym) {
                    // The frame's readonly journal records the removal, so
                    // `pop_call_frame` restores the caller's mark.
                    self.unmark_readonly_sym(sym);
                }
                continue;
            }
            if self.is_readonly_sym(sym) {
                continue; // already read-only (e.g. caller-owned same name)
            }
            // Journaled — dropped again at `pop_call_frame`.
            self.mark_readonly_sym(sym);
        }
    }

    /// Whether every explicit invocant constraint of `method_def` is trivially
    /// met by `base`, so the fast binder may skip the nominal check the full
    /// binder runs: a plain defined instance against `Mu`, `::?CLASS` (the
    /// class the method was resolved on) or its own class / the owner (with or
    /// without `:D`). An invocant that captures a type, a `:U` / `:_`
    /// smiley, a coercion or any other type keeps the full binder (which
    /// also raises the concreteness / type-check errors).
    // Cost: O(i), i = the method's invocant parameters (one).
    pub(super) fn fast_invocant_constraints_trivial(
        method_def: &crate::runtime::MethodDef,
        base: &Value,
        owner_class: &str,
        role_blind: bool,
    ) -> bool {
        // A role-mixed instance (`CArray[num64]`) is as defined as the
        // instance it wraps, but its type is no class name: only `Mu` and
        // `::?CLASS` accept it without the binder.
        let is_mixin = matches!(base.view(), ValueView::Mixin(inner, _) if matches!(inner.view(), ValueView::Instance { .. }));
        let class_name = match base.view() {
            ValueView::Instance { class_name, .. } => class_name,
            ValueView::Mixin(inner, _) => match inner.view() {
                ValueView::Instance { class_name, .. } => class_name,
                _ => return false,
            },
            // A `Proxy` subclass instance is as defined as any instance.
            ValueView::Proxy {
                subclass: Some((class_name, _)),
                ..
            } => *class_name,
            _ => return false,
        };
        method_def
            .param_defs
            .iter()
            .filter(|pd| pd.is_invocant)
            .all(|pd| {
                // A `::?CLASS` capture is harmless to a body that cannot see it.
                if pd.type_capture.is_some() && !(role_blind && pd.captures_class_fixture()) {
                    return false;
                }
                let Some(constraint) = pd.type_constraint.as_deref() else {
                    return true;
                };
                let (name, smiley) = crate::runtime::types::strip_type_smiley(constraint);
                if !matches!(smiley, None | Some(":D")) {
                    return false;
                }
                name == "Mu"
                    || name == "::?CLASS"
                    || (!is_mixin && (name == owner_class || name == class_name.as_str()))
            })
    }

    /// Fast path for read-only compiled methods. Bypasses all env_mut() calls
    /// to avoid the ~12μs Arc::make_mut deep clone. Populates locals directly
    /// from source data (the invocant's attribute cell, args, special
    /// variables). Attribute reads go through `base`'s live cell (no per-call
    /// `to_map()` snapshot — the whole-map clone dominated method-heavy
    /// profiles); the returned map is always `None`: the cell is the only
    /// attribute store (ADR-18356).
    #[allow(clippy::too_many_arguments)]
    /// Whether the fast path writes the bound parameter `param_name` into the
    /// frame env: not a plain scalar parameter the body reads through its slot
    /// only. `names_by_string` is a body that looks a lexical up by a run-time
    /// name (`EVAL`, `::('$x')`, `MY::`): it keeps every parameter in the env.
    /// A parameter whose name the env already holds (a class-body static
    /// injected for the method) is written too, so it shadows that entry as
    /// the slot does. The invocant is the frame record's (ADR-14843).
    // Cost: O(q), q = the method's parameters.
    fn fast_param_in_env(
        env: &crate::env::Env,
        param_name: &str,
        param_sym: Symbol,
        method_def: &crate::runtime::MethodDef,
        cc: &crate::opcode::CompiledCode,
        names_by_string: bool,
    ) -> bool {
        if param_sym.is_frame_fixture() {
            return false;
        }
        let slot_only = method_def
            .param_storage_names()
            .iter()
            .position(|n| n.as_str() == param_name)
            .and_then(|i| method_def.param_defs.get(i))
            .is_some_and(|pd| {
                !pd.sigilless
                    && !pd.is_invocant
                    && !pd.named
                    && !pd.slurpy
                    && pd.name.starts_with(|c: char| c.is_alphabetic())
                    && !pd
                        .traits
                        .iter()
                        .any(|t| t == "rw" || t == "copy" || t == "raw")
            });
        // A parameter some by-name reader of this frame reads (`needs_env_sync`:
        // a grammar's token code block, an attribute default or a nested
        // routine declared in the body) is in the env as well (#15863).
        !(slot_only
            && !names_by_string
            && !cc.local_needs_env_sync(param_name)
            && !env.contains_key_sym(param_sym))
    }

    /// Insert the fast path's bound parameters into the current frame env,
    /// except those [`Self::fast_param_in_env`] leaves to their slots. A frame
    /// with nothing to write never asks for the env mutably: the fresh
    /// overlay shares the process-wide empty tier until its first insert, so
    /// that ask alone copies it (one allocation per call).
    ///
    /// Symbol-keyed so a call does not allocate a `String` per parameter just
    /// to hand it to `Symbol::intern`. A parameter name IS name-derived, so
    /// this must still run [`crate::env::note_env_key`] -- a placeholder
    /// parameter (`$^a`) is stored under a `^`-prefixed key, and dropping that
    /// latch would leave `placeholder_var_possible()` reading false while such
    /// a key is live.
    // Cost: O(p * q), p = bound parameters, q = the method's parameters.
    pub(super) fn insert_fast_param_values_by_name(
        &mut self,
        param_values: &super::vm_param_values::ParamValues<'_>,
        method_def: &crate::runtime::MethodDef,
        cc: &crate::opcode::CompiledCode,
        names_by_string: bool,
    ) {
        let env = self.env();
        let any = param_values.iter().any(|(name, sym, _)| {
            Self::fast_param_in_env(env, name, *sym, method_def, cc, names_by_string)
        });
        if !any {
            return;
        }
        let env = self.env_mut();
        env.reserve(param_values.len());
        for (param_name, param_sym, param_val) in param_values.iter() {
            if !Self::fast_param_in_env(
                env,
                param_name,
                *param_sym,
                method_def,
                cc,
                names_by_string,
            ) {
                continue;
            }
            crate::env::note_env_key(param_name);
            env.insert_sym(*param_sym, param_val.clone());
        }
    }

    /// The fast twin of [`Self::call_compiled_method`]; it enters the
    /// method's compilation unit the same way.
    // Cost: O(c) to leave a lexical type scope when this body declares one,
    // c = lexical classes it declared, plus the body.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn call_compiled_method_fast(
        &mut self,
        receiver_class_name: &str,
        owner_sym: Symbol,
        method_sym: Symbol,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        args: Vec<Value>,
        base: Value,
        compiled_fns: &CompiledFns,
        hints: MethodFastHints,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        let unit = self.unit_of_source_sym(cc.source_file);
        let saved_unit = std::mem::replace(&mut self.current_unit, unit);
        let scopes_lexical_types = !cc.class_decl_plans.is_empty();
        if scopes_lexical_types {
            self.push_lexical_class_scope();
        }
        let result = self.call_compiled_method_fast_in_unit(
            receiver_class_name,
            owner_sym,
            method_sym,
            method_def,
            cc,
            args,
            base,
            compiled_fns,
            hints,
        );
        if scopes_lexical_types {
            self.pop_lexical_class_scope();
        }
        self.current_unit = saved_unit;
        result
    }

    #[allow(clippy::too_many_arguments)]
    fn call_compiled_method_fast_in_unit(
        &mut self,
        receiver_class_name: &str,
        owner_sym: Symbol,
        method_sym: Symbol,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        args: Vec<Value>,
        base: Value,
        compiled_fns: &CompiledFns,
        hints: MethodFastHints,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        // #10986: a multi winner's subset predicates already ran in dispatch
        // (`pending_skip_constraint_recheck`); taken at entry so it describes
        // only this method's own parameter checks below.
        // ADR-0100: refuse the call while there is still stack left to raise
        // with, so deep recursion becomes a catchable exception instead of a
        // guard-page abort. Same boundary as this path's `Call` GC safepoint.
        // Ahead of the flag taken below, so a call continued on a fresh stack
        // segment (ADR-17392) finds it still set.
        crate::guard_native_stack!(
            self,
            self.call_compiled_method_fast_in_unit(
                receiver_class_name,
                owner_sym,
                method_sym,
                method_def,
                cc,
                args,
                base,
                compiled_fns,
                hints,
            )
        );
        let can_skip_merge = hints.can_skip_merge;
        let facts = hints.facts;
        let skip_constraint_recheck = std::mem::take(&mut self.pending_skip_constraint_recheck);
        // See `call_compiled_method`: interned by the caller (#10961).
        let owner_class: &str = owner_sym.as_str();
        let method_name: &str = method_sym.as_str();
        crate::alloc_scope!("mfast");
        crate::alloc_scope_named!(_sc_pro, "mfast:prologue");
        // The owner's role-ness is asked here once per call and handed to the
        // frame, so the body's attribute accesses never ask it again.
        let owner_is_role = match facts {
            Some(facts) => facts.owner_is_role,
            None => self.is_role(owner_class),
        };
        // One walk for both cells: the primary is the role cell when `self`
        // composes the owner role, else the instance cell, which then also
        // serves as the fallback for keys the role cell lacks.
        let (role_attrs_cell, inner_attrs_cell) =
            Self::method_attr_cells_for(&base, owner_class, owner_is_role);
        let attrs_cell = role_attrs_cell.or_else(|| inner_attrs_cell.clone());
        let fallback_attrs_cell = inner_attrs_cell.filter(|cell| {
            attrs_cell.as_ref().is_some_and(|primary| {
                crate::gc::Gc::as_ptr(primary) != crate::gc::Gc::as_ptr(cell)
            })
        });
        // RAII (`MarkContextGuard`,
        // `todo/deep/mark-context-flags-leak-across-live-call-boundary.md`):
        // isolate the "mark context" one-shot flag family (bind_context et
        // al.) so a caller's pending `:=` mark does not leak into this
        // method's own vardecl/store opcodes -- restored on every exit path,
        // including a Rust panic unwind through the body loop below.
        let _mark_context_guard = crate::vm::vm_call_state_guard::MarkContextGuard::new(self);
        if let Some(ref msg) = method_def.deprecated_message {
            loan_env!(
                self,
                check_deprecation_for_method_with_line(method_name, owner_class, msg, None,)
            );
        }

        self.push_light_call_frame();
        // See `call_compiled_function_named_inner`: an END registered inside this
        // method closes over a frame that dies on return.
        let end_phaser_mark_before = self.end_phaser_capture_mark();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        // A method gets a fresh, writable `$_` (Any) — clear any readonly mark
        // leaked from the caller's topic (see the slow path above). Journaled by
        // the frame's readonly scope; an explicit `$_` param re-marks it.
        self.unmark_readonly_topic();
        // ADR-0035 slice 2: see the matching comment in `call_compiled_method`.
        // Pushed before the scoped-overlay install below so the pushed entry
        // captures the caller's (unoverlaid) env.
        let pushed_caller = cc.uses_callframe;
        if pushed_caller {
            self.push_caller_env_in_call_frame();
        }
        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller so the `self`/`?CLASS`/param/attr
        // env writes below land in a fresh map (strong_count 1) instead of
        // `make_mut`-forking the inherited caller env — this removes the per-
        // method-call env deep copy (the bench-class cost). Reads fall through to
        // the parent; on return `set_env(saved_env)` (can_skip_merge) drops the
        // overlay and `merge_method_env` iterates it overlay-only (the method's
        // own writes, which is exactly what the merge needs). It is the
        // method's frame root: a closure the method creates captures up to it
        // (`layered_capture`), never the caller's env above it (#12882).
        // A method declared at program scope links the root past its callers
        // (ADR-12529).
        let links_frame = cc.links_frame_to_unit();
        {
            let parent = self.env().clone();
            let mut scoped = crate::env::Env::scoped_child(parent);
            // A method owns its `$/` (`env::routine_owned`).
            scoped.mark_owns_routine_state();
            self.set_env(scoped);
        }
        // A role method's composition bindings, installed into the overlay
        // before the parameters bind (a `T`-typed parameter resolves `T`
        // through them), exactly as the full binder does. Their names are this
        // frame's own and are kept out of the exit merge below.
        // A role method whose body and signature name none of its bindings
        // skips the install (`vm_role_binding_observers`); `has_role_bindings`
        // then also skips the re-applications below.
        let mut has_role_bindings = match facts {
            Some(facts) => facts.has_role_bindings,
            None => self.method_has_role_bindings(method_def, owner_class, receiver_class_name),
        };
        let role_binding_names: std::sync::Arc<[String]> = if has_role_bindings {
            match self.install_observed_method_role_bindings(
                receiver_class_name,
                owner_class,
                method_name,
                method_def,
                cc,
            ) {
                Some(names) => names,
                None => {
                    has_role_bindings = false;
                    no_role_binding_names()
                }
            }
        } else {
            no_role_binding_names()
        };
        let saved_var_bindings = self.take_var_bindings();
        self.push_method_class_sym(owner_sym, Some(owner_is_role));
        // See the matching comment in `call_compiled_method`.
        if cc.uses_dispatcher {
            self.push_method_samewith_context(
                receiver_class_name,
                method_name,
                &args,
                Some(base.clone()),
            );
        }

        // Save/restore the current package only when this dispatch actually
        // switches it (rare) — the unconditional save cloned a String per call.
        // See the matching comment in `call_compiled_method`: keyed on
        // `owner_class`, not the dynamic `receiver_class_name` (#9008).
        let body_package = match facts {
            Some(facts) => facts.body_package,
            None => self.method_body_package(owner_class, owner_sym, method_def),
        };
        let saved_package: Option<Symbol> = body_package.map(|package| {
            let saved = self.current_package_sym();
            self.set_current_package_sym(package);
            saved
        });

        // Compute role context without touching env. A role owner is its own
        // context (already interned); a method composed from a role into a
        // class carries its role's name, interned once here for both the
        // `?ROLE` slot and the routine frame below.
        let role_context: Option<Symbol> = match facts {
            Some(facts) => facts.role_context,
            None => MethodFrameFacts::role_context_of(owner_is_role, owner_sym, method_def),
        };

        // Build param name → arg value mapping (skip invocant). Positional
        // args are consumed in order, skipping named (string-keyed Pair)
        // arguments the way the slow binder does; named params match Pair args
        // by key (rightmost wins), and attributive named params write through
        // the invocant's live attribute cell exactly like `bind_param_value`.
        crate::alloc_scope_end!(_sc_pro);
        crate::alloc_scope_named!(_sc_bind, "mfast:param-bind");
        // Undo the frame/package/env setup above when binding fails, before
        // the binding error is returned.
        macro_rules! unwind_fast_bind {
            () => {{
                self.restore_var_bindings(saved_var_bindings);
                if cc.uses_dispatcher {
                    self.pop_method_samewith_context();
                }
                self.pop_method_class();
                if let Some(pkg) = saved_package {
                    self.set_current_package_sym(pkg);
                }
                self.stack.truncate(saved_stack_depth);
                if pushed_caller {
                    self.pop_caller_env();
                }
                let frame = self.pop_call_frame();
                self.set_env(frame.saved_env);
            }};
        }
        // (name, interned name, value): the name for matching against the
        // body's local names, the symbol for the env insert.
        let mut param_values = super::vm_param_values::ParamValues::new();
        // Indices into `param_values` of the plain `@x`/`%x` parameters bound
        // below, whose constraint is registered once the callee's env exists.
        // Each carries the caller's variable name for `.VAR.name`, if any.
        let mut light_aggregate_binds: Vec<(usize, Option<String>)> = Vec::new();
        // Index into `param_values` of an explicit `*%h`, whose (absent)
        // constraint is registered once the callee's env exists.
        let mut named_slurpy_bind: Option<usize> = None;
        // (name, is-readonly) for a sigilless raw invocant, applied once the
        // param values are installed below.
        let mut raw_invocant_readonly: Option<(&str, bool)> = None;
        // `is raw` parameters bound to a value: immutable once the frame's
        // readonly marks are applied below.
        let mut raw_readonly_params: Vec<Symbol> = Vec::new();
        // Omitted parameters whose non-constant default the default prologue
        // evaluates in the finished frame (`vm_method_default_prologue`).
        let mut deferred_defaults: Vec<super::vm_method_default_prologue::DeferredDefault> =
            Vec::new();
        let mut arg_idx = 0;
        let param_syms = method_def.param_syms();
        let param_storage = method_def.param_storage_names();
        for (idx, param_name) in param_storage.iter().enumerate() {
            let pd = method_def.param_defs.get(idx);
            let (binding_name, binding_sym) = if pd.is_some_and(|pd| pd.declares_self_lexical()) {
                (crate::env::LEX_SELF, crate::symbol::wk::lex_self())
            } else {
                (param_name.as_str(), param_syms[idx])
            };
            let is_invocant = pd.map(|pd| pd.is_invocant).unwrap_or(false);
            if is_invocant {
                // ADR-0067 slice 3b — see the twin in `call_compiled_method`.
                // Which of the two binders runs is decided by the fast-path
                // eligibility gate above (`has_rw_params`), and the sigil-less
                // `\S:` spelling lands here while `$s is raw:` lands there, so
                // both have to learn the container.
                let arrival = self
                    .take_raw_invocant_arrival(method_name, pd)
                    .or_else(|| self.take_implicit_self_invocant_arrival(method_name, pd));
                let bound_to_container = arrival.is_some() || base.is_container_ref();
                let invocant_value = arrival.unwrap_or_else(|| base.clone());
                param_values.push((binding_name, binding_sym, invocant_value));
                // See the twin in `call_compiled_method`: with no container the
                // invocant is an immutable value with no location, and the
                // body's write to it must be refused rather than dropped.
                if pd.is_some_and(|pd| {
                    crate::runtime::raw_invocant::param_is_raw_invocant(pd) && pd.sigilless
                }) {
                    raw_invocant_readonly = Some((param_name.as_str(), !bound_to_container));
                }
                continue;
            }
            // An explicit `*%h`: the leftover named arguments, as the caller
            // promoted them (`vm_method_named_slurpy`).
            if pd.is_some_and(Self::method_named_slurpy_light) {
                named_slurpy_bind = Some(param_values.len());
                param_values.push((
                    binding_name,
                    binding_sym,
                    Self::implicit_method_named_slurpy(&method_def.param_defs, &args),
                ));
                continue;
            }
            if let Some(pd) = pd.filter(|pd| pd.named) {
                // Simple named/attributive param (gate: `named_param_fast_eligible`).
                let bound = Self::named_param_fast_match_key(pd).and_then(|key| {
                    args.iter().rev().find_map(|a| {
                        let a = a.unwrap_varref();
                        // The tag test spares the positional arguments a
                        // full `view()` decode.
                        if !a.is_string_pair_value() {
                            return None;
                        }
                        match a.view() {
                            ValueView::Pair(k, v) if k.as_str() == key => Some(v.clone()),
                            _ => None,
                        }
                    })
                });
                let bound = bound.map(|value| {
                    if crate::value::attr_twigil_base(&pd.name).is_some() {
                        value.deref_container()
                    } else {
                        value
                    }
                });
                let val = match bound {
                    // A `:$x is raw` the gate admitted
                    // (`method_container_args_light`) and the promotion
                    // rewrote: a shared cell or a Proxy aliases the caller's
                    // container, anything else is a read-only term.
                    Some(v) if pd.binds_caller_container() => {
                        let v = crate::runtime::types::unwrap_varref_value(v);
                        if !(v.is_container_ref() || v.is_proxy_value()) {
                            raw_readonly_params.push(binding_sym);
                        }
                        v
                    }
                    // `@`-sigiled (attributive) named param: re-home a List
                    // into an Array (mirrors `bind_param_value`); scalar named
                    // params are item bindings (mirrors the slow binder's
                    // named arm).
                    Some(v) if pd.name.starts_with("@!") || pd.name.starts_with("@.") => {
                        Self::attributive_array_param_value(v)
                    }
                    Some(v) if pd.name.starts_with('@') => {
                        Self::normalize_positional_param_value(v)
                    }
                    Some(v) if pd.name.starts_with('%') => v,
                    Some(v) => Self::itemize_plain_scalar_param(pd, v),
                    // An unsupplied optional named param binds its type-object
                    // default. For an attributive param this overwrites the
                    // attribute — the slow path and rakudo both do (pinned by
                    // t/tweak-named-fast-path.t).
                    None => match method_def
                        .named_fast(idx)
                        .and_then(|plan| plan.fill.clone())
                    {
                        Some(default) => default,
                        None if pd.default.is_some() && method_def.default_prologue(idx) => {
                            deferred_defaults.push(
                                super::vm_method_default_prologue::DeferredDefault {
                                    param_idx: idx,
                                    sym: binding_sym,
                                },
                            );
                            Self::missing_optional_param_value(pd)
                        }
                        None => Self::missing_optional_param_value(pd),
                    },
                };
                // `:$!x` binds by assignment: an explicit Nil stores the
                // attribute's container default (`BUILD(:$!x)` with
                // `.new(:x(Nil))` leaves `$!x` as `Any`).
                let val = if val.is_nil() && pd.name.starts_with('!') {
                    self.attr_store_nil_default(owner_class, &pd.name[1..], '$', val)
                } else {
                    val
                };
                // Attributive param: write the one attribute through `self`'s
                // shared cell (the single-key cell write `bind_param_value`
                // performs) BEFORE the locals-init loop below reads attribute
                // slots off the cell.
                if let Some((attr_name, is_private)) = crate::value::attr_twigil_base(&pd.name)
                    && let Some(cell) = &attrs_cell
                {
                    // Inside a BUILD phase this bind counts as "BUILD set it",
                    // even unpassed (the type-object bind must suppress the
                    // `has $.a = 5` initializer; the seed comparison alone
                    // cannot tell — an untyped attribute's seed IS the same
                    // type object). The slow path records this via its exit
                    // attr-local sync through `write_attr_cell_by_key`; the
                    // fast path records it here. No-op outside BUILD.
                    // One intern for both uses: the symbol-keyed insert also
                    // saves the `String` allocation the name-keyed one paid.
                    let bare_sym = crate::symbol::Symbol::intern(attr_name);
                    // A scalar and a container attribute may share a bare name
                    // (`has %!c; has $!c`); resolve the storage key by sigil so
                    // `:$!c` does not overwrite the `%!c` slot.
                    let sigil = crate::value::attr_twigil_sigil(&pd.name).unwrap_or('$');
                    let attr_sym = Self::attr_key_in_map(
                        Some(owner_sym),
                        bare_sym,
                        is_private,
                        sigil,
                        &cell.as_map(),
                    )
                    .unwrap_or(bare_sym);
                    self.record_build_attr_write(cell, attr_sym);
                    cell.insert(attr_sym, val.clone());
                }
                param_values.push((binding_name, binding_sym, val));
                continue;
            }
            // Positional param: skip named (string-Pair) args, as the slow
            // binder does when consuming positionals.
            while arg_idx < args.len() && args[arg_idx].unwrap_varref().is_string_pair_value() {
                arg_idx += 1;
            }
            if arg_idx < args.len() {
                let mut val = args[arg_idx].clone();
                // A container parameter (`is raw`, sigilless) reaches this
                // binder only as the gate admitted it
                // (`method_container_args_light`) and the promotion rewrote it
                // (`promote_method_container_args`): a shared cell or a Proxy
                // aliases the caller's container, anything else is a
                // read-only term, marked the way the general binder marks it.
                // Its type was checked at admission.
                if let Some(pd) = pd.filter(|pd| pd.binds_caller_container()) {
                    let val = crate::runtime::types::unwrap_varref_value(val);
                    let writable = val.is_container_ref() || val.is_proxy_value();
                    if pd.sigilless {
                        self.mark_light_sigilless_param(binding_sym, writable);
                    } else if !writable {
                        raw_readonly_params.push(binding_sym);
                    }
                    param_values.push((binding_name, binding_sym, val));
                    arg_idx += 1;
                    continue;
                }
                // A plain `@x`/`%x` binds the caller's container itself
                // (admitted per call by `method_aggregate_args_light`),
                // de-itemized like the general binder's `@`/`%` bind.
                if pd.is_some_and(|pd| {
                    crate::opcode::FastParamCheck::light_aggregate_sigil(pd).is_some()
                }) {
                    val =
                        crate::runtime::types::unwrap_varref_value(val).deitemize_for_sigil_bind();
                    let source = self
                        .pending_call_arg_sources()
                        .and_then(|sources| sources.get(arg_idx))
                        .and_then(|name| name.clone());
                    light_aggregate_binds.push((param_values.len(), source));
                    param_values.push((binding_name, binding_sym, val));
                    arg_idx += 1;
                    continue;
                }
                // Validate the original argument before detaching its copy.
                if let Some(pd) = pd.filter(|pd| pd.traits.iter().any(|t| t == "copy")) {
                    let val = match self.checked_method_param_copy(pd, val, skip_constraint_recheck)
                    {
                        Ok(val) => val,
                        Err(error) => {
                            unwind_fast_bind!();
                            return Err(error);
                        }
                    };
                    param_values.push((
                        binding_name,
                        binding_sym,
                        Self::itemize_fast_method_arg(Some(pd), val),
                    ));
                    arg_idx += 1;
                    continue;
                }
                // An untyped parameter is implicitly `Any` (#10878): a
                // `Mu`-only argument takes the typed check below, which fails.
                let implicit_any_fail = pd
                    .is_some_and(|pd| pd.type_constraint.is_none() && !pd.is_invocant)
                    && pd.is_some_and(crate::opcode::FastParamCheck::implicitly_any)
                    && !self.light_arg_is_any(&val);
                if let Some(constraint) = pd
                    .and_then(|pd| pd.type_constraint.as_deref())
                    .or(implicit_any_fail.then_some("Any"))
                {
                    // A `Proxy` argument is checked (and bound) by what its FETCH
                    // answers, as the sub binder does; `Mu` and an `is rw` /
                    // `is raw` parameter keep the Proxy itself.
                    if val.is_proxy_value()
                        && constraint != "Mu"
                        && !pd.is_some_and(|pd| {
                            pd.traits.iter().any(|t| matches!(t.as_str(), "rw" | "raw"))
                        })
                    {
                        match self.auto_fetch_proxy(&val) {
                            Ok(fetched) => val = fetched,
                            Err(e) => {
                                unwind_fast_bind!();
                                return Err(e);
                            }
                        }
                    }
                    let resolved_constraint: std::borrow::Cow<'_, str> = match pd {
                        Some(pd) => self.resolved_param_constraint_cow(pd, constraint),
                        None => match self.try_resolved_type_capture_name(constraint) {
                            Some(resolved) => std::borrow::Cow::Owned(resolved),
                            None => std::borrow::Cow::Borrowed(constraint),
                        },
                    };
                    // A native-int parameter binds the coerced value -- the
                    // same unbox/wrap the sub binders apply (#9533): an
                    // `int8` param wraps `200` to `-56`, and an Int-valued
                    // enum or a Bool binds its integer.
                    let mut native_err = None;
                    // A `Positional`-typed parameter binds a Seq through
                    // `PositionalBindFailover` (rakudo#4864), as the sub
                    // binder does: `method m(Positional $p)` given `.map(..)`,
                    // and with a `:D`/`:_` smiley too (Pod::To::HTML's
                    // `multi method node2html(Positional:D $node)`).
                    if crate::runtime::types::binds_through_positional_failover(
                        &resolved_constraint,
                    ) && !self.type_matches_value("Positional", &val)
                        && self.type_matches_value("PositionalBindFailover", &val)
                    {
                        match self.coerce_positional_bind_failover(val.clone()) {
                            Ok(coerced) => val = coerced,
                            Err(e) => native_err = Some(e),
                        }
                    }
                    if self.native_int_binding_type(&resolved_constraint).is_some() {
                        match crate::runtime::types::wrap_native_int_for_binding(
                            self,
                            &resolved_constraint,
                            val.clone(),
                        ) {
                            Ok(coerced) => val = coerced,
                            Err(e) => native_err = Some(e),
                        }
                    }
                    if native_err.is_none()
                        && ((skip_constraint_recheck
                            && self.constraint_is_user_subset(&resolved_constraint))
                            || if pd.is_some_and(|pd| pd.is_type_only())
                                && !resolved_constraint.as_bytes().starts_with(b"::")
                                && !self.is_resolvable_type(&resolved_constraint)
                            {
                                // A bare enum value / constant (`method m(Store)`).
                                self.type_only_param_value_matches(&resolved_constraint, &val)
                            } else {
                                self.type_matches_value(&resolved_constraint, &val)
                            })
                    {
                        param_values.push((
                            binding_name,
                            binding_sym,
                            Self::itemize_fast_method_arg(pd, val),
                        ));
                        arg_idx += 1;
                        continue;
                    }
                    let (base_type, smiley) =
                        crate::runtime::types::strip_type_smiley(&resolved_constraint);
                    let invalid_concreteness = smiley.is_some_and(|s| matches!(s, ":D" | ":U"))
                        && self.type_matches_value(base_type, &val);
                    let got_type = if let ValueView::Package(pkg) = val.view() {
                        pkg.resolve().to_string()
                    } else {
                        crate::runtime::utils::value_type_name(&val).to_string()
                    };
                    let expected = if smiley.is_some() {
                        base_type
                    } else {
                        &*resolved_constraint
                    };
                    // Type mismatch — fall back to slow path for proper error
                    unwind_fast_bind!();
                    if let Some(e) = native_err {
                        return Err(e);
                    }
                    if invalid_concreteness {
                        return Err(RuntimeError::parameter_invalid_concreteness(
                            base_type,
                            &got_type,
                            method_name,
                            &method_def.params[idx],
                            smiley == Some(":D"),
                            pd.is_some_and(|pd| pd.is_invocant),
                        ));
                    }
                    return Err(self
                        .typecheck_binding_parameter_failure(
                            &crate::runtime::types::param_display_name(pd.unwrap()),
                            expected,
                            &val,
                        )
                        .with_parameter_object(pd.unwrap(), Some(&*self)));
                }
                param_values.push((
                    binding_name,
                    binding_sym,
                    Self::itemize_fast_method_arg(pd, val),
                ));
                arg_idx += 1;
            } else if let Some(pd) = pd.filter(|pd| pd.default.is_some())
                && let Some(val) = crate::opcode::CompiledFunction::const_fill_for_param(pd)
            {
                // An omitted parameter whose default is a shareable constant
                // (`$params = \()`, `$n = 1`) binds that constant, already
                // checked against the parameter's constraint; the gate admits
                // no other default (`needs_default_eval`). An `is copy`
                // parameter gets its own container, as a supplied argument
                // does above.
                let val = if pd.traits.iter().any(|t| t == "copy") {
                    val.detach_shared_container()
                } else {
                    val
                };
                param_values.push((
                    binding_name,
                    binding_sym,
                    Self::itemize_fast_method_arg(Some(pd), val),
                ));
            } else if let Some(pd) =
                pd.filter(|pd| pd.default.is_some() && method_def.default_prologue(idx))
            {
                // Any other default runs in the finished frame, before the
                // body; the slot holds the parameter's shadow until then.
                deferred_defaults.push(super::vm_method_default_prologue::DeferredDefault {
                    param_idx: idx,
                    sym: binding_sym,
                });
                param_values.push((
                    binding_name,
                    binding_sym,
                    Self::missing_optional_param_value(pd),
                ));
            } else if let Some(pd) = pd.filter(|pd| pd.optional_marker) {
                // An omitted optional's implicit default still has to satisfy
                // a subset constraint (`method m(S $s?)`), as in the slow binder.
                let val = self.omitted_optional_param_value(pd);
                if let Err(e) = self.check_omitted_optional_value(pd, &val) {
                    unwind_fast_bind!();
                    return Err(e);
                }
                param_values.push((binding_name, binding_sym, val));
            }
        }

        crate::alloc_scope_end!(_sc_bind);
        crate::alloc_scope_named!(_sc_env, "mfast:env-setup");
        // Build class value for ?CLASS. Interned once and reused by the routine
        // frame push at the bottom — re-interning a name is a thread-local
        // string-hash round trip, not free.
        // `owner_sym` was interned for the method-class frame push above.
        // A role method composed into a mixin sees the mixin type as
        // `::?CLASS` (the binding `dispatch_mixin_method_call` stages), as in
        // the full binder.
        let class_val = method_def
            .role_param_bindings
            .as_deref()
            .and_then(|b| b.iter().find(|(n, _)| n == "?CLASS"))
            .map_or_else(|| Value::package(owner_sym), |(_, v)| v.clone());
        let method_callable_id = crate::value::next_instance_id();
        let any_val = Value::package(crate::symbol::wk::any());

        // For can_skip_merge methods with no closures, reduce env inserts.
        // Only insert ?CLASS/?ROLE (used by ::?CLASS/::?ROLE resolution and
        // GetGlobal in non-compiled paths). Skip self, params, attrs, and other
        // method-local vars since all reads go through GetLocal.
        let skip_env_setup = can_skip_merge && cc.closure_compiled_codes.is_empty();
        // Make class-body `my` statics visible to every read path in the body.
        // Injected BEFORE `self`/params are inserted so those still win, while an
        // unrelated CALLER lexical sharing the name does not — the statics are the
        // class's own lexicals and the method body is inside their scope.
        self.inject_class_body_statics_sym(owner_sym);
        // As in the full binder: a role method's own bindings are closer than
        // the statics of a class the role is declared in.
        if owner_is_role && has_role_bindings {
            self.apply_method_role_bindings(method_def);
        }
        // ADR-14843: the frame's record (pushed below with the routine frame) is
        // the only home of `self`, `__ANON_STATE#`, `?CLASS` and `?ROLE`; the
        // frame writes none of the four keys.
        if skip_env_setup {
            // A plain scalar parameter is read through its slot and nothing
            // else names it (no closure, `skip_env_setup`); an aggregate,
            // `rw`/`raw`/`copy` or sigilless parameter is shared by name with
            // the caller's container and stays in the env.
            self.insert_fast_param_values_by_name(
                &param_values,
                method_def,
                cc,
                cc.static_name_lookup
                    || cc.needs_reflective_capture
                    || !deferred_defaults.is_empty(),
            );
        } else {
            let env = self.env_mut();
            env.reserve(3 + param_values.len());
            // The rooted frame answers `$_` and `$!` (`env::routine_owned`).
            env.set_callable_id(method_callable_id as i64);
            // Array/hash attribute env copies are no longer materialized here:
            // reads are cell-direct (Stage 2b) and the mutating ops refresh
            // env/locals from the live cell pre-op (`array_hash_attr_env_snapshot`),
            // so a closure capturing this env resolves `@!a`/`%!h` through the
            // captured `self` instead of a stale env snapshot.
            self.insert_fast_param_values_by_name(
                &param_values,
                method_def,
                cc,
                cc.static_name_lookup
                    || cc.needs_reflective_capture
                    || !deferred_defaults.is_empty(),
            );
        }
        // A light-bound plain `@x`/`%x` carries its constraint in the callee's
        // scope, which exists only now.
        for (i, source) in &light_aggregate_binds {
            let (name, sym, ref val) = param_values[*i];
            let mut val = val.clone();
            self.bind_light_aggregate_constraint(name, sym, &val);
            val.name_attribute_container(source.as_deref());
        }
        if let Some(i) = named_slurpy_bind {
            let (name, sym, ref val) = param_values[i];
            let val = val.clone();
            self.bind_light_aggregate_constraint(name, sym, &val);
        }
        // The call-site arg-source names describe THIS call's binding only. The
        // light bind peeks them (`light_aggregate_binds`) instead of taking
        // them, so left installed they would reach the first bind the method
        // BODY makes: `CArray[uint8]` calls `^parameterize($, $)`, and
        // `["off", None]` from `self.rs($off, 1)` matched its two-argument arity
        // and bound `off` as a shared cell in the env, which the caller's slot
        // never adopted (the ADR-0097 §15 divergence, #16037).
        self.discard_pending_call_arg_sources();
        // A typed scalar `is copy` parameter is the method's own writable
        // variable: a store into it checks its declared type, which the
        // general binder records in the callee frame and this one did not
        // (#13783). Every other scalar parameter is read-only or aliases the
        // caller's container.
        for (idx, pd) in method_def.param_defs.iter().enumerate() {
            if pd.type_constraint.is_some()
                && !pd.sigilless
                && !pd.name.starts_with(['@', '%', '&', '!', '.'])
                && pd.traits.iter().any(|t| t == "copy")
                && let (Some(name), Some(&sym)) = (param_storage.get(idx), param_syms.get(idx))
            {
                self.bind_param_type_constraint_sym(name, sym, pd.assignment_type_constraint());
            }
        }
        self.request_call_capture(cc.uses_capture, Some(&base));
        self.finish_call_capture(&args);

        if let Some((name, readonly)) = raw_invocant_readonly {
            let key = crate::runtime::sigilless_readonly_key(name);
            if readonly {
                self.env_mut().insert_sym_noting(key, Value::TRUE);
            } else {
                self.env_mut().remove_sym(key);
            }
        }

        crate::alloc_scope_end!(_sc_env);
        crate::alloc_scope_named!(_sc_loc, "mfast:slurpy-captures-locals");
        // Bind the method's implicit `*%_` slurpy (a Hash of the leftover named
        // args, empty when none). The compiler adds a `*%_` param to every method;
        // the slow `bind_function_args_values` path fills it, but this fast path
        // binds positionals by index and skips slurpy params, leaving `%_` as
        // `Any` -- so `|%_` would splat a stray positional. Compute it here and let
        // the locals-init loop fill the `%_` slot.
        // ...but only when the body can actually observe `%_`. `cc` records
        // that conservatively at compile time (`may_observe_named_slurpy`);
        // a body that never spells `%_` -- `submethod TWEAK(:$!spec) { }`,
        // the overwhelming majority of methods -- built a hash of the leftover
        // named args on every call and immediately threw it away.
        crate::alloc_scope_named!(_sc_loc_slurpy, "mfast:loc:slurpy-hash");
        let implicit_named_slurpy: Option<Value> = if cc.may_observe_named_slurpy
            && method_def
                .param_defs
                .iter()
                .any(|pd| pd.slurpy && pd.name == "%_")
        {
            Some(Self::implicit_method_named_slurpy(
                &method_def.param_defs,
                &args,
            ))
        } else {
            None
        };

        // Bind the implicit `*%_` into env so a `%_` read finds it there first
        // (its method local is stored sigiled as "%_", which the bare-name local
        // lookup -- it strips the sigil and searches "_" -- can't resolve, falling
        // back to the topic `$_`).
        if let Some(slurpy) = &implicit_named_slurpy {
            self.env_mut()
                .insert_sym(crate::symbol::wk::named_slurpy(), slurpy.clone());
        }

        crate::alloc_scope_end!(_sc_loc_slurpy);
        crate::alloc_scope_named!(_sc_loc_cap, "mfast:loc:captured-env");
        // A method can carry its defining lexical environment either from an
        // `.^add_method` closure literal or from a class declared in a routine.
        // The fast path populates free-variable locals from this overlay.
        if let Some(captured) = &method_def.captured_env {
            let dynamic_add_method = captured
                .get("__mutsu_declared_method_capture")
                .is_some_and(|value| value.as_int() == Some(2));
            let env = self.env_mut();
            let authoritative = captured.contains_key_sym(crate::symbol::Symbol::intern(
                "__mutsu_declared_method_capture",
            ));
            for (sym, val) in captured.iter() {
                if !sym.with_str(|name| {
                    matches!(
                        name,
                        "self" | "?CLASS" | "?ROLE" | "__mutsu_declared_method_capture"
                    )
                }) && (authoritative || !env.contains_key_sym(*sym))
                    && captured_name_reachable(cc, method_def, *sym)
                {
                    env.insert_sym(*sym, val.clone());
                }
            }
            if dynamic_add_method {
                env.insert("__mutsu_declared_method_capture".to_string(), Value::int(2));
            }
            // A composed role's type parameters are more specific than a
            // lexical capture with the same spelling (see the full binder).
            if has_role_bindings {
                self.apply_method_role_bindings(method_def);
            }
        }

        // Raku: routine parameters are read-only by default (`method m($x) { $x = 5 }`
        // dies). The slow path does this in `bind_function_args_values`; the fast
        // path binds params directly, so mark `$` scalar params read-only here.
        self.mark_fast_method_params_readonly(method_def);
        for sym in raw_readonly_params {
            self.mark_readonly_sym_with(sym, crate::ast::ReadonlyKind::Immutable);
        }

        crate::alloc_scope_end!(_sc_loc_cap);
        crate::alloc_scope_named!(_sc_loc_init, "mfast:loc:init");
        // Populate locals directly. Attribute reads take one read guard over
        // the live cell for the whole loop (dropped before the body runs) —
        // no whole-map snapshot is materialized. A linked frame is marked
        // first, so a local seeded from the env reads through the link
        // (#12478).
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
            let role_state = if owner_is_role {
                match base.view() {
                    ValueView::Mixin(_, mixins)
                        if mixins.contains_key(MetaNs::Role.str_key_for_str(owner_class)) =>
                    {
                        Some(mixins.clone())
                    }
                    _ => None,
                }
            } else {
                None
            };
            // ADR-15737 D1: every slot's source was decided once per method
            // (`vm_method_frame_plan`); this loop only fetches it.
            let attr_get = |key: Symbol| -> Option<Value> {
                // A role's own state (a mixin carrying the role) keys the
                // attribute by the role's identity first.
                let role_key = role_state.as_ref().map(|state| {
                    let bare = key.as_str();
                    let bare = bare.rsplit('\0').next().unwrap_or(bare);
                    state.role_attribute_key(owner_class, bare)
                });
                role_key
                    .and_then(|key| attrs_guard.as_ref().and_then(|g| g.get(key).cloned()))
                    .or_else(|| attrs_guard.as_ref().and_then(|g| g.get(key).cloned()))
                    .or_else(|| {
                        fallback_attrs_guard
                            .as_ref()
                            .and_then(|g| g.get(key).cloned())
                    })
            };
            let plan = method_def.frame_plan(owner_sym, cc);
            // The binder pushes one value per parameter on every call the
            // fast path takes, in parameter order, which is what a
            // `LocalInit::Param` index counts; anything else falls back to
            // the by-name search the plan replaced.
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
                            // A parameter the fast binder bound nothing for
                            // (the implicit `*%_`, which lives in the env)
                            // reads as if it were no parameter.
                            None => plan.unbound_param[i],
                        }
                    }
                    other => other,
                };
                self.locals[i] = match init {
                    LocalInit::Invocant => base.clone(),
                    LocalInit::Class => class_val.clone(),
                    LocalInit::Role => role_context.map_or(Value::NIL, Value::package),
                    // `unbound_param` never plans a parameter, so `Param`
                    // cannot reach here; it would read as `Nil`.
                    LocalInit::Nil | LocalInit::Param(_) => Value::NIL,
                    LocalInit::Topic => any_val.clone(),
                    LocalInit::AttrPrivate { qualified, bare } => attr_get(qualified)
                        .or_else(|| attr_get(bare))
                        .unwrap_or(Value::NIL),
                    LocalInit::AttrPublic { bare } => attr_get(bare).unwrap_or(Value::NIL),
                    // A private declaration colliding with a public one of
                    // another sigil lives under `<sigil>bare`.
                    LocalInit::AttrAggregate { sigil_key, bare } => attr_get(sigil_key)
                        .or_else(|| attr_get(bare))
                        .unwrap_or(Value::NIL),
                    // Outer env (read-only, no deep clone)
                    LocalInit::Outer => cc
                        .local_sym(i)
                        .and_then(|sym| self.env().get_sym(sym))
                        .cloned()
                        .unwrap_or(Value::NIL),
                };
            }
        }

        crate::alloc_scope_end!(_sc_loc_init);
        crate::alloc_scope_named!(_sc_loc_state, "mfast:loc:state-and-defaults");
        // See the sibling path: a method body's `state` is keyed by the method,
        // not by the caller's closure scope. Restored after the sync.
        let saved_state_scope = self.lexicals.state_scope_id.take();
        self.scope_method_state(method_name, receiver_class_name, owner_sym);
        // Load persisted state variable values
        for (slot, key) in &cc.state_locals {
            // `state_scope_id` is `None` here (cleared just above), so this
            // resolves the raw, unscoped key.
            if let Some(val) = self.get_state_var(self.scoped_state_key(*key)) {
                self.locals[*slot] = val.clone();
            }
        }

        // Register `is default(...)` values for attribute variables. Gated on
        // the owner or receiver class declaring ANY attribute default (see the
        // slow path).
        // ...and skipped outright while the last registration for this class
        // pair is still in effect.
        let has_defaults = self.attr_default_classes(owner_class, receiver_class_name);
        if (has_defaults.0 || has_defaults.1)
            && let Some(cell) = &attrs_cell
            && !self.attr_var_defaults_are_current(owner_class, receiver_class_name)
        {
            let attr_names: Vec<&'static str> = cell
                .as_map()
                .keys()
                .map(|k| k.as_str())
                .filter(|k| !k.contains('\0') && !k.starts_with(ATTR_ALIAS_META_PREFIX))
                .collect();
            for attr_name in &attr_names {
                let default_val = self.attr_default_for_call(
                    owner_class,
                    receiver_class_name,
                    has_defaults,
                    attr_name,
                );
                if let Some(def) = default_val {
                    self.set_attr_var_defaults(attr_name, def);
                }
            }
            self.note_attr_var_defaults_current(owner_class, receiver_class_name);
        }

        crate::alloc_scope_end!(_sc_loc_state);
        crate::alloc_scope_end!(_sc_loc);
        crate::alloc_scope_named!(_sc_body, "mfast:body");
        // See the sibling path (#11054).
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
                role: role_context,
            },
            false,
        );

        // Execute bytecode (same as slow path)
        let let_mark = self.let_saves_len();
        // A `sub` declared directly in this method's body is lexical to it
        // (mirrors `call_compiled_function_fast`'s `routine_snapshot` for the
        // ordinary sub/closure call paths -- method dispatch had no equivalent,
        // so such a sub stayed registered and callable after the method
        // returned; see todo/tickets/nested-sub-in-method-leaks-to-global-scope.md).
        let routine_snapshot = if cc.declares_inner_routines() {
            Some(self.snapshot_routine_registry())
        } else {
            None
        };
        let closures_created_before = self.closures_created;
        // A method body is its own topicalizer for a bare `when`/`default`; its
        // `when_matched` must not leak to an enclosing given/with (see the slow
        // path above / vm_call_light.rs for the full rationale).
        let saved_when_matched = self.when_matched();
        // `use fatal` is lexical to the method's declaration (#11391) -- see
        // the slow path above.
        let saved_lexical_fatal_mode =
            std::mem::replace(&mut self.module.lexical_fatal_mode, cc.method_fatal_pragma);
        // The deferred defaults bind in the method frame, ahead of the body
        // (a default that dies fails the call the way a body error does).
        let prologue = if deferred_defaults.is_empty() {
            Ok(())
        } else {
            self.run_method_default_prologue(method_def, owner_sym, cc, &deferred_defaults)
        };
        let super::vm_method_body_run::MethodBodyOutcome {
            result,
            explicit_return,
            fail_bypass,
        } = match prologue {
            Ok(()) => self.run_compiled_method_body(
                cc,
                compiled_fns,
                super::vm_method_body_run::MethodBodyFrame {
                    owner_sym,
                    method_sym,
                    method_callable_id,
                    saved_stack_depth,
                    let_mark,
                },
            ),
            Err(error) => super::vm_method_body_run::MethodBodyOutcome {
                result: Err(error),
                explicit_return: None,
                fail_bypass: false,
            },
        };

        // The return path reads values across frames.
        if marked_frame {
            self.env_mut().unmark_outer_is_unit();
        }
        // Restore the caller's `when_matched` — see the slow path above.
        self.set_when_matched(saved_when_matched);
        self.module.lexical_fatal_mode = saved_lexical_fatal_mode;

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::NIL)
            } else {
                Value::NIL
            }
        } else {
            Value::NIL
        };

        // Natural fall-through completion: resolve the body's own `let`/`temp`
        // saves against the value it produced. A method body is a block, so it
        // owns the saves its `let`/`temp` recorded; without this they leaked
        // into the caller's scope (#7646).
        if result.is_ok() && explicit_return.is_none() {
            self.resolve_frame_let_saves(let_mark, &ret_val);
        }

        self.stack.truncate(saved_stack_depth);

        // Resolve a capture-valued `--> T` before the env teardown below (see
        // the slow path's note, #7984).
        // A plain core return type (`--> Int`) that names itself for this
        // owner is checked against the value directly, below; the spec is
        // resolved only for a call that falls out of that lane.
        let return_fast = method_def
            .return_type
            .as_deref()
            .and_then(|spec| self.method_return_fast(owner_class, spec));
        let effective_return_spec = method_def
            .return_type
            .as_deref()
            .filter(|_| return_fast.is_none())
            .map(|spec| self.method_effective_return_spec(owner_class, spec));

        // Sync state variables
        for (slot, key) in &cc.state_locals {
            let val = self.locals[*slot].clone();
            let scoped_key = self.scoped_state_key(*key);
            loan_env!(self, set_state_var(scoped_key, val));
        }
        self.lexicals.state_scope_id.set(saved_state_scope);
        crate::alloc_scope_end!(_sc_body);
        crate::alloc_scope!("mfast:epilogue");

        // Snapshot this frame's final lexical state for any END phaser it
        // registered. A `my` in a fast-path method usually lives only in a local
        // slot, so the slots are pushed into the (about-to-be-discarded) callee
        // env first — otherwise the phaser would keep the value the slot held
        // when it was registered. Gated on the phaser count, so an ordinary
        // method call pays one integer compare.
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
        if cc.uses_dispatcher {
            self.pop_method_samewith_context();
        }
        self.pop_method_class();
        if let Some(pkg) = saved_package {
            self.set_current_package_sym(pkg);
        }

        // ADR-0035 slice 2: both branches below unconditionally pop the call
        // frame, so pop the caller-env entry once here (mirrors the paired push
        // above) rather than duplicating the gate in each branch.
        if pushed_caller {
            self.pop_caller_env();
        }

        if can_skip_merge {
            // No env writes possible — just restore saved env (pure: caller
            // slots stay coherent, opcode skips the env_dirty mark).
            self.dispatch.method_dispatch_pure = true;
            let frame = self.pop_call_frame();
            self.set_env(frame.saved_env);
        } else {
            // The frame's writes reach the caller through the variables' own
            // cells (ADR-12529, #18274 slice 3): only a runtime-named write and
            // a nested method capture's plain value are carried.
            let frame = self.pop_call_frame();
            if self.env().ptr_eq(&frame.saved_env) {
                self.set_env(frame.saved_env);
            } else {
                let current_env = self.take_env();
                let carried = if self.pending_runtime_name_writes.is_empty() {
                    Vec::new()
                } else {
                    // Callee-frame key predicate for the carry (keys that should
                    // NOT leak to the caller): frame fixtures, params, attribute
                    // twigil forms, and compiled locals.
                    let class_body_static_names = self.lexicals.class_body_static_names.snapshot();
                    let is_method_local = |s: &str| -> bool {
                        matches!(s, "self" | "__ANON_STATE#" | "?CLASS" | "?ROLE" | "_")
                                || method_def.params.iter().any(|p| p == s)
                                // This call's role composition bindings (see the
                                // primary merge path's matching arm).
                                || role_binding_names.iter().any(|n| n == s)
                                || method_def
                                    .role_param_bindings
                                    .as_deref()
                                    .is_some_and(|b| b.iter().any(|(n, _)| n == s))
                                // As in the primary merge path above (#16361).
                                || s.starts_with(crate::runtime::enum_bare_names::ENUM_BARE_PREFIX)
                                || bound_by_sub_signature(&method_def.param_defs, s)
                                || bound_by_type_capture(&method_def.param_defs, s)
                                || cc.locals.iter().any(|l| !l.is_empty() && l == s)
                                // Env-only `my` declarations (no compiled slot, e.g.
                                // `if (my $file = ...)`) are frame state too — same
                                // exclusion as the primary merge path above (Text::CSV's
                                // `method csv` clobbered a same-named caller `$file`).
                                || cc.env_only_decls.iter().any(|n| n == s)
                                || dynamic_decl_local(&cc.locals, s)
                                || class_body_static_names
                                    .get(owner_class)
                                    .is_some_and(|names| names.contains(s))
                                || attrs_cell
                                    .as_ref()
                                    .is_some_and(|c| attr_twigil_local(&c.as_map(), s))
                                || s.strip_prefix("__mutsu_sigilless_alias::")
                                    .is_some_and(|name| method_def.param_defs.iter().any(|pd| pd.storage_name() == name))
                                || s.strip_prefix("__mutsu_sigilless_readonly::")
                                    .is_some_and(|name| method_def.param_defs.iter().any(|pd| pd.storage_name() == name))
                    };
                    self.carried_runtime_name_writes(&current_env, &is_method_local)
                };
                self.persist_nested_capture_writes(owner_sym, method_def, &current_env);
                drop(current_env);
                let mut merged = frame.saved_env;
                for (name, v) in carried {
                    merged.insert(name, v);
                }
                self.set_env(merged);
            }
            self.dispatch.method_dispatch_pure = true;
        }

        // The caller env is restored now, so a name the callee held that the
        // caller does not is one this frame took with it: freeze the phaser's
        // captured copy for those, leave the rest reading live.
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
            self.finalize_method_result_fast(
                cc,
                final_result,
                method_def.return_type.as_deref(),
                effective_return_spec.as_deref(),
                return_fast,
            )
        };

        // Restore the routine registry (removing this body's lexical routines)
        // unless an inner routine escaped via the return value, or a closure
        // literal created during the call may have escaped via a side channel
        // (e.g. handed to `.tap`) -- see `closures_created`.
        if let Some(snapshot) = routine_snapshot {
            let closure_escaped = self.closures_created != closures_created_before;
            let skip_restore = closure_escaped
                || matches!(&final_result, Ok(v) if Self::return_value_escapes_routine(v));
            if !skip_restore {
                self.restore_routine_registry(snapshot);
            }
        }

        // A result that is the invocant itself shares its live cell.
        final_result.map(|v| (Self::method_frame_result_sharing(&base, v), None))
    }
}

/// The empty role-binding name list, shared: an `Arc<[String]>` of length
/// zero still allocates its counts, once per call before this.
// Cost: O(1).
fn no_role_binding_names() -> std::sync::Arc<[String]> {
    static EMPTY: std::sync::OnceLock<std::sync::Arc<[String]>> = std::sync::OnceLock::new();
    std::sync::Arc::clone(EMPTY.get_or_init(|| std::sync::Arc::from([])))
}

/// O(1), conservative "is this value provably unchanged?" check used by
/// `merge_method_env` to avoid flipping `env_dirty` for value-identical copies
/// (e.g. globals pulled in by a nested call's overlay flatten). Returns `true`
/// only when equality is cheap to prove: Arc-pointer identity for the heap
/// container/string types, value compare for the immutable scalars, and id
/// compare for instances. Any case it cannot cheaply prove returns `false`, so
/// the caller treats it as a possible change (a redundant pull at worst).
pub(crate) fn cheaply_unchanged(old: &Value, new: &Value) -> bool {
    match (old.view(), new.view()) {
        (ValueView::Int(a), ValueView::Int(b)) => a == b,
        (ValueView::Num(a), ValueView::Num(b)) => a.to_bits() == b.to_bits(),
        (ValueView::Bool(a), ValueView::Bool(b)) => a == b,
        (ValueView::Rat(an, ad), ValueView::Rat(bn, bd)) => an == bn && ad == bd,
        (ValueView::Package(a), ValueView::Package(b)) => a == b,
        (ValueView::Nil, ValueView::Nil) => true,
        (ValueView::Str(a), ValueView::Str(b)) => Arc::ptr_eq(&a, &b),
        (ValueView::Array(a, _), ValueView::Array(b, _)) => crate::gc::Gc::ptr_eq(&a, &b),
        (ValueView::Hash(a), ValueView::Hash(b)) => crate::gc::Gc::ptr_eq(&a, &b),
        // A `ContainerRef` cell is the shared-identity primitive of the
        // single-store design: `my @a := @b` installs the *same* cell into both
        // the env entry and the local slot for both names. Without this arm the
        // generic `_ => false` reported every same-cell pair as "changed", so the
        // reverse-sync survey over-counted bound containers as effective stale
        // (and `merge_method_env` over-signalled env_dirty for a method that
        // returned the same cell). Same Arc => genuinely unchanged; distinct Arcs
        // stay "changed" (conservative).
        (ValueView::ContainerRef(a), ValueView::ContainerRef(b)) => crate::gc::Gc::ptr_eq(&a, &b),
        (ValueView::Instance { id: a, .. }, ValueView::Instance { id: b, .. }) => a == b,
        // A `Sub` is a Gc-backed heap value like `Str`/`Array`/`Hash` above, so
        // the same pointer-identity proof applies: the same routine object under
        // both names means the callee did not change the caller's variable.
        // Without this arm every Sub-valued env entry a callee merely INHERITED
        // from the flattened overlay was reported as changed, so
        // `merge_method_env` listed it in `changed_caller_locals` and the Slice F
        // write-through replaced the caller's live local slot with the env copy.
        // In a routine re-entered through a Sub held in a data structure, env
        // still carried the OUTER invocation's routine, so the inner frame's
        // freshly resolved callable was overwritten by its caller's and calling
        // it re-entered the wrong closure until the stack overflowed (#7729).
        (ValueView::Sub(a), ValueView::Sub(b)) => crate::gc::Gc::ptr_eq(&a, &b),
        // A mixin (`$attr does Role`, `$x but Role`) is an immutable wrapper
        // over `Arc<Value>` + `Gc<MixinOverrides>`, so the same pair of
        // pointers proves the callee did not rebind the captured variable.
        // Without this arm a captured mixin was always "changed" and a nested
        // call of a same-shaped closure leaked its own capture back into the
        // caller (AttrX::Lazy's accessors read each other's `$attr`).
        (ValueView::Mixin(ai, ao), ValueView::Mixin(bi, bo)) => {
            Arc::ptr_eq(ai, bi) && crate::gc::Gc::ptr_eq(ao, bo)
        }
        _ => false,
    }
}

/// True if `s` is an attribute-twigil env key (`!x` / `.x` / `@!x` / `@.x` /
/// `%!x` / `%.x`) whose bare name is an attribute of this instance. Predicate
/// form of the former per-call set build, which inserted all 6 twigil forms
/// for every attribute key (skipping class-qualified `"C\0attr"` storage keys
/// and `__mutsu_attr_alias::` metadata keys).
/// A `my $*x` declared in the method frame is stored under both `*x` (its
/// compiled local) and `$*x`; the `$*x` spelling is as frame-local as `*x`, so
/// the merge must not hand the method's `my $*OUT` back to the caller (where
/// `print` reads it by its `$*OUT` key).
// Cost: O(l), l = compiled locals.
pub(super) fn dynamic_decl_local(locals: &[String], s: &str) -> bool {
    s.strip_prefix('$')
        .is_some_and(|bare| bare.starts_with('*') && locals.iter().any(|l| l == bare))
}

pub(super) fn attr_twigil_local(attributes: &AttrMap, s: &str) -> bool {
    let bare = match s.as_bytes() {
        [b'@' | b'%', b'!' | b'.', ..] => &s[2..],
        [b'!' | b'.', ..] => &s[1..],
        _ => return false,
    };
    !bare.contains('\0')
        && !bare.starts_with(ATTR_ALIAS_META_PREFIX)
        && attributes.contains_key(bare)
}

/// True if `s` is a sigilless-attribute alias name for this instance: either
/// the alias name stored as the value of a `__mutsu_attr_alias::<attr>` key,
/// or the `<attr>` suffix of such a key. Predicate form of the former set
/// build's alias branch (slow path only — the fast path gate excludes
/// alias-carrying instances).
fn attr_alias_local(attributes: &AttrMap, s: &str) -> bool {
    attributes.iter().any(|(k, v)| {
        let k = k.as_str();
        !k.contains('\0')
            && k.strip_prefix(ATTR_ALIAS_META_PREFIX)
                .is_some_and(|actual| {
                    actual == s || matches!(v.view(), ValueView::Str(alias) if alias.as_str() == s)
                })
    })
}

/// True if `s` has the SHAPE of an attribute-twigil env key (`!x` / `.x` /
/// `@!x` / `@.x` / `%!x` / `%.x`), regardless of which instance -- unlike
/// `attr_twigil_local`, this does not check any particular attribute map. A
/// caller-side named-arg source encoded this way (e.g. `:%!plugin-config`,
/// where the argument expression IS an attribute read) is never a genuine
/// caller lexical the exit-time `rw_writeback` merge is entitled to rebind;
/// see the `rw_writeback` loop below.
fn is_attr_twigil_shaped(s: &str) -> bool {
    matches!(
        s.as_bytes(),
        [b'@' | b'%', b'!' | b'.', ..] | [b'!' | b'.', ..]
    )
}

/// Whether `name` is bound by a parameter's sub-signature (`:mq(@)! ($media,)`):
/// such a variable is as frame-local as the parameter itself.
// Cost: O(p), p = parameters and sub-signature entries of the method.
/// Whether `name` is an env key a `::T` type capture of the method's own
/// signature binds for this call: the captured name (`T`), its
/// package-qualified mirror (`Pkg::T`), or its bound marker
/// (`bind_type_capture`). The capture is the callee's lexical; merged back,
/// it rebound a caller's own same-named capture (`method a(::T \t) {
/// self.b(Int); T }` answered `Int`), which broke Red's `add-column`.
// Cost: O(p), p = parameters.
fn bound_by_type_capture(param_defs: &[crate::ast::ParamDef], name: &str) -> bool {
    let name = name
        .strip_prefix(crate::meta_ns::MetaNs::TypeCaptureBound.prefix())
        .unwrap_or(name);
    let bare = crate::str_scan::rsplit_once_double_colon(name).map_or(name, |(_, bare)| bare);
    param_defs.iter().any(|pd| {
        pd.captured_type_name()
            .is_some_and(|t| t == name || t == bare)
    })
}

fn bound_by_sub_signature(param_defs: &[crate::ast::ParamDef], name: &str) -> bool {
    param_defs.iter().any(|pd| {
        [
            pd.sub_signature.as_deref(),
            pd.outer_sub_signature.as_deref(),
        ]
        .into_iter()
        .flatten()
        .any(|sub| {
            sub.iter().any(|p| p.storage_name() == name) || bound_by_sub_signature(sub, name)
        })
    })
}

/// Whether a method can reach the entry `sym` of its defining scope's
/// capture (`MethodDef::captured_env`): a name its body mentions, or anything
/// when the body's lookups cannot be bounded (`CompiledCode::mentions_name_memo`)
/// or its signature can name the scope (`signature_reads_only_builtins`).
/// The capture of an `.^add_method` closure literal is the whole scope it was
/// written in, so installing every entry on every call cost the size of that
/// scope twice, once to insert and once more to merge back.
/// Internal `__mutsu_*` keys are always installed.
// Cost: O(p) for the signature test, p = parameters; the name test is O(1)
// after the chunk's first call (see `mentions_name_memo`).
fn captured_name_reachable(
    cc: &CompiledCode,
    method_def: &crate::runtime::MethodDef,
    sym: Symbol,
) -> bool {
    cc.mentions_name_memo(sym) != Some(false)
        || sym.starts_with("__mutsu_")
        || !signature_reads_only_builtins(method_def)
}

/// Whether binding `method_def`'s signature reads nothing from the method's
/// defining scope: every parameter is free of defaults, `where` clauses,
/// sub-signatures and trait arguments, and every type it names (parameters
/// and return) is a built-in one. A type capture (`T:U:`), a user type or a
/// default expression can name a lexical of that scope, which the body's
/// own mentions do not cover.
// Cost: O(p), p = parameters.
fn signature_reads_only_builtins(method_def: &crate::runtime::MethodDef) -> bool {
    let builtin = |tc: &str| {
        let base = tc
            .strip_suffix(":D")
            .or_else(|| tc.strip_suffix(":U"))
            .or_else(|| tc.strip_suffix(":_"))
            .unwrap_or(tc);
        crate::runtime::utils::is_known_type_constraint(base)
    };
    method_def.return_type.as_deref().is_none_or(builtin)
        && method_def.param_defs.iter().all(|pd| {
            pd.default.is_none()
                && pd.where_constraint.is_none()
                && pd.sub_signature.is_none()
                && pd.outer_sub_signature.is_none()
                && pd.code_signature.is_none()
                && pd.shape_constraints.is_none()
                && pd.trait_args.is_empty()
                && pd.type_capture.is_none()
                && pd.type_constraint.as_deref().is_none_or(builtin)
        })
}

/// The names a frame's record owns (`self`, `?CLASS`, `?ROLE`): no env key
/// stores them (ADR-14843).
fn is_fixed_frame_name(name: &str) -> bool {
    matches!(name, "self" | "?CLASS" | "?ROLE")
}

#[cfg(test)]
mod cheaply_unchanged_tests {
    use super::cheaply_unchanged;
    use crate::value::Value;

    fn make_sub() -> Value {
        Value::make_sub(
            crate::symbol::Symbol::intern("MAIN"),
            crate::symbol::well_known::anon(),
            Vec::new(),
            Vec::new(),
            Vec::new(),
            false,
            crate::env::Env::new(),
        )
    }

    #[test]
    fn container_ref_same_cell_is_unchanged() {
        // The single-store identity primitive: `my @a := @b` installs one shared
        // cell into both the env entry and the local slot. Comparing those two
        // (same Arc) must report "unchanged" so the reverse-sync survey does not
        // count a bound container as effective stale, and `merge_method_env` does
        // not over-signal env_dirty for a method returning the same cell.
        let cell = crate::gc::Gc::new(crate::value::ContainerCell::new(Value::array(vec![
            Value::int(1),
        ])));
        let a = Value::container_ref(cell.clone());
        let b = Value::container_ref(cell);
        assert!(cheaply_unchanged(&a, &b));
    }

    #[test]
    fn same_sub_object_is_unchanged() {
        // #7729: two env entries holding the SAME routine object must compare
        // unchanged, or `merge_method_env` reports the callee's inherited copy
        // as a caller-visible write and the write-through rewinds the caller's
        // local slot to it.
        let sub = make_sub();
        assert!(cheaply_unchanged(&sub, &sub.clone()));
    }

    #[test]
    fn distinct_subs_are_changed() {
        assert!(!cheaply_unchanged(&make_sub(), &make_sub()));
    }

    #[test]
    fn container_ref_distinct_cells_are_changed() {
        // Distinct cells (even with equal contents) stay "changed" — the test is
        // conservative Arc identity, never deep value equality.
        let a = Value::container_ref(crate::gc::Gc::new(crate::value::ContainerCell::new(
            Value::array(vec![Value::int(1)]),
        )));
        let b = Value::container_ref(crate::gc::Gc::new(crate::value::ContainerCell::new(
            Value::array(vec![Value::int(1)]),
        )));
        assert!(!cheaply_unchanged(&a, &b));
    }
}
