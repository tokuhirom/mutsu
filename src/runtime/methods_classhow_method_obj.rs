use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    pub(super) fn collect_class_methods(
        &self,
        class_name: &str,
        include_private: bool,
        result: &mut Vec<Value>,
    ) {
        let registry = self.registry();
        let Some(class_def) = registry.classes.get(class_name) else {
            return;
        };
        // First add accessor methods for public attributes (in order)
        for attr in &class_def.attributes {
            if attr.is_public
                && registry
                    .user_method_overloads(class_name, &attr.name)
                    .is_none()
            {
                result.push(self.make_native_method_object(&attr.name, class_name));
            }
        }
        // Then add explicit methods. The overload data comes from the
        // canonical `Registry::method_entries[(owner, name)].user_candidates`
        // table (ADR-0019 Phase F box F1 item 1); the name enumeration itself
        // is the canonical reverse index (F4c-1: `owner_method_names`,
        // zero-mismatch shadow-checked against `ClassDef::methods.keys()`
        // across the full local `t/` suite before this cutover).
        for method_name in registry.owner_method_names(class_name) {
            let method_name = method_name.resolve();
            let Some(overloads) = registry.user_method_overloads(class_name, &method_name) else {
                continue;
            };
            if overloads.is_empty() {
                continue;
            }
            // Skip private methods unless :private
            let first = &overloads[0];
            if first.is_private && !include_private {
                continue;
            }
            let is_multi = overloads.len() > 1;
            let return_type = first.return_type.clone();
            let method_obj = self.make_method_object_with_owner(
                &method_name,
                first,
                is_multi,
                return_type,
                Some(&overloads),
                Some(class_name),
            );
            result.push(method_obj);
        }
        // Also include native (built-in) methods
        for native_name in &class_def.native_methods {
            let method_obj = self.make_native_method_object(native_name, class_name);
            result.push(method_obj);
        }
    }

    /// Build the class's own method table (`.^method_table`): the methods
    /// declared directly on `class_name`, keyed by name.
    ///
    /// Rakudo keeps submethods in `.^submethod_table` and private methods in
    /// `.^private_method_table`, so neither appears here; public attribute
    /// accessors and role-composed methods do, and a `multi` contributes a
    /// single dispatcher entry.
    pub(super) fn class_method_table(&self, class_name: &str) -> HashMap<String, Value> {
        let mut table = HashMap::new();
        // RakuAST model classes are native type objects and therefore have no
        // ClassDef entry. Keep method_table in lockstep with
        // `.^methods(:local)` by deriving both from the same model metadata.
        if let Some(names) = crate::rakuast::local_method_names(class_name) {
            for name in names {
                table.insert(
                    name.to_string(),
                    self.make_native_method_object(name, class_name),
                );
            }
            return table;
        }
        let registry = self.registry();
        let Some(class_def) = registry.classes.get(class_name) else {
            return table;
        };
        for attr in &class_def.attributes {
            if attr.is_public
                && registry
                    .user_method_overloads(class_name, &attr.name)
                    .is_none()
            {
                table.insert(
                    attr.name.clone(),
                    self.make_native_method_object(&attr.name, class_name),
                );
            }
        }
        // ADR-0019 F4c-1: enumerate via the canonical reverse index instead
        // of `class_def.methods.keys()` (zero-mismatch shadow-checked across
        // the full local `t/` suite before this cutover).
        for method_name in registry.owner_method_names(class_name) {
            let method_name = method_name.resolve();
            let Some(overloads) = registry.user_method_overloads(class_name, &method_name) else {
                continue;
            };
            let Some(first) = overloads.first() else {
                continue;
            };
            if first.is_private || first.is_submethod {
                continue;
            }
            table.insert(
                method_name.clone(),
                self.make_method_object_with_owner(
                    &method_name,
                    first,
                    overloads.len() > 1,
                    first.return_type.clone(),
                    Some(&overloads),
                    Some(class_name),
                ),
            );
        }
        for native_name in &class_def.native_methods {
            table.insert(
                native_name.clone(),
                self.make_native_method_object(native_name, class_name),
            );
        }
        table
    }

    /// Collect methods from a runtime-mixed-in role definition.
    pub(super) fn collect_role_methods(
        &self,
        role_name: &str,
        include_private: bool,
        result: &mut Vec<Value>,
    ) {
        if let Some(role_def) = self.registry().roles.get(role_name) {
            // Add accessor methods for public attributes
            for attr in &role_def.attributes {
                if attr.is_public && !role_def.methods.contains_key(&attr.name) {
                    result.push(self.make_native_method_object(&attr.name, role_name));
                }
            }
            // Add explicit methods
            for (method_name, overloads) in &role_def.methods {
                if overloads.is_empty() {
                    continue;
                }
                let first = &overloads[0];
                if first.is_private && !include_private {
                    continue;
                }
                let is_multi = overloads.len() > 1;
                let return_type = first.return_type.clone();
                let method_obj = self.make_method_object_with_owner(
                    method_name,
                    first,
                    is_multi,
                    return_type,
                    Some(overloads),
                    Some(role_name),
                );
                result.push(method_obj);
            }
        }
    }

    /// `owner` is the catalog type this native method is being reported for --
    /// ADR-0019 Phase F box F1's mechanism slice: `.package` defaults to it,
    /// and `.signature` defaults to a synthesized generic shape (see
    /// [`crate::value::signature::synthesize_native_signature`]). Neither is
    /// always Rakudo's true answer (e.g. `Str.uc`'s real `.package` is
    /// `Cool`, not `Str`) -- that per-method fidelity data is a later,
    /// separate slice (see
    /// `todo/deep/adr0019-f1-f2-introspection-canonical-source.md`).
    pub(super) fn make_native_method_object(&self, name: &str, owner: &str) -> Value {
        self.make_native_method_object_ex(name, owner, false)
    }

    /// `is_regex`: the method is a grammar `token`/`rule`/`regex` -- its
    /// `__mutsu_method_callable` payload must carry that flag so invoking the
    /// returned Method `Instance` (`CALL-ME`) runs it against a `Cursor` like
    /// `call_sub_value`'s own `ValueView::Routine { is_regex, .. }` branch
    /// already does for a bare token value.
    pub(super) fn make_native_method_object_ex(
        &self,
        name: &str,
        owner: &str,
        is_regex: bool,
    ) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::str(name.to_string()));
        attrs.insert("is_dispatcher".to_string(), Value::FALSE);
        attrs.insert("multi".to_string(), Value::FALSE);
        attrs.insert("rw".to_string(), Value::FALSE);
        attrs.insert("readonly".to_string(), Value::TRUE);
        attrs.insert("package".to_string(), Value::package(Symbol::intern(owner)));
        attrs.insert(
            "signature".to_string(),
            crate::value::signature::make_signature_value(
                crate::value::signature::synthesize_native_signature(owner),
                Some(self),
            ),
        );
        attrs.insert("returns".to_string(), Value::package(Symbol::intern("Mu")));
        attrs.insert("of".to_string(), Value::package(Symbol::intern("Mu")));
        // `.WHY` (`dispatch_why`'s `Instance` branch) builds its doc-comment
        // lookup key from these two, the same way it does for a plain user
        // method -- without them, a grammar token/rule/regex's `#|` comment
        // was unreachable (roast S26-documentation/why-leading.t's "regex"
        // case).
        attrs.insert(
            "__mutsu_lookup_class".to_string(),
            Value::str(owner.to_string()),
        );
        attrs.insert(
            "__mutsu_lookup_method".to_string(),
            Value::str(name.to_string()),
        );
        // ADR-0019 Phase F box F1: the Sub-vs-Instance unification. Carrying
        // the original Routine marker lets `CALL-ME` invoke this Instance
        // exactly as `.^lookup`/`.^find_method` used to return it directly.
        attrs.insert(
            "__mutsu_method_callable".to_string(),
            Value::routine_parts(Symbol::intern(owner), Symbol::intern(name), is_regex),
        );
        // Real Raku: a grammar `token`/`rule`/`regex`'s `.^lookup`/`.^name` is
        // `Regex`, not `Method` (`raku -e 'grammar G { regex X {} }; say
        // G.^lookup("X").^name'` -> `Regex`) -- verified for all three
        // declarators, which are otherwise indistinguishable at this point.
        let class_name = if is_regex { "Regex" } else { "Method" };
        Value::make_instance(Symbol::intern(class_name), attrs)
    }

    /// Records the owning class/role so a `.wrap` on the returned Method
    /// object (e.g. from `.^methods(:local)` in a custom metaclass `compose`,
    /// `advent2011-day14`) can register into the class-keyed
    /// `method_wrap_chains` and take effect for later dispatch.
    pub(super) fn make_method_object_with_owner(
        &self,
        name: &str,
        method_def: &MethodDef,
        is_dispatcher: bool,
        return_type: Option<String>,
        overloads: Option<&[MethodDef]>,
        owner_class: Option<&str>,
    ) -> Value {
        self.make_method_object_with_owner_ex(
            name,
            method_def,
            is_dispatcher,
            return_type,
            overloads,
            owner_class,
            false,
            None,
            0,
        )
    }

    /// `is_multi_candidate`: this object is one entry of a multi family's
    /// `.candidates` array (real Raku: `.multi` answers `True` there, `False`
    /// on a plain single method and on the dispatcher itself -- see the
    /// `is_dispatcher`/`multi` handling `methods_instance_ops.rs` implements
    /// for the older Sub-shaped `.^lookup` return, which this generalizes to
    /// the `Instance` shape). Always `false` from the public wrapper; set
    /// `true` only by the recursive `candidates` build below.
    ///
    /// `explicit_candidates`, when `Some`, is used as the `.candidates` array
    /// verbatim instead of building it from `overloads`/`owner_class` --
    /// needed when a multi family combines candidates declared on more than
    /// one class in the MRO (`classhow_lookup_all_candidates`), where each
    /// candidate has its own owner and the single-`owner_class` builder below
    /// cannot represent that (roast S06-advanced/wrap.t "multi methods with
    /// a wrapped one are in order").
    #[allow(clippy::too_many_arguments)]
    pub(super) fn make_method_object_with_owner_ex(
        &self,
        name: &str,
        method_def: &MethodDef,
        is_dispatcher: bool,
        return_type: Option<String>,
        overloads: Option<&[MethodDef]>,
        owner_class: Option<&str>,
        is_multi_candidate: bool,
        explicit_candidates: Option<Vec<Value>>,
        candidate_idx: usize,
    ) -> Value {
        let mut attrs = std::collections::HashMap::new();

        // Store the display name (with ! prefix for private methods)
        let display_name = if method_def.is_private {
            format!("!{}", name)
        } else {
            name.to_string()
        };
        attrs.insert("name".to_string(), Value::str(display_name));
        attrs.insert("is_dispatcher".to_string(), Value::truth(is_dispatcher));
        attrs.insert("multi".to_string(), Value::truth(is_multi_candidate));
        attrs.insert("rw".to_string(), Value::truth(method_def.is_rw));
        attrs.insert("readonly".to_string(), Value::truth(!method_def.is_rw));
        // The method name is always recorded so `CALL-ME` on a dispatcher
        // (no single callable of its own) can re-dispatch on the first
        // argument, mirroring the old Sub-shaped dispatcher's
        // `sub_multi_method_dispatcher_name` re-dispatch.
        attrs.insert(
            "__mutsu_lookup_method".to_string(),
            Value::str(name.to_string()),
        );
        // The owning class is likewise recorded whenever known, dispatcher or
        // not: `^add_method`'s multi-family alias detection
        // (`unwrap_method_instance_callable`) needs `__mutsu_lookup_class`
        // present on a DISPATCHER too, mirroring the pre-Instance code, which
        // set the equivalent Sub env tag unconditionally.
        if let Some(owner) = owner_class {
            attrs.insert(
                "__mutsu_lookup_class".to_string(),
                Value::str(owner.to_string()),
            );
        }
        // Record the candidate slot + callable so a `.wrap` on this Method
        // object can register a class-keyed wrap chain (see `wrap` dispatch
        // for Method instances), and so `CALL-ME` can invoke it directly.
        // Single (non-multi) methods are candidate 0; multi methods are
        // wrapped through their `.candidates[N]` entries instead. `.package`
        // shares the same gate: a real Rakudo multi *dispatcher*'s own
        // `.package` is an internal synthetic type (`(Dummy)`), not the
        // declaring class, so it is deliberately left unset here rather than
        // guessed -- but each individual (non-dispatcher) candidate's
        // `.package` is exactly `owner_class`, verified against `raku`.
        //
        // ADR-0019 Phase F box F1: `__mutsu_method_callable` is the Sub value
        // that `CALL-ME` invokes, unifying this Instance's representation
        // with the plain-Sub shape `.^lookup`/`.^find_method` used to return
        // directly (`todo/tickets/classhow-lookup-returns-sub-not-method-
        // instance.md`) -- a dispatcher has no single callable (it must
        // re-dispatch on the first argument the way the old Sub-shaped
        // dispatcher's `sub_multi_method_dispatcher_name` did), so only a
        // non-dispatcher candidate gets one.
        if let Some(owner) = owner_class
            && !is_dispatcher
        {
            attrs.insert(
                "__mutsu_lookup_candidate_idx".to_string(),
                Value::int(candidate_idx as i64),
            );
            attrs.insert("package".to_string(), Value::package(Symbol::intern(owner)));
            attrs.insert(
                "__mutsu_method_callable".to_string(),
                Self::method_def_callable(owner, name, method_def),
            );
        }

        // Build a Signature object for this method, threading the return type
        // so that `.signature.returns` reflects a `--> Type` declaration. Real
        // Rakudo's `Method.signature` always carries the invocant as
        // `params[0]` (`B.^find_method('foo').signature.gist` is
        // `(B $:: $!a, *%_)`, not `($!a, *%_)`) -- prepend one the same way
        // `method_def_callable` does for the actual callable, unless the
        // declaration already names one explicitly. Without this,
        // `.signature.params[N]` was off by one against the invocant-carrying
        // `Sub`-shaped signature `.^lookup`/`.^find_method` used to return
        // directly (roast S06-signature/introspection.t).
        let has_explicit_invocant = method_def
            .param_defs
            .iter()
            .any(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"));
        let mut full_param_defs = Vec::with_capacity(method_def.param_defs.len() + 1);
        if let Some(owner) = owner_class
            && !has_explicit_invocant
        {
            full_param_defs.push(Self::make_invocant_param(owner));
        }
        full_param_defs.extend(method_def.param_defs.iter().cloned());
        // Cache the materialized Signature under a stable per-(owner, name,
        // candidate) key when one is available (a real candidate, not the
        // synthetic multi dispatcher -- mirrors the `__mutsu_lookup_
        // candidate_idx`/`package`/`__mutsu_method_callable` gate just
        // above), so repeated `.^find_method(...).signature` reads for the
        // SAME declaration return the same `Signature`/`Parameter` objects
        // instead of a fresh, un-mixin-able one every time -- see
        // `crate::value::signature::SubSignatureKey::Method`'s doc comment.
        let method_sig_cache_key = owner_class.filter(|_| !is_dispatcher).map(|owner| {
            crate::value::signature::SubSignatureKey::from_method(owner, name, candidate_idx)
        });
        if let Some(key) = &method_sig_cache_key
            && let Some(cached) = crate::value::signature::cached_sub_signature(key)
        {
            attrs.insert("signature".to_string(), cached);
        } else {
            let sig_info = crate::value::signature::param_defs_to_sig_info(
                &full_param_defs,
                return_type.clone(),
            );
            // Thread an owner key so a parameter's own `#=`/`#|` doc comment
            // is reachable through `.signature.params[N].WHY` -- mirrors
            // `sub_signature_value`'s owner-key format exactly (`"ClassName::
            // name"`), without which `__mutsu_owner_sub` was never set on the
            // Parameter object and the comment was unreachable (roast
            // S26-documentation/why-trailing.t's "invocant comment" case).
            let owner_key = owner_class.map(|owner| format!("{owner}::{name}"));
            let signature = crate::value::signature::make_signature_value_with_owner(
                sig_info,
                owner_key,
                Some(self),
            );
            if let Some(key) = method_sig_cache_key {
                crate::value::signature::cache_sub_signature(key, signature.clone());
            }
            attrs.insert("signature".to_string(), signature);
        }

        // Return type
        let rt = return_type.unwrap_or_else(|| "Mu".to_string());
        attrs.insert("returns".to_string(), Value::package(Symbol::intern(&rt)));
        attrs.insert("of".to_string(), Value::package(Symbol::intern(&rt)));

        // For a multi method dispatcher, attach one Method object per
        // candidate so that `.candidates` returns them. A non-multi (single)
        // method is its own sole candidate.
        let candidates: Vec<Value> = match explicit_candidates {
            Some(cands) => cands,
            None => match overloads {
                Some(defs) if is_dispatcher => defs
                    .iter()
                    .enumerate()
                    .map(|(idx, def)| {
                        self.make_method_object_with_owner_ex(
                            name,
                            def,
                            false,
                            def.return_type.clone(),
                            None,
                            owner_class,
                            true,
                            None,
                            idx,
                        )
                    })
                    .collect(),
                _ => Vec::new(),
            },
        };
        if !candidates.is_empty() {
            attrs.insert("candidates".to_string(), Value::array(candidates));
        }

        // Real Raku: `.^lookup`/`.^find_method` walk the whole MRO and DO
        // surface a submethod (`.^methods`/`.^method_table`, this function's
        // other callers, filter submethods out before ever reaching here --
        // see `collect_class_methods`'s doc comment -- so this case was
        // previously unreachable); its `.^name` is `Submethod`, not `Method`
        // (`raku -e 'class C { submethod BUILD {} }; say C.^lookup("BUILD")
        // .^name'` -> `Submethod`). A multi dispatcher's own submethod-ness
        // isn't modeled (Raku does not support `multi submethod`), so this
        // only matters for the non-dispatcher/candidate case.
        let instance_class_name = if method_def.is_submethod {
            "Submethod"
        } else {
            "Method"
        };
        let method_obj = Value::make_instance(Symbol::intern(instance_class_name), attrs);
        // A non-multi (single) method's own `.candidates` is itself, a
        // one-element list -- verified against `raku`
        // (`Foo.^lookup('bar').candidates[0]` on a plain method, not just a
        // multi's). Set once the Instance exists so it can hold a self-copy.
        if !is_dispatcher
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = method_obj.view()
        {
            let mut am = attributes.as_map().clone();
            am.insert(
                "candidates".to_string(),
                Value::array(vec![method_obj.clone()]),
            );
            return Value::make_instance(class_name, am);
        }
        method_obj
    }

    pub(super) fn classhow_methods_tree(
        &self,
        class_name: &str,
        include_private: bool,
    ) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();

        // First: own methods
        self.collect_class_methods(class_name, include_private, &mut result);

        // Then: each parent's tree as a nested array
        if let Some(class_def) = self.registry().classes.get(class_name) {
            for parent in &class_def.parents {
                let subtree = self.classhow_methods_tree(parent, include_private)?;
                result.push(subtree);
            }
        }

        Ok(Value::array(result))
    }

    /// Collect all methods named `method_name` across the MRO of `target`.
    /// Returns a list of callable Sub values, one per class in the MRO that
    /// defines the method. This implements `.can(method-name)`.
    pub(super) fn collect_can_methods(&mut self, target: &Value, method_name: &str) -> Vec<Value> {
        // A Mixin (`but`/`does`, or a trait handler's `$routine does Role`) has
        // no entry of its own in the class registry, so the generic MRO walk
        // below (keyed by `mop_receiver_owner`) never sees it and mixin-added
        // methods are invisible to `.^can` / `nqp::can` even though `.can`
        // (dispatch_mixin_method_call) already finds them. Delegate to the
        // wrapped value for the base MRO, then add methods contributed by
        // the mixed-in roles, mirroring the `.can` handling above.
        if let ValueView::Mixin(inner, mixins) = target.view() {
            let mut results = self.collect_can_methods(inner.as_ref(), method_name);
            if (mixins.contains_key(method_name)
                || mixins.contains_key(&format!("__mutsu_attr__{method_name}")))
                && results.is_empty()
            {
                results.push(Value::routine_parts(
                    Symbol::intern("Mixin"),
                    Symbol::intern(method_name),
                    false,
                ));
            }
            for role_name in mixins.keys().filter_map(|key| {
                key.strip_prefix("__mutsu_role__")
                    .map(|name| name.to_string())
            }) {
                if let Some(role) = self.registry().roles.get(&role_name)
                    && let Some(defs) = role.methods.get(method_name)
                {
                    for def in defs {
                        results.push(Value::make_sub(
                            Symbol::intern(&role_name),
                            Symbol::intern(method_name),
                            def.params.clone(),
                            def.param_defs.clone(),
                            (*def.body).clone(),
                            def.is_rw,
                            crate::env::Env::new(),
                        ));
                    }
                }
            }
            return results;
        }
        let class_name = match target.view() {
            ValueView::RakuAst(node) => node.class.printed_name().to_string(),
            ValueView::Enum { enum_type, .. } => enum_type.resolve(),
            _ => self.mop_receiver_owner(target),
        };
        // RakuAST model classes are native type objects rather than ordinary
        // ClassDef entries. Keep `.^can` in lockstep with `.^methods(:local)`
        // and `.^method_table` by consulting their shared model metadata.
        if let Some(names) = crate::rakuast::local_method_names(&class_name) {
            return if names.contains(&method_name) {
                vec![Value::routine_parts(
                    Symbol::intern(&class_name),
                    Symbol::intern(method_name),
                    false,
                )]
            } else {
                Vec::new()
            };
        }
        let mro = self.classhow_mro_unhidden_names(target);
        let mut results = Vec::new();
        for cn in &mro {
            // The candidate list comes from the canonical `Registry::method_
            // entries[(owner, name)].user_candidates` table (ADR-0019 Phase F
            // box F1 item 2) rather than `ClassDef::methods` directly --
            // `sync_user_method_entries` keeps the two in lockstep, verified
            // with zero mismatches across a full `t/`+roast sweep by the
            // shadow check this cutover retires (#6402).
            if let Some(defs) = self.registry().user_method_overloads(cn, method_name) {
                let visible: Vec<&MethodDef> = defs
                    .iter()
                    .filter(|def| !def.is_my || cn == &class_name)
                    .collect();
                // A multi method contributes ONE dispatcher entry per class
                // (Raku's `.can` returns the dispatcher Method, not one entry
                // per candidate). The dispatcher-shaped Sub re-dispatches on
                // invocation (see sub_multi_method_dispatcher_name).
                if visible.len() > 1 || visible.iter().any(|d| d.is_multi) {
                    if let Some(def) = visible.first() {
                        let mut full_param_defs = vec![Self::make_invocant_param(cn)];
                        full_param_defs.extend(
                            def.param_defs
                                .iter()
                                .filter(|p| p.name.as_str() != "self")
                                .cloned(),
                        );
                        let mut env = crate::env::Env::new();
                        env.insert(
                            "__mutsu_callable_type".to_string(),
                            Value::str_from(if def.is_submethod {
                                "Submethod"
                            } else {
                                "Method"
                            }),
                        );
                        env.insert(
                            "__mutsu_lookup_class".to_string(),
                            Value::str(cn.to_string()),
                        );
                        env.insert(
                            "__mutsu_lookup_method".to_string(),
                            Value::str(method_name.to_string()),
                        );
                        results.push(Value::make_sub(
                            Symbol::intern(cn),
                            Symbol::intern(method_name),
                            def.params.clone(),
                            full_param_defs,
                            (*def.body).clone(),
                            def.is_rw,
                            env,
                        ));
                    }
                    continue;
                }
                for def in visible {
                    // Prepend "self" to params so the method can be called
                    // as $meth($invocant) — the first argument binds as self.
                    let mut params = vec!["self".to_string()];
                    params.extend(def.params.iter().filter(|p| p.as_str() != "self").cloned());
                    let mut param_defs = vec![crate::ast::ParamDef {
                        name: "self".to_string(),
                        default: None,
                        multi_invocant: true,
                        required: false,
                        named: false,
                        slurpy: false,
                        double_slurpy: false,
                        onearg: false,
                        sigilless: false,
                        type_constraint: None,
                        literal_value: None,
                        sub_signature: None,
                        where_constraint: None,
                        is_invocant: true,
                        traits: Vec::new(),
                        optional_marker: false,
                        outer_sub_signature: None,
                        code_signature: None,
                        shape_constraints: None,
                        block_param: false,
                    }];
                    param_defs.extend(
                        def.param_defs
                            .iter()
                            .filter(|p| p.name.as_str() != "self")
                            .cloned(),
                    );
                    results.push(Value::make_sub(
                        Symbol::intern(cn),
                        Symbol::intern(method_name),
                        params,
                        param_defs,
                        (*def.body).clone(),
                        def.is_rw,
                        crate::env::Env::new(),
                    ));
                }
            }
        }
        // Check for auto-generated attribute accessors (has $.x creates an accessor method).
        if results.is_empty() {
            let class_attrs = self.collect_class_attributes(&class_name);
            for attr in &class_attrs {
                if attr.is_public && attr.name == method_name {
                    results.push(Value::routine_parts(
                        Symbol::intern(&class_name),
                        Symbol::intern(method_name),
                        false,
                    ));
                    // Tag the routine with rw status if needed — currently Routine
                    // doesn't carry rw info, but we at least return a truthy result.
                    let _ = attr.is_rw; // suppress unused warning
                    break;
                }
            }
        }
        // Also check for native/builtin methods if no user-defined methods found.
        // For built-in types, consult the native-method-row catalog to see if
        // the method exists.
        if results.is_empty() {
            let method_sym = Symbol::intern(method_name);
            // ADR-0019 Phase E box E11 (`todo/deep/adr0019-e5-e7-entry-
            // routing.md` "E7 step 4"): the arity-cascade catalog
            // (`Interpreter::e2_native_method_exists`) replaces invoking
            // `native_method_0arg`/`native_method_1arg` with a dummy
            // `Value::NIL` arg just to answer an EXISTENCE question --
            // `MUTSU_VM_STATS=1` shadow-checked against the dummy-probe it
            // replaces over the full `t/` suite with zero mismatches after
            // the E11 slice 2 catalog-completeness pass (Cool/Any/Mu/Code/
            // Signature/IO::Path/IO::Handle rows, plus the native-int-coerce
            // family). The declared per-type list and `classhow_find_method`
            // branches stay: they cover slow-path methods invisible to the
            // pure arity cascade (`Buf.allocate`) and class-registry-only
            // methods outside `BUILTIN_METHOD_OWNERS` (e.g. `Cancellation`'s
            // `cancel`, the one case that still diverges from the catalog).
            let has_native = self.e2_native_method_exists(target, method_sym.as_str())
                // Slow-path builtin methods (block-taking / `&mut self`, e.g.
                // `Buf.allocate`) are invisible to the pure native probe; the
                // declared per-type lists cover them. NativeHelpers::Blob's
                // `blob-from-pointer` branches on `$type.can('allocate')` and
                // takes a REPR-poking fallback when it wrongly answers false.
                || crate::builtins::builtin_type_methods::builtin_type_method_names(&class_name)
                    .contains(&method_name)
                // Check user-defined classes and their native_methods set
                || {
                    let pkg = Value::package(Symbol::intern(&class_name));
                    self.classhow_find_method(&pkg, method_name).is_some()
                };
            if has_native {
                results.push(Value::routine_parts(
                    Symbol::intern(&class_name),
                    Symbol::intern(method_name),
                    false,
                ));
            }
        }
        // Grammars inherit parse/subparse/parsefile from the built-in Grammar
        // type. These are dispatched natively (no ClassDef.methods entry), so the
        // MRO walk above misses them — probe the grammar-ness of the target.
        if results.is_empty()
            && matches!(method_name, "parse" | "subparse" | "parsefile")
            && self.package_looks_like_grammar(&class_name)
        {
            results.push(Value::routine_parts(
                Symbol::intern(&class_name),
                Symbol::intern(method_name),
                false,
            ));
        }
        // Built-in exception instances (X::...) expose their attributes as
        // accessor methods (e.g. X::Undeclared.suggestions, .symbol). The HOW
        // metamodel has no user class def for these, so probe the instance's own
        // attribute map directly.
        if results.is_empty()
            && let ValueView::Instance {
                class_name: cn,
                attributes,
                ..
            } = target.view()
            && cn.resolve().starts_with("X::")
            && attributes.contains_key(method_name)
        {
            results.push(Value::routine_parts(
                Symbol::intern(&class_name),
                Symbol::intern(method_name),
                false,
            ));
        }
        // For enum values, also check enum-specific methods (key, value, Int, Str, etc.)
        if results.is_empty()
            && matches!(target.view(), ValueView::Enum { .. })
            && self
                .dispatch_enum_method(target, method_name, &[])
                .is_some()
        {
            results.push(Value::routine_parts(
                Symbol::intern(&class_name),
                Symbol::intern(method_name),
                false,
            ));
        }
        results
    }
}
