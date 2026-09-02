use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// The user-visible `.Stringy`/`.Str` of a role-mixed (`but`/`does`) value,
    /// when the composition — or the wrapped value's own class — supplies one.
    /// `None` means "nothing user-defined here", so the caller must keep its
    /// native rendering (an `Array but R` with no `Str` still stringifies as a
    /// plain list).
    ///
    /// This exists because every interpreter-level string-coercion site
    /// (prefix `~`, infix `~`, `"$x"` interpolation, `eq`/`lt`, `join`) used to
    /// test `ValueView::Instance` directly and therefore silently downgraded a
    /// `ValueView::Mixin` to its native stringification — while `print`/`put`,
    /// which go through `render_str_value`'s method dispatch, got it right.
    /// That split is what made `print $r` show the mixin's `Str` and
    /// `join(">", $r)` / `~$r` / `"$r"` show the base list.
    pub(crate) fn mixin_user_stringifier(
        &mut self,
        value: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, _) = value.view() else {
            return None;
        };
        // A composed role's own `Stringy`/`Str` (and, for a value mixin like
        // `1 but "hi"`, the stored override) wins.
        if let Some(r) = self.dispatch_mixin_method_call(value, "Stringy", vec![]) {
            return Some(r);
        }
        if let Some(r) = self.dispatch_mixin_method_call(value, "Str", vec![]) {
            return Some(r);
        }
        // The composition supplies neither, so fall through to a user
        // stringifier on the wrapped value's own class (`C.new but R` where
        // `C` declares `method Str`). Dispatch on the mixin itself so `self`
        // inside the method is the mixed value, as raku has it.
        let class_name = match inner.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
            ValueView::Package(name) => name.resolve().to_string(),
            _ => return None,
        };
        for method in ["Stringy", "Str"] {
            if self.has_user_method(&class_name, method) {
                return Some(self.call_method_with_values(value.clone(), method, vec![]));
            }
        }
        None
    }

    /// The user-defined `.Str` of a role-mixed value, without the
    /// Stringy-first ordering used by [`Self::mixin_user_stringifier`]. This
    /// is for operations whose contract explicitly calls `.Str`, such as
    /// `join` and `sprintf("%s", ...)`.
    pub(crate) fn mixin_user_str(&mut self, value: &Value) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, _) = value.view() else {
            return None;
        };
        if let Some(r) = self.dispatch_mixin_method_call(value, "Str", vec![]) {
            return Some(r);
        }
        let class_name = match inner.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
            ValueView::Package(name) => name.resolve().to_string(),
            _ => return None,
        };
        if self.has_user_method(&class_name, "Str") {
            Some(self.call_method_with_values(value.clone(), "Str", vec![]))
        } else {
            None
        }
    }

    /// Does one of the roles composed onto this `Mixin` declare `method_name`?
    /// A cheap registry lookup with no dispatch, for sites that must *decide*
    /// whether to run a composed method (sink context) rather than just try it.
    pub(crate) fn mixin_composes_method(&self, value: &Value, method_name: &str) -> bool {
        let ValueView::Mixin(_, mixins) = value.view() else {
            return false;
        };
        mixins
            .keys()
            .filter_map(|k| k.strip_prefix("__mutsu_role__"))
            .any(|role_name| self.role_has_method(role_name, method_name))
    }

    /// Dispatch method calls on Mixin targets.
    /// Returns Some(result) if the method was handled, None if not.
    pub(crate) fn dispatch_mixin_method_call(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };

        // `.clone` on a role mixin. A punned role's attributes live in
        // `__mutsu_attr__*` mixin markers, not the inner instance's attr map (see
        // `dispatch_new` — a bare-role `.new` returns `Mixin(empty-instance,
        // {__mutsu_attr__x: ...})`). The generic instance clone unwraps to the
        // inner value and drops every marker, so the clone lost all attributes
        // (`Zef::Client.fetch`'s `$candi.clone(:$dist)` then had no `.as`). Clone
        // the mixin instead: recursively clone the inner value, copy every marker,
        // and apply each `:attr(val)` override to its `__mutsu_attr__` marker.
        // Restricted to role mixins (a `__mutsu_role__`/`__mutsu_attr__` marker is
        // present) so allomorph / non-role `but` mixins keep their existing path.
        if method == "clone"
            && mixins
                .keys()
                .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            let inner_owned = inner.as_ref().clone();
            let mut new_mixins: HashMap<String, Value> = mixins.as_ref().clone();
            // An override names either a role attribute (a `__mutsu_attr__`
            // marker) or an attribute of the inner class. Apply the former to the
            // marker; forward the rest to the inner clone so `$w.clone(:id(99))`
            // on a `Widget but Named` still overrides Widget's own `$.id`.
            // A punned role's attributes also live in the inner instance's own
            // cell (that is what `$!attr` reads), so an override that lands on a
            // marker must reach the cell too — otherwise the accessor, which
            // prefers the cell, keeps serving the pre-clone value.
            let inner_attrs = Self::self_instance_attrs(inner);
            let mut inner_args: Vec<Value> = Vec::new();
            for arg in &args {
                if let ValueView::Pair(key, val) = arg.view()
                    && let std::collections::hash_map::Entry::Occupied(mut e) =
                        new_mixins.entry(format!("__mutsu_attr__{}", key))
                {
                    e.insert(val.clone());
                    let in_cell = inner_attrs
                        .as_ref()
                        .is_some_and(|c| c.as_map().contains_key(Symbol::intern(key)));
                    if !in_cell {
                        continue;
                    }
                }
                inner_args.push(arg.clone());
            }
            let inner_clone = match self.call_method_with_values(inner_owned, "clone", inner_args) {
                Ok(v) => v,
                Err(e) => return Some(Err(e)),
            };
            return Some(Ok(Value::mixin(inner_clone, new_mixins)));
        }

        if args.is_empty() {
            if let Some(mixin_val) = mixins.get(method) {
                return Some(Ok(mixin_val.clone()));
            }
            // Check role attribute accessors: has $.foo stores as __mutsu_attr__foo
            let attr_key = format!("__mutsu_attr__{}", method);
            if let Some(attr_val) = mixins.get(&attr_key) {
                // An explicit method declared in a composed role shadows the
                // auto-generated accessor for an attribute of the same name
                // (e.g. `has $.b` + `method b() {...}` — the method wins).
                let has_explicit_method = mixins
                    .keys()
                    .filter_map(|k| k.strip_prefix("__mutsu_role__"))
                    .any(|role_name| {
                        self.registry()
                            .roles
                            .get(role_name)
                            .is_some_and(|role| role.methods.contains_key(method))
                    });
                let is_public = !has_explicit_method
                    && mixins
                        .keys()
                        .filter_map(|k| k.strip_prefix("__mutsu_role__"))
                        .any(|role_name| {
                            self.registry().roles.get(role_name).is_some_and(|role| {
                                role.attributes
                                    .iter()
                                    .any(|a| a.name == method && a.is_public)
                            })
                        });
                if is_public {
                    // The marker is only the construction-time seed. If the
                    // wrapped instance carries the attribute, that cell is the
                    // store of record — a `$!foo = ...` inside a role method
                    // writes there, and the accessor must not serve the stale
                    // seed afterwards.
                    if let Some(cell) = Self::self_instance_attrs(inner)
                        && let Some(live) = cell.as_map().get(Symbol::intern(method))
                    {
                        return Some(Ok(live.clone()));
                    }
                    return Some(Ok(attr_val.clone()));
                }
            }
            for mixin_val in mixins.values() {
                if let ValueView::Enum { enum_type, key, .. } = mixin_val.view() {
                    if method == key.resolve() {
                        return Some(Ok(Value::TRUE));
                    }
                    if let Some(variants) = self.registry().enum_types.get(&enum_type.resolve())
                        && variants.iter().any(|(variant, _)| variant == method)
                    {
                        return Some(Ok(Value::FALSE));
                    }
                }
            }
        }
        // Order by application order, most-recently-applied first: Rakudo
        // resolves a method-name collision between mixed-in roles by
        // later-wins precedence (`(0 but A) but B).m` answers from B), and
        // this loop returns on the first match below. Each application site
        // stamps `__mutsu_role_seq__{name}` with a monotonic counter (see
        // todo/tickets/mixin-role-order-not-tracked.md); missing/unstamped
        // entries (should not occur once every site is updated) sort last.
        let mut role_names: Vec<(i64, String)> = mixins
            .iter()
            .filter_map(|(key, value)| {
                key.strip_prefix("__mutsu_role__")
                    .and_then(|name| value.truthy().then_some(name.to_string()))
            })
            .map(|name| {
                let seq = mixins
                    .get(&format!("__mutsu_role_seq__{}", name))
                    .and_then(|v| match v.view() {
                        ValueView::Int(n) => Some(n),
                        _ => None,
                    })
                    .unwrap_or(i64::MIN);
                (seq, name)
            })
            .collect();
        role_names.sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
        let role_names: Vec<String> = role_names.into_iter().map(|(_, name)| name).collect();
        // Determine if this is a private method call (method starts with '!')
        let is_private_call = method.starts_with('!');
        let lookup_name = if is_private_call {
            &method[1..]
        } else {
            method
        };
        let mut role_has_method = false;
        for role_name in role_names {
            let Some(role) = self.role_def_for_mixin_role(mixins, &role_name) else {
                continue;
            };
            let Some(overloads) = role.methods.get(lookup_name).cloned() else {
                continue;
            };
            role_has_method = true;
            let role_param_bindings: Vec<(String, Value)> = mixins
                .iter()
                .filter_map(|(key, value)| {
                    key.strip_prefix("__mutsu_role_param__")
                        .map(|name| (name.to_string(), value.clone()))
                })
                .collect();
            let mut saved_role_params: Vec<(String, Option<Value>)> = Vec::new();
            for (name, value) in &role_param_bindings {
                saved_role_params.push((name.clone(), self.env.get(name).cloned()));
                self.env.insert(name.clone(), value.clone());
            }
            for def in overloads {
                // For private calls, only match private methods; for public calls, skip private
                if is_private_call != def.is_private
                    || !self.method_args_match(&args, &def.param_defs)
                {
                    continue;
                }
                // Build the attribute set visible to the role method body.
                // Start with the inner instance's own attributes (e.g. class
                // attributes like `@.order`) so that `$.attr` accessors inside
                // the role method see and can mutate the class's state, then
                // overlay the role's own `__mutsu_attr__` attributes — but only
                // where the instance does not already carry them. The markers are
                // construction-time seeds; the cell is the store of record, so
                // overlaying them unconditionally would resurrect the seed over a
                // value a role method wrote through `$!attr`.
                let (inner_cell, mut method_attrs) = match inner.as_ref().view() {
                    ValueView::Instance { attributes, .. } => {
                        (Some(attributes.clone()), attributes.to_map())
                    }
                    _ => (None, AttrMap::new()),
                };
                for (key, value) in mixins.iter() {
                    if let Some(attr) = key.strip_prefix("__mutsu_attr__") {
                        let sym = Symbol::intern(attr);
                        if !method_attrs.contains_key(sym) {
                            method_attrs.insert(attr, value.clone());
                        }
                    }
                }
                // Set up a method-dispatch frame so `nextsame`/`callsame` inside
                // the role method falls through to the mixed-in base object's
                // method of the same name: `A.new but Role` where the role's
                // method calls `nextsame` must reach the class's original method.
                let base_class = match inner.as_ref().view() {
                    ValueView::Instance { class_name, .. } => {
                        Some(class_name.resolve().to_string())
                    }
                    _ => None,
                };
                let base_remaining: Vec<super::DeferralEntry> = if let Some(bc) = &base_class {
                    self.resolve_all_methods_with_owner(bc, lookup_name, &args)
                        .into_iter()
                        .filter(|(_, d)| d.is_private == is_private_call)
                        .map(|(owner, def)| super::DeferralEntry::Candidate {
                            owner,
                            def: Box::new(def),
                            wraps_spliced: false,
                        })
                        .collect()
                } else {
                    Vec::new()
                };
                let pushed_base_dispatch = !base_remaining.is_empty();
                if pushed_base_dispatch {
                    let rw_params =
                        super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs);
                    self.push_samewith_context(lookup_name, Some(target.clone()), None);
                    let dispatch_token = self.next_dispatch_token();
                    self.method_dispatch_stack.push(super::MethodDispatchFrame {
                        receiver_class: base_class.clone().unwrap_or_default(),
                        invocant: target.clone(),
                        args: args.clone(),
                        remaining: base_remaining,
                        rw_params,
                        dispatch_token,
                        arg_sources: None,
                        in_wrapper: false,
                    });
                }
                let method_result = self.run_resolved_method_compiled_or_treewalk(
                    &role_name,
                    &role_name,
                    lookup_name,
                    def,
                    method_attrs,
                    args,
                    Some(target.clone()),
                );
                if pushed_base_dispatch {
                    self.method_dispatch_stack.pop();
                    self.pop_samewith_context();
                }
                for (name, previous) in &saved_role_params {
                    if let Some(prev) = previous {
                        self.env.insert(name.clone(), prev.clone());
                    } else {
                        self.env.remove(name);
                    }
                }
                let (result, updated) = match method_result {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                // Propagate attribute mutations made by the role method back to
                // the inner instance, so that changes to class attributes (e.g.
                // `push @.order, ...`) are visible after the call returns. This
                // updates every binding in scope that holds the same instance
                // (including the one wrapped inside this Mixin).
                if let Some(cell) = &inner_cell {
                    cell.commit_attrs(updated);
                }
                return Some(Ok(result));
            }
            for (name, previous) in saved_role_params {
                if let Some(prev) = previous {
                    self.env.insert(name, prev);
                } else {
                    self.env.remove(&name);
                }
            }
        }
        if role_has_method {
            return Some(Err(
                super::methods_signature_errors::make_multi_no_match_error(method),
            ));
        }
        // The native base implementation of `.gist` stringifies through the
        // invocant's virtual `.Str`. Keep that dispatch on the Mixin wrapper:
        // delegating `.gist` straight to `inner` would skip a `method Str`
        // supplied by a composed role (including a Hash element default).
        if method == "gist"
            && args.is_empty()
            && let Some(str_result) = self.dispatch_mixin_method_call(target, "Str", vec![])
        {
            return Some(str_result);
        }
        if method == "can" && args.len() == 1 {
            let method_name = args[0].to_string_value();
            // First collect from the inner value's MRO
            let mut results = self.collect_can_methods(inner, &method_name);
            // Also check mixin-specific methods
            if (mixins.contains_key(&method_name)
                || mixins.contains_key(&format!("__mutsu_attr__{}", method_name)))
                && results.is_empty()
            {
                results.push(Value::routine_parts(
                    Symbol::intern("Mixin"),
                    Symbol::intern(&method_name),
                    false,
                ));
            }
            for role_name in mixins.keys().filter_map(|key| {
                key.strip_prefix("__mutsu_role__")
                    .map(|name| name.to_string())
            }) {
                if let Some(role) = self.registry().roles.get(&role_name)
                    && let Some(defs) = role.methods.get(&method_name)
                {
                    for def in defs {
                        results.push(Value::make_sub(
                            Symbol::intern(&role_name),
                            Symbol::intern(&method_name),
                            def.params.clone(),
                            def.param_defs.clone(),
                            (*def.body).clone(),
                            def.is_rw,
                            crate::env::Env::new(),
                        ));
                    }
                }
            }
            return Some(Ok(Value::array(results)));
        }
        if method == "does" && args.len() == 1 {
            let does = match args[0].view() {
                ValueView::Enum {
                    enum_type,
                    key: probe_key,
                    ..
                } => matches!(
                    mixins.get(&enum_type.resolve()).map(Value::view),
                    Some(ValueView::Enum { key, .. }) if key == probe_key
                ),
                ValueView::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    let base = base_name.resolve();
                    let has_role = target.does_check(&base)
                        || mixins.contains_key(&base)
                        || mixins.contains_key(&format!("__mutsu_role__{}", base));
                    if has_role {
                        let key = format!("__mutsu_role_typeargs__{}", base);
                        if let Some(ValueView::Array(actual_args, ..)) =
                            mixins.get(&key).map(Value::view)
                        {
                            actual_args.len() == type_args.len()
                                && actual_args
                                    .iter()
                                    .zip(type_args.iter())
                                    .all(|(a, e)| self.parametric_arg_subtypes(a, e))
                        } else {
                            type_args.is_empty()
                        }
                    } else {
                        false
                    }
                }
                ValueView::Package(name) => {
                    let n = name.resolve();
                    let base = n.split('[').next().unwrap_or(&n);
                    mixins.contains_key(&n)
                        || mixins.contains_key(base)
                        || mixins.contains_key(&format!("__mutsu_role__{}", n))
                        || mixins.contains_key(&format!("__mutsu_role__{}", base))
                        || self.type_matches_value(&n, target)
                }
                ValueView::Str(name) => {
                    mixins.contains_key(name.as_str())
                        || mixins.contains_key(&format!("__mutsu_role__{}", *name))
                        || self.type_matches_value(&name, target)
                }
                ValueView::Instance { class_name, .. } => {
                    self.type_matches_value(&class_name.resolve(), target)
                }
                _ => self.type_matches_value(&args[0].to_string_value(), target),
            };
            return Some(Ok(Value::truth(does)));
        }
        if method == "isa" && args.len() == 1 {
            let arg0 = args.first().cloned().unwrap_or(Value::NIL);
            // `R.^pun` (a role's pun) is the concrete class the role
            // generates for its instances, wrapped as `Mixin(Package(role),
            // {__mutsu_role__role: ...})` (see `punned_role_type_object`).
            // It stringifies the same as the bare role name, but unlike the
            // bare role it IS a real class — `raku` accepts `.isa(R.^pun)`
            // while rejecting `.isa(R)` (see "Roles are excluded" below).
            // Unwrap it here so `target_name` resolves from the pun's inner
            // Package/Instance, and skip the role-exclusion rule for it.
            let (target_name, is_bare_role_arg) = match arg0.view() {
                ValueView::Package(name) => (name.resolve(), true),
                ValueView::Str(name) => (name.to_string(), false),
                ValueView::Instance { class_name, .. } => (class_name.resolve(), false),
                ValueView::Mixin(pun_inner, _) => {
                    let name = match pun_inner.view() {
                        ValueView::Package(name) => name.resolve(),
                        ValueView::Instance { class_name, .. } => class_name.resolve(),
                        _ => arg0.to_string_value(),
                    };
                    (name, false)
                }
                _ => (arg0.to_string_value(), false),
            };
            // Roles are excluded from isa checks, but only when the argument
            // is literally the bare role (a `Package`) — not its pun.
            if is_bare_role_arg {
                let role_key = format!("__mutsu_role__{}", target_name);
                if mixins.contains_key(&role_key) {
                    return Some(Ok(Value::FALSE));
                }
            }
            // Delegate to inner value's isa check using class MRO
            let result = match inner.as_ref().view() {
                ValueView::Instance { class_name, .. } => self
                    .class_mro(&class_name.resolve())
                    .contains(&crate::symbol::Symbol::intern(&target_name)),
                _ => inner.isa_check(&target_name),
            };
            return Some(Ok(Value::truth(result)));
        }

        None
    }
}
