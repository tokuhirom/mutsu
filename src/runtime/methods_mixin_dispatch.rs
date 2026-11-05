use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;

impl Interpreter {
    /// Apply a native array mutator to the value wrapped by a native `Mixin`.
    /// The mixin stores its inner value behind an `Arc`, so dispatching through
    /// the name-keyed mutable-call path would lose the receiver when there is
    /// no lexical target name. Mutate the shared array node directly instead.
    ///
    /// Cost: O(k * e) worst case, k = inserted elements and e = existing
    /// elements; push, append, pop and shift are O(k) amortized or O(1).
    pub(crate) fn native_mixin_array_mutation(
        &mut self,
        invocant: &Value,
        inner: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let mut storage = inner.clone();
        let result = self.native_array_storage_mut(&mut storage, method, args)?;
        Some(result.map(|value| {
            if matches!(method, "push" | "append" | "prepend" | "unshift") {
                invocant.clone()
            } else {
                value
            }
        }))
    }

    /// The `.of` of a mixin target is answered by dispatch rather than by a
    /// declared method, so `.can('of')` / `.^can('of')` must find it too.
    // Cost: O(r), r = roles mixed in (see `mixin_container_role_value_type`).
    pub(crate) fn mixin_of_can_entry(
        &self,
        target: &Value,
        method_name: &str,
        results: &mut Vec<Value>,
    ) {
        if method_name == "of"
            && results.is_empty()
            && self.mixin_container_role_value_type(target).is_some()
        {
            results.push(Value::routine_parts(
                Symbol::intern("Mixin"),
                Symbol::intern("of"),
                false,
            ));
        }
    }

    /// Return the value type supplied by a parameterized container role mixed
    /// onto a value, if any. Unlike native Hash/Array metadata, this lives in
    /// the Mixin marker map and must not be copied onto the shared inner
    /// container just to answer `.of`.
    pub(crate) fn mixin_container_role_value_type(&self, target: &Value) -> Option<Value> {
        let ValueView::Mixin(_, mixins) = target.view() else {
            return None;
        };
        for base in ["Associative", "Positional"] {
            let key = MetaNs::RoleTypeargs.owned_key_for_str(base);
            if let Some(ValueView::Array(items, ..)) = mixins.get(&key).map(Value::view)
                && let Some(value_type) = items.iter().next()
            {
                return Some(value_type.clone());
            }
        }
        // A mixed-in parametric role that passes its parameter on
        // (`role IntTypedCArray[::TValue] does Positional[TValue]`, #11726).
        for key in mixins.keys() {
            let Some(role) = key.strip_prefix("__mutsu_role__") else {
                continue;
            };
            let arg_values: Vec<Value> = match mixins
                .get(&MetaNs::RoleTypeargs.owned_key_for_str(role))
                .map(Value::view)
            {
                Some(ValueView::Array(items, ..)) => items.iter().cloned().collect(),
                _ => Vec::new(),
            };
            // A role that passes one of its parameters straight through
            // (`does Positional[TValue]`) answers with that very type
            // object, so a nested parameterization (`CArray[CArray[uint8]]`)
            // keeps its inner parameterization instead of a spelling.
            let placeholders: Vec<String> =
                (0..arg_values.len()).map(|i| format!("__arg{i}__")).collect();
            if !placeholders.is_empty()
                && let Some(picked) = self
                    .role_spelling_value_type(
                        &format!("{role}[{}]", placeholders.join(",")),
                        0,
                    )
                    .and_then(|p| placeholders.iter().position(|ph| *ph == p))
            {
                return Some(arg_values[picked].clone());
            }
            let args: Vec<String> = arg_values
                .iter()
                .map(|v| match v.view() {
                    ValueView::Package(name) => name.resolve(),
                    _ => v.to_string_value(),
                })
                .collect();
            let spelling = if args.is_empty() {
                role.to_string()
            } else {
                format!("{role}[{}]", args.join(","))
            };
            if let Some(value_type) = self.role_spelling_value_type(&spelling, 0) {
                return Some(Value::package(crate::symbol::Symbol::intern(&value_type)));
            }
        }
        None
    }

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
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
            _ => return None,
        };
        for method in ["Stringy", "Str"] {
            if self.has_user_method(&class_name, method) {
                return Some(self.call_method_with_values(value.clone(), method, vec![]));
            }
        }
        // A `does Sequence` instance under the mixin stringifies through its
        // own iterator (`vm_sequence_role_delegate.rs`).
        if self.is_iterator_sequence_instance(value) {
            return self.try_sequence_role_delegate(value, Symbol::intern("Str"), &[]);
        }
        None
    }

    /// The class's own `iterator` of a `but`-mixed `does Sequence` instance,
    /// run with the mixed value as `self` (the Sequence role's `iterator`
    /// would wrap the instance instead).
    // Same by-name bridge as `mixin_user_stringifier`, which this sits next to.
    pub(crate) fn call_user_iterator_on_mixin(
        &mut self,
        mixed: &Value,
    ) -> Result<Value, RuntimeError> {
        self.call_method_with_values(mixed.clone(), "iterator", vec![])
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
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
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

    /// Commit the merged attribute snapshot returned by a role method to the
    /// two stores it may contain. The method frame deliberately presents one
    /// flat namespace to the Raku body, but a role's attributes belong to the
    /// Mixin-owned cell while attributes inherited from the wrapped instance
    /// belong to that instance's cell. Committing the merged snapshot to one
    /// cell would leak class attributes into the role cell (and would replace
    /// owner-qualified role keys with bare names).
    pub(crate) fn commit_mixin_role_method_attrs(
        &self,
        mixins: &crate::value::MixinOverrides,
        role_name: &str,
        role_attrs: &[String],
        inner_cell: Option<&crate::gc::Gc<crate::value::InstanceAttrs>>,
        updated: AttrMap,
    ) {
        let role_attr_names: Vec<Symbol> =
            role_attrs.iter().map(|name| Symbol::intern(name)).collect();
        for name in role_attrs {
            if let Some(value) = updated.get(Symbol::intern(name)) {
                mixins.set_role_attribute(role_name, name, value.clone());
            }
        }
        let Some(inner_cell) = inner_cell else {
            return;
        };

        // `run_resolved_method_compiled_or_treewalk` returns only attributes
        // whose frame slots were dirty, rather than a complete copy of the
        // input map. Start with the live inner snapshot so an otherwise
        // untouched class attribute cannot disappear on a role call.
        let mut inner_updated = inner_cell.to_map();
        for (key, value) in &updated {
            if !role_attr_names.contains(key) {
                inner_updated.insert(*key, value.clone());
            }
        }
        // A role attribute can shadow a same-named class attribute in the
        // merged frame. Preserve the class cell's current value, including a
        // mutation made by nextsame/callsame, instead of writing the role value
        // into the class-owned cell.
        for key in &role_attr_names {
            if let Some(value) = inner_cell.as_map().get(*key) {
                inner_updated.insert(*key, value.clone());
            }
        }
        inner_cell.commit_attrs(inner_updated);
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

        // An allomorph's row that binds its own arguments, given one the guard
        // step declined (`<1e0>.words(<1>)`): the row, on the part it reads
        // (ADR-11276 §9.101), before the delegation to the number below.
        if !args.is_empty()
            && let Some(result) =
                crate::builtins::method_table::invoke_unadmitted(self, target, method, &args)
        {
            return Some(result);
        }

        // A straight-line role method this composition already resolved for
        // arguments of these types runs without the role walk below (#16215).
        let front_key = if args.is_empty() || self.caches.mixin_straight_front.is_empty() {
            None
        } else {
            self.mixin_front_key(target, inner, mixins, method, &args)
        };
        if let Some(key) = &front_key
            && self.pending_raw_invocant.is_none()
            && let Some(entry) = self.caches.mixin_straight_front.get(key)
            && entry.generation == self.registry_write_generation()
        {
            let def = Arc::clone(&entry.def);
            if let Some(result) = self.run_straight_line_method(&def, target, &args) {
                return Some(result);
            }
        }

        // `.clone` on a role mixin. A punned role's `__mutsu_attr__*` entries are
        // construction seeds for the Mixin-owned role cell, not the live state.
        // The generic instance clone unwraps to the inner value and drops the
        // role composition, so the clone lost all attributes (`Zef::Client.fetch`'s
        // `$candi.clone(:$dist)` then had no `.as`). Clone the mixin instead:
        // recursively clone the inner value, deep-copy the role cell, copy every
        // marker, and apply each `:attr(val)` override to the new role cell.
        // Restricted to role mixins (a `__mutsu_role__`/`__mutsu_attr__` marker is
        // present) so allomorph / non-role `but` mixins keep their existing path.
        if method == "clone"
            && mixins
                .keys()
                .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            let inner_owned = inner.as_ref().clone();
            let mut new_mixins = mixins.as_ref().clone();
            // An override names either a role attribute (a `__mutsu_attr__`
            // marker) or an attribute of the inner class. Apply the former to the
            // marker; forward the rest to the inner clone so `$w.clone(:id(99))`
            // on a `Widget but Named` still overrides Widget's own `$.id`.
            // An override that lands on a role marker must reach the deep-copied
            // role cell too — otherwise the accessor would keep serving the
            // pre-clone value from that cell.
            let inner_attrs = Self::self_instance_attrs(inner);
            let mut inner_args: Vec<Value> = Vec::new();
            for arg in &args {
                if let ValueView::Pair(key, val) = arg.view()
                    && let std::collections::hash_map::Entry::Occupied(mut e) =
                        new_mixins.entry(MetaNs::Attr.owned_key_for_str(key))
                {
                    e.insert(val.clone());
                    let attr = Symbol::intern(key);
                    if new_mixins.set_role_attribute_by_name(key, val.clone()) {
                        continue;
                    }
                    let in_cell = inner_attrs
                        .as_ref()
                        .is_some_and(|c| c.as_map().contains_key(attr));
                    if !in_cell {
                        continue;
                    }
                }
                inner_args.push(arg.clone());
            }
            let inner_clone =
                match self.call_method_with_values_unviewed(inner_owned, "clone", inner_args) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
            let cloned = Value::mixin_with_state(inner_clone, new_mixins);
            // A cloned routine's own cell holds the copied composition
            // (ADR-11827 §2.3).
            return Some(Ok(Self::note_routine_composition(cloned, None)));
        }

        if method == "of"
            && args.is_empty()
            && let Some(value_type) = self.mixin_container_role_value_type(target)
        {
            return Some(Ok(value_type));
        }

        if args.is_empty() {
            if let Some(mixin_val) = mixins.get(method) {
                return Some(Ok(mixin_val.clone()));
            }
            // Check role attribute accessors: has $.foo stores as __mutsu_attr__foo
            let attr_key = MetaNs::Attr.owned_key_for_str(method);
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
                    // Role attributes live in the Mixin-owned cell. The
                    // wrapped instance is only the fallback for an attribute
                    // belonging to the wrapped class with the same public
                    // spelling.
                    let attr = Symbol::intern(method);
                    let role_live = mixins
                        .keys()
                        .filter_map(|key| key.strip_prefix("__mutsu_role__"))
                        .filter_map(|role_name| {
                            self.registry()
                                .roles
                                .get(role_name)
                                .cloned()
                                .map(|role| (role_name.to_string(), role))
                        })
                        .find_map(|(role_name, role)| {
                            role.attributes
                                .iter()
                                .any(|attr| attr.name == method && attr.is_public)
                                .then(|| mixins.role_attribute(&role_name, method))
                                .flatten()
                        });
                    if let Some(live) = role_live {
                        return Some(Ok(live));
                    }
                    if let Some(cell) = Self::self_instance_attrs(inner)
                        && let Some(live) = cell.as_map().get(attr)
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
                    .get(MetaNs::RoleSeq.str_key_for_str(&name))
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
            // Only what the call reads, copied out of the borrowed def: the
            // whole `RoleDef` (every method) cost ~19k Ir per call (#16215).
            // The candidates themselves are cloned only when the winner memo
            // misses; a hit is looked up by the identity of their bodies.
            //
            // A `method m {...}` stub only *requires* `m` (checked when the
            // role was composed); it never supplies a body, so the mixed-in
            // value's own method must answer (`$attr does R` where
            // `R` stubs `method package`).
            let Some((role_id, role_attrs, bodies, sole_non_multi)) = self
                .with_mixin_role_def(mixins, &role_name, |role| {
                    role.methods.get(lookup_name).map(|overloads| {
                        let live: Vec<&MethodDef> = overloads
                            .iter()
                            .filter(|def| !Self::is_runtime_stub_method_def(def))
                            .collect();
                        (
                            role.role_id,
                            role.attributes
                                .iter()
                                .map(|attr| attr.name.clone())
                                .collect::<Vec<String>>(),
                            live.iter()
                                .map(|def| std::sync::Arc::as_ptr(&def.body) as usize)
                                .collect::<Vec<usize>>(),
                            live.len() == 1 && !live[0].is_multi,
                        )
                    })
                })
                .flatten()
            else {
                continue;
            };
            if bodies.is_empty() {
                continue;
            }
            role_has_method = true;
            let role_param_bindings: Vec<(String, Value)> = mixins
                .iter()
                .filter_map(|(key, value)| {
                    key.strip_prefix("__mutsu_role_param__")
                        .map(|name| (name.to_string(), value.clone()))
                })
                .collect();
            // Only a role parameter whose value is a callable (or the
            // `Callable` type object of an unsupplied `&cb?`) is ever bound
            // as a callable, so the candidate's parameter list is read only
            // then (a CArray's `TValue` is a type: nothing to look up).
            let any_callable_value = role_param_bindings.iter().any(|(_, value)| {
                match value.view() {
                    ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => true,
                    ValueView::Package(pkg) => pkg == "Callable",
                    _ => false,
                }
            });
            let callable_role_params: rustc_hash::FxHashSet<String> = if !any_callable_value {
                Default::default()
            } else {
                self.registry()
                .role_candidates
                .get(&role_name)
                .and_then(|candidates| {
                    candidates
                        .iter()
                        .find(|candidate| candidate.role_def.role_id == role_id)
                })
                .map(|candidate| {
                    candidate
                        .type_param_defs
                        .iter()
                        .enumerate()
                        .filter(|(_, param)| param.name.starts_with('&'))
                        .flat_map(|(i, param)| {
                            [
                                param.name.clone(),
                                param.name.trim_start_matches('&').to_string(),
                                candidate.type_params.get(i).cloned().unwrap_or_default(),
                            ]
                        })
                        .collect()
                })
                .unwrap_or_default()
            };
            let mut saved_role_params: Vec<(String, Option<Value>)> = Vec::new();
            for (name, value) in &role_param_bindings {
                // The role declaration stores callable parameters under their
                // sigilless body name (`role R[&f]` reads `f`), where a bare
                // read invokes the callable, while an explicit `&f` read
                // returns the callable itself. Only a sigilless binding is
                // that callable form: a typed scalar such as `Callable $block`
                // must remain under `$block` even when its value is callable.
                // The ordinary signature binder installs the `&f` entry and
                // leaves the bare name absent; role parameters are injected
                // directly at dispatch time, so keep the same contract here.
                let is_callable = callable_role_params.contains(name)
                    && match value.view() {
                        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => true,
                        // An unsupplied `&cb?` is the `Callable` type object.
                        ValueView::Package(pkg) => pkg == "Callable",
                        _ => false,
                    };
                saved_role_params.push((name.clone(), self.env.get(name).cloned()));
                if is_callable {
                    self.env.remove(name);
                    let callable_name = format!("&{name}");
                    saved_role_params
                        .push((callable_name.clone(), self.env.get(&callable_name).cloned()));
                    self.env.insert(callable_name, value.clone());
                } else {
                    self.env.insert(name.clone(), value.clone());
                }
            }
            // A lone non-multi private method has nothing to fall through to:
            // run it so a failed parameter bind raises its own
            // `X::TypeCheck::Binding::Parameter`, as rakudo does, rather than
            // a dispatch `X::Multi::NoMatch`.
            let sole_private = is_private_call && sole_non_multi;
            // The winner among a role's multi candidates is a function of the
            // arguments' type keys when every candidate is key-decided: a
            // CArray element store re-resolved every candidate's parameter
            // types on each call (505k Ir, #16215).
            // A hit needs only the body identities: an entry exists only for a
            // call whose candidates were all key-decided when it was made.
            let fast_key = if sole_private {
                None
            } else {
                self.mixin_winner_fast_key(
                    MixinCallSite {
                        target,
                        inner,
                        role_name: &role_name,
                        role_id,
                        method: lookup_name,
                        private: is_private_call,
                    },
                    bodies,
                    &args,
                )
            };
            let memoized = fast_key
                .as_ref()
                .and_then(|key| self.caches.mixin_method_winner.get(key).cloned());
            let mut overloads_miss: Vec<MethodDef> = Vec::new();
            let mut memo_key = fast_key;
            if memoized.is_none() {
                overloads_miss = self
                    .with_mixin_role_def(mixins, &role_name, |role| {
                        role.methods.get(lookup_name).cloned().unwrap_or_default()
                    })
                    .unwrap_or_default();
                overloads_miss.retain(|def| !Self::is_runtime_stub_method_def(def));
                // Re-derived from the candidates themselves: only a call whose
                // candidates are all key-decided may be remembered.
                memo_key = if sole_private {
                    None
                } else {
                    self.mixin_winner_key(
                        MixinCallSite {
                            target,
                            inner,
                            role_name: &role_name,
                            role_id,
                            method: lookup_name,
                            private: is_private_call,
                        },
                        &overloads_miss,
                        &role_param_bindings,
                        &args,
                    )
                };
            }
            // Every candidate is decided by built-in type keys when a key
            // exists: the winner's parameters need no role binding.
            let key_decided = memo_key.is_some();
            let picked = match memoized {
                Some(winner) => winner,
                None => {
                    // The visible multis that do not bind are kept as rivals: they
                    // still shape the tiers of `pick_method_winner` (#12795).
                    let (matching, rivals): (Vec<MethodDef>, Vec<MethodDef>) = overloads_miss
                        .into_iter()
                        // For private calls, only match private methods; for public calls, skip private.
                        .filter(|def| is_private_call == def.is_private)
                        .partition(|def| {
                            // Use the invocant-aware matcher (not `method_args_match`) so an
                            // invocant-discriminated multi pair (`::?ROLE:U:` / `::?ROLE:D:`)
                            // is told apart by the mixin's definedness, matching the ordinary
                            // MRO dispatch path (`resolve_method_with_owner_impl`). Without
                            // this, a punned role's `:D:` candidate never won over `:U:`
                            // because this filter never looked at the invocant at all.
                            sole_private
                                || self.method_args_match_for_invocant(
                                    &role_name,
                                    def,
                                    &args,
                                    None,
                                    Some(target),
                                )
                        });
                    let owner = Symbol::intern(&role_name);
                    let matching: Vec<(Symbol, MethodDef)> =
                        matching.into_iter().map(|def| (owner, def)).collect();
                    let rivals: Vec<(Symbol, Rival)> = rivals
                        .into_iter()
                        .filter(|def| def.is_multi)
                        .map(|def| (owner, Rival::owned(def)))
                        .collect();
                    let winner = self.pick_method_winner(
                        &[Symbol::intern(&role_name)],
                        &args,
                        Some(target),
                        matching,
                        rivals,
                    );
                    if let Some(key) = memo_key {
                        let memo = &mut self.caches.mixin_method_winner;
                        if memo.len() >= MIXIN_WINNER_MEMO_CAP {
                            memo.clear();
                        }
                        memo.insert(key, winner.clone());
                    }
                    winner
                }
            };
            let Some((_, def)) = picked else {
                for (name, previous) in saved_role_params {
                    if let Some(prev) = previous {
                        self.env.insert(name, prev);
                    } else {
                        self.env.remove(&name);
                    }
                }
                continue;
            };
            // A winner that is key-decided and whose body cannot observe the
            // role's parameters, statics or `::?CLASS` (`ignores_role_context`)
            // gets none of them bound: the call can then take the method fast
            // path (#16215).
            let mut def = def;
            let light = key_decided
                && def
                    .compiled_code
                    .as_ref()
                    .is_some_and(|code| code.ignores_role_context());
            // A straight-line body (local reads and `nqp::` ops) runs without
            // a method frame: no attribute snapshot, parameter binder or
            // scoped env (`run_straight_line_method`, #16215).
            if light
                && self.pending_raw_invocant.is_none()
                && let Some(result) = self.run_straight_line_method(&def, target, &args)
            {
                if !is_private_call
                    && let Some(key) = front_key.or_else(|| {
                        self.mixin_front_key(target, inner, mixins, method, &args)
                    })
                {
                    let memo = &mut self.caches.mixin_straight_front;
                    if memo.len() >= MIXIN_FRONT_CAP {
                        memo.clear();
                    }
                    let entry = MixinFrontEntry {
                        generation: self.types.registry_write_gen.load(std::sync::atomic::Ordering::Relaxed),
                        def: Arc::new(def.clone()),
                        _node: mixins.clone(),
                    };
                    self.caches.mixin_straight_front.insert(key, entry);
                }
                // Newest first: a name saved twice gets its oldest value back.
                for (name, previous) in saved_role_params.iter().rev() {
                    if let Some(prev) = previous {
                        self.env.insert(name.clone(), prev.clone());
                    } else {
                        self.env.remove(name);
                    }
                }
                return Some(result);
            }
            if !light {
                // Match ordinary signature binding: a callable parameter is
                // installed under `&name`, while the bare `name` entry is absent
                // so a bare read follows the compiler's implicit zero-arg call
                // path instead of returning the callable object.
                let mut method_role_bindings = Vec::new();
                for (name, value) in &role_param_bindings {
                    let is_callable = callable_role_params.contains(name)
                        && matches!(
                            value.view(),
                            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                        );
                    if is_callable && !name.starts_with('&') {
                        method_role_bindings.push((format!("&{name}"), value.clone()));
                    } else {
                        method_role_bindings.push((name.clone(), value.clone()));
                    }
                }
                // `::?CLASS` in a role method is the class the role is composed
                // into, here the mixin type (`K+{R[Str]}`), not the role: upstream
                // NativeCall's `TypedCArray!allocate` reads
                // `::?CLASS.^array_type` (#11203).
                let class_type = match inner.as_ref().view() {
                    ValueView::Package(_) => Some(target.clone()),
                    ValueView::Instance { class_name, .. } => Some(Value::mixin_parts(
                        Arc::new(Value::package(class_name)),
                        mixins.clone(),
                    )),
                    _ => None,
                };
                if let Some(class_type) = class_type {
                    method_role_bindings.push(("?CLASS".to_string(), class_type));
                }
                // Role-body lexicals are persisted with the concrete punned role
                // class during parameterized composition, while the role method's
                // lexical package remains the enclosing declaration package.
                // Both stores join the bindings the method installs in its OWN
                // frame, so bare role lexicals such as `my \\value = T` resolve to
                // this composition's value rather than to a bareword, and a
                // closure the method creates captures them. Staged in the caller's
                // env instead, they sat above the method's frame root, where a
                // closure's capture never reaches (#12882), and could clobber a
                // caller closure's own capture (upstream NativeCall, #11203).
                let mut static_packages = vec![def.lexical_package.resolve()];
                if let Some(ValueView::Array(type_args, _)) = mixins
                    .get(MetaNs::RoleTypeargs.str_key_for_str(&role_name))
                    .map(Value::view)
                {
                    let (_, punned_name) =
                        Self::parametric_role_pun_name(&role_name, type_args.as_slice());
                    static_packages.push(punned_name);
                }
                for package in &static_packages {
                    if let Some(statics) = self.lexicals.package_lexicals.get(package.as_str()) {
                        method_role_bindings.extend(statics.iter().map(|(k, v)| (k.clone(), v.clone())));
                    }
                }
                def.role_param_bindings = Some(std::sync::Arc::new(method_role_bindings));
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
                    // The element storage of a buffer-backed instance (a
                    // `CArray[T]`) is reached through `self`, never as an
                    // attribute; a second holder would make the method's
                    // in-place write fork it.
                    let mut snapshot = attributes.to_map();
                    crate::value::value_buf::remove_storage(&mut snapshot);
                    (Some(attributes.clone()), snapshot)
                }
                _ => (None, AttrMap::new()),
            };
            for attr_name in &role_attrs {
                if let Some(value) = mixins.role_attribute(&role_name, attr_name) {
                    method_attrs.insert(attr_name.clone(), value);
                }
            }
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
                ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
                _ => None,
            };
            // A body that is a straight line of local reads and `nqp::` ops
            // (`light`) cannot defer to the base object's method, so the
            // ranked deferral expansion and the dispatch frame are not built
            // for it (`matched_deferral_candidates` was ~10 interns and a
            // candidate walk per CArray access, #16215).
            let native_base = !light && base_class.is_none();
            let base_remaining: Vec<super::DeferralEntry> = if light {
                Vec::new()
            } else if let Some(bc) = &base_class {
                // The ranked deferral expansion every other method frame uses,
                // not declaration order: the most specific of the class's multis
                // is the next candidate (`callsame` out of a mixed-in role's
                // method, Red's `ScopedDriver.map-exception`).
                self.matched_deferral_candidates(bc, lookup_name, &args, target, None)
                    .into_iter()
                    .filter(|(_, d)| d.is_private == is_private_call)
                    .map(|(owner, def)| super::DeferralEntry::Candidate {
                        owner,
                        def: Box::new(def),
                        wraps_spliced: false,
                    })
                    .collect()
            } else {
                // A native inner value has no `MethodDef`s: the builtin on it
                // is the one candidate behind the role's method.
                vec![super::DeferralEntry::Native {
                    name: lookup_name.to_string(),
                    base: super::NativeBase::Value,
                }]
            };
            // An open handle (`$*ARGFILES does R[:reset]`) is a native class
            // instance with no `MethodDef`s either: the native `IO::Handle`
            // method on the wrapped handle is the one candidate behind the
            // role's method.
            let base_remaining = if !light
                && base_remaining.is_empty()
                && base_class.as_deref() == Some("IO::Handle")
            {
                vec![super::DeferralEntry::Native {
                    name: lookup_name.to_string(),
                    base: super::NativeBase::Value,
                }]
            } else {
                base_remaining
            };
            // The mixed-in class's own attribute accessor has no `MethodDef`, but
            // it is the next candidate behind the role's method of the same name
            // (`$obj but role { method output { callsame } }` over `has $.output`).
            let base_remaining = if !light
                && base_remaining.is_empty()
                && !is_private_call
                && let Some(bc) = &base_class
                && let Some(owner) = super::user_method_probe_memo::probe_key(lookup_name)
                    .and_then(|key| self.first_public_accessor_owner(bc, key))
            {
                vec![super::DeferralEntry::Accessor {
                    owner,
                    name: lookup_name.to_string(),
                    want_container: false,
                }]
            } else {
                base_remaining
            };
            let pushed_base_dispatch = !base_remaining.is_empty() || native_base;
            if pushed_base_dispatch {
                let rw_params =
                    super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs);
                self.push_samewith_context(lookup_name, Some(target.clone()), None);
                let dispatch_token = self.next_dispatch_token();
                self.dispatch
                    .method_dispatch_stack
                    .push(super::MethodDispatchFrame {
                        receiver_class: base_class.clone().unwrap_or_default(),
                        invocant: target.clone(),
                        args: args.clone(),
                        remaining: base_remaining,
                        rw_params,
                        dispatch_token,
                        arg_sources: None,
                        in_wrapper: false,
                        lazy_tail: None,
                    });
            }
            let invocant = self
                .pending_raw_invocant
                .as_ref()
                // A raw invocant parameter (`method m(\SELF:)`, ADR-0067 slice 3b)
                // binds the named receiver's container, as an implicit `self`
                // of an aggregate does.
                .filter(|pending| {
                    (pending.implicit_self || pending.raw_parameter)
                        && pending.method == lookup_name
                })
                .map(|pending| pending.cell.clone())
                .unwrap_or_else(|| target.clone());
            let method_result = self.run_resolved_method_compiled_or_treewalk(
                &role_name,
                &role_name,
                lookup_name,
                def,
                method_attrs,
                args,
                Some(invocant),
            );
            if pushed_base_dispatch {
                self.dispatch.method_dispatch_stack.pop();
                self.pop_samewith_context();
            }
            // Newest first: a name saved twice gets its oldest value back.
            for (name, previous) in saved_role_params.iter().rev() {
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
            // Propagate attribute mutations made by the role method to the
            // owner-selected store. Role attributes go to the Mixin-owned
            // cell; class attributes go to the wrapped instance cell. This
            // updates every binding in scope that holds the same store.
            if self.is_role(&role_name) {
                self.commit_mixin_role_method_attrs(
                    mixins,
                    &role_name,
                    &role_attrs,
                    inner_cell.as_ref(),
                    updated,
                );
            } else if let Some(cell) = &inner_cell {
                cell.commit_attrs(updated);
            }
            return Some(Ok(result));
        }
        if role_has_method {
            return Some(Err(
                super::methods_signature_errors::make_multi_no_match_error(method),
            ));
        }
        if method == "Str"
            && args.is_empty()
            && matches!(inner.as_ref().view(), ValueView::Package(_))
        {
            return Some(
                self.dispatch_caret_name(target)
                    .map(|name| Value::str(format!("({})", name.to_string_value()))),
            );
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
                || mixins.contains_key(MetaNs::Attr.str_key_for_str(&method_name)))
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
                // The candidate this mixin was built from, not the name's
                // registry entry: a role group (`role R[\u] {...}` beside a
                // bare `role R {...}`) keeps the last-declared body there.
                if let Some(role) = self.role_def_for_mixin_role(mixins, &role_name)
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
            self.mixin_of_can_entry(target, &method_name, &mut results);
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
                        || mixins.contains_key(MetaNs::Role.str_key_for_str(&base));
                    if has_role {
                        let key = MetaNs::RoleTypeargs.owned_key_for_str(&base);
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
                        || mixins.contains_key(MetaNs::Role.str_key_for_str(&n))
                        || mixins.contains_key(MetaNs::Role.str_key_for_str(base))
                        || self.type_matches_value(&n, target)
                }
                ValueView::Str(name) => {
                    mixins.contains_key(name.as_str())
                        || mixins.contains_key(MetaNs::Role.str_key_for_str(name.as_str()))
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
                let role_key = MetaNs::Role.owned_key_for_str(&target_name);
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

        // A mutator of the native array the role is mixed into is the `Mut`
        // row's, on that array (ADR-11276 §9.147).
        if matches!(inner.as_ref().view(), ValueView::Array(..))
            && crate::builtins::method_table::names_a_mut_row(Symbol::intern(method), args.len())
            && let Some(result) =
                self.native_mixin_array_mutation(target, inner.as_ref(), method, &args)
        {
            return Some(result);
        }

        None
    }
}

/// Most remembered winners before the table is dropped.
const MIXIN_WINNER_MEMO_CAP: usize = 4096;

/// Most remembered straight-line calls before the front table is dropped.
const MIXIN_FRONT_CAP: usize = 1024;

/// What a straight-line role method call on a mixin is looked up by.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct MixinFrontKey {
    /// Address of the composition node; the entry keeps the node alive.
    node: usize,
    method: Symbol,
    defined: bool,
    arg_keys: Vec<Symbol>,
}

/// The resolved straight-line method of a [`MixinFrontKey`].
#[derive(Clone)]
pub(crate) struct MixinFrontEntry {
    /// The registry write generation the entry was made under.
    generation: u64,
    def: Arc<MethodDef>,
    _node: crate::gc::Gc<crate::value::MixinOverrides>,
}

/// Where a role method is called: the receiver and which role method.
struct MixinCallSite<'a> {
    target: &'a Value,
    inner: &'a Value,
    role_name: &'a str,
    role_id: u64,
    method: &'a str,
    private: bool,
}

/// What a mixin receiver's role-method winner is computed from.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct MixinWinnerKey {
    /// The role declaration and the identity of each candidate's body (the
    /// `Arc`s stay alive in the role while it exists, so an address is not
    /// reused): a redefinition or an `augment` changes the candidates. Not the
    /// registry stamp, which a mixin dispatch itself moves on every call.
    role_id: u64,
    bodies: Vec<usize>,
    role: Symbol,
    method: Symbol,
    private: bool,
    /// Whether the receiver is a defined value (`::?CLASS:D:` / `:U:`).
    defined: bool,
    arg_keys: Vec<Symbol>,
}

impl Interpreter {
    /// The straight-line role method a call on a mixin already resolved to
    /// (`caches.mixin_straight_front`), run; `None` when the call was not seen
    /// before, the registry was written since, or the lane declines its
    /// arguments. Asked at the top of the method-call funnel, ahead of the
    /// prefix probes that decided nothing for these keys the first time
    /// (#16215).
    // Cost: O(a) for the key, plus the lane's call.
    pub(crate) fn mixin_front_hit(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.is_empty()
            || self.caches.mixin_straight_front.is_empty()
            || self.pending_raw_invocant.is_some()
        {
            return None;
        }
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };
        let key = self.mixin_front_key(target, inner, mixins, method, args)?;
        let entry = self.caches.mixin_straight_front.get(&key)?;
        if entry.generation != self.registry_write_generation() {
            return None;
        }
        let def = Arc::clone(&entry.def);
        self.run_straight_line_method(&def, target, args)
    }

    /// The front-table key of a call on a mixin, or `None` when an argument
    /// has no type key.
    // Cost: O(a), a = arguments.
    fn mixin_front_key(
        &mut self,
        target: &Value,
        inner: &Value,
        mixins: &crate::gc::Gc<crate::value::MixinOverrides>,
        method: &str,
        args: &[Value],
    ) -> Option<MixinFrontKey> {
        let arg_keys = self.multi_arg_type_keys_with_named(args)?;
        Some(MixinFrontKey {
            node: &**mixins as *const crate::value::MixinOverrides as usize,
            method: Symbol::intern(method),
            defined: !matches!(inner.view(), ValueView::Package(_))
                && crate::runtime::value_is_definite(target),
            arg_keys,
        })
    }

    /// [`Self::mixin_winner_key`] without inspecting the candidates: the key a
    /// remembered call is looked up by, built from the identity of their
    /// bodies (`bodies`) and the arguments' type keys alone. `None` when an
    /// argument has no type key.
    // Cost: O(c + a), c = candidates, a = arguments.
    fn mixin_winner_fast_key(
        &mut self,
        site: MixinCallSite<'_>,
        bodies: Vec<usize>,
        args: &[Value],
    ) -> Option<MixinWinnerKey> {
        let MixinCallSite { target, inner, role_name, role_id, method, private } = site;
        let arg_keys = self.multi_arg_type_keys_with_named(args)?;
        Some(MixinWinnerKey {
            role_id,
            bodies,
            role: Symbol::intern(role_name),
            method: Symbol::intern(method),
            private,
            defined: !matches!(inner.view(), ValueView::Package(_))
                && crate::runtime::value_is_definite(target),
            arg_keys,
        })
    }

    /// The memo key for a role method call on a mixin, or `None` when the
    /// verdict of some candidate reads more than the argument type keys, the
    /// receiver's definedness and the registry: a `where`, a subset, a type
    /// capture, a literal, a sub-signature, a role type parameter, a
    /// constraint that is not a built-in type, or an invocant other than
    /// `::?CLASS` with a smiley.
    // Cost: O(c * p + a), c = candidates, p = parameters each, a = arguments.
    fn mixin_winner_key(
        &mut self,
        site: MixinCallSite<'_>,
        overloads: &[MethodDef],
        role_params: &[(String, Value)],
        args: &[Value],
    ) -> Option<MixinWinnerKey> {
        let MixinCallSite { target, inner, role_name, role_id, method, private } = site;
        let key_decided = |def: &MethodDef| {
            !def.param_defs.is_empty()
                && def.param_defs.iter().all(|pd| {
                    pd.where_constraint.is_none()
                        && pd.literal_value.is_none()
                        && pd.code_signature.is_none()
                        && pd.outer_sub_signature.is_none()
                        && (pd.sub_signature.is_none() || pd.named_alias)
                        && pd.type_capture.is_none()
                        && pd.default.is_none()
                        && match pd.type_constraint.as_deref() {
                            None => !pd.is_invocant,
                            Some(tc) if pd.is_invocant => {
                                matches!(tc, "::?CLASS" | "::?CLASS:D" | "::?CLASS:U")
                            }
                            Some(tc) => {
                                let base = tc.strip_suffix(":D").or_else(|| tc.strip_suffix(":U")).unwrap_or(tc);
                                crate::runtime::utils::is_known_type_constraint(base)
                                    && !role_params.iter().any(|(name, _)| name == base)
                                    && !self.constraint_is_subset(base)
                            }
                        }
                })
        };
        if !overloads.iter().all(key_decided) {
            return None;
        }
        let arg_keys = self.multi_arg_type_keys_with_named(args)?;
        Some(MixinWinnerKey {
            role_id,
            bodies: overloads
                .iter()
                .map(|def| std::sync::Arc::as_ptr(&def.body) as usize)
                .collect(),
            role: Symbol::intern(role_name),
            method: Symbol::intern(method),
            private,
            defined: !matches!(inner.view(), ValueView::Package(_)) && crate::runtime::value_is_definite(target),
            arg_keys,
        })
    }
}
