use super::*;

/// Cache-key stand-in for the parser's synthetic test-assertion callsite-line
/// marker. Deliberately the marker's own reserved key name: it can never
/// collide with a `value_type_name`, and it keeps "marker present" and "marker
/// absent" in distinct cache buckets.
const CALLSITE_LINE_MARKER_KEY: &str = "__mutsu_test_callsite_line";

/// Cache-key marker appended after an argument's type key when that argument is
/// *undefined* (`value_is_defined` is false). A `:D`/`:U` smiley candidate set
/// dispatches on exactly that bit on top of the type, so a key that carries it
/// stays a function of the winner — which is what lets
/// `multi_dispatch_type_cacheable` / `func_multi_dispatch_type_cacheable` admit
/// smiley candidates instead of refusing the whole name. Emitted only for the
/// undefined case, so the common (defined) key shape is unchanged; the name is
/// reserved and can never collide with a type name or a `value_type_name`.
const UNDEFINED_ARG_KEY: &str = "__mutsu_key_undefined";

/// Cache-key marker introducing the *declared type* of a `VarRef` argument's
/// source variable. Without the marker a declared-type key would be
/// indistinguishable from the value key of an additional argument
/// (`f($x)` with `my Int $x` vs `f(1, 2)` would both key as `[Int, Int]`), and
/// the two calls have different arities and therefore different winners.
const DECLARED_TYPE_KEY: &str = "__mutsu_key_declared_type";

/// Cache-key marker introducing an enum VALUE argument's `(enum type, member)`
/// pair. An enum member refines within one `value_type_name` -- `Less` and
/// `More` are both `Order` -- so `multi f(Less)` and `multi f(More)` need
/// distinct buckets.
const ENUM_MEMBER_KEY: &str = "__mutsu_key_enum_member";

impl Interpreter {
    pub(crate) fn refresh_method_caches_for_generation(&mut self) {
        let generation = self.registry().method_generation;
        if self.method_cache_generation == generation {
            return;
        }
        self.method_cache_generation = generation;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.native_ctor_plan_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        self.native_lever_a_override_cache.clear();
        self.resolved_seq_cache.clear();
        self.dispatch_multi_candidate.clear();
        self.clear_private_zeroarg_method_cache();
    }

    /// Try compiled method fast path; fall back to interpreter.
    ///
    /// Wrapper that preserves the caller's env `self`: the inner dispatch can run
    /// an interpreted instance method, which binds `self` in env without the
    /// save/restore that `call_method_with_values` performs. Without this, a bare
    /// stringification (`"$obj"` → `StringConcat` → this) leaves the caller's
    /// `self` pointing at `$obj`, breaking a later `self` read in an enclosing
    /// nested sub (which resolves `self` from env via `GetSelfOrNoSelf`).
    /// Build the per-argument key for the sound multi-resolution caches.
    ///
    /// Each argument contributes its runtime type, plus the two other
    /// properties dispatch can read: its *definedness* (a `:D`/`:U` smiley
    /// candidate tests exactly that) and, for a `VarRef`, the *declared type*
    /// of the variable it came from. Both are emitted behind reserved markers
    /// so they cannot be confused with an ordinary type key.
    ///
    /// Returns `None` (do not cache) for an argument whose contribution to
    /// dispatch cannot be reduced to those: a `Junction` (which autothreads), a
    /// container or `Capture`, and a genuine named/positional `Pair` — the
    /// internal callsite-line marker excepted, since it is filtered out before
    /// binding and no signature can declare it.
    pub(crate) fn multi_arg_type_keys(
        &mut self,
        args: &[Value],
    ) -> Option<Vec<crate::symbol::Symbol>> {
        let mut keys = Vec::with_capacity(args.len());
        for raw in args {
            // A `VarRef` (a variable passed as an argument) dispatches on the
            // *source variable's declared type* as well as on the value's own
            // type: `unwrap_varref_for_dispatch` feeds that declared type into
            // `candidate_type_distance`, so `my int $y` and `my $x` holding the
            // same `Int` can pick different candidates (roast
            // S06-multi/by-trait.t). Key BOTH, behind a reserved marker so a
            // declared-type key can never be mistaken for the value key of an
            // extra argument. The only other thing a `VarRef` argument decides
            // is whether an `is rw` parameter accepts it, and a candidate set
            // containing an `is rw` parameter is already refused wholesale by
            // `func_multi_dispatch_type_cacheable` / `multi_dispatch_type_cacheable`.
            //
            // Refusing to key a `VarRef` at all — which is what this did before
            // — meant every assertion that passes a *variable*
            // (`is $got, $expected, "..."`, `is-deeply @a, @b, "..."`, i.e. most
            // of roast) re-ran the whole candidate walk on every call.
            let a = match raw.view() {
                ValueView::VarRef { name, value, .. } => {
                    if let Some(tc) = name.with_str(|n| self.var_type_constraint(n)) {
                        keys.push(crate::symbol::Symbol::intern(DECLARED_TYPE_KEY));
                        keys.push(crate::symbol::Symbol::intern(&tc));
                    }
                    value.clone()
                }
                _ => raw.clone(),
            };
            let a = &a;
            let key = match a.view() {
                ValueView::Instance { class_name, .. } => class_name,
                // A bare type object (`Int`, `Foo`, ...) must key on its OWN
                // name, not the generic `value_type_name` fallback below,
                // which collapses every type object to the literal string
                // "Package" regardless of which type it names — see
                // `todo/tickets/multi-arg-type-keys-package-collision.md`.
                ValueView::Package(name) => name,
                // An enum VALUE is its own dispatch identity, for the same
                // reason a type object is: `multi f(Less)` and `multi f(More)`
                // are distinct candidates that both see a `value_type_name` of
                // `Order`, so keying on the type alone hands the second call
                // the first one's winner (`t/anonymous-any-multi-dispatch.t`
                // "anonymous enum-value parameter still rejects peers"). Key on
                // `Type::Member`, which is unique and never a plain type name.
                ValueView::Enum {
                    enum_type, key: k, ..
                } => {
                    // Two symbols behind a reserved marker rather than one
                    // interned `"Type::Member"` string: no per-call `format!`,
                    // and no way for the pair to be read as the plain type name
                    // of some other argument.
                    keys.push(crate::symbol::Symbol::intern(ENUM_MEMBER_KEY));
                    keys.push(enum_type);
                    k
                }
                // The synthetic callsite-line marker the parser appends to every
                // test-assertion call (`ok 1, "x"` -> `..., "__mutsu_test_callsite_line" => 3`)
                // is a mutsu-internal diagnostic carrier, not a dispatch
                // participant: `bind_function_args_values` filters it out before
                // binding, and no signature can declare it (the name is reserved).
                // Keying it as a constant marker — rather than refusing to key the
                // whole call, as the general `Pair` arm below does — is what lets
                // the sound multi cache serve the vendored upstream `Test`, whose
                // `multi sub ok(Mu $cond, $desc = '')` otherwise paid a full
                // candidate walk on EVERY assertion. Only the marker's *name* is
                // keyed; its line-number value cannot select a candidate, because
                // any candidate that inspects a value at all (`where` / literal /
                // subset / smiley / coercion) makes the whole name un-cacheable in
                // `func_multi_dispatch_type_cacheable` before this key is used.
                _ if Self::is_callsite_line_marker(a) => {
                    crate::symbol::Symbol::intern(CALLSITE_LINE_MARKER_KEY)
                }
                ValueView::Junction { .. }
                | ValueView::Mixin(..)
                | ValueView::Scalar(_)
                | ValueView::ContainerRef(_)
                | ValueView::Pair(..)
                | ValueView::ValuePair(..)
                | ValueView::Capture { .. }
                // A `VarRef` nested inside a `VarRef` is not a shape the
                // unwrap above produces; refuse it rather than guess.
                | ValueView::VarRef { .. } => return None,
                _ => {
                    let name = crate::runtime::utils::value_type_name(a);
                    // ADR-0019 E1a shadow probe (zero behavior change): see
                    // `todo/deep/adr0019-e1-typeid-receiver-owner.md`.
                    self.shadow_check_owner("multi_arg_type_keys", a, name);
                    crate::symbol::Symbol::intern(name)
                }
            };
            keys.push(key);
            // Definedness is the one *value* property a type-keyed candidate set
            // is still allowed to depend on (`Mu:D` / `Mu:U`), and the type key
            // alone does not carry it: a type object `Int` and the instance `42`
            // both key as `Int`, and an empty `Slip` keys the same as a full one.
            // Append the marker so those land in different buckets. Cheap and
            // side-effect free -- `value_is_defined` is a pure view match, and
            // the views it would have to lock through (`ContainerRef`, `Mixin`)
            // already returned `None` above.
            if !crate::runtime::types::value_is_defined(a) {
                keys.push(crate::symbol::Symbol::intern(UNDEFINED_ARG_KEY));
            }
        }
        Some(keys)
    }

    /// Whether one candidate parameter's type constraint makes the enclosing
    /// multi's winner depend on an argument's *value* rather than on the
    /// `(type, definedness)` pair the resolve caches key on. Shared by the
    /// method-side [`Self::multi_dispatch_type_cacheable`] and the
    /// function-side `func_multi_dispatch_type_cacheable`, which must agree:
    /// they gate the same kind of cache over the same key shape.
    ///
    /// A trailing `:D`/`:U`/`:_` smiley is **not** value-dependent: the smiley
    /// tests exactly `value_is_defined`, and [`Self::multi_arg_type_keys`]
    /// carries that bit in the key ([`UNDEFINED_ARG_KEY`]). This is what lets
    /// the vendored upstream `Test`'s smiley-split assertions
    /// (`multi sub is(Mu $got, Mu:U $expected, …)` /
    /// `multi sub is(Mu $got, Mu:D $expected, …)`, and the four `is-deeply`
    /// candidates) be cached at all — before it, one smiley anywhere in the
    /// candidate set made every call re-run the whole candidate walk.
    /// Everything else that reads a value stays value-dependent: a coercion
    /// (`Int(Str)`), an enum-value or otherwise `::`-qualified refinement, the
    /// value-refining numeric pseudo-types, and a subset (an implicit `where`).
    pub(crate) fn type_constraint_is_value_dependent(&self, tc: &str) -> bool {
        let (base, _smiley) = crate::runtime::types::strip_type_smiley(tc);
        // A `Int:D()` coercion-with-smiley keeps its `(` here, so it is still
        // caught: only the trailing smiley is peeled.
        if base.contains(':') || base.contains('(') {
            return true;
        }
        if matches!(base, "Inf" | "NaN" | "-Inf" | "UInt") {
            return true;
        }
        let root = base.split(['[', ' ']).next().unwrap_or(base);
        self.registry().subsets.contains_key(root)
    }

    /// Whether a `(class, method)` is a MULTI whose dispatch is purely type+arity
    /// based — i.e. the resolved candidate is a function of the receiver class +
    /// method + positional arg types, so it is safe to cache in
    /// `multi_resolve_cache`. False for non-multi methods (the existing
    /// `method_resolve_cache` handles those) and for any multi with a value-/
    /// identity-dependent candidate (`where` / literal / subset / `:D`/`:U` smiley /
    /// coercion). Memoized per `(class, method)`.
    pub(crate) fn multi_dispatch_type_cacheable(
        &mut self,
        class_sym: crate::symbol::Symbol,
        method_sym: crate::symbol::Symbol,
        class_name: &str,
        method_name: &str,
    ) -> bool {
        if let Some(&c) = self.multi_type_cacheable.get(&(class_sym, method_sym)) {
            return c;
        }
        let mro = self.class_mro(class_name);
        let mut any_multi = false;
        let mut value_dependent = false;
        'outer: for cn in mro.iter() {
            // ADR-0019 F4a: `Registry::method_entries` (and thus the plain
            // `get_method_overloads`) has no row at all for a role that is
            // never `.new`-punned, so an un-punned role's own MRO slot always
            // came back empty here even though `class_mro` can list the role
            // by name directly (common in role-heavy diamond compositions).
            // Missing a role-only candidate can only make this cacheability
            // gate UNDER-report: a `where`/literal/rw/signature-shaped
            // candidate that lives only on the role would be invisible, so
            // `value_dependent` could wrongly stay `false` and the type-keyed
            // multi cache would then memoize a resolution that is not
            // actually type-deterministic. The role fallback closes that gap;
            // it cannot remove information the plain lookup already found, so
            // it can only push `any_multi`/`value_dependent` from false to
            // true, never the reverse. Winner selection itself
            // (`resolve_via_sequence_cache`) is untouched by this box's rule.
            let Some(overloads) = self
                .registry()
                .get_method_overloads_with_role_fallback(cn.as_str(), method_name)
            else {
                continue;
            };
            for def in &overloads {
                if def.is_multi {
                    any_multi = true;
                }
                for pd in &def.param_defs {
                    if pd.where_constraint.is_some() || pd.literal_value.is_some() {
                        value_dependent = true;
                        break 'outer;
                    }
                    // A code-signature callback (`&cb:(Int)`) or capture
                    // subsignature (`|c($a, $b)`) dispatches on the argument's
                    // signature/shape, not its `value_type_name`.
                    if pd.code_signature.is_some() || pd.sub_signature.is_some() {
                        value_dependent = true;
                        break 'outer;
                    }
                    // A CONSTRAINED `&`-sigil parameter dispatches on the
                    // passed routine's declared RETURN type — see the matching
                    // note in `func_multi_dispatch_type_cacheable`.
                    if pd.name.starts_with('&') && pd.type_constraint.is_some() {
                        value_dependent = true;
                        break 'outer;
                    }
                    // An `is rw` candidate matches only a writable-lvalue
                    // argument — a property of the call site, not of the arg's
                    // type — so `m($var)` and `m("lit")` need different winners
                    // under one type key (Text::IO::String's `new (Str $s! is
                    // rw)` / `new (Str $s!)` pair).
                    if pd.traits.iter().any(|t| t == "rw") {
                        value_dependent = true;
                        break 'outer;
                    }
                    if let Some(tc) = &pd.type_constraint
                        && self.type_constraint_is_value_dependent(tc)
                    {
                        value_dependent = true;
                        break 'outer;
                    }
                }
            }
        }
        let cacheable = any_multi && !value_dependent;
        self.multi_type_cacheable
            .insert((class_sym, method_sym), cacheable);
        cacheable
    }

    /// True when `target`'s intrinsic native type (or an ancestor in its MRO,
    /// e.g. `List` for `Array`) carries a user-declared/augmented method of this
    /// name. Guards the "lever A" pure-value native probes (`.sort`/`.map`/
    /// `.first`/the QuantHash/Map/Hash/IO/encode-decode coercions) in
    /// `try_compiled_method_or_interpret_inner` / `try_compiled_method_mut_or_interpret_sym`:
    /// those probes run for plain `Array`/`List`/`Hash`/`Str`/... values, not
    /// `Instance` receivers, so the `has_user_method` gate the Instance branch
    /// already applies earlier in the same functions never covers them. Without
    /// this guard a legal `augment class Array { method sort {...} }` (legal
    /// because `Array` itself does not declare `sort` — no redeclaration error,
    /// unlike `augment class Str { method uc {...} }`) was silently shadowed by
    /// the native fast path. See `t/augment-native-lever-a-methods.t`.
    ///
    /// Memoized on `(type name, method)` — the answer is a pure function of the
    /// registry shape, so it only changes when the registry generation does, and
    /// [`Self::refresh_method_caches_for_generation`] clears the memo alongside
    /// the other method caches. This gate sits on EVERY native method call, and
    /// uncached it re-walked the receiver's whole MRO (`Int` -> `Cool` -> `Any`
    /// -> `Mu`) asking `user_method_overloads` at each level, just to re-derive
    /// "no, nobody augmented `Int`".
    pub(crate) fn native_lever_a_user_override(&mut self, target: &Value, method: &str) -> bool {
        let type_name = crate::runtime::utils::value_type_name(target);
        self.refresh_method_caches_for_generation();
        let key = (
            crate::symbol::Symbol::intern(type_name),
            crate::symbol::Symbol::intern(method),
        );
        if let Some(&hit) = self.native_lever_a_override_cache.get(&key) {
            return hit;
        }
        let answer = self.has_user_method(type_name, method);
        self.native_lever_a_override_cache.insert(key, answer);
        answer
    }

    /// Resolve a method, consulting the sound multi-resolution cache for a
    /// type+arity-deterministic multi (avoids the per-call MRO/specificity walk).
    /// Non-multi / uncacheable / un-keyable calls resolve fresh (the non-multi
    /// caches live at the call sites). An AMBIGUOUS multi resolution is never
    /// cached — it must re-raise `X::Multi::Ambiguous` on every call (a cache hit
    /// would not set `dispatch_ambiguous`).
    pub(crate) fn resolve_method_cached(
        &mut self,
        cn: &str,
        method: &str,
        class_sym: crate::symbol::Symbol,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
        target: &Value,
    ) -> Option<(
        crate::symbol::Symbol,
        std::sync::Arc<crate::runtime::MethodDef>,
    )> {
        self.refresh_method_caches_for_generation();
        // Non-multi resolution depends only on (class, method) — not on arg
        // types/values — so it can be memoized. This mirrors the cache hierarchy
        // already used by the interpret path (`vm_call_method_compiled_interpret`);
        // without it the compiled-mut hot path re-ran the full MRO/specificity
        // walk in `resolve_method_with_owner_invocant` on *every* call to a plain
        // (non-multi) method. Both caches are invalidated together at every
        // registry/type/module mutation site (see `method_resolve_cache.clear()`
        // / `last_method_resolve = None`).

        // 1. Monomorphic inline cache: single-entry check before any HashMap.
        if let Some((cc, cm, co, ref cd)) = self.last_method_resolve
            && cc == class_sym
            && cm == method_sym
            && !cd.is_multi
        {
            return Some((co, cd.clone()));
        }
        // 2. Non-multi HashMap cache.
        if let Some(hit) = self
            .method_resolve_cache
            .get(&(class_sym, method_sym))
            .cloned()
            && let Some((owner, ref def)) = hit
            && !def.is_multi
        {
            self.last_method_resolve = Some((class_sym, method_sym, owner, def.clone()));
            return hit;
        }
        // 3. Sound multi-method resolution cache (type+arity deterministic).
        if let Some(mut arg_keys) = self.multi_arg_type_keys(args)
            && self.multi_dispatch_type_cacheable(class_sym, method_sym, cn, method)
        {
            // The INVOCANT's definedness is part of the dispatch too: an
            // invocant smiley (`multi method gist(Cook:U:)` /
            // `(Cook:D:)`) selects on exactly it, and `class_sym` is the same
            // for a type object and an instance. `multi_arg_type_keys` only
            // sees the argument list, so carry the receiver's bit here — the
            // arg keys are a `Vec`, so a leading marker is unambiguous.
            // Without it, `Cook.gist` and `Cook.new.gist` share one bucket and
            // the second is served the first's candidate
            // (`t/multi-method-invocant-definedness.t`).
            if !crate::runtime::types::value_is_defined(target) {
                arg_keys.insert(0, crate::symbol::Symbol::intern(UNDEFINED_ARG_KEY));
            }
            let mkey = (class_sym, method_sym, arg_keys);
            if let Some(hit) = self.multi_resolve_cache.get(&mkey) {
                return hit.clone();
            }
            // ADR-0019 E3: resolve via the cached candidate sequence instead
            // of a live per-call MRO walk. See
            // `todo/deep/adr0019-e2-e4-resolver-core.md` design decision 5.
            let resolved = self.resolve_via_sequence_cache(cn, method_sym, args, target);
            let resolved_arc = resolved.map(|(o, d)| (o, std::sync::Arc::new(d)));
            if !self.dispatch_ambiguous {
                self.multi_resolve_cache.insert(mkey, resolved_arc.clone());
            }
            return resolved_arc;
        }
        // 4. Resolve fresh; cache the result when it is non-multi.
        // ADR-0019 E3: resolve via the cached candidate sequence instead of a
        // live per-call MRO walk. See
        // `todo/deep/adr0019-e2-e4-resolver-core.md` design decision 5.
        let resolved = self.resolve_via_sequence_cache(cn, method_sym, args, target);
        let resolved_arc = resolved.map(|(o, d)| (o, std::sync::Arc::new(d)));
        if resolved_arc.as_ref().is_none_or(|(_, def)| !def.is_multi) {
            self.method_resolve_cache
                .insert((class_sym, method_sym), resolved_arc.clone());
            if let Some((owner, ref def)) = resolved_arc {
                self.last_method_resolve = Some((class_sym, method_sym, owner, def.clone()));
            }
        }
        resolved_arc
    }

    /// Compile a resolved user method's body on demand when it lacks bytecode,
    /// then dispatch as bytecode instead of through the interpreter bridge.
    /// Almost all user methods are already compiled at class registration
    /// (`compile_class_methods`); the gap this closes is methods inserted after
    /// that pass without their own bytecode — notably `.^add_multi_method`
    /// (which hardcodes `compiled_code = None`) and any future such site.
    /// (`.^add_method` already carries the method literal's compiled code.)
    /// Populates `compiled_code` in the canonical registry (idempotent) and
    /// re-resolves so the returned def carries the bytecode. Returns `None`
    /// when the owner is not a user class/role (native receiver) or the body
    /// stays uncompilable, preserving the interpreter fallback. Ledger §1.
    pub(crate) fn populate_uncompiled_method(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        args: &[Value],
        target: &Value,
    ) -> Option<(
        crate::symbol::Symbol,
        std::sync::Arc<crate::runtime::MethodDef>,
    )> {
        // Both compile passes are no-ops if the name is absent from the
        // respective registry, so calling both safely covers class- and
        // role-owned methods. Neither re-enters user code (pure compilation),
        // so the registry re-entrancy discipline (②) is respected.
        self.compile_class_methods(owner_class);
        self.compile_role_methods(owner_class);
        let (owner, def) = loan_env!(
            self,
            resolve_method_with_owner_invocant(cn, method, args, target)
        )?;
        if def.compiled_code.is_some() {
            Some((owner, std::sync::Arc::new(def)))
        } else {
            None
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn dispatch_compiled_method(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        method_def: &std::sync::Arc<crate::runtime::MethodDef>,
        cc: &std::sync::Arc<CompiledCode>,
        target: Value,
        args: Vec<Value>,
        can_skip_merge: Option<bool>,
    ) -> Result<Value, RuntimeError> {
        let target_id = match target.view() {
            ValueView::Instance { id, .. } => Some(id),
            _ => None,
        };
        let attrs_cell = match target.view() {
            ValueView::Instance { attributes, .. } => Some(attributes.clone()),
            _ => None,
        };
        // No whole-map `to_map()` snapshot here: the fast path reads attributes
        // through the live cell, and the slow path materializes its own map.
        let attrs_empty = attrs_cell.as_ref().is_none_or(|c| c.as_map().is_empty());
        let empty_fns = CompiledFns::default();
        // A `sub` declared inside this method's body compiles into
        // `method_def.compiled_fns`; without it, the nested routine's compiled
        // key can never resolve at call time (ADR-0019 C6e-3c).
        let fns_ref = method_def.compiled_fns.as_deref().unwrap_or(&empty_fns);
        let method_result = if let Some(csm) = can_skip_merge {
            // Fast path: move target directly as base (avoid extra clone).
            let invocant_for_dispatch = if attrs_empty {
                Value::package(crate::symbol::Symbol::intern(cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = loan_env!(
                self,
                push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
            );
            let result = self.call_compiled_method_fast(
                cn,
                owner_class,
                method,
                method_def,
                cc,
                args,
                target,
                fns_ref,
                csm,
            );
            if pushed_dispatch {
                self.pop_method_dispatch();
            }
            self.pop_method_samewith_context();
            result
        } else {
            let attributes = attrs_cell.as_ref().map(|c| c.to_map()).unwrap_or_default();
            let invocant_for_dispatch = if attrs_empty {
                Value::package(crate::symbol::Symbol::intern(cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = loan_env!(
                self,
                push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
            );
            let invocant = Some(target);
            let result = self.call_compiled_method(
                cn,
                owner_class,
                method,
                method_def,
                cc,
                &attributes,
                args,
                invocant,
                fns_ref,
            );
            if pushed_dispatch {
                self.pop_method_dispatch();
            }
            self.pop_method_samewith_context();
            result
        };
        let (result, reconciled) = method_result?;
        if let Some(id) = target_id {
            // Commit only a `:=`-adjusted snapshot: an unadjusted one equals the
            // cell and the whole-map write would race with concurrent cell-CAS
            // from another thread (lost updates).
            if let (Some(m), Some(cell)) = (&reconciled, &attrs_cell) {
                cell.commit_attrs(m.clone());
            }
            if !self.in_lvalue_assignment
                && let ValueView::Proxy { fetcher, .. } = result.view()
            {
                // Without a `:=` adjustment the returned map is absent —
                // re-snapshot the live cell for the proxy fetcher.
                let proxy_attrs = match (&reconciled, &attrs_cell) {
                    (Some(m), _) => m.clone(),
                    (None, Some(cell)) => cell.to_map(),
                    (None, None) => AttrMap::new(),
                };
                return loan_env!(self, proxy_fetch(fetcher, None, cn, &proxy_attrs, id));
            }
        }
        Ok(result)
    }

    /// Variant of [`Self::dispatch_compiled_method`] for callers whose bound
    /// `self`/invocant carries no attribute cell of its own — e.g. a role-mixin
    /// wrapper (`ValueView::Mixin`), whose real attribute storage lives on a
    /// DIFFERENT value (the mixin's `inner` instance) — but where `self` must
    /// still be the wrapper itself so a nested `self.foo` inside the method
    /// redispatches through the mixin's role overrides. `attrs_cell` supplies
    /// the actual attribute storage to read from and commit mutations back
    /// onto; `target` is the value bound as `self`.
    ///
    /// Always takes the slow (`call_compiled_method`) path, never the fast
    /// (`call_compiled_method_fast`) one: the fast path's live-cell
    /// optimization reads attributes directly off `self`'s own `ValueView`,
    /// which requires `self` to literally be `ValueView::Instance` — not true
    /// here by construction, so there is no live cell for it to find. This is
    /// an acceptable trade for a cold path (role-mixin class-method dispatch),
    /// matching ADR-0019 F6's box note that the general-call-dispatch family's
    /// mixin fallback "needs a new helper shape" rather than reusing
    /// `try_dispatch_compiled_method_direct`/`_as` as-is (those would silently
    /// derive an empty attribute map from the wrapper and drop mutations).
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn dispatch_compiled_method_with_attrs_cell(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        method_def: &std::sync::Arc<crate::runtime::MethodDef>,
        cc: &std::sync::Arc<CompiledCode>,
        target: Value,
        attrs_cell: &crate::gc::Gc<crate::value::InstanceAttrs>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let empty_fns = CompiledFns::default();
        let fns_ref = method_def.compiled_fns.as_deref().unwrap_or(&empty_fns);
        let attributes = attrs_cell.to_map();
        let pushed_dispatch = loan_env!(
            self,
            push_method_dispatch_frame(cn, method, &args, target.clone(),)
        );
        let invocant = Some(target);
        let result = self.call_compiled_method(
            cn,
            owner_class,
            method,
            method_def,
            cc,
            &attributes,
            args,
            invocant,
            fns_ref,
        );
        if pushed_dispatch {
            self.pop_method_dispatch();
        }
        self.pop_method_samewith_context();
        let (result, reconciled) = result?;
        // Same "only commit a `:=`-adjusted snapshot" reasoning as
        // `dispatch_compiled_method`: an unadjusted result equals the cell
        // already, so a whole-map write would race with concurrent cell-CAS.
        if let Some(m) = &reconciled {
            attrs_cell.commit_attrs(m.clone());
        }
        Ok(result)
    }

    /// Pre-compute and cache fast dispatch eligibility for a method.
    pub(crate) fn try_populate_fast_cache(
        &mut self,
        cache_key: (crate::symbol::Symbol, crate::symbol::Symbol),
        receiver_class: &str,
        owner_class: crate::symbol::Symbol,
        method_def: &std::sync::Arc<crate::runtime::MethodDef>,
        cc: &std::sync::Arc<CompiledCode>,
    ) {
        let has_rw_params = method_def
            .param_defs
            .iter()
            .any(|pd| pd.traits.iter().any(|t| t == "rw"));
        if has_rw_params {
            return;
        }
        let has_invocant_constraint = method_def.param_defs.iter().any(|pd| {
            (pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                && pd.type_constraint.is_some()
        });
        let has_complex_params = method_def.param_defs.iter().any(|pd| {
            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                return false;
            }
            if pd.slurpy && pd.name == "%_" {
                return false;
            }
            // An attributive parameter (`$!x`/`@!a`) binds straight to an
            // attribute, i.e. it mutates `self` — so it is not read-only and
            // must take the full path (which mirrors it into the shared
            // cell and writes it back; see the matching gate in
            // `call_compiled_method`, vm_method_dispatch.rs). Caching this
            // method as fast-dispatchable would make every call after the
            // first silently drop the attribute write.
            if Self::attr_twigil_base(&pd.name).is_some() {
                return true;
            }
            pd.slurpy
                || pd.double_slurpy
                || pd.named
                || pd.where_constraint.is_some()
                || pd.sub_signature.is_some()
                || pd.outer_sub_signature.is_some()
                || pd.code_signature.is_some()
                || pd
                    .type_constraint
                    .as_ref()
                    .is_some_and(|tc| tc.contains('('))
        });
        let has_role_bindings = method_def.role_param_bindings.is_some()
            || self
                .class_role_param_bindings(owner_class.as_str())
                .is_some()
            || self.class_role_param_bindings(receiver_class).is_some();
        if has_invocant_constraint || has_complex_params || has_role_bindings {
            return;
        }
        let positional_count = method_def
            .param_defs
            .iter()
            .filter(|pd| {
                !pd.is_invocant
                    && !pd.traits.iter().any(|t| t == "invocant")
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && !pd.named
            })
            .count();
        // Also gate on `cc.has_calls`: a body that invokes a CLOSURE
        // (`$f()` — `CallOnValue`/`CallOnCodeVar`) or a CallDefined can
        // write a dynamic var / captured-outer lexical / global into this frame's
        // env (those call ops are NOT in `has_env_writes`). Skipping the merge would
        // drop that write — e.g. `method go { my $f = { $*x = 1 }; $f() }`. #3658.
        let can_skip_merge = !cc.has_env_writes && !cc.has_calls;
        let has_defaults = method_def.param_defs.iter().any(|pd| {
            !pd.is_invocant && !pd.traits.iter().any(|t| t == "invocant") && pd.default.is_some()
        });
        self.fast_method_cache.insert(
            cache_key,
            super::FastMethodCacheEntry {
                owner_class,
                method_def: method_def.clone(),
                compiled_code: cc.clone(),
                can_skip_merge,
                positional_count,
                has_defaults,
            },
        );
    }
}

#[cfg(test)]
mod multi_arg_type_keys_tests {
    use super::*;

    fn interp() -> Interpreter {
        Interpreter::new()
    }

    #[test]
    fn distinct_type_object_args_key_distinctly() {
        // Regression for todo/tickets/multi-arg-type-keys-package-collision.md:
        // every bare type-object argument used to fall through to the generic
        // `value_type_name` arm, which reports the literal string "Package"
        // for ALL type objects regardless of which type they name. That
        // collapsed e.g. `f(Int)` and `f(Str)` to the same multi-dispatch
        // cache key.
        let mut i = interp();
        let int_key = i
            .multi_arg_type_keys(&[Value::package(crate::symbol::Symbol::intern("Int"))])
            .unwrap();
        let str_key = i
            .multi_arg_type_keys(&[Value::package(crate::symbol::Symbol::intern("Str"))])
            .unwrap();
        assert_ne!(int_key, str_key);
        assert_eq!(int_key[0].as_str(), "Int");
        assert_eq!(str_key[0].as_str(), "Str");
    }
}
