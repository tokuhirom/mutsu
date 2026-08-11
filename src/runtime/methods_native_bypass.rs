use super::resolution_sequence::ResolvedCandidate;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Check if a metamodel class name is a HOW type.
    pub(super) fn is_metamodel_how(class_name: &Symbol) -> bool {
        let cn = class_name.resolve();
        cn == "Perl6::Metamodel::ClassHOW"
            || cn == "Perl6::Metamodel::ModuleHOW"
            || cn == "Perl6::Metamodel::PackageHOW"
            || cn == "Perl6::Metamodel::SubsetHOW"
            || cn == "Perl6::Metamodel::EnumHOW"
            || cn == "Perl6::Metamodel::CurriedRoleHOW"
            || cn == "Perl6::Metamodel::ParametricRoleGroupHOW"
            || cn == "Perl6::Metamodel::CoercionHOW"
    }

    /// Check if a method name is a ClassHOW method.
    pub(crate) fn is_classhow_method(method: &str) -> bool {
        matches!(
            method,
            "can"
                | "does"
                | "isa"
                | "lookup"
                | "find_method"
                | "add_attribute"
                | "add_method"
                | "add_multi_method"
                | "add_fallback"
                | "add_parent"
                | "compose"
                | "archetypes"
                | "nominalize"
                | "name"
                | "shortname"
                | "array_type"
                | "set_name"
                | "ver"
                | "auth"
                | "api"
                | "mro"
                | "mro_unhidden"
                | "methods"
                | "attributes"
                | "parents"
                | "roles"
                | "candidates"
                | "concretization"
                | "curried_role"
                | "enum_value_list"
                | "coerce"
                | "parameterize"
                | "pun"
                | "language-revision"
                | "method_table"
                | "submethod_table"
        )
    }

    /// Check if a method on LazyList should force evaluation.
    pub(super) fn should_force_lazy_list(method: &str) -> bool {
        matches!(
            method,
            "list"
                | "Array"
                | "Numeric"
                | "Int"
                | "elems"
                | "hyper"
                | "race"
                | "first"
                | "grep"
                | "map"
                | "sort"
                | "reverse"
                | "join"
                | "head"
                | "tail"
                | "min"
                | "max"
                | "minmax"
                | "sum"
                | "flat"
                | "unique"
                | "repeated"
                | "squish"
                | "classify"
                | "categorize"
                | "produce"
                | "rotor"
                | "batch"
                | "reduce"
                | "combinations"
                | "permutations"
                | "values"
                | "List"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "Seq"
                | "item"
                | "cache"
                | "pick"
                | "roll"
                | "keys"
                | "kv"
                | "pairs"
                | "antipairs"
        )
    }

    /// ADR-0019 E4b design decision 3's "receiver-state facts become
    /// resolver guards" bucket (category 1 of the three-way split in
    /// `todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`):
    /// hazards where the native fast-path cascade itself would misbehave for
    /// this receiver if reached, independent of whether a user method/
    /// accessor/NativeCall binding also exists for the name (those are
    /// categories 2/3 — `is_native_method`/`resolve_user_method_or_accessor`
    /// — handled separately, not folded in here). The ADR's step-2 audit
    /// (2026-08-11) confirmed these do NOT reduce to "the row table has no
    /// entry for this (owner, method)": E4b's resolver falls back to the
    /// pure arity cascade on any row miss, so row absence alone would not
    /// stop the cascade from being (wrongly) tried — each of these stays an
    /// explicit guard even after the eventual resolver cutover. Only used
    /// from the main (non-lazy-Match) body of
    /// [`Self::should_bypass_native_fastpath`]: the lazy-Match branch avoids
    /// `target.view()` (it would materialize the lazy value) and instead
    /// inlines the subset of these checks that can ever apply to a Match
    /// receiver.
    fn native_fastpath_receiver_state_guard(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> bool {
        // `squish` always routes to the interpreter regardless of owner:
        // `methods_0arg/collection.rs` implements it per-view, so a row miss
        // must not fall through to a wrong native answer (ADR-0019 E4b step
        // 2's "confirmed NOT reducible" finding).
        method == "squish"
            || (matches!(
                method,
                "max"
                    | "min"
                    | "head"
                    | "flat"
                    | "sort"
                    | "comb"
                    | "words"
                    | "batch"
                    | "rotor"
                    | "rotate"
                    | "produce"
                    | "snip"
                    | "minmax"
                    | "start"
                    | "wait"
                    | "zip"
                    | "zip-latest"
                    | "list"
                    | "Array"
                    | "Seq"
            ) && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "elems" && matches!(target.view(), ValueView::Instance { .. }))
            // `.throw`/`.gist`/`.Str`/`.Stringy`/`.rethrow` render
            // `$exc.message`; the native fast path can only read the stored
            // `message` attribute, which is still undefined when the class
            // computes its message lazily. See the twin gate in
            // `vm_native_dispatch::try_native_method`.
            || (matches!(method, "throw" | "rethrow" | "gist" | "Str" | "Stringy")
                && matches!(target.view(), ValueView::Instance { class_name, .. }
                    if self.exception_render_needs_interpreter(target, &class_name.resolve())))
            || (matches!(target.view(), ValueView::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric")))
            // Only `Proc::Async.Supply` needs an explicit gate: the coercion
            // cascade's generic `"Supply"` arm (`methods_0arg/coercion.rs`)
            // does not special-case `Proc::Async` the way it does
            // `Supplier`, so a bare row-miss fallback would wrap the
            // `Proc::Async` instance itself in a bogus values-Supply
            // (ADR-0019 E4b step 6). `Supplier`/`Supplier::Preserving.Supply`
            // needs no such gate — the coercion arm already returns `None`
            // for both (step 5). `IO::Handle`'s `chomp`/`encoding`/`opened`/
            // `DESTROY` need no gate either — `chomp`'s own cascade arm
            // already self-guards and the other three have no cascade arm
            // at all (step 7).
            || (matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Proc::Async")
                && method == "Supply")
            // `Stash.keys`/`.values` need the interpreter's own package-stash
            // enumeration; the generic `.keys`/`.values` cascade arms
            // (`methods_0arg/collection.rs`) have a catch-all that would
            // misread an Instance receiver as a one-element list instead of
            // reading the Stash's own hash (step 7). `Stash.AT-KEY` needs no
            // gate — it has no cascade arm at all.
            || (matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Stash")
                && matches!(method, "keys" | "values"))
            || (method == "keys"
                && args.is_empty()
                && (matches!(target.view(), ValueView::Hash(_))
                    || matches!(target.view(), ValueView::Mixin(inner, _) if matches!(inner.as_ref().view(), ValueView::Hash(_)))))
    }

    /// Determine whether to bypass the native method fast path.
    pub(super) fn should_bypass_native_fastpath(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
        skip_pseudo: bool,
        is_pseudo_method: bool,
    ) -> bool {
        // Lazy-Match head branch: the chain below reads `target.view()`
        // repeatedly, which would materialize a lazy Match per method call.
        // Its class is statically "Match", so evaluate the Instance arms that
        // can apply to it directly (the Supply/IO::Handle/Proc::Async/Stash
        // arms and the Real/Numeric bridge arm can never hit a Match).
        if target.is_lazy_match_value() {
            return skip_pseudo
                || method == "squish"
                || method == "elems"
                || (matches!(method, "throw" | "rethrow" | "gist" | "Str" | "Stringy")
                    && self.exception_render_needs_interpreter(target, "Match"))
                || self.is_native_method("Match", method)
                || self.has_user_method("Match", "Bridge")
                || (!is_pseudo_method
                    && (self.has_user_method("Match", method)
                        || self.has_public_accessor("Match", method)
                        || (self.has_class_level_attr("Match", method)
                            && !self.has_public_accessor("Match", method))));
        }
        skip_pseudo
            || self.native_fastpath_receiver_state_guard(target, method, args)
            || matches!(target.view(), ValueView::Instance { class_name, .. }
                if self.is_native_method(&class_name.resolve(), method))
            || matches!(target.view(), ValueView::Instance { class_name, .. } if self.has_user_method(&class_name.resolve(), "Bridge"))
            || (!is_pseudo_method
                && matches!(target.view(), ValueView::Instance { class_name, .. } if self.has_user_method(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target.view(), ValueView::Instance { class_name, .. } if self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target.view(), ValueView::Package(class_name) if self.has_user_method(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target.view(), ValueView::Package(class_name) if self.has_class_level_attr(&class_name.resolve(), method) && !self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method
                && matches!(target.view(), ValueView::Instance { class_name, .. } if self.has_class_level_attr(&class_name.resolve(), method) && !self.has_public_accessor(&class_name.resolve(), method)))
            || (!is_pseudo_method && self.mixin_role_has_method(target, method))
    }

    /// ADR-0019 E4b shadow probe (`MUTSU_VM_STATS`-gated, a no-op otherwise):
    /// compare `should_bypass_native_fastpath`'s "does a user method/
    /// accessor/class-level-attr (or, for an Instance, a NativeCall binding)
    /// win" categories — the scoping doc's categories 2 and 3,
    /// `todo/deep/adr0019-e4b-should-bypass-native-fastpath-decomposition.md`
    /// — against the resolver's answer for the receiver's own class.
    /// Recomputes the same sub-expression `should_bypass_native_fastpath`
    /// evaluates at lines 179-180/214-224, independent of whatever the real
    /// bypass decision turned out to be (a category-1 special case can make
    /// the real decision `true` while this probe's `real` is `false`, and
    /// that is expected — this only asks whether the two answer this one
    /// sub-question the same way). Purely observational: nothing here feeds a
    /// dispatch decision.
    ///
    /// Step 3 (2026-08-11): category 2 (`is_native_method`) is not visible to
    /// `resolve_user_method_or_accessor` on its own (a pure native-method
    /// binding with no matching accessor, e.g. `Supply.tap`, ~82% of step 1's
    /// mismatches) — it now has its own `ResolvedCandidate::NativeCallBinding`
    /// entry in `resolve_sequence`, OR'd in here for an Instance receiver.
    pub(super) fn shadow_check_bypass_user_method_categories(
        &mut self,
        target: &Value,
        method: &str,
        is_pseudo_method: bool,
        arg_count: usize,
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let (class_name, is_instance) = match target.view() {
            ValueView::Instance { class_name, .. } => (class_name.resolve(), true),
            ValueView::Package(class_name) => (class_name.resolve(), false),
            _ => return,
        };
        let real = if is_instance {
            self.is_native_method(&class_name, method)
                || (!is_pseudo_method
                    && (self.has_user_method(&class_name, method)
                        || self.has_public_accessor(&class_name, method)
                        || (self.has_class_level_attr(&class_name, method)
                            && !self.has_public_accessor(&class_name, method))))
        } else {
            !is_pseudo_method
                && (self.has_user_method(&class_name, method)
                    || (self.has_class_level_attr(&class_name, method)
                        && !self.has_public_accessor(&class_name, method)))
        };
        // ADR-0019 E4b step 10 scoping: built for BOTH receiver kinds, not just
        // Instance. `is_native_method` (`real`'s category-2 term above) is
        // deliberately never checked for a Package receiver -- calling an `is
        // native(&sym)` binding through the bare type object rather than an
        // instance is not a case the real bypass logic accounts for. Widening
        // this shadow-only check to Package too (rather than short-circuiting
        // to `None`) answers, empirically, whether `resolve_sequence`'s
        // presence-only `NativeCallBinding` walk (which does not distinguish
        // receiver kind) ever actually disagrees with that omission --
        // load-bearing before the eventual authoritative switch can safely
        // consume `resolve_sequence` uniformly for both receiver kinds.
        let chain = self.dispatch_mro(target);
        let native_shape = super::resolution_sequence::NativeCallShape::new(arg_count, is_instance);
        let seq = self.resolve_sequence(&chain, Symbol::intern(method), native_shape);
        let native_binding_owner = seq.candidates.iter().find_map(|c| match c {
            ResolvedCandidate::NativeCallBinding { owner } => Some(owner.as_str().to_string()),
            ResolvedCandidate::User { .. } | ResolvedCandidate::Native { .. } => None,
        });
        let shadow = native_binding_owner.is_some()
            || self
                .resolve_user_method_or_accessor(&class_name, method)
                .is_some();
        crate::vm::vm_stats::record_bypass_shadow_check(real == shadow, || {
            format!(
                "class={class_name} method={method} real={real} shadow={shadow} native_binding_owner={native_binding_owner:?}"
            )
        });
    }

    /// Check if a Mixin's role mixins define the given method.
    /// Used so that role-method dispatch on punned role instances takes
    /// precedence over the built-in Cool fallbacks (e.g. `.uc`).
    pub(crate) fn mixin_role_has_method(&self, target: &Value, method: &str) -> bool {
        // Tag probe first — a `view()` on a lazy Match would materialize it.
        if !target.is_mixin_value() {
            return false;
        }
        let ValueView::Mixin(_, mixins) = target.view() else {
            return false;
        };
        for key in mixins.keys() {
            let Some(role_name) = key.strip_prefix("__mutsu_role__") else {
                continue;
            };
            if let Some(role) = self.registry().roles.get(role_name)
                && role.methods.contains_key(method)
            {
                return true;
            }
        }
        false
    }

    /// Render an object-hash pair KEY for `.raku`, applying Pair.raku's
    /// parenthesisation (mirrors `object_hash_key_repr`), but dispatching the
    /// key's own `.raku` through the real interpreter instead of the
    /// allocation-free `raku_value` fast path. `raku_value` cannot call a
    /// user-defined `method raku`, nor render an instance in its `T.new(...)`
    /// constructor form (both need `&mut self` dispatch) — it falls back to a
    /// generic stringification, which for a plain instance key rendered
    /// `U.new` as `U()`. The stored VALUE already dispatches this way
    /// (`call_method_with_values(v, "raku", ...)`); the key must match.
    fn object_hash_key_raku(&mut self, typed: &Value) -> String {
        let inner = self
            .call_method_with_values(typed.clone(), "raku", vec![])
            .map(|r| r.to_string_value())
            .unwrap_or_else(|_| crate::builtins::methods_0arg::raku_repr::raku_value(typed));
        match typed.view() {
            ValueView::Package(_)
            | ValueView::ParametricRole { .. }
            | ValueView::Pair(..)
            | ValueView::ValuePair(..) => {
                format!("({})", inner)
            }
            _ => inner,
        }
    }

    /// Dispatch .raku/.perl on constrained Hash.
    pub(super) fn dispatch_constrained_hash_raku(
        &mut self,
        map: &crate::value::HashData,
        info: &ContainerTypeInfo,
        itemized: bool,
    ) -> Result<Value, RuntimeError> {
        // A `$`-scalar-itemized typed hash wraps its repr with the itemization
        // sigil: a paren/bracket literal (the `(my Int %{Int} = ...)` typed form)
        // takes the bare `$` → `$(...)`; anything else (`Map.new(...)`) is
        // paren-wrapped → `$(Map.new(...))`.
        let itemize_wrap = |base: String| -> String {
            if !itemized {
                base
            } else if base.starts_with(['{', '[', '(']) {
                format!("${base}")
            } else {
                format!("$({base})")
            }
        };
        let mut sorted_keys: Vec<&String> = map.keys().collect();
        sorted_keys.sort();
        // An immutable Map renders as `Map.new((:k(v), ...))`, not the
        // `(my % = ...)` typed-hash form.
        if info.declared_type.as_deref() == Some("Map") {
            let parts: Vec<String> = sorted_keys
                .iter()
                .map(|k| {
                    let v = &map[*k];
                    let repr = if v.is_nil() {
                        "Any".to_string()
                    } else {
                        self.call_method_with_values(v.clone(), "raku", vec![])
                            .map(|r| r.to_string_value())
                            .unwrap_or_else(|_| format!("{:?}", v))
                    };
                    let typed = map.typed_key(k);
                    match typed.view() {
                        ValueView::Str(s)
                            if crate::builtins::methods_0arg::raku_repr::is_adverbial_pair_key(
                                &s,
                            ) =>
                        {
                            format!(":{}({})", *s, repr)
                        }
                        _ => format!("{} => {}", self.object_hash_key_raku(&typed), repr),
                    }
                })
                .collect();
            return Ok(Value::str(itemize_wrap(format!(
                "Map.new(({}))",
                parts.join(",")
            ))));
        }
        let parts: Vec<String> = sorted_keys
            .iter()
            .map(|k| {
                let v = &map[*k];
                let value_repr = if v.is_nil() {
                    "Any".to_string()
                } else {
                    self.call_method_with_values(v.clone(), "raku", vec![])
                        .map(|r| r.to_string_value())
                        .unwrap_or_else(|_| format!("{:?}", v))
                };
                // Object hashes store `.WHICH` string keys; serialize the original
                // typed key (`1`, not `Int|1`; `a`, not `Str|a`). Raku renders each
                // pair per its *key*, independent of the hash's key-type: a Str key
                // that is a valid identifier becomes a colonpair `:key(value)`;
                // every other key (a non-Str key, or a Str needing quotes) becomes
                // `key => value` with the key rendered via `.raku`.
                let typed = map.typed_key(k);
                match typed.view() {
                    ValueView::Str(s)
                        if crate::builtins::methods_0arg::raku_repr::is_adverbial_pair_key(&s) =>
                    {
                        format!(":{}({})", *s, value_repr)
                    }
                    _ => format!("{} => {}", self.object_hash_key_raku(&typed), value_repr),
                }
            })
            .collect();
        let key_suffix = if let Some(ref kt) = info.key_type {
            format!("{{{}}}", kt)
        } else {
            String::new()
        };
        // An object hash with no element-type constraint is a `:{...}`
        // literal — rakudo's Hash[Mu,Mu], shown as `my Mu`.
        let value_type = if !info.value_type.is_empty() {
            info.value_type.as_str()
        } else if info.key_type.is_some() {
            "Mu"
        } else {
            "Any"
        };
        let inner = parts.join(", ");
        let result = if inner.is_empty() {
            format!("(my {} %{})", value_type, key_suffix)
        } else {
            format!("(my {} %{} = {})", value_type, key_suffix, inner)
        };
        Ok(Value::str(itemize_wrap(result)))
    }

    /// Dispatch Complex->Num conversion.
    pub(super) fn dispatch_complex_to_num(
        &mut self,
        r: f64,
        im: f64,
        target: &Value,
    ) -> Result<Value, RuntimeError> {
        let tolerance = self
            .get_dynamic_var("*TOLERANCE")
            .ok()
            .and_then(|v| match v.view() {
                ValueView::Num(n) => Some(n),
                ValueView::Rat(n, d) if d != 0 => Some(n as f64 / d as f64),
                ValueView::Int(n) => Some(n as f64),
                _ => None,
            })
            .unwrap_or(1e-15);
        if im.abs() > tolerance {
            let msg = format!(
                "Cannot convert {}{}{}i to Num: imaginary part not zero",
                r,
                if im >= 0.0 { "+" } else { "" },
                im
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("target".to_string(), Value::package(Symbol::intern("Num")));
            attrs.insert("source".to_string(), target.clone());
            let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        Ok(Value::num(r))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// ADR-0019 E4b step-1 finding (2026-08-11 sweep): `resolve_user_method_or_accessor`
    /// only consults `ClassDef::native_methods` as a tiebreak when the same MRO level
    /// also has a matching public attribute accessor — it never independently answers
    /// "does this receiver's class have a pure NativeCall/native-methods-table binding
    /// for this name with no accessor of the same name". `Supply.tap` (a
    /// `runtime_init.rs`-seeded builtin with `native_methods: [.., "tap", ..]` and no
    /// `tap` accessor) is exactly that shape, and was ~81% of the sweep's shadow
    /// mismatches. This pins the gap so a future `resolve_user_method_or_accessor`
    /// change cannot silently "fix" it by accident without this test also changing.
    #[test]
    fn resolve_user_method_or_accessor_does_not_see_a_pure_native_methods_entry() {
        let mut i = Interpreter::new();
        assert!(i.is_native_method("Supply", "tap"));
        assert_eq!(i.resolve_user_method_or_accessor("Supply", "tap"), None);
    }

    /// ADR-0019 E4b step 11 finding: `our $.x` / `my $.x` class-level
    /// attributes (`ClassDef::class_level_attrs`, `t/class-level-attrs.t`)
    /// are a fourth candidate kind, invisible to BOTH
    /// `resolve_user_method_or_accessor` (which only ever consults
    /// `accessor_is_public`, the per-instance attribute table — class-level
    /// attrs get no autogenerated accessor method at all, see
    /// `registration_class_body_attr.rs`'s `SkipTail` return) and
    /// `resolve_sequence` (whose `User` candidates come from
    /// `user_method_overloads`, not `class_level_attrs`, and which has no
    /// notion of accessors at all). `should_bypass_native_fastpath`'s own
    /// `has_class_level_attr(..) && !has_public_accessor(..)` arm is
    /// therefore the ONLY place this is checked; it cannot be retired by
    /// folding into either resolver helper as they exist today.
    #[test]
    fn resolve_user_method_or_accessor_does_not_see_a_class_level_attr() {
        let mut i = Interpreter::new();
        i.run("class Foo { our $.bar = 23; }").unwrap();
        assert!(i.has_class_level_attr("Foo", "bar"));
        assert!(!i.has_public_accessor("Foo", "bar"));
        assert_eq!(i.resolve_user_method_or_accessor("Foo", "bar"), None);
        let chain = vec![crate::type_id::TypeId::intern("Foo")];
        let seq = i.resolve_sequence(
            &chain,
            Symbol::intern("bar"),
            super::resolution_sequence::NativeCallShape::new(0, false),
        );
        assert!(
            seq.candidates.is_empty(),
            "resolve_sequence has no notion of class-level attrs either"
        );
    }

    /// ADR-0019 E4b step 11 finding: the real `should_bypass_native_fastpath`
    /// deliberately never calls `has_public_accessor` for a `Package`
    /// (type-object) receiver — only `has_user_method`/`has_class_level_attr`
    /// apply, since an instance attribute accessor is meaningless on the bare
    /// type. `resolve_user_method_or_accessor`, by contrast, answers
    /// `Some(Accessor)` for ANY class with a public `has $.x`, regardless of
    /// receiver kind — folding it into the Package branch as-is (the way
    /// category 3 already works for Instance) would be a real behavior
    /// change, not just an unverified one. This test exists so a future
    /// change accidentally introducing that fold breaks visibly.
    #[test]
    fn resolve_user_method_or_accessor_would_wrongly_answer_for_a_package_receiver() {
        let mut i = Interpreter::new();
        i.run("class Foo { has $.x; }").unwrap();
        assert!(i.has_public_accessor("Foo", "x"));
        // The real Package-branch check the ADR must preserve as-is:
        assert!(!i.has_user_method("Foo", "x"));
        assert!(!i.has_class_level_attr("Foo", "x"));
        // But the Instance-branch helper disagrees -- proving it is not a
        // safe drop-in for the Package branch:
        assert!(i.resolve_user_method_or_accessor("Foo", "x").is_some());
    }
}
