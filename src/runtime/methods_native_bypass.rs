use super::*;
use crate::symbol::Symbol;
use crate::value::types::is_stash_class_name;

impl Interpreter {
    /// The object passed as the first argument of `target.^meta_method(...)`
    /// to the HOW. The builtin metaclasses are written against the type object,
    /// so an instance is replaced by its class; a user-defined metaclass
    /// (`class MetamodelX::Foo is Metamodel::ClassHOW`) receives the object
    /// itself, as in Rakudo (`$obj.^save` reaches `save($obj)` with the instance).
    // Cost: O(1).
    pub(crate) fn meta_call_type_target(how: &Value, meta_method: &str, target: &Value) -> Value {
        if matches!(
            meta_method,
            "mixin" | "set_name" | "language-revision" | "can"
        ) {
            return target.clone();
        }
        if matches!(
            how.view(),
            ValueView::Instance { ref class_name, .. } if !Self::is_metamodel_how(class_name)
        ) {
            return target.clone();
        }
        match target.view() {
            // One candidate of a role group (`R.^candidates[0]`) stays itself:
            // its index is what tells the candidates apart (#14186).
            ValueView::Instance { attributes, .. }
                if attributes.contains_key("__mutsu_role_candidate_idx") =>
            {
                target.clone()
            }
            ValueView::Instance { class_name, .. } => Value::package(class_name),
            _ => target.clone(),
        }
    }

    /// Check if a metamodel class name is a HOW type.
    pub(super) fn is_metamodel_how(class_name: &Symbol) -> bool {
        let cn = class_name.resolve();
        cn == "Perl6::Metamodel::ClassHOW"
            || cn == "Perl6::Metamodel::GrammarHOW"
            || cn == "Perl6::Metamodel::ModuleHOW"
            || cn == "Perl6::Metamodel::PackageHOW"
            || cn == "Perl6::Metamodel::SubsetHOW"
            || cn == "Perl6::Metamodel::EnumHOW"
            || cn == "Perl6::Metamodel::CurriedRoleHOW"
            || cn == "Perl6::Metamodel::ParametricRoleGroupHOW"
            // One individual `role` declaration, as opposed to the same-named
            // group above (`types/role_candidate.rs`). Answers the same
            // introspection methods.
            || cn == "Perl6::Metamodel::ParametricRoleHOW"
            || cn == "Perl6::Metamodel::ConcreteRoleHOW"
            || cn == "Perl6::Metamodel::CoercionHOW"
            // A definiteness-constrained type object's metaclass (ADR-0069).
            || cn == "Perl6::Metamodel::DefiniteHOW"
    }

    /// Build the argument list for a HOW method called on a metaobject
    /// receiver whose attributes are `attributes`.
    ///
    /// A HOW method takes the introspected object as its first argument
    /// (`$obj.^name` desugars to `$obj.HOW.name($obj)`). Two adjustments are
    /// needed before handing the list to [`Self::dispatch_classhow_method`]:
    ///
    /// * The explicit `Int.HOW.name` form passes NO object at all, so supply
    ///   the type name recorded on the receiving HOW.
    /// * The introspected object may itself be a metaobject
    ///   (`my $m = Str.HOW; $m.can($m, 'uc')` — the worked example in
    ///   `Language/structures.rakudoc`). Rakudo's `Metamodel::MethodContainer`
    ///   methods read `self`'s own cached MRO and method table and ignore
    ///   `$obj`, so a HOW in that slot stands for the type its RECEIVER
    ///   describes, not for the `Perl6::Metamodel::*HOW` class the argument
    ///   literally is. Without this, `$m.can($m, 'uc')` answered `()`,
    ///   `$m.name($m)` answered `Perl6::Metamodel::ClassHOW` and `$m.mro($m)`
    ///   answered the metaclass's own MRO.
    ///
    /// Deliberately narrowed to a *metaobject* argument: an ordinary value
    /// argument still drives dispatch, which is what keeps `(1 but R).^mro[0]`
    /// reporting the mixin type `Int+{R}` rather than the plain `name`
    /// attribute cached on the HOW (see
    /// `news/2026-08/role-instance-how-wrong-metaclass.md`).
    pub(super) fn how_dispatch_args(
        &self,
        attributes: &crate::value::InstanceAttrs,
        method: &str,
        args: &[Value],
    ) -> Vec<Value> {
        let mut how_args = args.to_vec();
        let how_target = match attributes.as_map().get("name").map(|v| v.view()) {
            Some(ValueView::Str(type_name)) => type_name.to_string(),
            _ => return how_args,
        };
        let target_package = || Value::package(Symbol::intern(&how_target));
        // `Metamodel::Documenting.set_why($why)` is the one HOW mutator whose
        // Rakudo signature has NO `$obj` parameter: `C.^set_why($pod)` is an
        // arity error there ("expected 2 arguments but got 3") and
        // `C.HOW.set_why($pod)` is the only spelling. Prepend the receiver's
        // own type so the dispatcher still sees the uniform
        // `(invocant, value)` shape every other mutator arm uses.
        if method == "set_why" {
            how_args.insert(0, target_package());
            return how_args;
        }
        // `H.^mixin(R)` on a metaobject instance `H` is `H.HOW.mixin(H, R)`:
        // the mixin must change `H` itself (and its cached `.HOW` slot), so
        // the instance stays the invocant instead of its metaclass's type.
        if method == "mixin"
            && how_args
                .first()
                .is_some_and(|a| Self::how_target_from_value(a).is_some())
        {
            return how_args;
        }
        match how_args.first().map(Value::view) {
            None => how_args.insert(0, target_package()),
            Some(ValueView::Instance { class_name, .. })
                if Self::is_metamodel_how(&class_name)
                    || self.is_metamodel_how_class(&class_name.resolve()) =>
            {
                how_args[0] = target_package();
            }
            _ => {}
        }
        how_args
    }

    /// Check if a method name is a ClassHOW method: one of the `Metamodel::*HOW`
    /// rows (ADR-11276 slice 3G), or a metamethod of `EnumHOW` /
    /// `DefiniteHOW`, whose own dispatchers answer before the rows.
    ///
    /// `elems` is listed even though it is also an ordinary value method: on a
    /// HOW *instance* the two are distinct, and without it `C.^elems` fell
    /// through to the generic `.elems` handler, which has no implementation
    /// for a HOW and recursed until the stack overflowed.
    // Cost: O(1), a bit-match on the name plus three hash lookups per HOW owner.
    pub(crate) fn is_classhow_method(method: &str) -> bool {
        matches!(
            method,
            "base_type"
                | "definite"
                | "enum_value_list"
                | "enum_values"
                | "enum_from_value"
                | "add_enum_value"
                | "compose_values"
                | "set_export_callback"
                | "is_composed"
                | "elems"
        ) || crate::builtins::method_table::mop_declares(method)
    }

    /// Whether a user method on a bare type object is applicable to this call.
    ///
    /// Most native-shadow checks only need the presence of a user method.  The
    /// representation methods are different: a type object may inherit a
    /// multi `raku`/`gist`/`perl` candidate constrained to `:D`, which is not
    /// applicable to the type object.  Rakudo then uses Mu's representation
    /// instead of turning that inapplicable candidate into a dispatch error.
    /// Keep the ordinary presence check for every other method and for calls
    /// with arguments, where the normal binding error remains observable.
    pub(crate) fn package_has_applicable_user_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> bool {
        let ValueView::Package(class_name) = target.view() else {
            return false;
        };
        let class_name = class_name.resolve();
        if !self.has_user_method(&class_name, method) {
            return false;
        }
        if matches!(method, "gist" | "raku") && args.is_empty() {
            return self
                .resolve_method_with_owner_invocant(&class_name, method, args, target)
                .is_some();
        }
        true
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
    /// [`Self::native_receiver_defers`]: the lazy-Match branch avoids
    /// `target.view()` (it would materialize the lazy value) and instead
    /// inlines the subset of these checks that can ever apply to a Match
    /// receiver.
    fn native_fastpath_receiver_state_guard(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> bool {
        (matches!(
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
            // `.gist`/`.Str`/`.Stringy` render
            // `$exc.message`; the native fast path can only read the stored
            // `message` attribute, which is still undefined when the class
            // computes its message lazily. See the twin gate in
            // `vm_native_dispatch::try_native_method`.
            || (matches!(method, "gist" | "Str" | "Stringy")
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
            || (matches!(target.view(), ValueView::Instance { class_name, .. } if is_stash_class_name(class_name.as_str()))
                && matches!(method, "keys" | "values"))
            || (method == "keys"
                && args.is_empty()
                && (matches!(target.view(), ValueView::Hash(_))
                    || matches!(target.view(), ValueView::Mixin(inner, _) if matches!(inner.as_ref().view(), ValueView::Hash(_)))))
    }

    /// The builtin method names a plain `Any`-derived instance (no `Cool` in
    /// its MRO) cannot resolve, so `handles *` / `FALLBACK` must be given the
    /// chance to intercept them and the receiver-class-blind arity cascades
    /// must not answer them for an instance whose ancestry lacks them.
    /// Derived from the native-method row catalog (ADR-0051 P5), see
    /// [`crate::runtime::any_cool_method_gate::is_cool_only_method`].
    pub(crate) fn cool_only_builtin_method(method: &str) -> bool {
        crate::runtime::any_cool_method_gate::is_cool_only_method(method)
    }

    /// A `Cool`-subtype-only builtin name (`succ`, `base`, `lazy`, ...), see
    /// [`crate::runtime::any_cool_method_gate::is_cool_subtype_only_method`].
    pub(crate) fn cool_subtype_only_builtin_method(method: &str) -> bool {
        crate::runtime::any_cool_method_gate::is_cool_subtype_only_method(method)
    }

    /// Whether `class_name` (or any ancestor in its MRO) declares `handles *`
    /// or a `FALLBACK` method — the two mechanisms that intercept a
    /// `X::Method::NotFound` from normal resolution. Used to gate the native
    /// builtin fast path off a Cool-only method name (`cool_only_builtin_method`)
    /// so the interpreter's wildcard-delegation/`FALLBACK` fallback chain gets
    /// a chance to run instead of the fast path unconditionally stringifying
    /// the receiver.
    pub(crate) fn class_has_wildcard_handles_or_fallback(&mut self, class_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        let has_wildcard = mro.iter().any(|cn| {
            self.registry()
                .classes
                .get(cn.as_str())
                .is_some_and(|cd| !cd.wildcard_handles.is_empty())
        });
        has_wildcard || self.has_user_method(class_name, "FALLBACK")
    }

    /// Whether the class of the buffer `target` (an instance or a type object)
    /// answers `method` itself: a method it or a role it does declares (an
    /// `augment` of `Buf` included), a `handles *` or a `FALLBACK`. Read by the
    /// gate that refuses a `Cool`-only method on a buffer
    /// (`any_cool_method_gate::buffer_refuses_cool_method`, #13313).
    // Cost: O(m), m = classes in the receiver's MRO.
    pub(crate) fn buffer_class_answers(&mut self, target: &Value, method: &str) -> bool {
        let class_name = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
            _ => return false,
        };
        self.has_user_method_including_role(&class_name, method)
            || self.class_has_wildcard_handles_or_fallback(&class_name)
    }

    /// Whether `target`'s own state needs the interpreter for this call, so
    /// the built-in rows and cascades must not answer it (ADR-11276 §2.4's
    /// guard step): the checks both native entries share
    /// ([`Self::native_receiver_guard`]) plus the interpreter entry's own
    /// receiver kinds the receiver-blind cascades misread. Whether a
    /// *declaration* takes the call is the other question,
    /// [`Self::user_shadows_builtin`].
    // Cost: O(1) amortized for most calls (name and tag probes); a memoized
    // MRO probe for the `Cool`-only names.
    pub(super) fn native_receiver_defers(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> bool {
        self.native_receiver_guard(target, method)
            || if target.is_lazy_match_value() {
                // `view()` would materialize it; of the receiver kinds below
                // only this one can apply to a Match.
                method == "elems"
            } else {
                self.native_fastpath_receiver_state_guard(target, method, args)
            }
    }

    /// The receiver guard both native entries (the VM's
    /// `try_native_method_raw` and the interpreter's `call_method_with_values`)
    /// run before a row or cascade answers: a method the instance's class
    /// implements in the interpreter (a NativeCall binding, `IO::Handle.say`),
    /// a computed exception message, or a `Cool` name the receiver's ancestry
    /// does not declare (normal resolution throws `X::Method::NotFound`).
    // Cost: O(1) amortized: name and tag probes, plus memoized MRO probes.
    pub(crate) fn native_receiver_guard(&mut self, target: &Value, method: &str) -> bool {
        // ADR-0051 §3 (#9948): a name only some `Cool` subtypes declare
        // (`succ`, `base`, `lazy`, ...) is answered by receiver-blind cascades
        // that coerce ANY receiver, so a Match, Str, Int, Pair or Complex whose
        // own ancestry lacks it must reach normal resolution.
        if Self::cool_subtype_only_builtin_method(method)
            && !self.e2_native_method_exists(target, crate::symbol::Symbol::intern(method).as_str())
        {
            return true;
        }
        // A lazy Match dispatches under its cursor's class; `view()` would
        // materialize it.
        let (class, instance) = if target.is_lazy_match_value() {
            (crate::symbol::Symbol::intern(target.match_dispatch_class()), false)
        } else if let ValueView::Instance { class_name, .. } = target.view() {
            (class_name, true)
        } else {
            return false;
        };
        let cn = class.as_str();
        // An interpreter-side handler serves the method. Decided before the
        // VM's numeric-bridge probe, which reaches the same answer for such a
        // class only after two `type_matches_value` walks.
        self.is_native_method(cn, method)
            // `.gist`/`.Str` render the exception's message, and the native
            // path can only read the stored `message` attribute: a class that
            // computes its message leaves it undefined until the method runs.
            || (matches!(method, "gist" | "Str" | "Stringy")
                && self.exception_render_needs_interpreter(target, cn))
            // ADR-0051 P4: a `Cool`-only builtin the instance's ancestry does
            // NOT provide (no `Cool`, or for the few types with their own row,
            // e.g. `Instant.Date`, no such row): its cascade arms are
            // receiver-class-blind and would stringify the receiver
            // (`"G()".uc`), where Rakudo throws `X::Method::NotFound`.
            || (instance
                && Self::cool_only_builtin_method(method)
                && !self.e2_native_method_exists(target, crate::symbol::Symbol::intern(method).as_str()))
    }

    /// Check if a Mixin's role mixins define the given method (either a real
    /// declared method, or a public attribute's auto-generated accessor —
    /// `has $.sum` makes `sum` a method-shaped read just as much as an
    /// explicit `method sum {...}` would). Used so that role-method dispatch
    /// on punned role instances takes precedence over the built-in Cool
    /// fallbacks (e.g. `.uc`, or `List.sum` shadowing a mixed-in `$.sum`
    /// attribute — see t/mixin-attribute-shadowed-by-builtin-method.t).
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
            // The candidate the mixin was made from: a role group's plain
            // `R` lacks the methods of its parametric `R[...]` (#14175).
            if self.mixin_role_satisfies(mixins, role_name, |role| {
                role.methods.contains_key(method)
                    || role
                        .attributes
                        .iter()
                        .any(|a| a.is_public && a.name == method)
            }) {
                return true;
            }
        }
        false
    }

    /// Whether `target` is a role mixin over an object whose class declares
    /// `method` itself: `Pointer.new(..) but TypedPointer[uint16]` still has the
    /// class's own `Numeric`/`succ`, which `mixin_role_has_method` (the roles'
    /// methods only) cannot see.
    // Cost: O(d) for the mixin nesting depth d, plus one class-registry probe.
    pub(crate) fn mixin_inner_instance_has_user_method(
        &mut self,
        target: &Value,
        method: &str,
    ) -> bool {
        if !target.is_mixin_value() {
            return false;
        }
        let mut inner = target.clone();
        while let ValueView::Mixin(next, _) = inner.view() {
            let next: Value = (**next).clone();
            inner = next;
        }
        match inner.view() {
            ValueView::Instance { class_name, .. } => {
                self.has_user_method(&class_name.resolve(), method)
            }
            _ => false,
        }
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
        map: &crate::gc::Gc<crate::value::HashData>,
        info: &ContainerTypeInfo,
        itemized: bool,
    ) -> Result<Value, RuntimeError> {
        // A typed hash can be circular (`my %oh{Any}; %oh<a> = %oh`). Join the
        // walk `raku_value` maintains so the loop renders as the `%Hash_<addr>`
        // back-reference instead of recursing until the stack dies.
        use crate::builtins::methods_0arg::raku_repr::{hash_cycle_enter, hash_cycle_exit};
        let ptr = crate::gc::Gc::as_ptr(map) as usize;
        let is_top = match hash_cycle_enter(ptr) {
            Ok(is_top) => is_top,
            Err(name) => return Ok(Value::str(name)),
        };
        let result = self.constrained_hash_raku_body(map, info, itemized);
        let had_cycle = hash_cycle_exit(ptr);
        let Ok(rendered) = result else {
            return result;
        };
        if is_top && had_cycle {
            let base = rendered.to_string_value();
            return Ok(Value::str(format!(
                "((my {}) = {})",
                crate::builtins::methods_0arg::raku_repr::hash_cycle_var_name(ptr),
                base
            )));
        }
        Ok(rendered)
    }

    /// The body of [`Self::dispatch_constrained_hash_raku`], without the
    /// cycle bookkeeping its caller wraps around it.
    fn constrained_hash_raku_body(
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
                    // A promoted element cell must be invisible to `.raku`
                    // (ADR-0045 row 40): render what the element HOLDS, not the
                    // container an element producer (`.values`/`.kv`/...) left
                    // in its place. Without this, calling `%h.values` alone
                    // turned a later `%h.raku` from `1 => "a"` into `1 => a`.
                    let v = &map[*k].deref_container();
                    let typed = map.typed_key(k);
                    match typed.view() {
                        ValueView::Str(s)
                            if crate::builtins::methods_0arg::raku_repr::is_adverbial_pair_key(
                                &s,
                            ) =>
                        {
                            let repr = self
                                .call_method_with_values(v.clone(), "raku", vec![])
                                .map(|r| r.to_string_value())
                                .unwrap_or_else(|_| format!("{:?}", v));
                            crate::builtins::methods_0arg::raku_repr::raku_hash_value_repr(
                                &s, v, &repr,
                            )
                        }
                        _ => {
                            let repr = self
                                .call_method_with_values(v.clone(), "raku", vec![])
                                .map(|r| r.to_string_value())
                                .unwrap_or_else(|_| format!("{:?}", v));
                            format!("{} => {}", self.object_hash_key_raku(&typed), repr)
                        }
                    }
                })
                .collect();
            return Ok(Value::str(itemize_wrap(
                crate::builtins::methods_0arg::raku_repr::raku_map_new(&parts),
            )));
        }
        let parts: Vec<String> = sorted_keys
            .iter()
            .map(|k| {
                // See the `Map` arm above: the promoted element cell is not
                // part of what `.raku` renders.
                let v = &map[*k].deref_container();
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
                        let value_repr = self
                            .call_method_with_values(v.clone(), "raku", vec![])
                            .map(|r| r.to_string_value())
                            .unwrap_or_else(|_| format!("{:?}", v));
                        crate::builtins::methods_0arg::raku_repr::raku_hash_value_repr(
                            &s,
                            v,
                            &value_repr,
                        )
                    }
                    _ => {
                        let value_repr = self
                            .call_method_with_values(v.clone(), "raku", vec![])
                            .map(|r| r.to_string_value())
                            .unwrap_or_else(|_| format!("{:?}", v));
                        format!("{} => {}", self.object_hash_key_raku(&typed), value_repr)
                    }
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
    /// notion of accessors at all). `user_shadows_builtin`'s
    /// `class_scoped_accessor_shadows` term is
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
            super::resolution_sequence::MethodVisibility::Public,
            super::resolution_sequence::RoleFallback::Disabled,
        );
        assert!(
            seq.candidates.is_empty(),
            "resolve_sequence has no notion of class-level attrs either"
        );
    }

    /// ADR-0019 E4b step 11 finding: `user_shadows_builtin`
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
