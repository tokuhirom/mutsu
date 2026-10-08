use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn classhow_lookup(&mut self, invocant: &Value, method_name: &str) -> Option<Value> {
        self.classhow_lookup_impl(invocant, method_name, true)
            .or_else(|| self.classhow_native_method_fallback(invocant, method_name))
    }

    /// What `.^lookup` / `.^find_method` answer for a method with no user
    /// definition in the MRO: a core class's native method, as the same
    /// callable Method object `.^method_table` hands out for it (not a bare
    /// name string -- `Lock.^find_method('new')(Lock)` has to work), and
    /// otherwise the `Mu.new` every type inherits
    /// (`Supplier.^find_method('new')` is `Mu`'s).
    fn classhow_native_method_fallback(
        &self,
        invocant: &Value,
        method_name: &str,
    ) -> Option<Value> {
        if let ValueView::Package(class_name) = invocant.view() {
            let class_name = class_name.resolve();
            if self
                .registry()
                .classes
                .get(&class_name)
                .is_some_and(|class_def| class_def.native_methods.contains(method_name))
            {
                return Some(self.make_native_method_object(method_name, &class_name));
            }
        }
        (method_name == "new").then(|| self.make_native_method_object("new", "Mu"))
    }

    /// Shared implementation for `.^lookup` (`include_ancestor_submethods =
    /// true`) and `classhow_find_method`'s fallback -- which backs
    /// `.^find_method` directly, and `.can` on a Package receiver indirectly
    /// -- (`include_ancestor_submethods = false`).
    ///
    /// Real Raku's `.^lookup` walks the whole MRO unconditionally, including
    /// finding an ancestor's submethod (confirmed: `class M{submethod
    /// boot{}}; class N is M{}; N.^lookup("boot").defined` is `True`), while
    /// `.^find_method`/`.can` do NOT surface an ancestor's submethod
    /// (`N.^find_method("boot").defined` and `N.can("boot").elems` are both
    /// false/0; only the DECLARING class finds it via either). Rather than
    /// duplicating the whole per-level `Value::make_sub` construction twice,
    /// `classhow_find_method` calls this with `false` instead.
    fn classhow_lookup_impl(
        &mut self,
        invocant: &Value,
        method_name: &str,
        include_ancestor_submethods: bool,
    ) -> Option<Value> {
        // A `does`/`but` mixin (`$obj but R`, `$class.HOW does R`) has no
        // registry entry of its own: a method a mixed-in role declares wins
        // over the wrapped value's, then the wrapped value's own MRO answers
        // (Rakudo: the mixin type's MRO is the role pun followed by the
        // original type's). Keeps `.^find_method`/`.^lookup` in lockstep with
        // `.^can` (`collect_can_methods`) and with dispatch.
        if let ValueView::Mixin(inner, mixins) = invocant.view() {
            for role_name in mixins
                .keys()
                .filter_map(|key| key.strip_prefix("__mutsu_role__"))
            {
                let Some(role) = self.role_def_for_mixin_role(mixins, role_name) else {
                    continue;
                };
                let Some(defs) = role.methods.get(method_name) else {
                    continue;
                };
                let defs: Vec<MethodDef> = defs.iter().filter(|d| !d.is_private).cloned().collect();
                let Some(first) = defs.first() else {
                    continue;
                };
                let has_multi = defs.iter().any(|d| d.is_multi);
                return Some(self.make_method_object_with_owner(
                    method_name,
                    first,
                    has_multi,
                    first.return_type.clone(),
                    has_multi.then_some(defs.as_slice()),
                    Some(role_name),
                ));
            }
            return self.classhow_lookup_impl(inner, method_name, include_ancestor_submethods);
        }
        let (class_name, class_name_str) = match invocant.view() {
            ValueView::Package(name) => (name, name.resolve()),
            // An instance of a user class carries its class name; `value_type_name` would
            // flatten it to "Any" and lose the registry entry, so `$obj.^lookup('m')` found
            // nothing while `Class.^lookup('m')` did.
            ValueView::Instance { class_name, .. } => (class_name, class_name.resolve()),
            _ => {
                // For concrete values, derive the type name via the classifier.
                let type_name = self.dispatch_owner_name(invocant).to_string();
                (Symbol::intern(&type_name), type_name)
            }
        };
        // RakuAST model classes are native type objects with no registry entry,
        // so the MRO walk below finds nothing for them. Answer from the same
        // model metadata `.^methods` and `.^can` use, keeping the three in
        // lockstep — `RakuAST::IntLiteral.^lookup("value")` returned `(Mu)`
        // while `.^can("value")` found it.
        if let Some(names) = crate::rakuast::inherited_method_names(&class_name_str) {
            return names.contains(&method_name).then(|| {
                Value::routine_parts(
                    Symbol::intern(&class_name_str),
                    Symbol::intern(method_name),
                    false,
                )
            });
        }
        // Check user-defined class methods first, walking the receiver's full
        // MRO rather than only its own class — `B.^lookup('foo')` must find a
        // `foo` declared only on an ancestor `A` (`class B is A {}`), exactly
        // as real Raku does. The per-level construction logic below
        // (has_multi/is_submethod/return type/...) is unchanged from before
        // this fix; only the search now spans the whole chain instead of
        // stopping at the receiver's own class. Matches the `owner`-not-
        // `class_name` convention `classhow_lookup_all_candidates` below
        // already uses for an inherited method's declaring class.
        let mro = self.class_mro(&class_name_str);
        for owner_sym in mro.iter() {
            let owner_str = owner_sym.as_str();
            let defs_all: Vec<MethodDef> = {
                let registry = self.registry();
                let usable = registry
                    .user_method_overloads(owner_str, method_name)
                    // `.^lookup`/`.^find_method` never surface a private method by
                    // its bare (unqualified, no `!`) name -- real Raku answers
                    // `Nil` even from inside the declaring class itself. Every
                    // other visibility-aware dispatch path already skips
                    // `is_private` defs (`resolve_method_with_owner_impl`'s
                    // `Public` filtering, `resolve_sequence`'s
                    // `MethodVisibility::Public` tier); this lookup's `defs.first()`
                    // did not.
                    .map(|defs| {
                        defs.into_iter()
                            .filter(|d| !d.is_private)
                            .collect::<Vec<MethodDef>>()
                    })
                    .filter(|defs| {
                        defs.first().is_some_and(|first| {
                            // A submethod (`is_my`) is visible only at its own
                            // declaring level for `.^find_method`/`.can`
                            // (`include_ancestor_submethods = false`) --
                            // `.^lookup` has no such restriction.
                            !(first.is_my
                                && owner_str != class_name_str
                                && !include_ancestor_submethods)
                        })
                    });
                if let Some(defs) = usable {
                    defs
                } else if let Some(proto) = registry.method_entry_proto(owner_str, method_name) {
                    // A `proto method`/`proto submethod` with no candidates yet
                    // (`proto method bar {*}` alone) has no `user_method_overloads`
                    // row at all, so it was invisible to `.^lookup`/`.^find_method`/
                    // `.can` even though real Raku reports it as a defined method —
                    // see news/2026-08/proto-method-visible-to-find-method.md. Build
                    // the same synthetic `MethodDef` shape `run_proto_method`
                    // (`dispatch_proto.rs`) already uses to actually DISPATCH a
                    // proto, so introspection and dispatch agree on what the proto
                    // looks like as a method.
                    vec![MethodDef {
                        syms: Default::default(),
                        lexical_package: proto.package,
                        params: proto.params.clone(),
                        param_defs: proto.param_defs.clone(),
                        body: std::sync::Arc::new(proto.body.clone()),
                        is_rw: false,
                        is_raw: false,
                        is_private: false,
                        is_multi: false,
                        is_my: false,
                        role_origin: None,
                        original_role: None,
                        return_type: None,
                        compiled_code: None,
                        compiled_fns: None,
                        delegation: None,
                        is_default: false,
                        deprecated_message: None,
                        is_submethod: false,
                        is_hidden_from_backtrace: false,
                        captured_env: None,
                        source_file: proto.source_file,
                        role_param_bindings: None,
                        nested_capture_index: None,
                        captured_readonly: None,
                        routine_cell: Default::default(),
                    }]
                } else {
                    continue;
                }
            };
            let first = &defs_all[0];
            let has_multi = defs_all.iter().any(|d| d.is_multi);
            // ADR-0019 Phase F box F1: return the same `Method`/`Submethod`
            // `Instance` shape `.^methods`/`.^method_table` build, instead of
            // an ad hoc `Sub` -- see `todo/tickets/classhow-lookup-returns-
            // sub-not-method-instance.md`. `__mutsu_method_callable`
            // (attached for a non-dispatcher) is what `CALL-ME` runs; a multi
            // dispatcher has none and re-dispatches on the first argument
            // instead.
            if has_multi {
                // A multi family can combine candidates declared across
                // several classes in the MRO (roast S06-advanced/wrap.t:
                // `C1`/`C2` each contribute `bar` candidates) -- unlike
                // `.^methods`, whose `.^method_table` view is intentionally
                // per-class-only, `.^lookup`'s dispatcher must expose the
                // FULL combined family, matching the old Sub-shaped
                // dispatcher's `classhow_lookup_all_candidates` walk.
                let candidates =
                    self.classhow_lookup_all_candidates(&class_name_str, method_name, class_name);
                return Some(self.make_method_object_with_owner_ex(
                    method_name,
                    first,
                    true,
                    first.return_type.clone(),
                    None,
                    Some(owner_str),
                    false,
                    Some(candidates),
                    0,
                ));
            }
            return Some(self.make_method_object_with_owner(
                method_name,
                first,
                false,
                first.return_type.clone(),
                None,
                Some(owner_str),
            ));
        }
        // Check role methods
        if let Some(role_def) = self.registry().roles.get(&class_name_str)
            && let Some(defs) = role_def.methods.get(method_name)
            && !defs.is_empty()
        {
            let defs_all = defs.clone();
            let first = &defs_all[0];
            let has_multi = defs_all.iter().any(|d| d.is_multi);
            return Some(self.make_method_object_with_owner(
                method_name,
                first,
                has_multi,
                first.return_type.clone(),
                if has_multi { Some(&defs_all) } else { None },
                Some(&class_name_str),
            ));
        }
        // Check auto-generated accessor methods for public attributes (has $.x, has $.x is rw).
        // These are not stored in class_def.methods but are generated on-the-fly.
        // ClassAttributeDef fields: name, is_public, default, is_rw, is_required, sigil, where_constraint
        if let Some(class_def) = self.registry().classes.get(&class_name_str) {
            for attr in &class_def.attributes {
                let (attr_name, is_public, is_rw) = (&attr.name, attr.is_public, attr.is_rw);
                if is_public && attr_name == method_name {
                    let (source_line, source_file) = (attr.source_line, attr.source_file);
                    return Some(self.instance_accessor_method_object(
                        method_name,
                        &class_name_str,
                        is_rw,
                        source_line,
                        source_file.map(|f| f.as_str()),
                    ));
                }
            }
        }
        // Class-level attributes (`my $.x` / `our $.x`) get an auto-generated
        // accessor too, but they are never `class_def.attributes` entries
        // (real Raku: `Foo.^attributes` stays empty for one of these — the
        // declaration is a class-scoped lexical, not an instance attribute;
        // see the matching comment in `collect_class_methods`). Unlike the
        // instance-attribute loop above, `has_class_level_attr` already walks
        // the MRO on its own, so this checks the receiver's class directly
        // rather than needing a second per-level pass. `Foo.counter = 99`
        // works even though `rw` is `False` here — assignment is handled by a
        // dedicated write path (`set_class_level_attr`), not by the
        // accessor's own `rw`-ness, matching real Raku (`.^lookup('counter')
        // .rw` is also `False`).
        if self.has_class_level_attr(&class_name_str, method_name) {
            return Some(self.accessor_method_object(
                method_name,
                &class_name_str,
                false,
                None,
                None,
                false,
            ));
        }
        // Check grammar token/rule/regex definitions -- walks the MRO like the
        // class-methods loop above, so a `token`/`rule` declared only on an
        // ancestor grammar (`grammar Child is Parent {}`) is found too,
        // exactly as real Raku's `.^lookup` finds an inherited method.
        // Without this, `Child.^lookup("inherited-tok")` answered `(Mu)`
        // even though `Child.parse(..., :rule<inherited-tok>)` dispatches to
        // it correctly -- a pre-existing MRO gap in `.^lookup` alone, not in
        // token dispatch.
        for owner_sym in mro.iter() {
            let owner_str = owner_sym.as_str();
            let token_key = crate::qualified::qualified_text(owner_str, method_name)
                .as_str()
                .to_string();
            if let Some(defs) = self.registry().token_defs.get(&Symbol::intern(&token_key))
                && let Some(first) = defs.first()
            {
                // `Code.line`/`Code.file`: report the first-declared
                // candidate's location, mirroring how a multi `sub`/`method`
                // dispatcher reports its first candidate's line (there is no
                // separate dispatcher-level declaration site to point at
                // instead). `owner_str` (the DECLARING grammar), not
                // `class_name_str` (the receiver), both for this and for the
                // reported `.package` -- matching a method's own "declaring
                // class wins" rule, and required for the returned callable
                // to actually resolve back to `Registry::token_defs`.
                return Some(self.make_native_method_object_ex_loc(
                    method_name,
                    owner_str,
                    true,
                    first.source_line,
                    first.source_file.clone(),
                    Some(&first.param_defs),
                ));
            }
            // A `proto token p {*}` has no `token_defs` entry of its own (only
            // its `:sym<..>` candidates do), but it is still a method of the
            // grammar that `.wrap` can target.
            if self.registry().proto_tokens.contains(&token_key) {
                return Some(self.make_native_method_object_ex_loc(
                    method_name,
                    owner_str,
                    true,
                    None,
                    None,
                    None,
                ));
            }
        }
        // Check built-in type methods — return a native Method Instance
        // (`__mutsu_method_callable` carries the Routine marker the runtime
        // dispatches when called).
        if self.is_builtin_type_method(&class_name_str, method_name) {
            return Some(self.make_native_method_object(method_name, &class_name_str));
        }
        // A built-in type outside the introspected owner set (`DateTime`,
        // `Date`, ...) still declares its methods in the native row table,
        // which is what `.^can` consults per MRO level. Without this,
        // `DateTime.^find_method('timezone')` answered `(Mu)` while
        // `DateTime.^can('timezone')` found it, so `.wrap` on it died
        // (DateTime::Timezones).
        for level in mro.iter() {
            if crate::builtins::native_method_row::native_method_declared(
                level.as_str(),
                method_name,
            ) {
                return Some(self.make_native_method_object(method_name, level.as_str()));
            }
        }
        // The NQP cursor protocol (#7883): `Match.^lookup("!cursor_init")`.
        // A leading `!` is an ordinary identifier character in NQP, not Raku's
        // private-method marker, and rakudo answers these from `.^lookup` /
        // `.^find_method` / `.^methods` like any other method -- so they are
        // their own named source here rather than a `Match` entry in the
        // builtin catalog (`Match`'s catalog row carries no methods at all).
        if crate::runtime::regex::regex_cursor::is_cursor_protocol_method(method_name)
            && (class_name_str == "Match" || mro.iter().any(|c| c.as_str() == "Match"))
        {
            return Some(self.make_native_method_object(method_name, &class_name_str));
        }
        // A class that inherits a built-in scalar type (`class CM is Str
        // {}`) inherits its `.COERCE` from that parent -- checked last, since
        // any user-declared `COERCE` was already found by the MRO walk
        // above. `CM.^lookup('COERCE').package.^name` answers `Str`, exactly
        // as rakudo's does, rather than `Nil` for a class with no COERCE of
        // its own. See `types::native_backed_class` and #8856.
        if method_name == "COERCE"
            && let Some(builtin) = self.native_scalar_backing_parent(&class_name_str)
        {
            return Some(Value::routine_parts(
                Symbol::intern(builtin),
                Symbol::intern("COERCE"),
                false,
            ));
        }
        None
    }

    /// The Method `Instance` for the auto-generated accessor of `owner`'s public
    /// instance attribute `name` — what `.^lookup`, `.^find_method` and
    /// `.^can` all hand out for it, so a `.wrap` on any of them registers the
    /// same wrap chain.
    pub(super) fn instance_accessor_method_object(
        &self,
        name: &str,
        owner: &str,
        is_rw: bool,
        source_line: Option<i64>,
        source_file: Option<&str>,
    ) -> Value {
        self.accessor_method_object(name, owner, is_rw, source_line, source_file, true)
    }

    /// A Method `Instance` for an auto-generated attribute accessor
    /// (`has $.x`, `my $.x`). There is no `MethodDef` for these (the accessor
    /// is synthesized, not declared), so this is the native-method shape
    /// (`make_native_method_object_ex_loc`): its `__mutsu_method_callable` is
    /// the `Routine` carrier that dispatches the accessor on the invocant when
    /// the object is invoked (`D.^lookup('x')($d)`), exactly like the object
    /// `.^methods` hands out. An empty-bodied `Sub` stood here before and
    /// every direct invocation answered `Nil` (#10220).
    ///
    /// `source_line`/`source_file` are the attribute's own `has`-declaration
    /// site (`ClassAttributeDef::source_line`/`source_file`, threaded from
    /// `CompiledAttrDecl::decl_line` -- see `Compiler::compile_class_attr_decls`),
    /// mirroring how `make_method_object_with_owner_ex` reports `Code.line`/
    /// `.file` for a user-declared method. `None` (reported as `Nil`, never a
    /// fabricated location) for a class-level `our`/`my` attribute.
    ///
    /// `wrap_identity`: an instance attribute's accessor is candidate slot
    /// zero of the method-wrap registry, exactly like the accessor objects
    /// `.^method_table` hands out, so `.^find_method('x').wrap(...)` installs
    /// a wrapper that later reads and assignments run (Object::Permission's
    /// `is authorised-by` wraps an attribute's accessor from its `compose`).
    /// A class-level `my $.x` accessor has no dispatch that consults that
    /// registry, so it gets no wrap identity and `.wrap` on it stays
    /// unsupported rather than silently ineffective.
    pub(super) fn accessor_method_object(
        &self,
        name: &str,
        owner: &str,
        is_rw: bool,
        source_line: Option<i64>,
        source_file: Option<&str>,
        wrap_identity: bool,
    ) -> Value {
        let obj = self.make_native_method_object_ex_loc(
            name,
            owner,
            false,
            source_line,
            source_file.map(str::to_string),
            None,
        );
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = obj.view()
        else {
            return obj;
        };
        let mut attrs = attributes.as_map().clone();
        attrs.insert("rw".to_string(), Value::truth(is_rw));
        attrs.insert("readonly".to_string(), Value::truth(!is_rw));
        // An accessor takes only its invocant (`(Owner:D $:: *%_)` in
        // Rakudo), so `.count`/`.arity` are 1. The native-method fallback's
        // raw capture made `.count` Inf, and Template::Jinja2's host-object
        // attribute lookup (`$obj.can($attr)[0].count <= 1`) refused every
        // accessor.
        attrs.insert(
            "signature".to_string(),
            crate::value::signature::make_signature_value(
                crate::value::signature::param_defs_to_sig_info(
                    &[
                        Self::make_invocant_param(&format!("{owner}:D")),
                        Self::implicit_named_slurpy_param(),
                    ],
                    None,
                ),
                Some(self),
            ),
        );
        if !wrap_identity {
            attrs.remove("__mutsu_lookup_class");
            attrs.remove("__mutsu_lookup_method");
            attrs.remove("__mutsu_lookup_candidate_idx");
        }
        Value::make_instance(class_name, attrs)
    }

    /// Build the callable `Sub` value for a single (non-dispatcher) method
    /// candidate -- shared by `.^lookup`/`.^find_method`
    /// (`classhow_lookup_impl`) and the Method `Instance` object's
    /// `__mutsu_method_callable` payload (`make_method_object_with_owner`),
    /// so invoking either shape runs the exact same body/params/env. This is
    /// the ADR-0019 Phase F box F1 Sub-vs-Instance unification: the Instance
    /// carries introspection attributes generically, while this attached Sub
    /// is what `CALL-ME` actually runs.
    pub(super) fn method_def_callable(owner: &str, name: &str, def: &MethodDef) -> Value {
        let has_explicit_invocant = def
            .param_defs
            .iter()
            .any(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"));
        let mut full_param_defs = Vec::with_capacity(def.param_defs.len() + 1);
        if !has_explicit_invocant {
            full_param_defs.push(Self::make_invocant_param(owner));
        }
        full_param_defs.extend(def.param_defs.iter().cloned());
        let mut env = crate::env::Env::new();
        let callable_type = if def.is_submethod {
            "Submethod"
        } else {
            "Method"
        };
        env.insert(
            "__mutsu_callable_type".to_string(),
            Value::str_from(callable_type),
        );
        if let Some(rt) = &def.return_type {
            env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
        }
        Value::make_sub(
            Symbol::intern(owner),
            Symbol::intern(name),
            def.params.clone(),
            full_param_defs,
            (*def.body).clone(),
            def.is_rw,
            env,
        )
    }

    /// The implicit `*%_` every method carries.
    pub(super) fn implicit_named_slurpy_param() -> crate::ast::ParamDef {
        crate::ast::ParamDef {
            name: "%_".to_string(),
            multi_invocant: true,
            slurpy: true,
            is_invocant: false,
            type_constraint: None,
            ..Self::make_invocant_param("")
        }
    }

    pub(super) fn make_invocant_param(class_name: &str) -> crate::ast::ParamDef {
        crate::ast::ParamDef {
            type_capture: None,
            name: String::new(),
            default: None,
            multi_invocant: true,
            required: false,
            named: false,
            named_alias: false,
            slurpy: false,
            double_slurpy: false,
            onearg: false,
            sigilless: false,
            type_constraint: Some(class_name.to_string()),
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
            code: Default::default(),
            trait_args: Vec::new(),
        }
    }

    /// Return all multi method candidates for a class method as Sub values.
    ///
    /// A *multi* method dispatched from `class_name` combines candidates across
    /// its inheritance chain: the list reported by `^find_method(name).candidates`
    /// includes every multi candidate defined in `class_name` and its ancestors,
    /// ordered base-class-first (e.g. for `class C2 is C1`, C1's candidates
    /// precede C2's). Each candidate carries its *owner* class and the index
    /// within that owner's own candidate list, so `.wrap()` keys into the same
    /// `(class, method, idx)` slot the dispatcher consults when it reaches that
    /// candidate during the `callsame`/`nextsame` MRO walk (S06-advanced/wrap.t
    /// GH#2178).
    ///
    /// A class only contributes when *its own* `method_name` is `multi`: a
    /// non-multi (single) method in a parent shadows the family rather than
    /// joining it, so it is not a candidate (S06-advanced/dispatching.t). When
    /// the resolved method is itself non-multi, only that single method is
    /// returned without any MRO combination.
    pub(super) fn classhow_lookup_all_candidates(
        &self,
        class_name: &str,
        method_name: &str,
        _package: crate::symbol::Symbol,
    ) -> Vec<Value> {
        // No user-code re-entry below (pure Value construction), so a let-bound
        // guard is safe.
        let registry = self.registry();

        let class_method_is_multi = |cls: &str| -> bool {
            registry
                .user_method_overloads(cls, method_name)
                .is_some_and(|defs| defs.iter().any(|d| d.is_multi))
        };

        let mut stack = Vec::new();
        let mro = registry
            .compute_class_mro(class_name, &mut stack)
            .unwrap_or_else(|_| vec![class_name.to_string()]);

        // The class that actually resolves `method_name` may be an ancestor,
        // not `class_name` itself (`class B is A {}` inheriting `A`'s
        // `method`/`multi method foo`) — walk the MRO (most-derived first)
        // for the first level with an own def, mirroring the MRO walk
        // `classhow_lookup` does for its own non-multi tier. Multi-ness must
        // be decided from THIS owning level, not the receiver's own class:
        // checking `class_name` directly missed an inherited multi family
        // entirely when the receiver has no own override.
        let owning_class = mro
            .iter()
            .find(|cls| registry.user_method_overloads(cls, method_name).is_some())
            .cloned()
            .unwrap_or_else(|| class_name.to_string());

        // Build the owner list base-class-first. A non-multi resolved method
        // does not combine across the MRO — it is its own sole candidate.
        let owners: Vec<String> = if class_method_is_multi(&owning_class) {
            let mut mro_base_first = mro;
            mro_base_first.reverse();
            // Only classes whose own `method_name` is multi join the family.
            mro_base_first
                .into_iter()
                .filter(|owner| class_method_is_multi(owner))
                .collect()
        } else {
            vec![owning_class]
        };

        let mut out = Vec::new();
        for owner in &owners {
            let Some(defs) = registry.user_method_overloads(owner, method_name) else {
                continue;
            };
            for (idx, def) in defs.iter().enumerate() {
                // ADR-0019 Phase F box F1: build the same Method `Instance`
                // shape `.^methods`'s own multi-candidate builder does,
                // instead of an ad hoc `Sub` -- see `todo/tickets/classhow-
                // lookup-returns-sub-not-method-instance.md`. The explicit
                // `idx` (not the single-owner builder's hardcoded 0) keeps
                // `.wrap()` on `.candidates[N]` targeting the right
                // `(class, method, idx)` slot when a class declares more
                // than one candidate of its own (roast S06-advanced/wrap.t).
                out.push(self.make_method_object_with_owner_ex(
                    method_name,
                    def,
                    false,
                    def.return_type.clone(),
                    None,
                    Some(owner),
                    true,
                    None,
                    idx,
                ));
            }
        }
        out
    }

    /// Check if a method name belongs to a built-in type (Str, Int, etc.)
    /// by checking the builtin method rows of the type and its ancestors.
    pub(super) fn is_builtin_type_method(&self, type_name: &str, method_name: &str) -> bool {
        let ancestors = self.builtin_method_ancestors(type_name);
        for tn in &ancestors {
            let mut methods = Vec::new();
            self.collect_builtin_type_methods(tn, &mut methods);
            if methods.iter().any(|m| m.to_string_value() == method_name) {
                return true;
            }
        }
        false
    }

    /// The ancestors whose builtin method rows `type_name` inherits, in MRO
    /// order -- the chain `.^mro` reports (ADR-0051, #10132). The registry's
    /// cached MRO of a bootstrap class can stop at the class itself
    /// (`Promise` caches `[Promise]`), and a registered class whose MRO is not
    /// computed yet has none at all, so this walks the parents, splices in
    /// the builtin type catalog's chain for every catalog type it meets, and
    /// ends in `Any`/`Mu` like `classhow_mro_names`. It never guesses `Cool`:
    /// a class is `Cool` only when that chain says so. `Pair`'s real MRO is
    /// `[Pair, Any, Mu]`, and probing `Cool`'s rows for it made
    /// `Pair.^can($any_cool_coercion_method)` a false positive (ADR-0019
    /// Phase F box F3 step 2, `t/native-int-coerce-methods-are-cool-only.t`'s
    /// "Pair cannot int8" pin).
    // Cost: O(d * m), d = ancestors, m = MRO length of a catalog ancestor.
    fn builtin_method_ancestors(&self, type_name: &str) -> Vec<String> {
        use crate::builtin_types::catalog::builtin_type_info;
        let own: Vec<String> = match builtin_type_info(type_name) {
            Some(info) => info.mro.iter().map(|s| s.to_string()).collect(),
            None => self.mro_readonly(type_name),
        };
        let mut chain: Vec<String> = Vec::with_capacity(own.len() + 2);
        for name in own {
            let catalog = builtin_type_info(&name);
            if !chain.contains(&name) {
                chain.push(name);
            }
            for ancestor in catalog.map_or(&[][..], |info| info.mro) {
                if !chain.iter().any(|n| n == ancestor) {
                    chain.push(ancestor.to_string());
                }
            }
        }
        if !chain.iter().any(|n| n == "Mu") {
            for tail in ["Any", "Mu"] {
                if !chain.iter().any(|n| n == tail) {
                    chain.push(tail.to_string());
                }
            }
        }
        chain
    }

    pub(super) fn classhow_find_method(
        &mut self,
        invocant: &Value,
        method_name: &str,
    ) -> Option<Value> {
        // `false`: `.^find_method` (and `.can` on a Package receiver, which
        // routes through this function) is stricter than `.^lookup` about
        // ancestor submethods -- see `classhow_lookup_impl`'s doc comment.
        if let Some(value) = self.classhow_lookup_impl(invocant, method_name, false) {
            return Some(value);
        }
        if matches!(
            method_name,
            "name"
                | "shortname"
                | "array_type"
                | "ver"
                | "auth"
                | "api"
                | "mro"
                | "mro_unhidden"
                | "archetypes"
                | "isa"
                | "can"
                | "does"
                | "declares_method"
                | "lookup"
                | "find_method"
                | "add_attribute"
                | "add_method"
                | "add_multi_method"
                | "add_fallback"
                | "set_body_block"
                | "compose"
                | "methods"
                | "method_names"
                | "attributes"
                | "parents"
                | "roles"
                | "concretization"
                | "curried_role"
                | "pun"
                | "language-revision"
                | "method_table"
                | "private_method_table"
                | "private_methods"
                | "submethod_table"
        ) {
            // A real `Method` object in Rakudo (`Routine`-typed parameters, e.g.
            // JSON::RPC's `!validate_params(Routine $method, ...)`, accept it).
            return Some(Value::routine_parts(
                Symbol::intern("Mu"),
                Symbol::intern(method_name),
                false,
            ));
        }
        // CREATE is a built-in method on all types
        if method_name == "CREATE" {
            return Some(Value::routine_parts(
                Symbol::intern("Mu"),
                Symbol::intern("CREATE"),
                false,
            ));
        }
        self.classhow_native_method_fallback(invocant, method_name)
    }
}
