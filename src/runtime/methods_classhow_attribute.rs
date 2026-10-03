use super::attribute_core_traits::AttrCoreTraitEffects;
use super::registration_class_body::PendingAttrCompose;
use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn collect_attribute_objects(
        &self,
        class_name: &str,
        local_only: bool,
    ) -> Vec<Value> {
        // RakuAST classes live in the native model registry, not the ordinary
        // class registry. Their fields still participate in Attribute
        // introspection so callers can walk the model uniformly.
        if let Some(names) = crate::rakuast::local_attribute_names(class_name) {
            return names
                .iter()
                .map(|name| Self::make_builtin_attribute_object(name, "Mu", class_name))
                .collect();
        }
        // For Attribute itself, return BOOTSTRAPATTR instances for its well-known attributes
        if class_name == "Attribute" && !self.registry().classes.contains_key("Attribute") {
            return Self::make_bootstrapattr_list();
        }
        // Built-in types have no registry entry; serve their modelled
        // attributes (e.g. Rat's $!numerator/$!denominator). They are declared
        // on the leaf type itself, so :local and the MRO walk agree.
        if !self.registry().classes.contains_key(class_name) {
            let builtin =
                crate::builtins::builtin_type_methods::builtin_type_attributes(class_name);
            if !builtin.is_empty() {
                return builtin
                    .iter()
                    .map(|(name, type_name)| {
                        Self::make_builtin_attribute_object(name, type_name, class_name)
                    })
                    .collect();
            }
        }
        if local_only {
            if let Some(class_def) = self.registry().classes.get(class_name) {
                class_def
                    .attributes
                    .iter()
                    .map(|attr| self.make_attribute_object(attr, class_name))
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            let mro = if let Some(class_def) = self.registry().classes.get(class_name)
                && !class_def.mro.is_empty()
            {
                class_def.mro.clone()
            } else {
                [crate::symbol::Symbol::intern(class_name)].into()
            };
            let mut result = Vec::new();
            let mut seen_names = std::collections::HashSet::new();
            for cn in mro.iter().map(|s| s.as_str()) {
                if cn == "Any" || cn == "Mu" {
                    continue;
                }
                if let Some(cd) = self.registry().classes.get(cn) {
                    for attr in &cd.attributes {
                        if seen_names.insert((attr.name.clone(), attr.sigil)) {
                            result.push(self.make_attribute_object(attr, cn));
                        }
                    }
                }
            }
            result
        }
    }

    /// Map an attribute's `is required` state to the value `.required` reports:
    /// `Mu` (not required), `1` (bare `is required`), or the reason string
    /// (`is required("reason")`).
    fn required_meta_value(is_required: &Option<Option<String>>) -> Value {
        match is_required {
            None => Value::package(Symbol::intern("Mu")),
            Some(None) => Value::int(1),
            Some(Some(reason)) => Value::str(reason.clone()),
        }
    }

    /// The storage name a lexical (`my class P`) type is registered under,
    /// when `name` is that type's short name bound in the current scope:
    /// `.type` must be the type object itself (`=== P`, with its attributes),
    /// not a fresh package of the bare spelling (JSON::Unmarshal's
    /// `$attr.type.^attributes` saw an empty list).
    // Cost: O(1), one env probe.
    pub(crate) fn lexical_type_storage_name(&self, name: String) -> String {
        if let Some(bound) = self.env().get(&name)
            && let ValueView::Package(sym) = bound.view()
        {
            let storage = sym.resolve();
            if storage != name && storage.split('\u{0}').next() == Some(name.as_str()) {
                return storage.to_string();
            }
        }
        name
    }

    fn make_attribute_object(&self, attr: &super::ClassAttributeDef, owner: &str) -> Value {
        let attr_name = &attr.name;
        let is_public = attr.is_public;
        let default = &attr.default;
        let is_rw = attr.is_rw;
        let is_required = &attr.is_required;
        let sigil = attr.sigil;
        // A custom trait_mod:<is> was applied to this attribute at class
        // registration: serve the SAME meta-object it mutated (role mixins,
        // values like JSON::Name's `$.json-name`), topped up below with the
        // standard keys the ephemeral trait-time object lacks. Returning a
        // fresh object instead would silently drop the trait's work.
        let stored = self
            .registry()
            .class_attribute_trait_objects
            .get(&(owner.to_string(), attr_name.clone()))
            .cloned();
        let full_name = format!("{}!{}", sigil, attr_name);
        // Resolve a short declared type against the owner's package chain: a
        // nested class (`class META6 { class Support {…}; has Support $.support }`)
        // registers as `META6::Support`, and `.type` must report that resolved
        // type (rakudo does) — JSON::Unmarshal constructs nested typed
        // attributes from it.
        let raw_type_name = attr
            .type_constraint
            .clone()
            .or_else(|| {
                let registry = self.registry();
                let class_def = registry.classes.get(owner)?;
                let collides = class_def
                    .attributes
                    .iter()
                    .any(|other| other.name == *attr_name && other.sigil != sigil);
                (!collides)
                    .then(|| class_def.attribute_types.get(attr_name).cloned())
                    .flatten()
            })
            .map(|t| self.resolve_type_name_for_owner(owner, t))
            .map(|t| self.lexical_type_storage_name(t));
        // For @ sigil, the exposed type is Positional[T]; for % it is Associative[T]
        let type_name = match sigil {
            '@' => {
                if let Some(ref inner) = raw_type_name {
                    format!("Positional[{}]", inner)
                } else {
                    "Positional".to_string()
                }
            }
            '%' => {
                if let Some(ref inner) = raw_type_name {
                    format!("Associative[{}]", inner)
                } else {
                    "Associative".to_string()
                }
            }
            _ => raw_type_name.unwrap_or_else(|| "Mu".to_string()),
        };
        let mut meta = HashMap::new();
        meta.insert("name".to_string(), Value::str(full_name));
        meta.insert(
            "__mutsu_attr_name".to_string(),
            Value::str(attr_name.clone()),
        );
        meta.insert(
            "__mutsu_attr_owner".to_string(),
            Value::str(owner.to_string()),
        );
        meta.insert("is_public".to_string(), Value::truth(is_public));
        meta.insert("is_rw".to_string(), Value::truth(is_rw));
        meta.insert("sigil".to_string(), Value::str(sigil.to_string()));
        meta.insert(
            "type".to_string(),
            Value::package(Symbol::intern(&type_name)),
        );
        meta.insert("has_accessor".to_string(), Value::truth(is_public));
        if let Some(message) = self.class_attribute_deprecated(owner, attr_name) {
            meta.insert("DEPRECATED".to_string(), Value::str(message));
        }
        // `is_built`: whether `.new` initializes the attribute from a named
        // arg — true for public attrs, false for `has $!x` privates, and
        // overridable by the `is built(...)` trait (rakudo; JSON::Marshal's
        // `_marshal` probes it to include `has $!half-priv is built`).
        let is_built = self
            .registry()
            .classes
            .get(owner)
            .and_then(|cd| cd.attribute_built.get(attr_name).copied())
            .unwrap_or(is_public);
        meta.insert("is_built".to_string(), Value::truth(is_built));
        // `is required` introspection: `.required` returns the type object `Mu`
        // when not required, `1` for a bare `is required`, and the reason string
        // for `is required("reason")` (rakudo).
        meta.insert(
            "required".to_string(),
            Self::required_meta_value(is_required),
        );
        if let Some(default_arg) = default {
            meta.insert("__mutsu_has_build".to_string(), Value::TRUE);
            if let Some(v) = default_arg.literal() {
                meta.insert("build".to_string(), v.clone());
            } else {
                // Wrap the default expression in a Sub closure so .build returns Code.
                // A `Compiled` chunk (ADR-0019 D2c-4) carries its own bytecode
                // directly — no AST reconstruction (`.as_expr()`, which panics
                // on `Compiled`) is needed for that case. `is_decl_expr_thunk`
                // tells dispatch this `compiled_code` is a standalone
                // declaration-expression chunk, not closure bytecode — see its
                // doc comment. An `Ast` chunk still wraps the raw expression
                // as a body statement, unchanged.
                let (body, compiled_code, compiled_fns, is_decl_expr_thunk) = match default_arg {
                    crate::opcode::DeclTraitArg::Compiled(chunk) => (
                        Vec::new(),
                        Some(chunk.code.clone()),
                        Some(chunk.fns.clone()),
                        true,
                    ),
                    _ => (
                        vec![crate::ast::Stmt::Expr(default_arg.as_expr())],
                        None,
                        None,
                        false,
                    ),
                };
                let sub_data = crate::value::SubData {
                    package: Symbol::intern("GLOBAL"),
                    name: Symbol::intern("<attribute-build>"),
                    params: crate::value::empty_params(),
                    param_defs: crate::value::empty_param_defs(),
                    body: std::sync::Arc::new(body),
                    is_rw: false,
                    is_raw: false,
                    env: self.env().clone(),
                    assumed_positional: Vec::new(),
                    assumed_named: ValueMap::default(),
                    id: crate::value::next_instance_id(),
                    empty_sig: false,
                    is_bare_block: false,
                    compiled_code,
                    compiled_fns,
                    is_decl_expr_thunk,
                    compiled_routine: None,
                    deprecated_message: None,
                    source_line: None,
                    source_file: None,
                    owned_captures: Vec::new(),
                    authoritative_captures: Vec::new(),
                    upvalues: Vec::new(),
                    captured_fatal_mode: false,
                    param_name_syms_cache: std::sync::OnceLock::new(),
                    source_file_sym_cache: std::sync::OnceLock::new(),
                    state_scope_guard: None,
                    captured_readonly: None,
                };
                meta.insert(
                    "build".to_string(),
                    Value::sub_value(crate::gc::Gc::new(sub_data)),
                );
            }
        }
        match stored {
            Some(stored) => {
                // Top up the trait-time object with the standard keys it lacks
                // (build, is_rw, ...) without overwriting anything the trait
                // set, and return THAT object so its mixins/values survive.
                // The stored value is a Mixin when the trait did `$a does R`;
                // its inner Instance shares the original attr cell.
                let inner = match stored.view() {
                    ValueView::Mixin(inner, _) => inner.as_ref().clone(),
                    _ => stored.clone(),
                };
                if let ValueView::Instance { attributes, .. } = inner.view() {
                    for (k, v) in meta {
                        attributes.insert_if_absent(k, v);
                    }
                }
                stored
            }
            None => super::attribute_identity::attribute_meta_object(
                super::attribute_identity::AttributeIdentity {
                    owner: Symbol::intern(owner),
                    sigil,
                    name: Symbol::intern(attr_name),
                },
                meta,
            ),
        }
    }

    /// Build a minimal Attribute introspection object for a `has` declaration
    /// that is being processed at class-registration time, so it can be passed
    /// to a user-defined `trait_mod:<is>`. Unlike `make_attribute_object`, this
    /// does not require the owning class to already be present in `self.registry().classes`.
    pub(crate) fn make_trait_attribute_object(
        &self,
        attr_name: &str,
        sigil: char,
        is_public: bool,
        owner: &str,
        type_constraint: Option<&str>,
    ) -> Value {
        let full_name = format!("{}!{}", sigil, attr_name);
        // Same owner-chain resolution as `make_attribute_object`: a nested
        // class's short name must resolve to its qualified registration.
        let type_constraint =
            type_constraint.map(|t| self.resolve_type_name_for_owner(owner, t.to_string()));
        let type_name = match sigil {
            '@' => type_constraint
                .map(|t| format!("Positional[{}]", t))
                .unwrap_or_else(|| "Positional".to_string()),
            '%' => type_constraint
                .map(|t| format!("Associative[{}]", t))
                .unwrap_or_else(|| "Associative".to_string()),
            _ => type_constraint.unwrap_or_else(|| "Mu".to_string()),
        };
        let mut meta = HashMap::new();
        meta.insert("name".to_string(), Value::str(full_name));
        meta.insert(
            "__mutsu_attr_name".to_string(),
            Value::str(attr_name.to_string()),
        );
        meta.insert(
            "__mutsu_attr_owner".to_string(),
            Value::str(owner.to_string()),
        );
        meta.insert("is_public".to_string(), Value::truth(is_public));
        meta.insert("has_accessor".to_string(), Value::truth(is_public));
        meta.insert("is_built".to_string(), Value::truth(is_public));
        meta.insert("required".to_string(), Value::package(Symbol::intern("Mu")));
        meta.insert("sigil".to_string(), Value::str(sigil.to_string()));
        meta.insert(
            "type".to_string(),
            Value::package(Symbol::intern(&type_name)),
        );
        Value::make_instance(Symbol::intern("Attribute"), meta)
    }

    /// The packages to dispatch a declaration's `name` trait as, in order: every
    /// package on the `current_package` `::` chain with a local proto or multi
    /// candidates for `name` (innermost first), then the loading module's own
    /// package, then the current package when only GLOBAL has a handler. Empty
    /// when nothing has a handler.
    ///
    /// A package nearer the declaration can hold unrelated candidates of the
    /// same trait (`HTML::Component` holds some, while the `is html-attr` that
    /// `HTML::Component::Tag::META-CHARSET` uses was imported by its module
    /// `HTML::Component::Tag::META`), so the caller tries each in turn until
    /// one dispatches.
    ///
    /// Cost: O(d), d = `::` segments of the current package.
    fn trait_handler_packages(&mut self, name: &str) -> Vec<String> {
        let base_keys = self.fn_keys_for_base(name);
        let current = self.current_package();
        let mut found: Vec<String> = Vec::new();
        let has_local = |this: &Self, pkg: &str| {
            let pkg_sym = Symbol::intern(pkg);
            let proto_key = crate::qualified::qualified(pkg_sym, Symbol::intern(name));
            this.registry().proto_subs_contains(proto_key.as_str())
                || this
                    .registry()
                    .has_multi_function(Some(&base_keys), &[pkg_sym], name)
        };
        // Local candidates only at each level — has_proto/has_multi_candidates
        // also match GLOBAL. The dispatch probes {pkg}:: AND GLOBAL:: anyway,
        // so dispatching as a local package still sees imported candidates.
        let mut pkg = current.as_str();
        loop {
            if has_local(self, pkg) {
                found.push(pkg.to_string());
            }
            match pkg.rsplit_once("::") {
                Some((parent, _)) => pkg = parent,
                None => break,
            }
        }
        // A namespaced module's imports are recorded under the module's own
        // package (`runtime_module_exports.rs`, `module_import_pkg`), which a
        // class it declares need not be nested in: `HTML::Component::Tag::META`
        // declares `class HTML::Component::Tag::META-CHARSET` with the
        // `is html-attr` trait it imported.
        if let Some(unit_pkg) = self.module_load_stack.last().cloned()
            && !found.contains(&unit_pkg)
            && has_local(self, &unit_pkg)
        {
            found.push(unit_pkg);
        }
        if found.is_empty() {
            let global = [Symbol::intern("GLOBAL")];
            if self.registry().has_proto("GLOBAL", name)
                || self
                    .registry()
                    .has_multi_candidates(Some(&base_keys), &global, name)
            {
                found.push(current);
            }
        }
        found
    }

    /// Whether a `does`-mixin value carries a non-private `compose` method on
    /// any role it mixes in — the "does this need a deferred `compose` hook
    /// call" test shared by both the attribute-level and `$class.HOW`-level
    /// paths in [`Interpreter::apply_attribute_traits`].
    pub(crate) fn mixin_has_compose_hook(&self, value: &Value) -> bool {
        match value.view() {
            ValueView::Mixin(_, mixins) => mixins.keys().any(|key| {
                key.strip_prefix("__mutsu_role__").is_some_and(|role_name| {
                    self.role_def_for_mixin_role(mixins, role_name)
                        .is_some_and(|role| {
                            role.methods
                                .get("compose")
                                .is_some_and(|defs| defs.iter().any(|def| !def.is_private))
                        })
                })
            }),
            _ => false,
        }
    }

    /// Dispatch unknown attribute traits to user-defined `trait_mod:<...>` subs,
    /// or raise X::Comp::Trait::Unknown if no handler is registered. Called at
    /// class registration for each `has` declaration that carries unknown traits.
    ///
    /// `pending_composes` collects, for every mixin whose `compose` hook must
    /// fire (see the call site below), which object it must fire on — either
    /// the attribute itself or the composing class's `.HOW` — instead of
    /// invoking it inline: `run_class_body` drains it once the whole class
    /// body has registered (#8845).
    pub(super) fn apply_attribute_traits(
        &mut self,
        decl: &crate::opcode::CompiledAttrDecl,
        attr_name_str: &str,
        owner: &str,
        pending_composes: &mut Vec<PendingAttrCompose>,
    ) -> Result<AttrCoreTraitEffects, RuntimeError> {
        let sigil = decl.sigil;
        let is_public = decl.is_public;
        let type_constraint = decl.type_constraint.as_deref();
        for (kind, trait_name, trait_arg) in &decl.unknown_traits {
            // `has $.x does Foo` — record the role so construction mixes it into
            // the attribute's value (its container does the role). Not a
            // `trait_mod:<does>` dispatch.
            if kind == "does" {
                // A `my role` is registered under its declaration-site storage
                // name (ADR-0047, #9894); resolve the spelling here, where the
                // declaring scope's env binding is still visible.
                let role = self.lexical_env_remap_name(trait_name);
                self.registry_mut()
                    .class_attribute_does_roles
                    .entry((owner.to_string(), attr_name_str.to_string()))
                    .or_default()
                    .push(role);
                continue;
            }
            let trait_mod_name = format!("trait_mod:<{}>", kind);
            // The trait multi may be declared in an ENCLOSING package's body:
            // META6 declares `multi sub trait_mod:<is>(Attribute, Optionality
            // :$specification!)` in `class META6`'s own body and uses it inside
            // nested classes (current_package "META6::Support"). Multi lookup
            // probes current_package + GLOBAL only, so walk up the package
            // chain to the nearest package that has a handler and dispatch as
            // that package.
            let dispatch_pkgs = self.trait_handler_packages(&trait_mod_name);
            let has_handler = !dispatch_pkgs.is_empty();
            if has_handler {
                // Reuse the Attribute meta-object across every trait applied to
                // this attr, and store it in the registry: instance attrs are a
                // shared cell, so whatever the trait sub does to `$a`
                // (`$a does NamedAttribute; $a.json-name = $v` — JSON::Name)
                // lands in this object, and `^attributes` serves it back.
                let attr_key = (owner.to_string(), attr_name_str.to_string());
                let attr_obj = if let Some(existing) = self
                    .registry()
                    .class_attribute_trait_objects
                    .get(&attr_key)
                    .cloned()
                {
                    existing
                } else {
                    let fresh = self.make_trait_attribute_object(
                        attr_name_str,
                        sigil,
                        is_public,
                        owner,
                        type_constraint,
                    );
                    self.registry_mut()
                        .class_attribute_trait_objects
                        .insert(attr_key, fresh.clone());
                    fresh
                };
                let trait_arg_val = if let Some(arg_expr) = trait_arg {
                    Some(self.eval_block_value(&[crate::ast::Stmt::Expr(arg_expr.clone())])?)
                } else {
                    None
                };
                // A parameterized role may bind a value under the same name
                // as an attribute trait (XML::Class[xml-element => ...] with
                // `is xml-element`).  That binding can be a type object due
                // to its declared constraint, but it is still a role argument
                // rather than a trait type and must not turn named dispatch
                // into positional dispatch.
                let mut role_owner = owner;
                let mut is_role_argument = false;
                loop {
                    if self
                        .registry()
                        .class_role_param_bindings
                        .get(role_owner)
                        .is_some_and(|bindings| bindings.contains_key(trait_name))
                    {
                        is_role_argument = true;
                        break;
                    }
                    let Some((outer, _)) = role_owner.rsplit_once("::") else {
                        break;
                    };
                    role_owner = outer;
                }
                let type_obj = (!is_role_argument)
                    .then(|| self.resolve_type_object(trait_name))
                    .flatten();
                let mut args = vec![attr_obj];
                if kind == "will" {
                    // `will name { ... }` passes the block positionally and
                    // the trait name as a named argument:
                    // `trait_mod:<will>($attr, { ... }, :name)`. A `will`
                    // trait without a block keeps the named-only spelling.
                    if let Some(arg_val) = trait_arg_val {
                        args.push(arg_val);
                    }
                    args.push(Value::pair(trait_name.clone(), Value::TRUE));
                } else if let Some(type_val) = type_obj {
                    args.push(type_val);
                    if let Some(arg_val) = trait_arg_val {
                        args.push(arg_val);
                    }
                } else {
                    let named_val =
                        Value::pair(trait_name.clone(), trait_arg_val.unwrap_or(Value::TRUE));
                    args.push(named_val);
                }
                // `$a does SomeRole` inside the trait produces a Mixin wrapper
                // bound only to the trait sub's local `$a` — arm the same
                // writeback DoesVar uses for `&sub` trait targets (see
                // registration_sub.rs) and store the captured Mixin as this
                // attribute's meta-object, so the mixin's methods/values
                // survive to `^attributes` (JSON::Name's `is json-name`).
                let saved_wb_key = self.trait_mod_writeback_key.take();
                self.trait_mod_writeback_key =
                    Some(MetaNs::AttrTrait.owned_key_pair_for_strs(owner, attr_name_str));
                let saved_attr_wb_value = self.trait_mod_attr_writeback_value.take();
                let saved_pkg = self.current_package();
                let mut call_result = Ok(Value::NIL);
                for pkg in &dispatch_pkgs {
                    if *pkg != saved_pkg {
                        self.set_current_package(pkg.clone());
                    }
                    call_result = self.call_function(&trait_mod_name, args.clone());
                    self.set_current_package(saved_pkg.clone());
                    if !matches!(&call_result, Err(err) if Self::is_trait_mod_no_candidate(err)) {
                        break;
                    }
                }
                self.trait_mod_writeback_key = saved_wb_key;
                // The attribute's OWN resulting mixin (from `$attr does
                // SomeRole` specifically, never from a `$class.HOW` mixin
                // elsewhere in the same handler — see
                // `trait_mod_attr_writeback_value`'s doc comment), cached as
                // this attribute's `^attributes` meta-object. Read before the
                // `compose` hook call below, which can itself perform further
                // `does` mixins and would otherwise overwrite this slot.
                let attr_mixin_val = std::mem::replace(
                    &mut self.trait_mod_attr_writeback_value,
                    saved_attr_wb_value,
                );
                // Store the attribute's own mixin BEFORE the `compose` hook
                // below runs, not after: `compose` (AttrX::Lazy's
                // `LazyAttributeContainerHOW.compose`, among others) typically
                // reads `type.^attributes` right away to find attributes that
                // did a role the trait just mixed in (`$attr does
                // LazyAttribute`), and `^attributes` serves this very
                // registry entry (`make_attribute_object`'s `stored` lookup).
                // Storing it only after `compose` returned left `^attributes`
                // seeing the pre-mixin object while `compose` ran, so a
                // `.grep(LazyAttribute)` inside `compose` always came back
                // empty and the lazy accessor was never installed (#8815).
                //
                // It is also, independently, one of the two places a
                // `compose` hook itself can live (Attribute::Lazy's `Builder`
                // role: `$attr does Builder[$block]`, with no `$class.HOW`
                // involved at all -- real Rakudo's `Attribute` has a native
                // no-op `compose(Mu $package)` that every attribute's own
                // mixed-in override polymorphically replaces). Queue it the
                // same deferred way as the `$class.HOW` mixin below, so
                // `run_pending_attr_composes` fires it once the whole class
                // body has registered.
                if let Some(attr_mixin_val) = &attr_mixin_val {
                    self.registry_mut().class_attribute_trait_objects.insert(
                        (owner.to_string(), attr_name_str.to_string()),
                        attr_mixin_val.clone(),
                    );
                    if self.mixin_has_compose_hook(attr_mixin_val) {
                        pending_composes.push(PendingAttrCompose::Attribute(
                            owner.to_string(),
                            attr_name_str.to_string(),
                        ));
                    }
                }
                if let Some(mixin_val) = self.trait_mod_writeback_value.take() {
                    // Attribute traits may compose a role whose `compose`
                    // method edits the declaring class's method table. This
                    // is how AttrX::Lazy installs its lazy accessor: a role is
                    // mixed into `$class.HOW`, then its compose hook is
                    // called with the owning class. `mixin_val` is whichever
                    // `does` ran LAST in the handler, not necessarily the
                    // attribute's own value -- see `attr_mixin_val` above for
                    // that (and for the direct-on-the-attribute mechanism).
                    // Only a `$class.HOW` mixin belongs here: an
                    // attribute-own mixin was already queued above, and
                    // queuing it again here (it can also be `mixin_val` when
                    // no HOW `does` followed it in the same handler) would
                    // fire its `compose` hook twice.
                    if let Some(how_owner) = Self::how_target_from_value(&mixin_val)
                        && self.mixin_has_compose_hook(&mixin_val)
                    {
                        // Do NOT call `compose` here: at this point in the
                        // class-body walk, statements after this `has`
                        // declaration (in particular later `method`
                        // declarations) have not registered yet, so a
                        // `compose` hook that inspects
                        // `type.^private_method_table`/`.^method_table`
                        // (AttrX::Lazy's conflict/existence checks) sees an
                        // incomplete class -- source-order-sensitive, unlike
                        // Rakudo (#8845). Queue the owner instead; the caller
                        // (`run_class_body`) drains this once every
                        // class-body statement has run, re-reading the
                        // owner's current HOW from the registry so a compose
                        // that runs after further mixins still sees the
                        // fully-composed HOW.
                        pending_composes.push(PendingAttrCompose::How(how_owner));
                    }
                }
                // Raku dispatches `trait_mod:<is>` as an ordinary multi: the
                // built-in candidates and any user-declared one (e.g.
                // Test.rakumod's own `trait_mod:<is>(Routine:D $r,
                // :$test-assertion!)`, exported into scope by `use Test`)
                // share one multi, so a user candidate whose signature does
                // not match THIS trait's shape simply does not claim it --
                // dispatch falls through to the unknown-trait diagnosis
                // below, exactly like the sibling variable-trait path
                // (`is_trait_mod_no_candidate`, `vm_var_trait_ops.rs`,
                // `news/2026-08/user-trait-mod-does-not-consume-every-trait.md`).
                // An error raised from *inside* a handler that DID match is a
                // real error and still propagates.
                match call_result {
                    Ok(_) => continue,
                    Err(err) if Self::is_trait_mod_no_candidate(&err) => {}
                    Err(err) => return Err(err),
                }
            }
            let msg = format!(
                "Can't use unknown trait '{}' -> '{}' in an attribute declaration.",
                kind, trait_name
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            attrs.insert("type".to_string(), Value::str(kind.clone()));
            attrs.insert("subtype".to_string(), Value::str(trait_name.clone()));
            attrs.insert("declaring".to_string(), Value::str("attribute".to_string()));
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Comp::Trait::Unknown"),
                attrs,
            )));
            return Err(err);
        }
        // A handler that re-dispatched to CORE's `:rw` / `:built` candidates
        // left the result on the attribute's meta-object.
        Ok(self
            .registry()
            .class_attribute_trait_objects
            .get(&(owner.to_string(), attr_name_str.to_string()))
            .map(|obj| AttrCoreTraitEffects::read(obj, is_public))
            .unwrap_or_default())
    }
}
