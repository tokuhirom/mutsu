use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn collect_attribute_objects(
        &self,
        class_name: &str,
        local_only: bool,
    ) -> Vec<Value> {
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
                        if seen_names.insert(attr.0.clone()) {
                            result.push(self.make_attribute_object(attr, cn));
                        }
                    }
                }
            }
            result
        }
    }

    /// Create a list of BOOTSTRAPATTR instances for the Attribute class's own attributes.
    fn make_bootstrapattr_list() -> Vec<Value> {
        // Raku's Attribute class has these well-known attributes (in order).
        // We model a subset that matches what the roast tests check.
        let bootstrapattrs: &[(&str, &str)] = &[
            ("name", "str"),
            ("type", "Mu"),
            ("build", "Mu"),
            ("package", "Mu"),
            ("inlined", "int"),
            ("has_accessor", "int"),
            ("rw", "int"),
            ("is_built", "int"),
            ("is_bound", "int"),
            ("required", "Mu"),
            ("container_descriptor", "Mu"),
            ("auto_viv_container", "Mu"),
            ("positional_delegate", "int"),
            ("associative_delegate", "int"),
            ("why", "Mu"),
            ("container_initializer", "Mu"),
            ("original", "Mu"),
            ("compose_order", "int"),
            ("composed", "int"),
            ("is_required", "int"),
            ("dimensions", "Mu"),
        ];
        bootstrapattrs
            .iter()
            .map(|(attr_name, type_name)| {
                let mut meta = HashMap::new();
                meta.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                meta.insert(
                    "type".to_string(),
                    Value::package(Symbol::intern(type_name)),
                );
                meta.insert(
                    "__mutsu_attr_name".to_string(),
                    Value::str(attr_name.to_string()),
                );
                meta.insert("__mutsu_is_bootstrapattr".to_string(), Value::TRUE);
                Value::make_instance(Symbol::intern("Attribute"), meta)
            })
            .collect()
    }

    /// Build an Attribute introspection object for a modelled built-in type
    /// attribute (private `$!name`, no accessor, read-only).
    fn make_builtin_attribute_object(attr_name: &str, type_name: &str, owner: &str) -> Value {
        let has_accessor =
            crate::builtins::builtin_type_methods::builtin_type_attr_has_accessor(owner, attr_name);
        let mut meta = HashMap::new();
        meta.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
        meta.insert(
            "__mutsu_attr_name".to_string(),
            Value::str(attr_name.to_string()),
        );
        meta.insert(
            "__mutsu_attr_owner".to_string(),
            Value::str(owner.to_string()),
        );
        meta.insert("is_public".to_string(), Value::truth(has_accessor));
        meta.insert("is_rw".to_string(), Value::FALSE);
        meta.insert("sigil".to_string(), Value::str("$".to_string()));
        meta.insert(
            "type".to_string(),
            Value::package(Symbol::intern(type_name)),
        );
        meta.insert("has_accessor".to_string(), Value::truth(has_accessor));
        Value::make_instance(Symbol::intern("Attribute"), meta)
    }

    fn make_attribute_object(&self, attr: &super::ClassAttributeDef, owner: &str) -> Value {
        let (ref attr_name, is_public, ref default, is_rw, _, sigil, _) = *attr;
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
        let raw_type_name = self
            .registry()
            .classes
            .get(owner)
            .and_then(|cd| cd.attribute_types.get(attr_name))
            .cloned();
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
        if let Some(default_expr) = default {
            meta.insert("__mutsu_has_build".to_string(), Value::TRUE);
            if let crate::ast::Expr::Literal(v) = default_expr {
                meta.insert("build".to_string(), v.clone());
            } else {
                // Wrap the default expression in a Sub closure so .build returns Code
                let sub_data = crate::value::SubData {
                    package: Symbol::intern("GLOBAL"),
                    name: Symbol::intern("<attribute-build>"),
                    params: Vec::new(),
                    param_defs: Vec::new(),
                    body: vec![crate::ast::Stmt::Expr(default_expr.clone())],
                    is_rw: false,
                    is_raw: false,
                    env: self.env().clone(),
                    assumed_positional: Vec::new(),
                    assumed_named: HashMap::new(),
                    id: crate::value::next_instance_id(),
                    empty_sig: false,
                    is_bare_block: false,
                    compiled_code: None,
                    deprecated_message: None,
                    source_line: None,
                    source_file: None,
                    owned_captures: Vec::new(),
                    authoritative_captures: Vec::new(),
                    upvalues: Vec::new(),
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
            None => Value::make_instance(Symbol::intern("Attribute"), meta),
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
        let type_name = match sigil {
            '@' => type_constraint
                .map(|t| format!("Positional[{}]", t))
                .unwrap_or_else(|| "Positional".to_string()),
            '%' => type_constraint
                .map(|t| format!("Associative[{}]", t))
                .unwrap_or_else(|| "Associative".to_string()),
            _ => type_constraint.unwrap_or("Mu").to_string(),
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
        meta.insert("sigil".to_string(), Value::str(sigil.to_string()));
        meta.insert(
            "type".to_string(),
            Value::package(Symbol::intern(&type_name)),
        );
        Value::make_instance(Symbol::intern("Attribute"), meta)
    }

    /// The nearest package (walking `current_package` up through its `::`
    /// ancestors, ending at the current package's registry view of GLOBAL) that
    /// has a proto or multi candidates for `name`. None when no package in the
    /// chain has a handler.
    fn nearest_package_with_trait_handler(&self, name: &str) -> Option<String> {
        let current = self.current_package();
        let mut pkg = current.as_str();
        // Local candidates only at each level — has_proto/has_multi_candidates
        // also match GLOBAL, which would stop the walk at the innermost package
        // whenever some module exported a same-named trait, hiding the
        // enclosing package's own candidates. GLOBAL is the last resort; the
        // dispatch probes {pkg}:: AND GLOBAL:: anyway, so dispatching as the
        // nearest local package still sees the imported candidates too.
        loop {
            let local_proto = self
                .registry()
                .proto_subs
                .contains(&format!("{}::{}", pkg, name));
            if local_proto || self.registry().has_multi_function(pkg, name) {
                return Some(pkg.to_string());
            }
            match pkg.rsplit_once("::") {
                Some((parent, _)) => pkg = parent,
                None => break,
            }
        }
        (self.registry().has_proto("GLOBAL", name)
            || self.registry().has_multi_candidates("GLOBAL", name))
        .then(|| current.clone())
    }

    /// Dispatch unknown attribute traits to user-defined `trait_mod:<...>` subs,
    /// or raise X::Comp::Trait::Unknown if no handler is registered. Called at
    /// class registration for each `has` declaration that carries unknown traits.
    pub(crate) fn apply_attribute_traits(
        &mut self,
        unknown_traits: &[(String, String, Option<crate::ast::Expr>)],
        attr_name_str: &str,
        sigil: char,
        is_public: bool,
        owner: &str,
        type_constraint: Option<&str>,
    ) -> Result<(), RuntimeError> {
        for (kind, trait_name, trait_arg) in unknown_traits {
            // `has $.x does Foo` — record the role so construction mixes it into
            // the attribute's value (its container does the role). Not a
            // `trait_mod:<does>` dispatch.
            if kind == "does" {
                self.registry_mut()
                    .class_attribute_does_roles
                    .entry((owner.to_string(), attr_name_str.to_string()))
                    .or_default()
                    .push(trait_name.clone());
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
            let dispatch_pkg = self.nearest_package_with_trait_handler(&trait_mod_name);
            let has_handler = dispatch_pkg.is_some();
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
                let type_obj = self.resolve_type_object(trait_name);
                let mut args = vec![attr_obj];
                if let Some(type_val) = type_obj {
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
                    Some(format!("__mutsu_attr_trait__{}!{}", owner, attr_name_str));
                let saved_pkg = self.current_package();
                if let Some(pkg) = &dispatch_pkg
                    && *pkg != saved_pkg
                {
                    self.set_current_package(pkg.clone());
                }
                let call_result = self.call_function(&trait_mod_name, args);
                self.set_current_package(saved_pkg);
                self.trait_mod_writeback_key = saved_wb_key;
                if let Some(mixin_val) = self.trait_mod_writeback_value.take() {
                    self.registry_mut()
                        .class_attribute_trait_objects
                        .insert((owner.to_string(), attr_name_str.to_string()), mixin_val);
                }
                call_result?;
                continue;
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
        Ok(())
    }
}
