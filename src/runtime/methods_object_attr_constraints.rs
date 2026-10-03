use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;

/// Where an attribute declaration was written -- see
/// [`Interpreter::attribute_decl_scope`].
#[derive(Clone, Copy)]
pub(crate) struct AttrWhereScope {
    pub(crate) unit: Option<Symbol>,
    pub(crate) package: Symbol,
}

impl Interpreter {
    /// The scope an attribute declaration was written in: its compunit (the
    /// captured unit of a role-composed declaration, else the unit that
    /// declared its owning package) and that owning package, falling back to
    /// `fallback_class` for a declaration that does not record its package.
    // Cost: O(1) hash lookups.
    pub(crate) fn attribute_decl_scope(
        &self,
        attr: &ClassAttributeDef,
        fallback_class: &str,
    ) -> AttrWhereScope {
        let package = attr
            .declaring_package
            .unwrap_or_else(|| Symbol::intern(fallback_class));
        let unit = attr
            .captured_unit
            .or_else(|| self.class_declaring_units.get(package.as_str()).copied())
            .or_else(|| self.class_declaring_units.get(fallback_class).copied());
        AttrWhereScope { unit, package }
    }

    /// Does `value` satisfy an attribute's `where` predicate? `scope` is where
    /// the declaration was written (see [`Self::attribute_decl_scope`]): the
    /// predicate is checked during construction or through an accessor called
    /// from another compunit, so -- like an attribute default
    /// (`eval_attr_default_expr`) -- it has to run with its own unit and
    /// package in effect, or a bare call to a sub declared beside a `unit
    /// class` (`where { check-locale($_) }`) dies with "Unknown function" and
    /// the constraint reads as failed (ecosystem `Date::Calendar::Gregorian`).
    // Cost: O(P), P = cost of evaluating and invoking the predicate.
    pub(crate) fn check_attribute_where_constraint(
        &mut self,
        pred: &crate::opcode::DeclTraitArg,
        value: &Value,
        scope: AttrWhereScope,
    ) -> bool {
        let saved_unit = self.current_unit;
        if let Some(unit) = scope.unit {
            self.current_unit = unit;
        }
        let saved_package = self.current_package();
        let saved_package_sym = self.current_package_sym();
        self.set_current_package_with_sym(scope.package.as_str().to_string(), scope.package);
        let ok = self.check_attribute_where_constraint_in_scope(pred, value);
        self.set_current_package_with_sym(saved_package, saved_package_sym);
        self.current_unit = saved_unit;
        ok
    }

    fn check_attribute_where_constraint_in_scope(
        &mut self,
        pred: &crate::opcode::DeclTraitArg,
        value: &Value,
    ) -> bool {
        // The implicit topic `$_` is seeded to the candidate value BEFORE
        // evaluating the predicate, unconditionally -- mirroring the subset
        // `where`-predicate's own inline execution (`type_matches_value`'s
        // `Expr::Block`/`Expr::Lambda` handling), which always binds the
        // candidate to `$_`/the param name rather than trying to detect
        // whether the predicate "uses" it first.
        //
        // A prior version only seeded `_` when the compiled chunk's
        // `free_var_syms` recorded a read of it, to cover `where .so` /
        // `where .all ~~ Cool` (implicit-topic method calls) without
        // disturbing a plain value/type/Junction predicate. But `$_` is a
        // magic/dynamic variable resolved through env, not a genuine lexical
        // closure capture, so an ordinary BLOCK predicate that reads it
        // explicitly (`has Numeric $.lat where { -90 <= $_ <= 90 }`) does
        // NOT reliably show up in `free_var_syms` either -- `_` then stayed
        // unseeded, the block evaluated against a stale/absent topic, and
        // the predicate silently always passed (ecosystem `Date::Event`
        // t/5-lat-lon.t: `$o.lat: 999` never died).
        //
        // Seeding unconditionally is safe for every predicate shape: a
        // plain value/type/Junction predicate (`where Int`, `where 42|3`)
        // never reads `_`, so seeding it is a no-op for evaluation, and
        // `smart_match_values` below already implements "a Bool RHS is the
        // match result regardless of the topic" (rakudo: smartmatch against
        // `True` always matches) -- so a block/`.so` predicate's boolean
        // result and a plain value/type/Junction predicate both resolve
        // correctly through the same `smart_match_values` call, and the
        // `uses_topic` branch this replaced is no longer needed.
        let saved_topic = self.env().get("_").cloned();
        self.env_mut().insert("_".to_string(), value.clone());
        let pred_val = match self.eval_decl_trait_arg(pred) {
            Ok(v) => v,
            Err(_) => {
                match saved_topic {
                    Some(v) => {
                        self.env_mut().insert("_".to_string(), v);
                    }
                    None => {
                        self.env_mut().remove("_");
                    }
                }
                return false;
            }
        };
        match saved_topic {
            Some(v) => {
                self.env_mut().insert("_".to_string(), v);
            }
            None => {
                self.env_mut().remove("_");
            }
        }
        // `has $.x is default(V) where PRED` passes iff `value ~~ PRED`.
        // Smartmatch handles every predicate shape uniformly: a
        // Callable/WhateverCode (`* == 42`) is invoked with the value, a Junction
        // (`42|3`) is threaded, and a plain value/type is compared. Calling the
        // predicate directly only worked for Callables and mis-handled a Junction
        // predicate (S02-types/whatever.t "compile time Junction in `where`").
        self.smart_match_values(value, &pred_val)
    }

    pub(crate) fn collect_attribute_type_constraints(
        &mut self,
        class_name: &str,
    ) -> HashMap<String, String> {
        let mut constraints = HashMap::new();
        let mro = self.class_mro(class_name);
        for owner in mro.iter() {
            let owned: Vec<(String, String)> = match self.registry().classes.get(owner.as_str()) {
                Some(class_def) => class_def
                    .attribute_types
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect(),
                None => continue,
            };
            for (attr_name, tc) in owned {
                // See `get_attr_type_constraint`: a type declared in the owning
                // package is named `Owner::Name`, and the short name recorded at
                // the declaration site has to be resolved to it.
                let tc = self.resolve_type_name_for_owner(owner.as_str(), tc);
                constraints.entry(attr_name).or_insert(tc);
            }
        }
        constraints
    }

    pub(crate) fn enforce_attribute_where_constraints(
        &mut self,
        class_name: &str,
        class_attrs_info: &[ClassAttributeDef],
        attrs: &AttrMap,
    ) -> Result<(), RuntimeError> {
        let type_constraints = self.collect_attribute_type_constraints(class_name);
        for attr in class_attrs_info {
            let attr_name = &attr.name;
            let sigil = &attr.sigil;
            let where_constraint = &attr.where_constraint;
            let storage_key = super::attribute_storage_key(class_attrs_info, attr_name, *sigil);
            if let Some(constraint) =
                super::attribute_type_constraint(class_attrs_info, attr, &type_constraints)
                && (constraint.starts_with(char::is_uppercase) || constraint.starts_with("::"))
                && let Some(value) = attrs.get(storage_key)
                && !value.is_nil()
            {
                // For array/hash attributes, the type constraint applies to
                // elements/values, not to the container itself.
                if *sigil == '@' || *sigil == '%' {
                    // Skip container-level type check for @ and % attributes;
                    // element-level checking happens at assignment time.
                } else if !self.type_matches_value(&constraint, value)
                    && !self.is_container_subclass(&constraint)
                {
                    // Rakudo raises a typed X::TypeCheck::Assignment here (with
                    // the `expected X but got Y (repr)` wording), not an
                    // untyped AdHoc. The reported type carries the attribute's
                    // smiley (`Str:D`), which the constraint map drops.
                    let reported =
                        self.attribute_reported_constraint(class_name, attr_name, &constraint);
                    return Err(self.type_check_assignment_failure(
                        &format!("$!{}", attr_name),
                        &reported,
                        value,
                    ));
                }
            }
            let Some(pred) = where_constraint else {
                continue;
            };
            let Some(value) = attrs.get(storage_key) else {
                continue;
            };
            if value.is_nil() {
                continue;
            }
            // An attribute left unset holds its (undefined) type object, which
            // Rakudo never runs the `where` predicate against: `has $.x where
            // Positional|Associative` (or `has Int $.y where * > 5`) constructs
            // fine with no value given (JSON::Marshal's type-constraint tests).
            // A typed attribute's synthesized default is that type object too.
            if matches!(value.view(), ValueView::Package(_)) {
                continue;
            }
            let scope = self.attribute_decl_scope(attr, class_name);
            if !self.check_attribute_where_constraint(pred, value, scope) {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to $!{}; where constraint failed",
                    attr_name
                )));
            }
        }
        Ok(())
    }

    /// Enforce type smiley constraints (`:U`, `:D`) on attribute values during `.new`.
    ///
    /// `provided` — the attribute names explicitly passed to the constructor,
    /// when the call site knows them (pre-BUILD assembly). An `is required`
    /// `:D` attribute that WAS provided with an undefined value throws
    /// X::TypeCheck::Assignment; one that is merely still unset is left to the
    /// required check (X::Attribute::Required). Pass `None` post-BUILD or when
    /// the arg list is unavailable — then required attrs are skipped entirely
    /// (the old behavior).
    /// The attribute's declared type with its smiley reattached (`Int:D`), the
    /// form rakudo names in a type-check message. `Any` when the attribute is
    /// untyped; the bare type when the smiley is `_` (no constraint).
    pub(crate) fn attribute_constraint_with_smiley(
        &self,
        class_name: &str,
        attr_name: &str,
        smiley: &str,
    ) -> String {
        let base = self
            .registry()
            .classes
            .get(class_name)
            .and_then(|cd| cd.attribute_types.get(attr_name))
            .cloned()
            .unwrap_or_else(|| "Any".to_string());
        Self::join_constraint_smiley(&base, smiley)
    }

    /// Same, for a caller that already resolved the constraint (possibly from a
    /// parent class): look the smiley up by attribute name and reattach it.
    pub(crate) fn attribute_reported_constraint(
        &mut self,
        class_name: &str,
        attr_name: &str,
        constraint: &str,
    ) -> String {
        let smiley = self
            .class_mro(class_name)
            .iter()
            .find_map(|c| {
                self.registry()
                    .classes
                    .get(c.as_str())
                    .and_then(|cd| cd.attribute_smileys.get(attr_name))
                    .cloned()
            })
            .unwrap_or_else(|| "_".to_string());
        Self::join_constraint_smiley(constraint, &smiley)
    }

    pub(crate) fn join_constraint_smiley(base: &str, smiley: &str) -> String {
        // An object-hash constraint (`Str{Int}`): the smiley belongs to the
        // value type, ahead of the key part.
        if let (value_type, Some(key_type)) =
            crate::runtime::types::split_object_hash_constraint(base)
        {
            return format!(
                "{}{{{}}}",
                Self::join_constraint_smiley(value_type, smiley),
                key_type
            );
        }
        let base = base
            .trim_end_matches(":D")
            .trim_end_matches(":U")
            .trim_end_matches(":_");
        match smiley {
            "D" | "U" => format!("{}:{}", base, smiley),
            _ => base.to_string(),
        }
    }

    pub(crate) fn enforce_attribute_smiley_constraints(
        &mut self,
        class_name: &str,
        attrs: &AttrMap,
        provided: Option<&std::collections::HashSet<String>>,
    ) -> Result<(), RuntimeError> {
        // Collect smileys and required status from this class and all parent classes in the MRO
        let mut smileys: HashMap<String, String> = HashMap::new();
        let mut required_attrs: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        let mro = self.class_mro(class_name);
        for mro_class in mro.iter().map(|s| s.as_str()) {
            if let Some(class_def) = self.registry().classes.get(mro_class) {
                for (attr_name, smiley) in &class_def.attribute_smileys {
                    smileys
                        .entry(attr_name.clone())
                        .or_insert_with(|| smiley.clone());
                }
                for attr in &class_def.attributes {
                    if attr.is_required.is_some() {
                        required_attrs.insert(attr.name.clone());
                    }
                }
            }
        }

        let class_attrs = self.collect_class_attributes(class_name);
        let attr_type_constraints = self.collect_attribute_type_constraints(class_name);

        for (attr_name, smiley) in &smileys {
            // For an @/% attribute the declared type and its definedness smiley
            // constrain each element/value, not the collection itself. An empty
            // collection is therefore valid for either :D or :U, while a
            // populated collection must check the smiley on every stored item.
            if let Some(attr) = class_attrs.iter().find(|attr| attr.name == *attr_name)
                && matches!(attr.sigil, '@' | '%')
            {
                let storage_key = super::attribute_storage_key(&class_attrs, attr_name, attr.sigil);
                if let Some(value) = attrs.get(storage_key) {
                    let base = super::attribute_type_constraint(
                        &class_attrs,
                        attr,
                        &attr_type_constraints,
                    )
                    .unwrap_or_else(|| "Any".to_string());
                    // Elements are checked against the value type alone; an
                    // object hash's key part (`Str{Int}`) is not theirs.
                    let (value_type, _) =
                        crate::runtime::types::split_object_hash_constraint(&base);
                    let constraint = Self::join_constraint_smiley(value_type, smiley);
                    let display = format!("{}!{}", attr.sigil, attr_name);
                    let mut elements = Vec::new();
                    match value.view() {
                        ValueView::Array(items, kind) => {
                            fn collect_shaped_elements(
                                items: &crate::value::ArrayData,
                                out: &mut Vec<Value>,
                            ) {
                                for item in items.iter() {
                                    match item.view() {
                                        ValueView::Array(nested, _) => {
                                            collect_shaped_elements(&nested, out)
                                        }
                                        _ => out.push(item.clone()),
                                    }
                                }
                            }
                            if matches!(kind, crate::value::ArrayKind::Shaped) {
                                collect_shaped_elements(&items, &mut elements);
                            } else {
                                elements.extend(items.iter().cloned());
                            }
                        }
                        ValueView::Hash(map) => elements.extend(map.values().cloned()),
                        _ => {}
                    }
                    for element in elements {
                        if !self.type_matches_value(&constraint, &element) {
                            return Err(self.type_check_element_failure(
                                &display,
                                &constraint,
                                &element,
                            ));
                        }
                    }
                }
                continue;
            }
            // For required attributes the missing-value case is left to the
            // required check (it produces the better error) — but a required
            // `:D` attribute that WAS supplied with an undefined value fails
            // the assignment typecheck, like rakudo (JSON::Unmarshal 040:
            // `unmarshal('{"attr": null}', IntDClass)`).
            if required_attrs.contains(attr_name) {
                if smiley == "D"
                    && provided.is_some_and(|p| p.contains(attr_name))
                    && let Some(value) = attrs.get(attr_name)
                    && !super::types::value_is_defined(value)
                {
                    let constraint =
                        self.attribute_constraint_with_smiley(class_name, attr_name, smiley);
                    return Err(crate::runtime::utils::definite_type_check_assignment_error(
                        &format!("$!{}", attr_name),
                        &constraint,
                        value,
                    ));
                }
                continue;
            }
            let Some(value) = attrs.get(attr_name) else {
                continue;
            };
            let constraint = self.attribute_constraint_with_smiley(class_name, attr_name, smiley);
            match smiley.as_str() {
                // `:U` wants a type object. A *defined* value can never satisfy
                // it, which rakudo reports as the attribute-default error
                // ("Can never assign default value ..."), not an assignment
                // failure.
                "U" if super::types::value_is_defined(value) => {
                    return Err(crate::runtime::utils::attribute_default_never_assign_error(
                        attr_name,
                        &constraint,
                        value,
                    ));
                }
                // `:D` wants a defined value; a type object reaching the slot is
                // the ordinary assignment type-check failure.
                "D" if !super::types::value_is_defined(value) => {
                    return Err(crate::runtime::utils::definite_type_check_assignment_error(
                        &format!("$!{}", attr_name),
                        &constraint,
                        value,
                    ));
                }
                _ => {} // "_" or anything else: no constraint
            }
        }
        Ok(())
    }

    /// Construct a Proxy subclass instance: extracts FETCH/STORE from args,
    /// initializes subclass attributes (with defaults), and returns a Proxy
    /// with shared mutable subclass attrs.
    pub(crate) fn construct_proxy_subclass(
        &mut self,
        class_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut fetcher = Value::NIL;
        let mut storer = Value::NIL;
        let mut extra_attrs = AttrMap::new();

        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "FETCH" => fetcher = value.clone(),
                    "STORE" => storer = value.clone(),
                    _ => {
                        extra_attrs.insert(key.clone(), value.clone());
                    }
                }
            }
        }

        // Initialize subclass attributes with defaults
        let class_attrs_info = self.collect_class_attributes(class_name);
        for attr in &class_attrs_info {
            let attr_name = &attr.name;
            let default_expr = &attr.default;
            let sigil = &attr.sigil;
            if !extra_attrs.contains_key(attr_name) {
                let default_val = if let Some(arg) = default_expr {
                    let result = self.eval_decl_trait_arg(arg)?;
                    Self::coerce_attr_value_by_sigil(result, *sigil)
                } else {
                    match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash(ValueMap::default()),
                        _ => Value::NIL,
                    }
                };
                extra_attrs.insert(attr_name.clone(), default_val);
            }
        }
        self.enforce_attribute_where_constraints(class_name, &class_attrs_info, &extra_attrs)?;

        let subclass_attrs =
            std::sync::Arc::new(std::sync::Mutex::new(HashMap::from(&extra_attrs)));
        Ok(Value::proxy_parts(
            fetcher,
            storer,
            Some((Symbol::intern(class_name), subclass_attrs)),
            false,
        ))
    }

    /// Create a Collation instance with the given level settings.
    pub(super) fn make_collation_instance(
        primary: i64,
        secondary: i64,
        tertiary: i64,
        quaternary: i64,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("primary".to_string(), Value::int(primary));
        attrs.insert("secondary".to_string(), Value::int(secondary));
        attrs.insert("tertiary".to_string(), Value::int(tertiary));
        attrs.insert("quaternary".to_string(), Value::int(quaternary));
        Value::make_instance(Symbol::intern("Collation"), attrs)
    }

    /// Check if a class composes the Baggy or Setty role (directly or transitively).
    pub(crate) fn class_does_baggy_or_setty(&self, class_name: &str) -> bool {
        // Direct builtin Setty/Baggy types
        const SETTY_BAGGY_TYPES: &[&str] = &[
            "Set",
            "SetHash",
            "Bag",
            "BagHash",
            "Mix",
            "MixHash",
            "Baggy",
            "Setty",
            "QuantHash",
        ];
        // Check the class definition's parents and MRO for Baggy/Setty
        if let Some(class_def) = self.registry().classes.get(class_name) {
            if class_def
                .parents
                .iter()
                .any(|p| SETTY_BAGGY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
            if class_def
                .mro
                .iter()
                .any(|p| SETTY_BAGGY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
        }
        // Also check composed roles
        if let Some(roles) = self.registry().class_composed_roles.get(class_name)
            && roles
                .iter()
                .any(|r| SETTY_BAGGY_TYPES.contains(&r.as_str()))
        {
            return true;
        }
        false
    }

    /// Determine whether a class inherits from a Set-like type (vs Bag-like).
    fn class_is_setty(&self, class_name: &str) -> bool {
        const SETTY_TYPES: &[&str] = &["Set", "SetHash"];
        if let Some(class_def) = self.registry().classes.get(class_name) {
            if class_def
                .parents
                .iter()
                .any(|p| SETTY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
            if class_def
                .mro
                .iter()
                .any(|p| SETTY_TYPES.contains(&p.as_str()))
            {
                return true;
            }
        }
        if let Some(roles) = self.registry().class_composed_roles.get(class_name)
            && roles
                .iter()
                .any(|r| r == "Setty" || r == "Set" || r == "SetHash")
        {
            return true;
        }
        false
    }

    /// Construct an instance for a class that does Baggy/Setty.
    /// Positional args are counted like a Bag (or treated as Set elements),
    /// and the result is stored as an Instance with internal storage.
    pub(crate) fn construct_baggy_instance(
        &mut self,
        class_name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // A concrete QuantHash base in the MRO decides both the element
        // semantics and the mutability of the backing store: `class C is
        // BagHash` must be backed by a MUTABLE bag so `%c<a> = 5` works, while
        // `class C is Bag` keeps the immutable one and still raises. Only a
        // class that composes the bare `Baggy`/`Setty` role (no concrete base)
        // falls back to the immutable default.
        if let Some(base) = self.quanthash_base_kind(class_name) {
            let storage = self.quanthash_base_storage(base, args.to_vec())?;
            let mut attrs = HashMap::new();
            attrs.insert("__baggy_data__".to_string(), storage);
            return Ok(Value::make_instance(Symbol::intern(class_name), attrs));
        }
        let is_setty = self.class_is_setty(class_name);

        if is_setty {
            // Set-like construction: delegate to Set.new
            // TODO: properly track the subclass type on the resulting value
            self.dispatch_new(Value::package(Symbol::intern("Set")), args.to_vec())
        } else {
            // Bag-like construction: delegate to dispatch_to_bag_with_what for
            // proper handling (pairs, hashes, etc.), then wrap as an Instance
            // so that isa-ok checks for the subclass still work.
            let items_array = Value::array(args.to_vec());
            let bag_value = self.dispatch_to_bag_with_what(items_array, "Bag")?;

            // Store the real Bag as __baggy_data__ on an Instance of the subclass
            let mut attrs = HashMap::new();
            attrs.insert("__baggy_data__".to_string(), bag_value);
            Ok(Value::make_instance(Symbol::intern(class_name), attrs))
        }
    }
}
