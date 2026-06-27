use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    fn check_attribute_where_constraint(&mut self, pred: &Expr, value: &Value) -> bool {
        let pred_val = match self.eval_block_value(&[Stmt::Expr(pred.clone())]) {
            Ok(v) => v,
            Err(_) => return false,
        };
        match self.call_sub_value(pred_val, vec![value.clone()], false) {
            Ok(result) => result.truthy(),
            Err(_) => false,
        }
    }

    /// For a given class, return the ordered list of (role_name, MethodDef) pairs
    /// for role submethods with the given name (e.g. "BUILD", "TWEAK", "DESTROY").
    /// The order respects role composition: sub-roles come before the role that
    /// composes them. Only submethods (is_my == true) are included; regular methods
    /// in roles are skipped.
    pub(super) fn ordered_role_submethods_for_class(
        &self,
        class_name: &str,
        method_name: &str,
    ) -> Vec<(String, MethodDef)> {
        let composed = match self.registry().class_composed_roles.get(class_name) {
            Some(roles) => roles.clone(),
            None => return Vec::new(),
        };
        // Build the correct order: for each directly composed role (in order),
        // recursively include parent roles (depth-first) before the role itself.
        // Deduplicate to avoid calling the same role submethod twice for the same class.
        let mut ordered = Vec::new();
        let mut seen = std::collections::HashSet::new();
        // Figure out which roles are "direct" (from `does` declarations) vs transitive.
        // Direct roles are those not reachable through another direct role's parents.
        // However, `class_composed_roles` includes both direct and transitive roles in
        // an unspecified order. We need to reconstruct the proper depth-first order.
        //
        // Strategy: for each role in composed list, expand it depth-first (parents first).
        // The composed list may have the order [R1, R0, R2] where R0 is a parent of R1.
        // We want [R0, R1, R2]. We achieve this by expanding each role and skipping
        // already-seen roles.
        for role in &composed {
            let role_base = role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(role.as_str());
            self.expand_role_depth_first(role_base, &mut ordered, &mut seen);
        }
        // Now filter to only roles that have the requested submethod
        let mut result = Vec::new();
        for role_name in &ordered {
            if let Some(role_def) = self.registry().roles.get(role_name)
                && let Some(overloads) = role_def.methods.get(method_name)
            {
                for md in overloads {
                    if md.is_my {
                        result.push((role_name.clone(), md.clone()));
                    }
                }
            }
        }
        result
    }

    /// Recursively expand a role and its parent roles in depth-first order
    /// (parent roles first, then the role itself).
    fn expand_role_depth_first(
        &self,
        role_name: &str,
        ordered: &mut Vec<String>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        if !seen.insert(role_name.to_string()) {
            return;
        }
        // First, expand parent roles
        if let Some(parents) = self.registry().role_parents.get(role_name) {
            for parent in parents {
                let parent_base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.registry().roles.contains_key(parent_base) {
                    self.expand_role_depth_first(parent_base, ordered, seen);
                }
            }
        }
        // Then add the role itself
        ordered.push(role_name.to_string());
    }

    pub(crate) fn collect_attribute_type_constraints(
        &mut self,
        class_name: &str,
    ) -> HashMap<String, String> {
        let mut constraints = HashMap::new();
        for owner in self.class_mro(class_name) {
            if let Some(class_def) = self.registry().classes.get(&owner) {
                for (attr_name, tc) in &class_def.attribute_types {
                    constraints
                        .entry(attr_name.clone())
                        .or_insert_with(|| tc.clone());
                }
            }
        }
        constraints
    }

    pub(crate) fn enforce_attribute_where_constraints(
        &mut self,
        class_name: &str,
        class_attrs_info: &[ClassAttributeDef],
        attrs: &HashMap<String, Value>,
    ) -> Result<(), RuntimeError> {
        let type_constraints = self.collect_attribute_type_constraints(class_name);
        for (attr_name, _is_public, _default, _is_rw, _is_required, sigil, where_constraint) in
            class_attrs_info
        {
            if let Some(constraint) = type_constraints.get(attr_name)
                && (constraint.starts_with(char::is_uppercase) || constraint.starts_with("::"))
                && let Some(value) = attrs.get(attr_name)
                && !matches!(value, Value::Nil)
            {
                // For array/hash attributes, the type constraint applies to
                // elements/values, not to the container itself.
                if *sigil == '@' || *sigil == '%' {
                    // Skip container-level type check for @ and % attributes;
                    // element-level checking happens at assignment time.
                } else if !self.type_matches_value(constraint, value)
                    && !self.is_container_subclass(constraint)
                {
                    return Err(RuntimeError::new(format!(
                        "Type check failed in assignment to $!{}; expected {}, got {}",
                        attr_name,
                        constraint,
                        super::value_type_name(value)
                    )));
                }
            }
            let Some(pred) = where_constraint else {
                continue;
            };
            let Some(value) = attrs.get(attr_name) else {
                continue;
            };
            if matches!(value, Value::Nil) {
                continue;
            }
            if !self.check_attribute_where_constraint(pred, value) {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to $!{}; where constraint failed",
                    attr_name
                )));
            }
        }
        Ok(())
    }

    /// Enforce type smiley constraints (`:U`, `:D`) on attribute values during `.new`.
    pub(crate) fn enforce_attribute_smiley_constraints(
        &mut self,
        class_name: &str,
        attrs: &HashMap<String, Value>,
    ) -> Result<(), RuntimeError> {
        // Collect smileys and required status from this class and all parent classes in the MRO
        let mut smileys: HashMap<String, String> = HashMap::new();
        let mut required_attrs: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        let mro = self.class_mro(class_name);
        for mro_class in &mro {
            if let Some(class_def) = self.registry().classes.get(mro_class) {
                for (attr_name, smiley) in &class_def.attribute_smileys {
                    smileys
                        .entry(attr_name.clone())
                        .or_insert_with(|| smiley.clone());
                }
                for (attr_name, _, _, _, is_required, _, _) in &class_def.attributes {
                    if is_required.is_some() {
                        required_attrs.insert(attr_name.clone());
                    }
                }
            }
        }

        for (attr_name, smiley) in &smileys {
            // Skip smiley check for required attributes — the required check
            // should take priority (it fires separately and produces a better error)
            if required_attrs.contains(attr_name) {
                continue;
            }
            let Some(value) = attrs.get(attr_name) else {
                continue;
            };
            match smiley.as_str() {
                "U" => {
                    // :U means the value must be undefined (type object)
                    if super::types::value_is_defined(value) {
                        let mut ex_attrs = HashMap::new();
                        ex_attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                        ex_attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Type check failed in default value of attribute $!{}; expected {}, got {}",
                                attr_name,
                                self.registry().classes.get(class_name)
                                    .and_then(|cd| cd.attribute_types.get(attr_name))
                                    .map(|t| format!("{}:U", t))
                                    .unwrap_or_else(|| "Any:U".to_string()),
                                super::value_type_name(value),
                            )),
                        );
                        let ex = Value::make_instance(
                            Symbol::intern("X::TypeCheck::Attribute::Default"),
                            ex_attrs,
                        );
                        let mut err = RuntimeError::new(format!(
                            "Type check failed in default value of attribute $!{}",
                            attr_name
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
                // :D means the value must be defined
                "D" if !super::types::value_is_defined(value) => {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("name".to_string(), Value::str(format!("$!{}", attr_name)));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Type check failed in default value of attribute $!{}; expected {}, got {}",
                            attr_name,
                            self.registry().classes.get(class_name)
                                .and_then(|cd| cd.attribute_types.get(attr_name))
                                .map(|t| format!("{}:D", t))
                                .unwrap_or_else(|| "Any:D".to_string()),
                            super::value_type_name(value),
                        )),
                    );
                    let ex = Value::make_instance(
                        Symbol::intern("X::TypeCheck::Attribute::Default"),
                        ex_attrs,
                    );
                    let mut err = RuntimeError::new(format!(
                        "Type check failed in default value of attribute $!{}",
                        attr_name
                    ));
                    err.exception = Some(Box::new(ex));
                    return Err(err);
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
        let mut fetcher = Value::Nil;
        let mut storer = Value::Nil;
        let mut extra_attrs = HashMap::new();

        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "FETCH" => fetcher = *value.clone(),
                    "STORE" => storer = *value.clone(),
                    _ => {
                        extra_attrs.insert(key.clone(), *value.clone());
                    }
                }
            }
        }

        // Initialize subclass attributes with defaults
        let class_attrs_info = self.collect_class_attributes(class_name);
        for (attr_name, _is_public, default_expr, _is_rw, _is_required, sigil, _) in
            &class_attrs_info
        {
            if !extra_attrs.contains_key(attr_name) {
                let default_val = if let Some(expr) = default_expr {
                    let result = self.eval_block_value(&[crate::ast::Stmt::Expr(expr.clone())])?;
                    Self::coerce_attr_value_by_sigil(result, *sigil)
                } else {
                    match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash(HashMap::new()),
                        _ => Value::Nil,
                    }
                };
                extra_attrs.insert(attr_name.clone(), default_val);
            }
        }
        self.enforce_attribute_where_constraints(class_name, &class_attrs_info, &extra_attrs)?;

        let subclass_attrs = std::sync::Arc::new(std::sync::Mutex::new(extra_attrs));
        Ok(Value::Proxy {
            fetcher: Box::new(fetcher),
            storer: Box::new(storer),
            subclass: Some((Symbol::intern(class_name), subclass_attrs)),
            decontainerized: false,
        })
    }

    /// Call a Date formatter and store the rendered result in `__formatter_rendered`.
    pub(super) fn render_date_formatter(
        &mut self,
        date: Value,
        formatter_value: Value,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            class_name,
            ref attributes,
            id,
        } = date
        {
            let saved_env = self.env().clone();
            let saved_readonly = self.save_readonly_vars();
            let rendered = self
                .eval_call_on_value(formatter_value, vec![date.clone()])?
                .to_string_value();
            *self.env_mut() = saved_env;
            self.restore_readonly_vars(saved_readonly);
            let mut updated = attributes.to_map();
            updated.insert("__formatter_rendered".to_string(), Value::str(rendered));
            Ok(Value::write_back_sharing(
                attributes, class_name, updated, id,
            ))
        } else {
            Ok(date)
        }
    }

    /// Create a Collation instance with the given level settings.
    pub(super) fn make_collation_instance(
        primary: i64,
        secondary: i64,
        tertiary: i64,
        quaternary: i64,
    ) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("primary".to_string(), Value::Int(primary));
        attrs.insert("secondary".to_string(), Value::Int(secondary));
        attrs.insert("tertiary".to_string(), Value::Int(tertiary));
        attrs.insert("quaternary".to_string(), Value::Int(quaternary));
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
        let is_setty = self.class_is_setty(class_name);

        if is_setty {
            // Set-like construction: delegate to Set.new
            // TODO: properly track the subclass type on the resulting value
            self.dispatch_new(Value::Package(Symbol::intern("Set")), args.to_vec())
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
