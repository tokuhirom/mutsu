use super::*;
use crate::symbol::Symbol;
use crate::value::signature::make_params_value_from_param_defs;

impl Interpreter {
    pub(super) fn classhow_lookup(&self, invocant: &Value, method_name: &str) -> Option<Value> {
        let Value::Package(class_name) = invocant else {
            return None;
        };
        let class_def = self.classes.get(&class_name.resolve())?;
        let defs = class_def.methods.get(method_name)?;
        let def = defs.first()?;
        Some(Value::make_sub(
            *class_name,
            Symbol::intern(method_name),
            def.params.clone(),
            def.param_defs.clone(),
            def.body.clone(),
            def.is_rw,
            HashMap::new(),
        ))
    }

    pub(super) fn classhow_find_method(
        &self,
        invocant: &Value,
        method_name: &str,
    ) -> Option<Value> {
        if matches!(
            method_name,
            "name"
                | "ver"
                | "auth"
                | "mro"
                | "mro_unhidden"
                | "archetypes"
                | "isa"
                | "can"
                | "lookup"
                | "find_method"
                | "add_method"
                | "methods"
                | "concretization"
                | "curried_role"
        ) {
            return Some(Value::Str(method_name.to_string()));
        }
        if let Some(value) = self.classhow_lookup(invocant, method_name) {
            return Some(value);
        }
        // CREATE is a built-in method on all types
        if method_name == "CREATE" {
            return Some(Value::Routine {
                package: Symbol::intern("Mu"),
                name: Symbol::intern("CREATE"),
                is_regex: false,
            });
        }
        if let Value::Package(class_name) = invocant
            && let Some(class_def) = self.classes.get(&class_name.resolve())
            && class_def.native_methods.contains(method_name)
        {
            return Some(Value::Str(method_name.to_string()));
        }
        None
    }

    pub(super) fn dispatch_classhow_method(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" if args.len() == 1 => Ok(Value::Str(match &args[0] {
                Value::Package(name) => name.resolve(),
                Value::Instance { class_name, .. } => class_name.resolve(),
                Value::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    let args_str = type_args
                        .iter()
                        .map(|v| match v {
                            Value::Package(n) => n.resolve(),
                            other => other.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{}[{}]", base_name, args_str)
                }
                other => value_type_name(other).to_string(),
            })),
            "ver" if args.len() == 1 => {
                let invocant_name = match &args[0] {
                    Value::Package(name) => *name,
                    Value::Instance { class_name, .. } => *class_name,
                    _ => {
                        return Err(RuntimeError::new(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                        ));
                    }
                };
                let Some(meta) = self.type_metadata.get(&invocant_name.resolve()) else {
                    if invocant_name == "Grammar" {
                        return Ok(Self::version_from_value(Value::Str("6.e".to_string())));
                    }
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                    ));
                };
                let Some(value) = meta.get("ver").cloned() else {
                    if invocant_name == "Grammar" {
                        return Ok(Self::version_from_value(Value::Str("6.e".to_string())));
                    }
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                    ));
                };
                Ok(Self::version_from_value(value))
            }
            "auth" if args.len() == 1 => {
                let invocant_name = match &args[0] {
                    Value::Package(name) => *name,
                    Value::Instance { class_name, .. } => *class_name,
                    _ => {
                        return Err(RuntimeError::new(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                        ));
                    }
                };
                let Some(meta) = self.type_metadata.get(&invocant_name.resolve()) else {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                    ));
                };
                let Some(value) = meta.get("auth").cloned() else {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                    ));
                };
                Ok(Value::Str(value.to_string_value()))
            }
            "isa" if args.len() == 2 => {
                let Value::Package(class_name) = &args[0] else {
                    return Ok(Value::Bool(false));
                };
                let Value::Package(other_name) = &args[1] else {
                    return Ok(Value::Bool(false));
                };
                let is_same = class_name == other_name;
                if is_same {
                    return Ok(Value::Bool(true));
                }
                let mro = self.class_mro(&class_name.resolve());
                let other_resolved = other_name.resolve();
                Ok(Value::Bool(mro.iter().any(|p| p == &other_resolved)))
            }
            "mro" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg {
                        Value::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        Value::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    Ok(Value::array(mro))
                } else {
                    let mro = self.classhow_mro_names(&args[0]);
                    Ok(Value::array(
                        mro.into_iter()
                            .map(|s| Value::Package(Symbol::intern(&s)))
                            .collect::<Vec<_>>(),
                    ))
                }
            }
            "archetypes" if !args.is_empty() => {
                let invocant_name = match &args[0] {
                    Value::Package(name) => name.resolve(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                let base_name = invocant_name
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(invocant_name.as_str());
                let mut attrs = HashMap::new();
                attrs.insert(
                    "composable".to_string(),
                    Value::Bool(self.roles.contains_key(base_name)),
                );
                Ok(Value::make_instance(
                    Symbol::intern("Perl6::Metamodel::Archetypes"),
                    attrs,
                ))
            }
            "mro_unhidden" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg {
                        Value::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        Value::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    let filtered = self.filter_mro_unhidden(&args[0], mro);
                    Ok(Value::array(filtered))
                } else {
                    let mro = self.classhow_mro_unhidden_names(&args[0]);
                    Ok(Value::array(
                        mro.into_iter()
                            .map(|s| Value::Package(Symbol::intern(&s)))
                            .collect::<Vec<_>>(),
                    ))
                }
            }
            "can" if args.len() >= 2 => {
                let invocant = &args[0];
                let method_name = args[1].to_string_value();
                if let Some(value) = self.classhow_find_method(invocant, &method_name) {
                    return Ok(Value::array(vec![value]));
                }
                Ok(Value::array(Vec::new()))
            }
            "lookup" if args.len() >= 2 => {
                let invocant = &args[0];
                let method_name = args[1].to_string_value();
                Ok(self
                    .classhow_lookup(invocant, &method_name)
                    .unwrap_or(Value::Nil))
            }
            "find_method" if args.len() >= 2 => {
                let invocant = &args[0];
                let method_name = args[1].to_string_value();
                Ok(self
                    .classhow_find_method(invocant, &method_name)
                    .unwrap_or(Value::Nil))
            }
            "add_method" if args.len() >= 3 => {
                let class_name = match &args[0] {
                    Value::Package(name) => name.resolve(),
                    Value::Str(name) => name.clone(),
                    _ => {
                        return Err(RuntimeError::new("add_method target must be a type object"));
                    }
                };
                let method_name = args[1].to_string_value();
                let method_value = args[2].clone();
                let Value::Sub(sub_data) = method_value else {
                    return Ok(Value::Nil);
                };
                let def = MethodDef {
                    params: sub_data.params.clone(),
                    param_defs: sub_data
                        .params
                        .iter()
                        .map(|name| ParamDef {
                            name: name.clone(),
                            default: None,
                            multi_invocant: true,
                            required: false,
                            named: false,
                            slurpy: false,
                            double_slurpy: false,
                            sigilless: false,
                            type_constraint: None,
                            literal_value: None,
                            sub_signature: None,
                            where_constraint: None,
                            traits: Vec::new(),
                            optional_marker: false,
                            outer_sub_signature: None,
                            code_signature: None,
                            is_invocant: false,
                            shape_constraints: None,
                        })
                        .collect(),
                    body: sub_data.body.clone(),
                    is_rw: false,
                    is_private: false,
                    return_type: None,
                };
                if let Some(class_def) = self.classes.get_mut(&class_name) {
                    class_def.methods.insert(method_name, vec![def]);
                    return Ok(Value::Nil);
                }
                Err(RuntimeError::new(format!(
                    "Unknown class for add_method: {}",
                    class_name
                )))
            }
            "methods" if !args.is_empty() => self.dispatch_classhow_methods(&args),
            "candidates" if !args.is_empty() => {
                let base_name = match &args[0] {
                    Value::Package(name) => name.resolve(),
                    Value::ParametricRole { base_name, .. } => base_name.resolve(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    other => other
                        .to_string_value()
                        .trim_start_matches('(')
                        .trim_end_matches(')')
                        .to_string(),
                };
                if let Some(candidates) = self.role_candidates.get(&base_name) {
                    let values = candidates
                        .iter()
                        .map(|_| Value::Package(Symbol::intern(&base_name)))
                        .collect::<Vec<_>>();
                    return Ok(Value::array(values));
                }
                if self.roles.contains_key(&base_name) {
                    return Ok(Value::array(vec![Value::Package(Symbol::intern(
                        &base_name,
                    ))]));
                }
                Ok(Value::array(Vec::new()))
            }
            "concretization" if args.len() >= 2 => {
                let class_name = match &args[0] {
                    Value::Package(name) => name.resolve(),
                    Value::Instance { class_name, .. } => class_name.resolve(),
                    other => value_type_name(other).to_string(),
                };
                let role_name = match &args[1] {
                    Value::Package(name) => name.resolve(),
                    Value::ParametricRole {
                        base_name,
                        type_args,
                    } => {
                        let args_str = type_args
                            .iter()
                            .map(|v| match v {
                                Value::Package(n) => n.resolve(),
                                other => other.to_string_value(),
                            })
                            .collect::<Vec<_>>()
                            .join(",");
                        format!("{}[{}]", base_name, args_str)
                    }
                    _ => args[1].to_string_value(),
                };
                let base_role_name = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role_name.as_str());
                // Check for :local named arg
                let local_only = args[2..]
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "local" && v.truthy()));
                // Check direct composed roles and transitive sub-roles
                let check_transitive = |class_composed: &HashMap<String, Vec<String>>,
                                        role_parents: &HashMap<String, Vec<String>>,
                                        cn: &str|
                 -> Option<Value> {
                    let composed = class_composed.get(cn).cloned().unwrap_or_default();
                    // Check direct matches
                    for cr in &composed {
                        let cr_base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                        if *cr == role_name || cr_base == base_role_name {
                            return Some(Value::Package(Symbol::intern(cr_base)));
                        }
                    }
                    // Check transitive sub-roles
                    let mut stack: Vec<String> = composed
                        .iter()
                        .map(|cr| {
                            cr.split_once('[')
                                .map(|(b, _)| b)
                                .unwrap_or(cr.as_str())
                                .to_string()
                        })
                        .collect();
                    let mut seen = std::collections::HashSet::new();
                    while let Some(rn) = stack.pop() {
                        if !seen.insert(rn.clone()) {
                            continue;
                        }
                        if let Some(rp) = role_parents.get(&rn) {
                            for p in rp {
                                let p_base =
                                    p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                                if p_base == base_role_name || *p == role_name {
                                    return Some(Value::Package(Symbol::intern(p_base)));
                                }
                                stack.push(p_base.to_string());
                            }
                        }
                    }
                    None
                };
                if let Some(result) =
                    check_transitive(&self.class_composed_roles, &self.role_parents, &class_name)
                {
                    return Ok(result);
                }
                if !local_only {
                    let mro = self.class_mro(&class_name);
                    for cn in &mro[1..] {
                        if let Some(result) =
                            check_transitive(&self.class_composed_roles, &self.role_parents, cn)
                        {
                            return Ok(result);
                        }
                    }
                }
                Err(RuntimeError::new(format!(
                    "No concretization of {} found for {}",
                    role_name, class_name
                )))
            }
            "curried_role" if !args.is_empty() => {
                // For a parameterized role like R[Int], return the base role R
                match &args[0] {
                    Value::ParametricRole { base_name, .. } => Ok(Value::Package(*base_name)),
                    Value::Package(name) => {
                        let resolved = name.resolve();
                        let base = resolved
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(resolved.as_str());
                        Ok(Value::Package(Symbol::intern(base)))
                    }
                    _ => {
                        let s = args[0].to_string_value();
                        let base = s.split_once('[').map(|(b, _)| b).unwrap_or(s.as_str());
                        Ok(Value::Package(Symbol::intern(base)))
                    }
                }
            }
            _ => Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                method
            ))),
        }
    }

    pub(super) fn classhow_mro_names(&mut self, invocant: &Value) -> Vec<String> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        let mut mro = if self.classes.contains_key(&class_name) {
            self.class_mro(&class_name)
        } else {
            vec![class_name.clone()]
        };
        if self.package_looks_like_grammar(&class_name) {
            for parent in ["Grammar", "Match", "Capture", "Cool", "Any", "Mu"] {
                if !mro.iter().any(|name| name == parent) {
                    mro.push(parent.to_string());
                }
            }
            return mro;
        }
        if class_name != "Mu" && !mro.iter().any(|name| name == "Any") {
            mro.push("Any".to_string());
        }
        if !mro.iter().any(|name| name == "Mu") {
            mro.push("Mu".to_string());
        }
        mro
    }

    /// Build MRO with roles interleaved (for :roles or :concretizations).
    /// For each class in the MRO, insert its composed roles right after it.
    /// For :roles mode, use base role names. For :concretizations, use as-is.
    pub(super) fn classhow_mro_with_roles(
        &mut self,
        invocant: &Value,
        _concretizations: bool,
    ) -> Vec<Value> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        let base_mro = self.classhow_mro_names(invocant);
        let mut result: Vec<Value> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        for entry in &base_mro {
            // Check if this entry is a role (in the parents list because of `does`)
            let base_entry = entry
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(entry.as_str());
            if self.roles.contains_key(base_entry)
                && entry != "Any"
                && entry != "Mu"
                && entry != &class_name
            {
                // This is a role in the class's parent list - include it with base name
                if seen.insert(base_entry.to_string()) {
                    result.push(Value::Package(Symbol::intern(base_entry)));
                    // Also add the role's parent classes that aren't already in base MRO
                    self.add_role_parents_to_mro(base_entry, &base_mro, &mut result, &mut seen);
                }
            } else {
                // This is a class
                if seen.insert(entry.clone()) {
                    result.push(Value::Package(Symbol::intern(entry)));
                    // Insert composed roles for this class
                    let composed = self
                        .class_composed_roles
                        .get(entry)
                        .cloned()
                        .unwrap_or_default();
                    for role_name in &composed {
                        let base_role = role_name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(role_name.as_str());
                        if seen.insert(base_role.to_string()) {
                            result.push(Value::Package(Symbol::intern(base_role)));
                            // Add role's sub-roles (from `does` inside the role)
                            self.add_role_parents_to_mro(
                                base_role,
                                &base_mro,
                                &mut result,
                                &mut seen,
                            );
                        }
                    }
                }
            }
        }
        result
    }

    /// Add a role's parent roles/classes to the MRO result.
    pub(super) fn add_role_parents_to_mro(
        &self,
        role_name: &str,
        _base_mro: &[String],
        result: &mut Vec<Value>,
        seen: &mut HashSet<String>,
    ) {
        if let Some(parents) = self.role_parents.get(role_name) {
            for parent in parents {
                let base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.roles.contains_key(base) && seen.insert(base.to_string()) {
                    result.push(Value::Package(Symbol::intern(base)));
                    self.add_role_parents_to_mro(base, _base_mro, result, seen);
                }
                // Parent classes from roles are handled by the class MRO
            }
        }
    }

    /// Filter MRO to remove hidden classes and their associated roles.
    pub(super) fn filter_mro_unhidden(&self, invocant: &Value, mro: Vec<Value>) -> Vec<Value> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        // Collect hidden parent names for this class
        let hidden_parents: HashSet<String> = self
            .hidden_defer_parents
            .get(&class_name)
            .cloned()
            .unwrap_or_default();
        // Also collect classes marked `is hidden`
        let mut hidden_set: HashSet<String> = HashSet::new();
        for hp in &hidden_parents {
            hidden_set.insert(hp.clone());
        }
        for hc in &self.hidden_classes {
            hidden_set.insert(hc.clone());
        }
        if hidden_set.is_empty() {
            return mro;
        }
        // Build set of all entries to hide: hidden classes + their composed roles
        let mut to_hide: HashSet<String> = HashSet::new();
        for hidden in &hidden_set {
            to_hide.insert(hidden.clone());
            // Also add the base name (strip type params)
            let hidden_base = hidden
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(hidden.as_str());
            to_hide.insert(hidden_base.to_string());
            // Also hide roles composed by the hidden class (try both full and base name)
            let composed_full = self.class_composed_roles.get(hidden.as_str()).cloned();
            let composed_base = self.class_composed_roles.get(hidden_base).cloned();
            let composed = composed_full.or(composed_base).unwrap_or_default();
            for role in &composed {
                let base = role
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role.as_str());
                to_hide.insert(base.to_string());
                // And roles composed by those roles
                self.collect_hidden_roles(base, &mut to_hide);
            }
            // Also check role_parents for the hidden entry (in case it's a punned role)
            self.collect_hidden_roles(hidden_base, &mut to_hide);
        }
        mro.into_iter()
            .filter(|v| {
                if let Value::Package(name) = v {
                    !to_hide.contains(&name.resolve())
                } else {
                    true
                }
            })
            .collect()
    }

    pub(super) fn collect_hidden_roles(&self, role_name: &str, to_hide: &mut HashSet<String>) {
        if let Some(parents) = self.role_parents.get(role_name) {
            for parent in parents {
                let base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.roles.contains_key(base) && to_hide.insert(base.to_string()) {
                    self.collect_hidden_roles(base, to_hide);
                }
            }
        }
    }

    /// MRO without hidden classes (no roles)
    pub(super) fn classhow_mro_unhidden_names(&mut self, invocant: &Value) -> Vec<String> {
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };
        let mro = self.classhow_mro_names(invocant);
        let hidden_parents: HashSet<String> = self
            .hidden_defer_parents
            .get(&class_name)
            .cloned()
            .unwrap_or_default();
        let mut hidden_set: HashSet<String> = HashSet::new();
        for hp in &hidden_parents {
            hidden_set.insert(hp.clone());
        }
        for hc in &self.hidden_classes {
            hidden_set.insert(hc.clone());
        }
        if hidden_set.is_empty() {
            return mro;
        }
        mro.into_iter()
            .filter(|name| !hidden_set.contains(name))
            .collect()
    }

    pub(super) fn package_looks_like_grammar(&self, package_name: &str) -> bool {
        let prefix = format!("{package_name}::");
        self.token_defs.keys().any(|key| key.starts_with(&prefix))
    }

    pub(super) fn dispatch_classhow_methods(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let invocant = &args[0];
        let class_name = match invocant {
            Value::Package(name) => name.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            other => value_type_name(other).to_string(),
        };

        // Parse named arguments
        let mut local = false;
        let mut all = false;
        let mut private = false;
        let mut tree = false;
        for arg in &args[1..] {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "local" => local = value.truthy(),
                    "all" => all = value.truthy(),
                    "private" => private = value.truthy(),
                    "tree" => tree = value.truthy(),
                    _ => {}
                }
            }
        }

        if tree {
            return self.classhow_methods_tree(&class_name, private);
        }

        let mut result = Vec::new();

        if local {
            // Only methods defined directly on this class
            self.collect_class_methods(&class_name, private, &mut result);
        } else {
            // Walk MRO (already includes the class itself)
            let mro = self.class_mro(&class_name);

            for cn in &mro {
                if !all && (cn == "Any" || cn == "Mu") {
                    continue;
                }
                self.collect_class_methods(cn, private, &mut result);
            }

            // For built-in types that don't have class defs, add known methods
            if result.is_empty() || !self.classes.contains_key(&class_name) {
                self.collect_builtin_type_methods(&class_name, &mut result);
                if all {
                    self.collect_builtin_type_methods("Any", &mut result);
                    self.collect_builtin_type_methods("Mu", &mut result);
                }
            }
        }

        Ok(Value::array(result))
    }

    pub(super) fn collect_builtin_type_methods(&self, type_name: &str, result: &mut Vec<Value>) {
        let methods: &[&str] = match type_name {
            "Str" => &[
                "chars",
                "codes",
                "comb",
                "chomp",
                "chop",
                "contains",
                "ends-with",
                "fc",
                "flip",
                "index",
                "indices",
                "lc",
                "lines",
                "match",
                "ords",
                "pred",
                "rindex",
                "split",
                "starts-with",
                "substr",
                "succ",
                "tc",
                "trim",
                "trim-leading",
                "trim-trailing",
                "uc",
                "words",
                "wordcase",
                "NFC",
                "NFD",
                "NFKC",
                "NFKD",
                "encode",
                "IO",
                "Numeric",
                "Int",
                "Num",
                "Rat",
                "Bool",
                "Str",
                "gist",
                "raku",
                "elems",
                "fmt",
            ],
            "Int" | "Num" | "Rat" | "Complex" => &[
                "abs", "ceiling", "floor", "round", "sign", "sqrt", "log", "log10", "exp", "roots",
                "is-prime", "chr", "base", "polymod", "pred", "succ", "Numeric", "Int", "Num",
                "Rat", "Bool", "Str", "gist", "raku",
            ],
            "List" | "Array" => &[
                "elems",
                "end",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "join",
                "map",
                "grep",
                "first",
                "sort",
                "reverse",
                "rotate",
                "unique",
                "repeated",
                "squish",
                "flat",
                "eager",
                "lazy",
                "head",
                "tail",
                "skip",
                "push",
                "pop",
                "shift",
                "unshift",
                "splice",
                "append",
                "prepend",
                "classify",
                "categorize",
                "min",
                "max",
                "minmax",
                "sum",
                "pick",
                "roll",
                "permutations",
                "combinations",
                "rotor",
                "batch",
                "produce",
                "reduce",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
                "Array",
                "List",
            ],
            "Hash" => &[
                "elems",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "push",
                "append",
                "classify-list",
                "categorize-list",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
            ],
            "Bool" => &[
                "pred", "succ", "pick", "roll", "Numeric", "Int", "Num", "Rat", "Bool", "Str",
                "gist", "raku",
            ],
            "Range" => &[
                "min", "max", "bounds", "elems", "list", "flat", "reverse", "pick", "roll", "sum",
                "rand", "minmax", "infinite", "is-int", "Bool", "Str", "gist", "raku", "Numeric",
                "Int",
            ],
            "Sub" | "Method" | "Block" | "Routine" | "Code" => &[
                "name",
                "signature",
                "arity",
                "count",
                "of",
                "returns",
                "Bool",
                "Str",
                "gist",
                "raku",
            ],
            "Signature" => &[
                "params", "arity", "count", "returns", "Bool", "Str", "gist", "raku",
            ],
            "Any" => &[
                "say",
                "put",
                "print",
                "note",
                "so",
                "not",
                "defined",
                "WHAT",
                "WHERE",
                "HOW",
                "WHY",
                "iterator",
                "flat",
                "eager",
                "lazy",
                "map",
                "grep",
                "first",
                "sort",
                "reverse",
                "unique",
                "repeated",
                "squish",
                "head",
                "tail",
                "skip",
                "min",
                "max",
                "minmax",
                "elems",
                "end",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "classify",
                "categorize",
                "join",
                "pick",
                "roll",
                "sum",
                "reduce",
                "produce",
                "rotor",
                "batch",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
            ],
            "Mu" => &[
                "defined", "WHAT", "WHERE", "HOW", "WHY", "WHICH", "Bool", "Str", "gist", "raku",
                "clone", "new",
            ],
            _ => &[],
        };
        for name in methods {
            if !result.iter().any(|v| {
                if let Value::Instance { attributes, .. } = v {
                    attributes
                        .get("name")
                        .map(|n| n.to_string_value())
                        .as_deref()
                        == Some(name)
                } else {
                    false
                }
            }) {
                result.push(self.make_native_method_object(name));
            }
        }
    }

    pub(super) fn collect_class_methods(
        &self,
        class_name: &str,
        include_private: bool,
        result: &mut Vec<Value>,
    ) {
        if let Some(class_def) = self.classes.get(class_name) {
            // First add accessor methods for public attributes (in order)
            for (attr_name, is_public, ..) in &class_def.attributes {
                if *is_public && !class_def.methods.contains_key(attr_name) {
                    result.push(self.make_native_method_object(attr_name));
                }
            }
            // Then add explicit methods
            for (method_name, overloads) in &class_def.methods {
                if overloads.is_empty() {
                    continue;
                }
                // Skip private methods unless :private
                let first = &overloads[0];
                if first.is_private && !include_private {
                    continue;
                }
                let is_multi = overloads.len() > 1;
                let return_type = first.return_type.clone();
                let method_obj = self.make_method_object(method_name, first, is_multi, return_type);
                result.push(method_obj);
            }
            // Also include native (built-in) methods
            for native_name in &class_def.native_methods {
                let method_obj = self.make_native_method_object(native_name);
                result.push(method_obj);
            }
        }
    }

    pub(super) fn make_native_method_object(&self, name: &str) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::Str(name.to_string()));
        attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
        let sig_attrs = {
            let mut sa = std::collections::HashMap::new();
            sa.insert("params".to_string(), Value::array(Vec::new()));
            sa
        };
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Signature"), sig_attrs),
        );
        attrs.insert("returns".to_string(), Value::Package(Symbol::intern("Mu")));
        attrs.insert("of".to_string(), Value::Package(Symbol::intern("Mu")));
        Value::make_instance(Symbol::intern("Method"), attrs)
    }

    pub(super) fn make_method_object(
        &self,
        name: &str,
        method_def: &MethodDef,
        is_dispatcher: bool,
        return_type: Option<String>,
    ) -> Value {
        let mut attrs = std::collections::HashMap::new();

        // Store the display name (with ! prefix for private methods)
        let display_name = if method_def.is_private {
            format!("!{}", name)
        } else {
            name.to_string()
        };
        attrs.insert("name".to_string(), Value::Str(display_name));
        attrs.insert("is_dispatcher".to_string(), Value::Bool(is_dispatcher));

        // Build a Signature object for this method
        let param_defs = &method_def.param_defs;
        let sig_attrs = {
            let mut sa = std::collections::HashMap::new();
            sa.insert(
                "params".to_string(),
                make_params_value_from_param_defs(param_defs),
            );
            sa
        };
        attrs.insert(
            "signature".to_string(),
            Value::make_instance(Symbol::intern("Signature"), sig_attrs),
        );

        // Return type
        let rt = return_type.unwrap_or_else(|| "Mu".to_string());
        attrs.insert("returns".to_string(), Value::Package(Symbol::intern(&rt)));
        attrs.insert("of".to_string(), Value::Package(Symbol::intern(&rt)));

        Value::make_instance(Symbol::intern("Method"), attrs)
    }

    pub(super) fn classhow_methods_tree(
        &self,
        class_name: &str,
        include_private: bool,
    ) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();

        // First: own methods
        self.collect_class_methods(class_name, include_private, &mut result);

        // Then: each parent's tree as a nested array
        if let Some(class_def) = self.classes.get(class_name) {
            for parent in &class_def.parents {
                let subtree = self.classhow_methods_tree(parent, include_private)?;
                result.push(subtree);
            }
        }

        Ok(Value::array(result))
    }
}
