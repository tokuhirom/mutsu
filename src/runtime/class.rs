use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn run_pending_instance_destroys(&mut self) -> Result<(), RuntimeError> {
        let pending = take_pending_instance_destroys();
        for item in pending {
            let instance_class = item.class_name.resolve();
            // Collect the MRO so we call DESTROY on each class in order (child → parent).
            let mro: Vec<String> = self
                .classes
                .get(&instance_class)
                .map(|cd| cd.mro.clone())
                .unwrap_or_default();
            // Walk the MRO; submethods are per-class, not inherited.
            for mro_class in &mro {
                let Some(class_def) = self.classes.get(mro_class) else {
                    continue;
                };
                let Some(overloads) = class_def.methods.get("DESTROY").cloned() else {
                    continue;
                };
                let Some(method_def) = overloads.into_iter().find(|def| {
                    def.is_my && !def.is_private && self.method_args_match(&[], &def.param_defs)
                }) else {
                    continue;
                };
                let invocant =
                    Value::make_instance_without_destroy(item.class_name, item.attributes.clone());
                let _ = self.run_instance_method_resolved(
                    mro_class,
                    &instance_class,
                    method_def,
                    item.attributes.clone(),
                    Vec::new(),
                    Some(invocant),
                )?;
            }
        }
        Ok(())
    }

    pub(super) fn detect_unresolved_role_method_conflicts(
        &self,
        class_name: &str,
        class_def: &ClassDef,
    ) -> Result<(), RuntimeError> {
        for (method_name, defs) in &class_def.methods {
            // Check non-multi methods
            let non_multi: Vec<&MethodDef> = defs
                .iter()
                .filter(|d| !d.is_multi && !Self::is_stub_routine_body(&d.body))
                .collect();
            let class_defined_non_multi = non_multi.iter().any(|d| d.role_origin.is_none());
            if !class_defined_non_multi {
                let mut conflicting_roles = Vec::new();
                let mut seen_origins = Vec::new();
                for def in &non_multi {
                    let Some(role_name) = &def.role_origin else {
                        continue;
                    };
                    // Use original_role for diamond detection: if two methods
                    // trace back to the same original role, they are not in conflict.
                    let origin = def.original_role.as_ref().unwrap_or(role_name);
                    if seen_origins.contains(origin) {
                        continue;
                    }
                    seen_origins.push(origin.clone());
                    if !conflicting_roles.contains(role_name) {
                        conflicting_roles.push(role_name.clone());
                    }
                }
                if conflicting_roles.len() > 1 {
                    conflicting_roles.reverse();
                    return Err(RuntimeError::new(format!(
                        "X::Role::Composition::Conflict: Method '{}' must be resolved by class {} because it exists in multiple roles ({})",
                        method_name,
                        class_name,
                        conflicting_roles.join(", "),
                    )));
                }
            }

            // Check multi methods: detect per-signature conflicts from different roles
            let multi_defs: Vec<&MethodDef> = defs
                .iter()
                .filter(|d| d.is_multi && !Self::is_stub_routine_body(&d.body))
                .collect();
            if multi_defs.len() > 1 {
                // Group by signature, check if any signature has methods from multiple roles
                // without a class-provided resolution
                let mut checked: Vec<Vec<String>> = Vec::new();
                for (i, def_a) in multi_defs.iter().enumerate() {
                    let sig = Self::method_positional_signature(def_a);
                    if checked.contains(&sig) {
                        continue;
                    }
                    checked.push(sig);
                    // Find all methods with matching signature
                    let mut roles_for_sig: Vec<String> = Vec::new();
                    let mut class_resolves = def_a.role_origin.is_none();
                    if let Some(r) = &def_a.role_origin
                        && !roles_for_sig.contains(r)
                    {
                        roles_for_sig.push(r.clone());
                    }
                    for def_b in multi_defs.iter().skip(i + 1) {
                        if Self::method_signatures_match(def_a, def_b) {
                            if def_b.role_origin.is_none() {
                                class_resolves = true;
                            }
                            if let Some(r) = &def_b.role_origin
                                && !roles_for_sig.contains(r)
                            {
                                roles_for_sig.push(r.clone());
                            }
                        }
                    }
                    if roles_for_sig.len() > 1 && !class_resolves {
                        return Err(RuntimeError::new(format!(
                            "X::Role::Composition::Conflict: Method '{}' must be resolved by class {} because it exists in multiple roles ({})",
                            method_name,
                            class_name,
                            roles_for_sig.join(", "),
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn compute_class_mro(
        &mut self,
        class_name: &str,
        stack: &mut Vec<String>,
    ) -> Result<Vec<String>, RuntimeError> {
        if stack.iter().any(|name| name == class_name) {
            return Err(RuntimeError::new(format!(
                "C3 MRO cycle detected at {}",
                class_name
            )));
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return Ok(class_def.mro.clone());
        }
        stack.push(class_name.to_string());
        let explicit_parents = self
            .classes
            .get(class_name)
            .map(|c| c.parents.clone())
            .unwrap_or_default();
        // If a user-defined class has no explicit parents, it implicitly
        // inherits from Any (which in turn inherits from Mu).  This matches
        // Raku's default class hierarchy.
        let parents = if explicit_parents.is_empty() && self.classes.contains_key(class_name) {
            vec!["Any".to_string()]
        } else {
            explicit_parents
        };
        let mut seqs: Vec<Vec<String>> = Vec::new();
        for parent in &parents {
            if self.classes.contains_key(parent) {
                let mro = self.compute_class_mro(parent, stack)?;
                seqs.push(mro);
            } else if parent == "Any" {
                // Any implicitly inherits from Mu
                seqs.push(vec!["Any".to_string(), "Mu".to_string()]);
            } else if parent == "Cool" {
                seqs.push(vec![
                    "Cool".to_string(),
                    "Any".to_string(),
                    "Mu".to_string(),
                ]);
            } else {
                seqs.push(vec![parent.clone()]);
            }
        }
        seqs.push(parents.clone());
        let mut result = vec![class_name.to_string()];
        while seqs.iter().any(|s| !s.is_empty()) {
            let mut candidate = None;
            for seq in &seqs {
                if seq.is_empty() {
                    continue;
                }
                let head = &seq[0];
                let mut in_tail = false;
                for other in &seqs {
                    if other.len() > 1 && other[1..].contains(head) {
                        in_tail = true;
                        break;
                    }
                }
                if !in_tail {
                    candidate = Some(head.clone());
                    break;
                }
            }
            if let Some(head) = candidate {
                result.push(head.clone());
                for seq in seqs.iter_mut() {
                    if !seq.is_empty() && seq[0] == head {
                        seq.remove(0);
                    }
                }
            } else {
                stack.pop();
                return Err(RuntimeError::new(format!(
                    "Inconsistent class hierarchy for {}",
                    class_name
                )));
            }
        }
        stack.pop();
        Ok(result)
    }

    pub(super) fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && (class_def.methods.contains_key(method_name)
                    || class_def.native_methods.contains(method_name))
            {
                return true;
            }
        }
        false
    }

    /// Check whether the class (or its MRO ancestors) has a `new` method
    /// variant with a non-named positional parameter whose type matches the
    /// given value.  This is used by the coercion fallback: only try `new`
    /// when there is an explicit `new(TargetType:U: ValueType $x)` multi.
    pub(super) fn class_has_new_accepting_positional(
        &mut self,
        class_name: &str,
        value: &Value,
    ) -> bool {
        let mro = self.class_mro(class_name);
        for cn in &mro {
            let methods = match self.classes.get(cn.as_str()) {
                Some(cd) => cd.methods.get("new").cloned(),
                None => None,
            };
            if let Some(overloads) = methods {
                for method in &overloads {
                    // Look for a positional (non-named, non-invocant) param
                    // that type-matches the value.
                    let has_matching_positional = method.param_defs.iter().any(|pd| {
                        !pd.named
                            && !pd.is_invocant
                            && pd
                                .type_constraint
                                .as_deref()
                                .is_some_and(|tc| self.type_matches_value(tc, value))
                    });
                    if has_matching_positional {
                        return true;
                    }
                }
            }
        }
        false
    }

    pub(crate) fn is_native_method(&mut self, class_name: &str, method_name: &str) -> bool {
        // IO::Handle has native methods handled by native_io_handle
        if class_name == "IO::Handle"
            && matches!(
                method_name,
                "DESTROY"
                    | "path"
                    | "IO"
                    | "Str"
                    | "gist"
                    | "open"
                    | "nl-out"
                    | "nl-in"
                    | "chomp"
                    | "print-nl"
                    | "close"
                    | "get"
                    | "getc"
                    | "readchars"
                    | "lines"
                    | "words"
                    | "read"
                    | "write"
                    | "print"
                    | "say"
                    | "put"
                    | "flush"
                    | "out-buffer"
                    | "seek"
                    | "tell"
                    | "eof"
                    | "encoding"
                    | "opened"
                    | "slurp"
                    | "Supply"
                    | "native-descriptor"
                    | "spurt"
            )
        {
            return true;
        }
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && class_def.native_methods.contains(method_name)
            {
                return true;
            }
        }
        false
    }

    pub(crate) fn has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && let Some(defs) = class_def.methods.get(method_name)
            {
                return defs.iter().any(|d| !d.is_private);
            }
        }
        false
    }

    /// Check if a class has a public attribute accessor for the given name.
    pub(crate) fn has_public_accessor(&mut self, class_name: &str, method_name: &str) -> bool {
        let attrs = self.collect_class_attributes(class_name);
        attrs
            .iter()
            .any(|(attr_name, is_public, ..)| *is_public && attr_name == method_name)
    }

    /// Look up a class-level attribute (declared with `our $.x` or `my $.x`).
    /// Searches the class and its MRO.
    pub(crate) fn get_class_level_attr(
        &mut self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<Value> {
        // Check own class first
        if let Some(class_def) = self.classes.get(class_name)
            && let Some(val) = class_def.class_level_attrs.get(attr_name)
        {
            return Some(val.clone());
        }
        // Walk MRO for inherited class-level attributes
        let mro = self.class_mro(class_name);
        for parent in &mro {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.classes.get(parent)
                && let Some(val) = parent_def.class_level_attrs.get(attr_name)
            {
                return Some(val.clone());
            }
        }
        None
    }

    /// Check if a class (or its MRO) has a class-level attribute.
    pub(crate) fn has_class_level_attr(&mut self, class_name: &str, attr_name: &str) -> bool {
        self.get_class_level_attr(class_name, attr_name).is_some()
    }

    /// Set a class-level attribute value. Searches the class and its MRO to find
    /// where the attribute is defined, then updates it.
    pub(crate) fn set_class_level_attr(
        &mut self,
        class_name: &str,
        attr_name: &str,
        value: Value,
    ) -> bool {
        // Check own class first
        if let Some(class_def) = self.classes.get_mut(class_name)
            && class_def.class_level_attrs.contains_key(attr_name)
        {
            class_def
                .class_level_attrs
                .insert(attr_name.to_string(), value);
            return true;
        }
        // Walk MRO
        let mro = self.class_mro(class_name);
        for parent in &mro {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.classes.get_mut(parent)
                && parent_def.class_level_attrs.contains_key(attr_name)
            {
                parent_def
                    .class_level_attrs
                    .insert(attr_name.to_string(), value);
                return true;
            }
        }
        false
    }

    /// Collect wildcard-handles attribute var names from the class and its MRO.
    pub(super) fn collect_wildcard_handles(&mut self, class_name: &str) -> Vec<String> {
        let mro = self.class_mro(class_name);
        let mut result = Vec::new();
        for cn in &mro {
            if let Some(class_def) = self.classes.get(cn) {
                result.extend(class_def.wildcard_handles.iter().cloned());
            }
        }
        result
    }

    /// Add `__mutsu_attr_alias::x` metadata for attributes declared with `has $x`
    /// (no twigil), so the method call dispatch can set up bidirectional aliases.
    pub(super) fn add_alias_attribute_metadata(
        &mut self,
        class_name: &str,
        attrs: &mut HashMap<String, Value>,
    ) {
        let mro = self.class_mro(class_name);
        for cn in &mro {
            if let Some(class_def) = self.classes.get(cn) {
                for attr_name in &class_def.alias_attributes {
                    attrs.insert(
                        format!("__mutsu_attr_alias::{}", attr_name),
                        Value::str(attr_name.to_string()),
                    );
                }
            }
        }
    }

    pub(super) fn collect_class_attributes(&mut self, class_name: &str) -> Vec<ClassAttributeDef> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        for cn in mro.iter().rev() {
            if let Some(class_def) = self.classes.get(cn) {
                for attr in &class_def.attributes {
                    if let Some(pos) = attrs.iter().position(|(n, ..)| n == &attr.0) {
                        attrs.remove(pos);
                    }
                    attrs.push(attr.clone());
                }
            }
        }
        attrs
    }

    pub(crate) fn run_instance_method(
        &mut self,
        receiver_class_name: &str,
        attributes: HashMap<String, Value>,
        method_name: &str,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        let Some((owner_class, method_def)) =
            self.resolve_method_with_owner(receiver_class_name, method_name, &args)
        else {
            return Err(super::methods_signature::make_multi_no_match_error(
                method_name,
            ));
        };
        let all_candidates =
            self.resolve_all_methods_with_owner(receiver_class_name, method_name, &args);
        let invocant_for_dispatch = if let Some(inv) = &invocant {
            inv.clone()
        } else if attributes.is_empty() {
            Value::Package(Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
        };
        let remaining: Vec<(String, MethodDef)> = all_candidates
            .into_iter()
            .skip(1)
            .filter(|(candidate_owner, _)| {
                !self.should_skip_defer_method_candidate(receiver_class_name, candidate_owner)
            })
            .collect();
        let pushed_dispatch = !remaining.is_empty();
        self.samewith_context_stack
            .push((method_name.to_string(), Some(invocant_for_dispatch.clone())));
        if pushed_dispatch {
            self.method_dispatch_stack.push(MethodDispatchFrame {
                receiver_class: receiver_class_name.to_string(),
                invocant: invocant_for_dispatch,
                args: args.clone(),
                remaining,
            });
        }
        let result = self.run_instance_method_resolved(
            receiver_class_name,
            &owner_class,
            method_def,
            attributes,
            args,
            invocant,
        );
        self.samewith_context_stack.pop();
        if pushed_dispatch {
            self.method_dispatch_stack.pop();
        }
        result
    }

    pub(super) fn run_instance_method_resolved(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_def: MethodDef,
        mut attributes: HashMap<String, Value>,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        // Fast path: delegation methods forward the call to the delegate object
        if let Some((ref attr_var_name, ref target_method)) = method_def.delegation {
            let attr_key = attr_var_name
                .trim_start_matches('.')
                .trim_start_matches('!');
            let delegate = attributes.get(attr_key).cloned().unwrap_or(Value::Nil);
            if delegate == Value::Nil {
                return Err(RuntimeError::new(format!(
                    "No such method '{}' for invocant of type '{}'",
                    target_method, receiver_class_name
                )));
            }
            let delegate_id = match &delegate {
                Value::Instance { id, .. } => Some(*id),
                _ => None,
            };
            let delegate_class = match &delegate {
                Value::Instance { class_name, .. } => Some(*class_name),
                _ => None,
            };
            let result = self.call_method_with_values(delegate, target_method, args)?;
            // For Instance delegates, check if the delegate was mutated and update
            // the frontend's attribute with the updated delegate.
            if let (Some(did), Some(dcn)) = (delegate_id, delegate_class) {
                // Look for the updated delegate in env bindings
                let mut updated_delegate = None;
                for val in self.env.values() {
                    if let Value::Instance { class_name, id, .. } = val
                        && *class_name == dcn
                        && *id == did
                    {
                        updated_delegate = Some(val.clone());
                        break;
                    }
                }
                if let Some(updated) = updated_delegate {
                    let mut attrs = attributes;
                    attrs.insert(attr_key.to_string(), updated);
                    return Ok((result, attrs));
                }
            }
            return Ok((result, attributes));
        }
        // For type-object calls (no attributes), use Package so self.new works
        let mut base = if let Some(invocant) = invocant {
            invocant
        } else if attributes.is_empty() {
            Value::Package(Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
        };
        let saved_env = self.env.clone();
        let saved_var_bindings = self.var_bindings.clone();
        let saved_readonly = self.save_readonly_vars();
        self.method_class_stack.push(owner_class.to_string());
        let role_context = if self.roles.contains_key(owner_class) {
            Some(owner_class.to_string())
        } else {
            method_def.role_origin.clone()
        };
        // Set ::?CLASS / ::?ROLE compile-time variable for the method body
        self.env.insert(
            "?CLASS".to_string(),
            Value::Package(Symbol::intern(owner_class)),
        );
        if let Some(role_name) = role_context {
            self.env.insert(
                "?ROLE".to_string(),
                Value::Package(Symbol::intern(&role_name)),
            );
        } else {
            self.env.remove("?ROLE");
        }
        self.env.insert("self".to_string(), base.clone());
        // Also set __ANON_STATE__ for `$.foo` compound-assignment desugaring
        self.env.insert("__ANON_STATE__".to_string(), base.clone());
        // In Raku, methods do NOT set $_ to the invocant by default.
        // $_ in a method body is Any unless the invocant is explicitly named $_
        // (e.g. `method foo ($_: ) { ... }`). The invocant binding loop below
        // will set $_ back to self if the invocant param is named "_".
        self.env
            .insert("_".to_string(), Value::Package(Symbol::intern("Any")));
        if let Some(role_bindings) = self.class_role_param_bindings.get(owner_class) {
            for (name, value) in role_bindings {
                self.env.insert(name.clone(), value.clone());
            }
        } else if let Some(role_bindings) = self.class_role_param_bindings.get(receiver_class_name)
        {
            for (name, value) in role_bindings {
                self.env.insert(name.clone(), value.clone());
            }
        }

        let mut bind_params = Vec::new();
        let mut bind_param_defs = Vec::new();
        for (idx, param_name) in method_def.params.iter().enumerate() {
            let is_invocant = method_def
                .param_defs
                .get(idx)
                .map(|pd| pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                .unwrap_or(false);
            if is_invocant {
                if let Some(pd) = method_def.param_defs.get(idx)
                    && let Some(constraint) = &pd.type_constraint
                {
                    if let Some(captured_name) = constraint.strip_prefix("::") {
                        self.env
                            .insert(captured_name.to_string(), Self::captured_type_object(&base));
                    } else {
                        let coercion_target = if let Some(open) = constraint.find('(') {
                            if constraint.ends_with(')') && open > 0 {
                                Some(&constraint[..open])
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        let expected = coercion_target.unwrap_or(constraint.as_str());
                        if coercion_target.is_some() {
                            let mut candidate = self
                                .try_coerce_value_for_constraint(constraint, base.clone())
                                .unwrap_or_else(|_| base.clone());
                            if !self.type_matches_value(expected, &candidate)
                                && let Ok(coerced) =
                                    self.call_method_with_values(base.clone(), expected, vec![])
                            {
                                candidate = coerced;
                            }
                            if self.type_matches_value(expected, &candidate) {
                                base = candidate;
                                self.env.insert("self".to_string(), base.clone());
                            }
                        } else if !self.type_matches_value(constraint, &base)
                            && let Ok(coerced) =
                                self.try_coerce_value_for_constraint(constraint, base.clone())
                        {
                            base = coerced;
                            self.env.insert("self".to_string(), base.clone());
                        }
                        if !self.type_matches_value(expected, &base) {
                            self.method_class_stack.pop();
                            self.env = saved_env;
                            self.var_bindings = saved_var_bindings;
                            self.restore_readonly_vars(saved_readonly);
                            return Err(RuntimeError::new(format!(
                                "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter '{}'; expected {}, got {}",
                                param_name,
                                constraint,
                                super::value_type_name(&base)
                            )));
                        }
                    }
                }
                self.env.insert(param_name.clone(), base.clone());
                continue;
            }
            bind_params.push(param_name.clone());
            if let Some(pd) = method_def.param_defs.get(idx) {
                bind_param_defs.push(pd.clone());
            }
        }

        for (attr_name, attr_val) in &attributes {
            self.env.insert(format!("!{}", attr_name), attr_val.clone());
            self.env.insert(format!(".{}", attr_name), attr_val.clone());
            self.var_bindings
                .insert(attr_name.clone(), format!("!{}", attr_name));
            self.var_bindings.insert(
                format!("{}::{}", owner_class, attr_name),
                format!("!{}", attr_name),
            );
        }
        // Method signatures must support full parameter binding semantics
        // (coercions, slurpy params, defaults, and named args) for both
        // type-object and instance invocations.
        match self.bind_function_args_values(&bind_param_defs, &bind_params, &args) {
            Ok(_) => {}
            Err(e) => {
                self.method_class_stack.pop();
                self.env = saved_env;
                self.var_bindings = saved_var_bindings;
                self.restore_readonly_vars(saved_readonly);
                return Err(e);
            }
        }
        for p in &bind_params {
            self.var_bindings.remove(p);
        }
        // When the method body is re-compiled by run_block, the compiler
        // qualifies bare variable names with current_package (e.g. "m" →
        // "G::m").  Mirror bound params under their qualified names so the
        // generated GetGlobal lookup succeeds.
        let pkg = self.current_package.clone();
        if pkg != "GLOBAL" {
            for p in &bind_params {
                if !p.contains("::")
                    && !p.starts_with('_')
                    && !p.starts_with('/')
                    && !p.starts_with('!')
                    && !p.starts_with('?')
                    && !p.starts_with('*')
                    && !p.starts_with('.')
                    && !p.starts_with('=')
                    && !p.starts_with('$')
                    && !p.starts_with('@')
                    && !p.starts_with('%')
                    && !p.starts_with('&')
                    && let Some(v) = self.env.get(p).cloned()
                {
                    self.env.insert(format!("{}::{}", pkg, p), v);
                }
            }
        }
        // Push onto routine_stack so that `return` inside the method body
        // compiles as `Return` (not `ReturnFromNonRoutine`).
        self.routine_stack
            .push((owner_class.to_string(), String::new()));
        let block_result = self.run_block(&method_def.body);
        self.routine_stack.pop();
        let implicit_return = self.env.get("_").cloned();
        let result = match block_result {
            Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
        };
        // Apply return type spec (e.g. `--> 5` returns literal 5 from empty body)
        let result = if let Some(ref return_spec) = method_def.return_type {
            self.finalize_return_with_spec(result, Some(return_spec.as_str()))
        } else {
            result
        };
        for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
            let original = attributes.get(&attr_name).cloned().unwrap_or(Value::Nil);
            let env_key = format!("!{}", attr_name);
            let public_env_key = format!(".{}", attr_name);
            let env_private = self.env.get(&env_key).cloned();
            let env_public = self.env.get(&public_env_key).cloned();
            if let (Some(private_val), Some(public_val)) = (&env_private, &env_public) {
                // `$.attr` aliases (public) should still write back when only the
                // public mirror changed (e.g. `$.count++`).
                if *private_val == original && *public_val != original {
                    attributes.insert(attr_name, public_val.clone());
                } else {
                    attributes.insert(attr_name, private_val.clone());
                }
                continue;
            }
            if let Some(val) = self.env.get(&env_key) {
                attributes.insert(attr_name, val.clone());
                continue;
            }
            if let Some(val) = self.env.get(&public_env_key) {
                attributes.insert(attr_name, val.clone());
            }
        }
        let mut merged_env = saved_env.clone();
        for (k, v) in self.env.iter() {
            // $_ is method-local; don't bleed the method's $_ back to the caller
            if k == "_" {
                continue;
            }
            if saved_env.contains_key(k) {
                merged_env.insert(k.clone(), v.clone());
            }
            if (k.starts_with('&') && !k.starts_with("&?"))
                || k.starts_with("__mutsu_method_value::")
            {
                merged_env.insert(k.clone(), v.clone());
            }
        }
        self.method_class_stack.pop();
        self.env = merged_env;
        self.var_bindings = saved_var_bindings;
        self.restore_readonly_vars(saved_readonly);
        result.map(|v| {
            let adjusted = match (&base, &v) {
                (
                    Value::Instance {
                        class_name,
                        id: base_id,
                        ..
                    },
                    Value::Instance { id: ret_id, .. },
                ) if base_id == ret_id => {
                    Value::make_instance_with_id(*class_name, attributes.clone(), *base_id)
                }
                _ => v,
            };
            (adjusted, attributes)
        })
    }
}
