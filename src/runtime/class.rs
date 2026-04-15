use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn run_pending_instance_destroys(&mut self) -> Result<(), RuntimeError> {
        let pending = take_pending_instance_destroys();
        if pending.is_empty() {
            return Ok(());
        }
        let is_6e = crate::parser::current_language_version().starts_with("6.e");
        // Set reentrancy guard to prevent infinite DESTROY recursion:
        // instances created during DESTROY execution should not queue new DESTROYs.
        crate::value::set_in_destroy_handler(true);
        let result = self.run_pending_instance_destroys_inner(&pending, is_6e);
        crate::value::set_in_destroy_handler(false);
        result
    }

    fn run_pending_instance_destroys_inner(
        &mut self,
        pending: &[crate::value::PendingInstanceDestroy],
        is_6e: bool,
    ) -> Result<(), RuntimeError> {
        for item in pending {
            let instance_class = item.class_name.resolve();
            // Collect the MRO so we call DESTROY on each class in order (child → parent).
            let mro: Vec<String> = self
                .classes
                .get(&instance_class)
                .map(|cd| cd.mro.clone())
                .unwrap_or_default();
            // Track attributes across DESTROY calls so mutations are visible
            let mut current_attrs = item.attributes.clone();
            // Walk the MRO; submethods are per-class, not inherited.
            for mro_class in &mro {
                // Skip role entries in MRO
                if self.roles.contains_key(mro_class) && !self.classes.contains_key(mro_class) {
                    continue;
                }
                let Some(class_def) = self.classes.get(mro_class) else {
                    continue;
                };
                // Call class's own DESTROY submethod
                if let Some(overloads) = class_def.methods.get("DESTROY").cloned()
                    && let Some(method_def) = overloads.into_iter().find(|def| {
                        def.is_my && !def.is_private && self.method_args_match(&[], &def.param_defs)
                    })
                {
                    let invocant = Value::make_instance_without_destroy(
                        item.class_name,
                        current_attrs.clone(),
                    );
                    if let Ok((_v, updated)) = self.run_instance_method_resolved(
                        &instance_class,
                        mro_class,
                        method_def,
                        current_attrs.clone(),
                        Vec::new(),
                        Some(invocant),
                    ) {
                        current_attrs = updated;
                    }
                }
                // Under v6.e+, call DESTROY submethods from composed roles
                // (in reverse order: role submethods after the class's own DESTROY)
                if is_6e {
                    let role_order = self.ordered_role_submethods_for_class(mro_class, "DESTROY");
                    // DESTROY order is reverse of BUILD: role submethods after class
                    for (role_name, method_def) in role_order.into_iter().rev() {
                        let invocant = Value::make_instance_without_destroy(
                            item.class_name,
                            current_attrs.clone(),
                        );
                        if let Ok((_v, updated)) = self.run_instance_method_resolved(
                            &instance_class,
                            &role_name,
                            method_def,
                            current_attrs.clone(),
                            Vec::new(),
                            Some(invocant),
                        ) {
                            current_attrs = updated;
                        }
                    }
                }
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
        // IO::Pipe has native methods handled by native_io_pipe
        if class_name == "IO::Pipe"
            && matches!(
                method_name,
                "slurp"
                    | "Str"
                    | "gist"
                    | "encoding"
                    | "close"
                    | "split"
                    | "print"
                    | "get"
                    | "lines"
                    | "eof"
            )
        {
            return true;
        }
        // IO::Special has native methods handled by native_io_special
        if class_name == "IO::Special"
            && matches!(
                method_name,
                "Str"
                    | "gist"
                    | "what"
                    | "IO"
                    | "e"
                    | "d"
                    | "f"
                    | "l"
                    | "x"
                    | "s"
                    | "r"
                    | "w"
                    | "modified"
                    | "accessed"
                    | "changed"
                    | "mode"
                    | "raku"
                    | "perl"
                    | "WHICH"
                    | "new"
                    | "Bool"
                    | "defined"
            )
        {
            return true;
        }
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
                    | "t"
                    | "printf"
                    | "split"
                    | "comb"
            )
        {
            return true;
        }
        // IO::Path's comb reads file content and combs the result.
        if class_name == "IO::Path" && method_name == "comb" {
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

    /// Collect per-class attributes for all classes in the MRO.
    /// Returns `(declaring_class, ClassAttributeDef)` pairs.
    /// Unlike `collect_class_attributes`, this does NOT deduplicate by name —
    /// if Parent and Child both declare an attribute with the same name,
    /// both entries are returned. Used to initialize class-qualified attribute
    /// storage so that each class has its own private copy.
    pub(super) fn collect_per_class_attrs(
        &mut self,
        class_name: &str,
    ) -> Vec<(String, ClassAttributeDef)> {
        let mro = self.class_mro(class_name);
        let mut result: Vec<(String, ClassAttributeDef)> = Vec::new();
        // Track which attribute names appear in multiple classes (need qualified storage)
        let mut attr_counts: HashMap<String, usize> = HashMap::new();
        for cn in &mro {
            if let Some(class_def) = self.classes.get(cn) {
                for attr in &class_def.attributes {
                    *attr_counts.entry(attr.0.clone()).or_insert(0) += 1;
                }
            }
        }
        // Only include attrs that appear in multiple classes (duplicated across hierarchy)
        for cn in &mro {
            if let Some(class_def) = self.classes.get(cn) {
                for attr in &class_def.attributes {
                    if attr_counts.get(&attr.0).copied().unwrap_or(0) > 1 {
                        result.push((cn.clone(), attr.clone()));
                    }
                }
            }
        }
        result
    }

    /// Collect attributes from a role and all its composed parent roles.
    /// Used when the role has been punned (instantiated via mixin) and we need
    /// to check attribute metadata (e.g. `is rw`).
    pub(super) fn collect_role_attributes_for_class(
        &self,
        role_name: &str,
    ) -> Vec<ClassAttributeDef> {
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        if let Some(role) = self.roles.get(role_name) {
            attrs.extend(role.attributes.clone());
        }
        if let Some(parent_names) = self.role_parents.get(role_name) {
            let mut role_stack: Vec<String> = parent_names.clone();
            let mut visited = vec![role_name.to_string()];
            while let Some(parent_role_name) = role_stack.pop() {
                if !visited.contains(&parent_role_name) {
                    visited.push(parent_role_name.clone());
                    if let Some(parent_role) = self.roles.get(&parent_role_name) {
                        for attr in &parent_role.attributes {
                            if !attrs.iter().any(|a| a.0 == attr.0) {
                                attrs.push(attr.clone());
                            }
                        }
                    }
                    if let Some(grandparents) = self.role_parents.get(&parent_role_name) {
                        for gp in grandparents {
                            if !visited.contains(gp) {
                                role_stack.push(gp.clone());
                            }
                        }
                    }
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
        let inv_value = if let Some(inv) = &invocant {
            inv.clone()
        } else if attributes.is_empty() {
            Value::Package(crate::symbol::Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(
                crate::symbol::Symbol::intern(receiver_class_name),
                attributes.clone(),
            )
        };
        let Some((owner_class, method_def)) = self.resolve_method_with_owner_invocant(
            receiver_class_name,
            method_name,
            &args,
            &inv_value,
        ) else {
            return Err(super::methods_signature::make_multi_no_match_error(
                method_name,
            ));
        };
        // Helper to build remaining candidates, skipping the chosen one
        let build_remaining = |this: &mut Self,
                               method_def: &MethodDef|
         -> Vec<(String, MethodDef)> {
            let all = this.resolve_all_methods_with_owner(receiver_class_name, method_name, &args);
            let chosen_fp = crate::ast::function_body_fingerprint(
                &method_def.params,
                &method_def.param_defs,
                &method_def.body,
            );
            let mut remaining = Vec::new();
            let mut skipped = false;
            for (owner, def) in all {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if !skipped && fp == chosen_fp {
                    skipped = true;
                    continue;
                }
                if this.should_skip_defer_method_candidate(receiver_class_name, &owner) {
                    continue;
                }
                remaining.push((owner, def));
            }
            remaining
        };
        let make_invocant_for_dispatch =
            |invocant: &Option<Value>, attributes: &HashMap<String, Value>| -> Value {
                if let Some(inv) = invocant {
                    inv.clone()
                } else if attributes.is_empty() {
                    Value::Package(Symbol::intern(receiver_class_name))
                } else {
                    Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
                }
            };
        // Check for method-level wrap chain on this candidate
        if !self.is_inside_wrap_dispatch()
            && let Some(cand_idx) =
                self.find_method_candidate_index(&owner_class, method_name, &method_def)
            && let Some(chain) = self
                .get_method_wrap_chain(&owner_class, method_name, cand_idx)
                .cloned()
        {
            let invocant_for_dispatch = make_invocant_for_dispatch(&invocant, &attributes);
            let remaining = build_remaining(self, &method_def);
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
            let mut orig_env = crate::env::Env::new();
            orig_env.insert(
                "__mutsu_method_wrap_original".to_string(),
                Value::Bool(true),
            );
            let original_sub = Value::make_sub(
                Symbol::intern(&owner_class),
                Symbol::intern(method_name),
                method_def.params.clone(),
                method_def.param_defs.clone(),
                (*method_def.body).clone(),
                method_def.is_rw,
                orig_env,
            );
            let outermost = chain.last().unwrap().1.clone();
            let mut wrap_remaining: Vec<Value> = Vec::new();
            for i in (0..chain.len() - 1).rev() {
                wrap_remaining.push(chain[i].1.clone());
            }
            wrap_remaining.push(original_sub);
            let mut call_args = vec![inv_value.clone()];
            call_args.extend(args);
            let frame = WrapDispatchFrame {
                sub_id: 0,
                remaining: wrap_remaining,
                args: call_args.clone(),
            };
            self.wrap_dispatch_stack.push(frame);
            let result = self.call_sub_value(outermost, call_args, false);
            self.wrap_dispatch_stack.pop();
            self.samewith_context_stack.pop();
            if pushed_dispatch {
                self.method_dispatch_stack.pop();
            }
            return result.map(|v| (v, attributes));
        }
        let invocant_for_dispatch = make_invocant_for_dispatch(&invocant, &attributes);
        let remaining = build_remaining(self, &method_def);
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
        // Check for `is DEPRECATED` trait on the method
        if let Some(ref msg) = method_def.deprecated_message {
            self.check_deprecation_for_method(method_name, &owner_class, msg);
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
        if let Some((ref attr_var_name, ref target_method)) = method_def.delegation {
            // Clear skip_pseudo_method_native: the outer call set it for the
            // delegator's own method name, but we're about to forward to a
            // different name on a (possibly different) target, so the flag
            // must not leak into the delegate dispatch.
            let saved_skip_pseudo = self.skip_pseudo_method_native.take();
            // Method-based delegation: attr_var_name starts with `&`, meaning
            // the delegate is obtained by invoking the named method on self.
            let delegate = if let Some(source_method) = attr_var_name.strip_prefix('&') {
                let invocant_val = if let Some(ref inv) = invocant {
                    inv.clone()
                } else if attributes.is_empty() {
                    Value::Package(Symbol::intern(receiver_class_name))
                } else {
                    Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
                };
                self.call_method_with_values(invocant_val, source_method, Vec::new())?
            } else {
                let attr_key = attr_var_name
                    .trim_start_matches('.')
                    .trim_start_matches('!');
                attributes.get(attr_key).cloned().unwrap_or(Value::Nil)
            };
            let is_method_based = attr_var_name.starts_with('&');
            let attr_key = attr_var_name
                .trim_start_matches('&')
                .trim_start_matches('.')
                .trim_start_matches('!');
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
            // Restore the saved skip_pseudo flag so the outer caller is unaffected.
            self.skip_pseudo_method_native = saved_skip_pseudo;
            // For Instance delegates, check if the delegate was mutated and update
            // the frontend's attribute with the updated delegate.
            if !is_method_based && let (Some(did), Some(dcn)) = (delegate_id, delegate_class) {
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
                        self.bind_type_capture(captured_name, &base);
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
                            // :D/:U smiley mismatch → X::Parameter::InvalidConcreteness
                            if constraint.ends_with(":D") || constraint.ends_with(":U") {
                                let (base_type, _) = super::types::strip_type_smiley(constraint);
                                let should_be_concrete = constraint.ends_with(":D");
                                let routine = self
                                    .samewith_context_stack
                                    .last()
                                    .map(|(name, _)| name.as_str())
                                    .unwrap_or("<method>");
                                return Err(RuntimeError::parameter_invalid_concreteness(
                                    base_type,
                                    base_type,
                                    routine,
                                    &format!("${}", param_name),
                                    should_be_concrete,
                                    pd.is_invocant,
                                ));
                            }
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
            // Skip class-qualified private attribute keys (ClassName\0attrName)
            if attr_name.contains('\0') {
                continue;
            }
            // For private attributes, prefer the class-qualified value from
            // the method's owner class (so Parent's $!priv != Child's $!priv).
            let qualified_key = format!("{}\0{}", owner_class, attr_name);
            let private_val = attributes.get(&qualified_key).unwrap_or(attr_val);
            self.env
                .insert(format!("!{}", attr_name), private_val.clone());
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
        let rw_bindings =
            match self.bind_function_args_values(&bind_param_defs, &bind_params, &args) {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.method_class_stack.pop();
                    self.env = saved_env;
                    self.var_bindings = saved_var_bindings;
                    self.restore_readonly_vars(saved_readonly);
                    return Err(e);
                }
            };
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
            Err(e) if e.return_value.is_some() && e.return_target_callable_id.is_some() => {
                // Targeted non-local return: propagate to the correct routine
                Err(e)
            }
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
        };
        // Apply return type spec (e.g. `--> 5` returns literal 5 from empty body)
        let result = if let Some(ref return_spec) = method_def.return_type {
            let effective_return_spec = self.resolved_type_capture_name(return_spec);
            self.finalize_return_with_spec(result, Some(effective_return_spec.as_str()))
        } else {
            result
        };
        for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
            // Skip class-qualified private attribute keys
            if attr_name.contains('\0') {
                continue;
            }
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
        // Apply `is rw` parameter writebacks so that changes to `is rw`
        // params inside the method body propagate back to the caller's
        // variables.
        // The method body is compiled with the class as current_package,
        // so sigilless params like "x" are stored as "C::x" at runtime.
        // Check both the bare param name and the package-qualified name.
        for (param_name, source_name) in &rw_bindings {
            let updated = self.env.get(param_name).cloned().or_else(|| {
                let qualified = format!("{}::{}", owner_class, param_name);
                self.env.get(&qualified).cloned()
            });
            if let Some(val) = updated {
                merged_env.insert(source_name.clone(), val);
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
