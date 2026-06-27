use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn assign_method_lvalue_with_values(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method: &str,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // If the target variable is deep-readonly (e.g. $_ in a for-loop
        // over an immutable type like Mix/Set/Bag), disallow method-based
        // mutation such as .value = ... on pairs.
        if let Some(var_name) = target_var {
            let deep_key = format!("__mutsu_deep_readonly::{}", var_name);
            if matches!(self.env.get(&deep_key), Some(Value::Bool(true))) {
                let repr = value.to_string_value();
                return Err(RuntimeError::assignment_ro_typename(
                    &crate::value::what_type_name(&value),
                    &repr,
                ));
            }
        }
        // IO::Path read-only accessors (`.SPEC`, `.CWD`) are not `rw`: assigning
        // to them raises X::Assignment::RO referencing the current value, matching
        // Raku (`'.'.IO.SPEC = ...` / `'.'.IO.CWD = ...`).
        if method_args.is_empty()
            && matches!(method, "SPEC" | "CWD")
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "IO::Path")
        {
            let cur = self
                .call_method_with_values(target.clone(), method, vec![])
                .unwrap_or(Value::Nil);
            let typename = crate::value::what_type_name(&cur);
            let repr = cur.to_string_value();
            return Err(RuntimeError::assignment_ro_typename(&typename, &repr));
        }
        // Handle AT-POS lvalue assignment: @arr.AT-POS(idx...) = v  =>  ASSIGN-POS(idx..., v)
        if method == "AT-POS" && !method_args.is_empty() && matches!(&target, Value::Array(..)) {
            let mut assign_args = method_args.clone();
            assign_args.push(value.clone());
            self.call_method_with_values(target, "ASSIGN-POS", assign_args)?;
            return Ok(value);
        }
        // Handle AT-KEY assignment on Hash: h.AT-KEY("k") = v  =>  ASSIGN-KEY("k", v)
        if method == "AT-KEY" && method_args.len() == 1 {
            let inner = match &target {
                Value::Scalar(inner) => inner.as_ref(),
                other => other,
            };
            if matches!(inner, Value::Hash(_) | Value::Nil)
                || matches!(inner, Value::Package(n) if matches!(n.resolve().as_str(), "Any" | "Mu"))
            {
                let old_meta = self.container_type_metadata(inner).clone();
                // Enforce the declared element type, so
                // `my Str %a; %a.AT-KEY("K") = 1` raises X::TypeCheck::Assignment
                // just like `%a<K> = 1` does.
                let value_type = old_meta
                    .as_ref()
                    .map(|m| m.value_type.clone())
                    .or_else(|| target_var.and_then(|v| self.var_type_constraint(v)));
                if let Some(vt) = value_type.as_deref()
                    && !matches!(vt, "Any" | "Mu" | "")
                    && !matches!(&value, Value::Nil)
                    && !self.type_matches_value(vt, &value)
                {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        target_var.unwrap_or("%"),
                        vt,
                        &value,
                    ));
                }
                let key = method_args[0].to_string_value();
                let mut hash = match inner {
                    Value::Hash(map) => map.map.clone(),
                    _ => std::collections::HashMap::new(),
                };
                hash.insert(key, value.clone());
                let mut new_hash = Value::Hash(Value::hash_arc(hash));
                // Propagate container type metadata to avoid stale pointer reuse
                let meta = old_meta.unwrap_or(ContainerTypeInfo {
                    value_type: "Any".to_string(),
                    key_type: None,
                    declared_type: None,
                });
                new_hash = self.tag_container_metadata(new_hash, meta);
                if let Some(var_name) = target_var {
                    self.env.insert(var_name.to_string(), new_hash);
                }
                return Ok(value);
            }
        }
        if let Value::Instance { class_name, .. } = &target
            && (class_name == "Date" || class_name == "DateTime")
        {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: {} is immutable",
                class_name.resolve()
            )));
        }
        // Handle .first rw write-back: @a.first(matcher).++ etc.
        if method == "first"
            && let Value::Array(ref items, ref kind) = target
        {
            // Re-run .first to find the matching index
            let func = method_args.first().cloned();
            if let Some((idx, _)) =
                self.find_first_match_over_items(func, &items.to_vec(), false)?
            {
                let mut updated = items.to_vec();
                if idx < updated.len() {
                    updated[idx] = value.clone();
                    let replacement = Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                        *kind,
                    );
                    if let Some(var_name) = target_var {
                        self.env.insert(var_name.to_string(), replacement);
                    }
                }
            }
            return Ok(value);
        }
        // Handle .head/.tail rw write-back: `@a.head = v` / `@a.tail = v`.
        if matches!(method, "head" | "tail")
            && method_args.is_empty()
            && let Value::Array(ref items, ref kind) = target
            && !items.is_empty()
        {
            let idx = if method == "head" { 0 } else { items.len() - 1 };
            let mut updated = items.to_vec();
            updated[idx] = value.clone();
            let replacement = Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                *kind,
            );
            if let Some(var_name) = target_var {
                self.env.insert(var_name.to_string(), replacement);
            }
            return Ok(value);
        }
        // Handle class-level attribute assignment (our $.x / my $.x)
        {
            let class_name_for_lookup = match &target {
                Value::Package(name) => Some(name.resolve()),
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name_for_lookup
                && self.has_class_level_attr(&cn, method)
            {
                self.set_class_level_attr(&cn, method, value.clone());
                return Ok(value);
            }
        }
        // Failure.handled = value: set the handled state via global registry
        if method == "handled"
            && method_args.is_empty()
            && let Value::Instance { class_name, id, .. } = &target
            && class_name.resolve() == "Failure"
        {
            let handled = value.truthy();
            crate::value::set_failure_handled(*id, handled);
            return Ok(Value::Bool(handled));
        }
        if method == "substr-rw" {
            return self.assign_substr_rw(target_var, target, method_args, value);
        }
        if method == "subbuf-rw" {
            return self.assign_subbuf_rw(target_var, target, method_args, value);
        }
        if method == "out-buffer"
            && let Value::Instance { class_name, .. } = &target
            && class_name == "IO::Handle"
            && method_args.is_empty()
        {
            let _ = self.call_method_with_values(target.clone(), method, vec![value.clone()])?;
            return Ok(value);
        }
        // nl-in setter for IO::Socket::INET and IO::Handle
        if method == "nl-in"
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && (class_name == "IO::Socket::INET" || class_name == "IO::Handle")
            && let Some(Value::Int(handle_id)) = attributes.as_map().get("handle")
        {
            let id = *handle_id as usize;
            let new_seps = match &value {
                Value::Str(s) => vec![s.as_bytes().to_vec()],
                Value::Array(items, ..) => items
                    .iter()
                    .map(|v| v.to_string_value().into_bytes())
                    .collect(),
                other => vec![other.to_string_value().into_bytes()],
            };
            if let Some(state) = self.io_handles_mut().map.get_mut(&id) {
                state.line_separators = new_seps;
            }
            return Ok(value);
        }
        // nl-out setter for IO::Handle
        if method == "nl-out"
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && class_name == "IO::Handle"
            && let Some(Value::Int(handle_id)) = attributes.as_map().get("handle")
        {
            let id = *handle_id as usize;
            let new_nl_out = value.to_string_value();
            if let Some(state) = self.io_handles_mut().map.get_mut(&id) {
                state.nl_out = new_nl_out;
            }
            return Ok(value);
        }
        // chomp setter for IO::Handle
        if method == "chomp"
            && let Value::Instance {
                class_name,
                attributes,
                id: inst_id,
            } = &target
            && class_name == "IO::Handle"
            && let Some(Value::Int(handle_id)) = attributes.as_map().get("handle")
        {
            let hid = *handle_id as usize;
            let new_chomp = value.truthy();
            if let Some(state) = self.io_handles_mut().map.get_mut(&hid) {
                state.line_chomp = new_chomp;
            }
            // Also update instance attribute so .open can inherit it
            let mut new_attrs = attributes.to_map();
            new_attrs.insert("chomp".to_string(), Value::Bool(new_chomp));
            let tid = *inst_id;
            attributes.commit_attrs(new_attrs);
            if let Some(var_name) = target_var {
                self.env.insert(
                    var_name.to_string(),
                    Value::instance_sharing_cell(attributes, *class_name, tid),
                );
            }
            return Ok(value);
        }
        if method == "value"
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && class_name == "Pair"
            && let Some(Value::Str(key)) = attributes.as_map().get("key")
            && let Some(Value::Hash(source_hash)) = attributes.as_map().get("__mutsu_hash_ref")
        {
            let mut updated = (**source_hash).clone();
            updated.insert(key.to_string(), value.clone());
            let replacement = Value::hash(updated);
            self.overwrite_hash_bindings_by_identity(source_hash, replacement);
            return Ok(value);
        }
        if method == "value" {
            let pair_data = match &target {
                Value::Pair(key, current_value) => Some((key.clone(), current_value.clone())),
                Value::ValuePair(key, current_value) => {
                    Some((key.to_string_value(), current_value.clone()))
                }
                _ => None,
            };
            if let Some((key, current_value)) = pair_data {
                // A Pair whose value is a shared `ContainerRef` (built by `key =>
                // $var`) aliases the source variable's container. Writing `.value`
                // updates the cell in place, so `$pair.value = X` writes through to
                // `$var` (S02:1704). The type constraint, if any, is enforced by
                // the cell itself on assignment.
                if let Value::ContainerRef(cell) = current_value.as_ref() {
                    // Enforce a typed container's `of`-type constraint, so
                    // `Pair.new("foo", my Int $).value = "bar"` raises
                    // X::TypeCheck::Assignment (S02-types/pair.t).
                    if let Some(constraint) = crate::value::lookup_container_constraint(cell)
                        && !matches!(constraint.as_str(), "Any" | "Mu")
                        && !matches!(&value, Value::Nil)
                        && !self.type_matches_value(&constraint, &value)
                    {
                        return Err(RuntimeError::typecheck_assignment(
                            &constraint,
                            &value,
                            None,
                        ));
                    }
                    *cell.lock().unwrap() = value.clone();
                    return Ok(value);
                }
                // `.value = X` / `.value--` on a Pair yielded by a mutable
                // QuantHash's `.pairs` (`for $b.pairs { .value = 42 }`,
                // `$b` a BagHash/MixHash/SetHash) writes the new weight back to
                // the source container. `topic_source_var` names that container
                // (set by the for-loop). Weight 0 removes the key; a non-numeric
                // Str coercion raises X::Str::Numeric. Immutable Bag/Mix/Set fall
                // through to the read-only Bool guard below.
                if let Some(source) = self.topic_source_var.clone()
                    && matches!(
                        self.env.get(&source),
                        Some(Value::Bag(_, true) | Value::Mix(_, true) | Value::Set(_, true))
                    )
                {
                    // The current bytecode isn't threaded into this builtin path,
                    // so pass an empty code: `quanthash_set_weight` then updates
                    // env (and main-alias) only, which is what the post-loop read
                    // of `$b` resolves through here.
                    let code = crate::opcode::CompiledCode::new();
                    self.quanthash_set_weight(&code, &source, key, &value)?;
                    return Ok(value);
                }
                let mut selected_hash: Option<std::sync::Arc<crate::value::HashData>> = None;
                let mut selected_array: Option<std::sync::Arc<crate::value::ArrayData>> = None;

                if let Some(var_name) = target_var
                    && let Some(Value::Hash(candidate)) = self.env.get(var_name)
                    && candidate.contains_key(&key)
                {
                    selected_hash = Some(candidate.clone());
                }
                if selected_hash.is_none()
                    && let Ok(i) = key.parse::<usize>()
                    && let Some(var_name) = target_var
                    && let Some(Value::Array(candidate, ..)) = self.env.get(var_name)
                    && candidate.get(i) == Some(current_value.as_ref())
                {
                    selected_array = Some(candidate.clone());
                }

                if selected_hash.is_none() {
                    let mut candidates = self.env.values().filter_map(|bound| match bound {
                        Value::Hash(map)
                            if map
                                .get(&key)
                                .is_some_and(|existing| existing == current_value.as_ref()) =>
                        {
                            Some(map.clone())
                        }
                        _ => None,
                    });
                    if let Some(first) = candidates.next()
                        && candidates.all(|other| std::sync::Arc::ptr_eq(&first, &other))
                    {
                        selected_hash = Some(first);
                    }
                }
                if selected_array.is_none()
                    && let Ok(i) = key.parse::<usize>()
                {
                    let mut candidates = self.env.values().filter_map(|bound| match bound {
                        Value::Array(arr, ..) if arr.get(i) == Some(current_value.as_ref()) => {
                            Some(arr.clone())
                        }
                        _ => None,
                    });
                    if let Some(first) = candidates.next()
                        && candidates.all(|other| std::sync::Arc::ptr_eq(&first, &other))
                    {
                        selected_array = Some(first);
                    }
                }

                if let Some(source_hash) = selected_hash {
                    let mut updated = (*source_hash).clone();
                    updated.insert(key, value.clone());
                    let replacement = Value::hash(updated);
                    self.overwrite_hash_bindings_by_identity(&source_hash, replacement);
                    return Ok(value);
                }
                if let Some(source_array) = selected_array
                    && let Ok(i) = key.parse::<usize>()
                {
                    let mut updated = (*source_array).clone();
                    if i < updated.len() {
                        updated[i] = value.clone();
                        let replacement =
                            Value::Array(std::sync::Arc::new(updated), ArrayKind::List);
                        self.overwrite_array_bindings_by_identity(&source_array, replacement);
                        return Ok(value);
                    }
                }

                // If the pair value is Bool and the pair is NOT directly backed
                // by a user-visible hash variable, the Bool is immutable.
                // This handles Set.pairs[0].value = 0 which should die.
                if matches!(current_value.as_ref(), Value::Bool(_)) {
                    let has_backing_hash = target_var
                        .is_some_and(|vn| matches!(self.env.get(vn), Some(Value::Hash(_))));
                    if !has_backing_hash {
                        let type_name = crate::value::what_type_name(current_value.as_ref());
                        return Err(RuntimeError::assignment_ro_typename(
                            &type_name,
                            &current_value.to_string_value(),
                        ));
                    }
                }

                // Standalone pair (not derived from a hash or array): update the
                // pair value directly by replacing the variable binding, and also
                // propagate to any other environment bindings that hold a pair
                // with the same key and same original value (simulating Raku
                // container semantics where pair values are aliases).
                {
                    let old_value = current_value.as_ref().clone();
                    // Collect all variable names in the environment that hold an
                    // equivalent pair (same key and same old value).
                    let vars_to_update: Vec<Symbol> = self
                        .env
                        .iter()
                        .filter_map(|(name, val)| {
                            let matches = match val {
                                Value::Pair(k, v) => k == &key && v.as_ref() == &old_value,
                                Value::ValuePair(k, v) => {
                                    k.to_string_value() == key && v.as_ref() == &old_value
                                }
                                _ => false,
                            };
                            if matches { Some(*name) } else { None }
                        })
                        .collect();

                    if !vars_to_update.is_empty() {
                        for var_name in &vars_to_update {
                            let current = self.env.get_sym(*var_name).cloned();
                            let new_pair = match current {
                                Some(Value::Pair(k, _)) => Value::Pair(k, Box::new(value.clone())),
                                Some(Value::ValuePair(k, _)) => {
                                    Value::ValuePair(k, Box::new(value.clone()))
                                }
                                _ => continue,
                            };
                            self.env.insert_sym(*var_name, new_pair);
                        }
                        return Ok(value);
                    } else if let Some(var_name) = target_var {
                        let new_pair = match &target {
                            Value::Pair(k, _) => Value::Pair(k.clone(), Box::new(value.clone())),
                            Value::ValuePair(k, _) => {
                                Value::ValuePair(k.clone(), Box::new(value.clone()))
                            }
                            _ => unreachable!(),
                        };
                        self.env.insert(var_name.to_string(), new_pair);
                        return Ok(value);
                    }
                }
            }
        }

        // Handle assignment to Proxy subclass attributes (e.g., $a.VAR.history = ())
        if let Value::Proxy {
            subclass: Some((_, ref subclass_attrs)),
            ..
        } = target
        {
            let attrs = subclass_attrs.clone();
            if attrs.lock().unwrap().contains_key(method) {
                // Coerce to array if the existing attribute is an array
                let new_val = {
                    let guard = attrs.lock().unwrap();
                    if matches!(guard.get(method), Some(Value::Array(..))) {
                        crate::runtime::coerce_to_array(value.clone())
                    } else {
                        value.clone()
                    }
                };
                attrs.lock().unwrap().insert(method.to_string(), new_val);
                return Ok(value);
            }
        }

        // Handle private attribute assignment via trust: $a!A::foo = value
        // The method name is "!Owner::attr" when the `!` modifier was used.
        if let Some(private_rest) = method.strip_prefix('!')
            && let Value::Instance {
                ref class_name,
                ref attributes,
                id: target_id,
            } = target
        {
            let caller_class = self
                .method_class_stack
                .last()
                .cloned()
                .or_else(|| Some(self.current_package().to_string()));
            let (owner_class, attr_name) =
                if let Some((owner, attr)) = private_rest.split_once("::") {
                    (owner.to_string(), attr.to_string())
                } else {
                    (class_name.resolve(), private_rest.to_string())
                };
            let caller_allowed = caller_class.as_deref() == Some(owner_class.as_str())
                || self
                    .registry()
                    .class_trusts
                    .get(&owner_class)
                    .is_some_and(|trusted| {
                        caller_class
                            .as_ref()
                            .is_some_and(|caller| trusted.contains(caller))
                    });
            if !caller_allowed {
                return Err(RuntimeError::new(format!(
                    "X::Method::Private::Permission: Cannot call private method '{}' on {} because it does not trust {}",
                    attr_name,
                    owner_class,
                    caller_class.as_deref().unwrap_or("GLOBAL")
                )));
            }
            let mut updated = attributes.to_map();
            updated.insert(attr_name, value.clone());
            let cn = *class_name;
            attributes.commit_attrs(updated);
            if let Some(var_name) = target_var {
                self.env.insert(
                    var_name.to_string(),
                    Value::instance_sharing_cell(attributes, cn, target_id),
                );
            }
            return Ok(value);
        }

        // Handle qualified method names early: Class::method (e.g., $o.Parent::x = 5)
        // Must be before call_method_mut_with_values which can't handle qualified names.
        if method.contains("::")
            && !method.starts_with('!')
            && let Value::Instance {
                ref class_name,
                ref attributes,
                id: target_id,
            } = target
            && let Some((qualifier, actual_method)) = method.split_once("::")
        {
            // First try explicit method resolution
            if let Some(method_def) = self.resolve_method(qualifier, actual_method, &method_args) {
                if !method_def.is_rw {
                    return Err(RuntimeError::new(format!(
                        "X::Assignment::RO: method '{}' is not rw",
                        actual_method
                    )));
                }
                if let Some(attr_name) = Self::rw_method_attribute_target(&method_def.body) {
                    let mut updated = attributes.to_map();
                    let current = if method_args.is_empty() {
                        self.call_method_with_values(target.clone(), actual_method, Vec::new())
                            .ok()
                    } else {
                        None
                    };
                    let mut assigned_value = if method_args.is_empty() {
                        Self::normalize_rw_accessor_assignment(current, value)
                    } else {
                        value
                    };
                    // When Nil is assigned to an attribute with `is default(...)`,
                    // restore the default value instead of setting Nil.
                    if matches!(assigned_value, Value::Nil)
                        && let Some(def) = self.class_attribute_default(qualifier, &attr_name)
                    {
                        assigned_value = def;
                    }
                    let cn = *class_name;
                    self.store_qualified_attr(
                        &mut updated,
                        &cn.resolve(),
                        qualifier,
                        &attr_name,
                        assigned_value.clone(),
                    );
                    if let Some(var_name) = target_var {
                        self.env.insert(
                            var_name.to_string(),
                            Value::write_back_sharing(attributes, cn, updated, target_id),
                        );
                    }
                    return Ok(assigned_value);
                }
                // `method at($i) is rw { @!d[$i] }` — assign into the indexed
                // attribute element.
                if let Some((attr, param, is_pos)) =
                    Self::rw_method_indexed_attr_target(&method_def.body)
                    && let Some(pos) = method_def.params.iter().position(|p| p == &param)
                    && let Some(idx_val) = method_args.get(pos).cloned()
                {
                    return self.assign_rw_indexed_attr(
                        attributes,
                        *class_name,
                        target_id,
                        target_var,
                        &attr,
                        idx_val,
                        is_pos,
                        value,
                    );
                }
            } else {
                // No explicit method found — try auto-accessor for public `is rw` attributes
                let class_attrs = self.collect_class_attributes(qualifier);
                let mut found_rw = false;
                for (attr_name, is_public, _default, is_rw, _is_required, sigil, ..) in &class_attrs
                {
                    if attr_name == actual_method && *is_public {
                        if !is_rw && *sigil != '@' && *sigil != '%' {
                            return Err(RuntimeError::new(format!(
                                "X::Assignment::RO: method '{}' is not rw",
                                actual_method
                            )));
                        }
                        found_rw = true;
                        break;
                    }
                }
                if found_rw {
                    let mut updated = attributes.to_map();
                    let current = if method_args.is_empty() {
                        self.call_method_with_values(target.clone(), actual_method, Vec::new())
                            .ok()
                    } else {
                        None
                    };
                    let mut assigned_value = if method_args.is_empty() {
                        Self::normalize_rw_accessor_assignment(current, value)
                    } else {
                        value
                    };
                    // When Nil is assigned to an attribute with `is default(...)`,
                    // restore the default value instead of setting Nil.
                    if matches!(assigned_value, Value::Nil)
                        && let Some(def) = self.class_attribute_default(qualifier, actual_method)
                    {
                        assigned_value = def;
                    }
                    let cn = *class_name;
                    self.store_qualified_attr(
                        &mut updated,
                        &cn.resolve(),
                        qualifier,
                        actual_method,
                        assigned_value.clone(),
                    );
                    if let Some(var_name) = target_var {
                        self.env.insert(
                            var_name.to_string(),
                            Value::write_back_sharing(attributes, cn, updated, target_id),
                        );
                    }
                    return Ok(assigned_value);
                }
            }
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: rw method '{}' does not expose an assignable attribute",
                actual_method
            )));
        }

        // Preserve existing accessor/setter assignment behavior for concrete variables.
        if let Some(var_name) = target_var
            && !method_args.is_empty()
        {
            match self.call_method_mut_with_values(
                var_name,
                target.clone(),
                method,
                vec![value.clone()],
            ) {
                Ok(result) => return Ok(result),
                Err(err) => {
                    if !err.is_multi_no_match() && !err.is_method_not_found() {
                        return Err(err);
                    }
                }
            }
        }

        // Handle Mixin-wrapped instances (e.g. from role punning) by updating
        // the mixin attribute entry directly.
        if let Value::Mixin(ref inner, ref mixins) = target
            && let Value::Instance { class_name, .. } = inner.as_ref()
        {
            let mixin_attr_key = format!("__mutsu_attr__{}", method);
            // Check if the role attribute is public and rw before allowing assignment
            let cn = class_name.resolve();
            let role_attrs = self.collect_role_attributes_for_class(&cn);
            for (attr_name, is_public, _default, is_rw, _, sigil, _) in &role_attrs {
                if attr_name == method && *is_public {
                    if !is_rw && *sigil != '@' && *sigil != '%' {
                        return Err(RuntimeError::new(format!(
                            "X::Assignment::RO: method '{}' is not rw",
                            method
                        )));
                    }
                    let mut updated_mixins = (**mixins).clone();
                    updated_mixins.insert(
                        if mixins.contains_key(&mixin_attr_key) {
                            mixin_attr_key
                        } else {
                            format!("__mutsu_attr__{}", method)
                        },
                        value.clone(),
                    );
                    if let Some(var_name) = target_var {
                        self.env.insert(
                            var_name.to_string(),
                            Value::Mixin(inner.clone(), std::sync::Arc::new(updated_mixins)),
                        );
                    }
                    return Ok(value);
                }
            }
            // If we have the mixin key but didn't find a matching role attribute,
            // still allow the update (e.g. for ad-hoc mixins)
            if mixins.contains_key(&mixin_attr_key) {
                let mut updated_mixins = (**mixins).clone();
                updated_mixins.insert(mixin_attr_key, value.clone());
                if let Some(var_name) = target_var {
                    self.env.insert(
                        var_name.to_string(),
                        Value::Mixin(inner.clone(), std::sync::Arc::new(updated_mixins)),
                    );
                }
                return Ok(value);
            }
        }

        let Value::Instance {
            class_name,
            attributes,
            id: target_id,
        } = target
        else {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: cannot assign through .{} on non-instance",
                method
            )));
        };

        let method_def = if let Some(def) =
            self.resolve_method(&class_name.resolve(), method, &method_args)
        {
            def
        } else if method_args.is_empty() {
            let class_attrs = self.collect_class_attributes(&class_name.resolve());
            let mut found_public_rw = false;
            let mut attr_sigil = '$';
            for (attr_name, is_public, _default, is_rw, _is_required, sigil, ..) in &class_attrs {
                if attr_name == method && *is_public {
                    // @ and % attributes are containers whose elements are always writable
                    // through indexing, even without `is rw`.
                    if !is_rw && *sigil != '@' && *sigil != '%' {
                        return Err(RuntimeError::new(format!(
                            "X::Assignment::RO: method '{}' is not rw",
                            method
                        )));
                    }
                    found_public_rw = true;
                    attr_sigil = *sigil;
                    break;
                }
            }
            if found_public_rw {
                // Check type constraint on the attribute before assignment.
                // For @ and % attributes, the type constraint applies to elements/values,
                // not to the container itself, so skip the container-level check.
                // Skip the check when Nil is assigned and the attribute has `is default(...)`
                // because Nil will be replaced by the default value.
                let nil_has_default = matches!(value, Value::Nil)
                    && self
                        .class_attribute_default(&class_name.resolve(), method)
                        .is_some();
                // Nil assigned to a typed attribute restores the type object default
                let nil_restores_type = matches!(value, Value::Nil) && !nil_has_default;
                if attr_sigil == '$'
                    && !nil_has_default
                    && !nil_restores_type
                    && let Some(type_constraint) =
                        self.get_attr_type_constraint(&class_name.resolve(), method)
                    && !self.type_matches_value(&type_constraint, &value)
                    && !self.is_container_subclass(&type_constraint)
                {
                    return Err(RuntimeError::typecheck_assignment(
                        &type_constraint,
                        &value,
                        Some(&format!("$!{}", method)),
                    ));
                }
                // Element-level type check for @ attributes (e.g. `has @.a of int`)
                if attr_sigil == '@'
                    && let Some(type_constraint) =
                        self.get_attr_type_constraint(&class_name.resolve(), method)
                    && let Value::Array(items, ..) = &value
                {
                    for item in items.iter() {
                        if !self.type_matches_value(&type_constraint, item) {
                            let native_name = match type_constraint.as_str() {
                                "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8"
                                | "uint16" | "uint32" | "uint64" => "a native integer",
                                "num" | "num32" | "num64" => "a native number",
                                _ => &type_constraint,
                            };
                            return Err(RuntimeError::new(format!(
                                "This type cannot unbox to {}: P6opaque, {}",
                                native_name,
                                super::utils::value_type_name(item),
                            )));
                        }
                    }
                }
                // Value-level type check for % hash attributes (e.g. `has Int %.h is rw`)
                if attr_sigil == '%'
                    && let Some(type_constraint) =
                        self.get_attr_type_constraint(&class_name.resolve(), method)
                    && !matches!(type_constraint.as_str(), "Mu" | "Any")
                {
                    let hash_vals: Vec<Value> = match &value {
                        Value::Hash(h) => h.values().cloned().collect(),
                        _ => Vec::new(),
                    };
                    for v in &hash_vals {
                        if !self.type_matches_value(&type_constraint, v) {
                            return Err(RuntimeError::new(format!(
                                "Type check failed for an element of %{}; expected {} but got {}",
                                method,
                                type_constraint,
                                super::utils::value_type_name(v),
                            )));
                        }
                    }
                }
                let attr_key = if attributes.contains_key(method) {
                    method.to_string()
                } else if attributes.contains_key(&format!("@{}", method)) {
                    format!("@{}", method)
                } else if attributes.contains_key(&format!("%{}", method)) {
                    format!("%{}", method)
                } else if attributes.contains_key(&format!("${}", method)) {
                    format!("${}", method)
                } else if attributes.contains_key(&format!("!{}", method)) {
                    format!("!{}", method)
                } else {
                    method.to_string()
                };
                let mut updated = attributes.to_map();
                let mut assigned_value =
                    Self::normalize_rw_accessor_assignment(updated.get(&attr_key).cloned(), value);
                // When Nil is assigned to an attribute with `is default(...)`,
                // restore the default value instead of setting Nil.
                if matches!(assigned_value, Value::Nil)
                    && let Some(def) = self.class_attribute_default(&class_name.resolve(), method)
                {
                    assigned_value = def;
                }
                // When Nil is assigned to a typed attribute without `is default`,
                // restore the type object (e.g., Nil -> Int for `has Int $.a`).
                if matches!(assigned_value, Value::Nil)
                    && attr_sigil == '$'
                    && let Some(tc) = self.get_attr_type_constraint(&class_name.resolve(), method)
                {
                    assigned_value = Value::Package(crate::symbol::Symbol::intern(&tc));
                }
                // Embed the attribute's declared element type into the stored
                // container so it survives later reads (`$o.h.of`, `.push` type
                // enforcement). Hash metadata lives in `HashData`; without this
                // an assignment to a typed `%`/`@` attribute would drop the type
                // (the old side table re-derived it on every read).
                if matches!(attr_sigil, '@' | '%')
                    && matches!(assigned_value, Value::Hash(_) | Value::Array(..))
                    && let Some(tc) = self.get_attr_type_constraint(&class_name.resolve(), method)
                    && !matches!(tc.as_str(), "Mu" | "Any")
                {
                    let info = ContainerTypeInfo {
                        value_type: tc,
                        key_type: None,
                        declared_type: None,
                    };
                    assigned_value = self.tag_container_metadata(assigned_value, info);
                }
                updated.insert(attr_key.clone(), assigned_value.clone());
                // Always propagate the change into this instance's live shared
                // cell. This handles chained accessor assignment like
                // `$outer.inner.arr = ...` where target_var may be None but the
                // instance is reachable through other aliases.
                attributes.commit_attrs(updated);
                if let Some(var_name) = target_var {
                    self.env.insert(
                        var_name.to_string(),
                        Value::instance_sharing_cell(&attributes, class_name, target_id),
                    );
                    // Also update attribute env variables so compiled method
                    // writeback picks up the change (e.g. $.cnt += 4 inside a method)
                    self.env
                        .insert(format!("!{}", attr_key), assigned_value.clone());
                    self.env
                        .insert(format!(".{}", attr_key), assigned_value.clone());
                }
                return Ok(assigned_value);
            }
            // Try native mutable method dispatch for native classes (e.g. Scheduler.uncaught_handler)
            if self.is_native_method(&class_name.resolve(), method) {
                let mut all_args = method_args.clone();
                all_args.push(value.clone());
                match self.call_native_instance_method_mut(
                    &class_name.resolve(),
                    attributes.to_map(),
                    method,
                    all_args,
                ) {
                    Ok((result, updated_attrs)) => {
                        attributes.commit_attrs(updated_attrs);
                        if let Some(var_name) = target_var {
                            self.env.insert(
                                var_name.to_string(),
                                Value::instance_sharing_cell(&attributes, class_name, target_id),
                            );
                        }
                        return Ok(result);
                    }
                    Err(err) => {
                        if !err.message.starts_with("No native mutable method") {
                            return Err(err);
                        }
                    }
                }
            }
            return Err(super::methods_signature_errors::make_multi_no_match_error(
                method,
            ));
        } else {
            return Err(super::methods_signature_errors::make_multi_no_match_error(
                method,
            ));
        };
        // Delegation methods: forward assignment to the delegate
        if let Some((attr_var_name, target_method)) = &method_def.delegation
            && !attr_var_name.starts_with('&')
        {
            let attr_key = attr_var_name
                .trim_start_matches('.')
                .trim_start_matches('!');
            let delegate = attributes
                .as_map()
                .get(attr_key)
                .cloned()
                .unwrap_or(Value::Nil);
            if delegate != Value::Nil {
                let sigil = match &delegate {
                    Value::Array(..) => "@",
                    Value::Hash(_) => "%",
                    _ => "$",
                };
                let temp_var = format!("{}__mutsu_delegation_tmp__", sigil);
                self.env.insert(temp_var.clone(), delegate.clone());
                let result = self.assign_method_lvalue_with_values(
                    Some(&temp_var),
                    delegate,
                    target_method,
                    method_args,
                    value,
                )?;
                let updated_delegate = self.env.get(&temp_var).cloned().unwrap_or(Value::Nil);
                self.env.remove(&temp_var);
                let mut updated = attributes.to_map();
                updated.insert(attr_key.to_string(), updated_delegate);
                if let Some(var_name) = target_var {
                    self.env.insert(
                        var_name.to_string(),
                        Value::write_back_sharing(&attributes, class_name, updated, target_id),
                    );
                }
                return Ok(result);
            }
        }
        if !method_def.is_rw {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: method '{}' is not rw",
                method
            )));
        }
        if let Some(attr_name) = Self::rw_method_attribute_target(&method_def.body) {
            let mut updated = attributes.to_map();
            let current = if method_args.is_empty() {
                self.call_method_with_values(
                    Value::Instance {
                        class_name,
                        attributes: attributes.clone(),
                        id: target_id,
                    },
                    method,
                    Vec::new(),
                )
                .ok()
            } else {
                None
            };
            let mut assigned_value = if method_args.is_empty() {
                Self::normalize_rw_accessor_assignment(current, value)
            } else {
                value
            };
            // When Nil is assigned to an attribute with `is default(...)`,
            // restore the default value instead of setting Nil.
            if matches!(assigned_value, Value::Nil)
                && let Some(def) = self.class_attribute_default(&class_name.resolve(), &attr_name)
            {
                assigned_value = def;
            }
            updated.insert(attr_name, assigned_value.clone());
            if let Some(var_name) = target_var {
                self.env.insert(
                    var_name.to_string(),
                    Value::write_back_sharing(&attributes, class_name, updated, target_id),
                );
            }
            return Ok(assigned_value);
        }
        // `method at($i) is rw { @!d[$i] }` — assign into the indexed attribute
        // element.
        if let Some((attr, param, is_pos)) = Self::rw_method_indexed_attr_target(&method_def.body)
            && let Some(pos) = method_def.params.iter().position(|p| p == &param)
            && let Some(idx_val) = method_args.get(pos).cloned()
        {
            return self.assign_rw_indexed_attr(
                &attributes,
                class_name,
                target_id,
                target_var,
                &attr,
                idx_val,
                is_pos,
                value,
            );
        }

        // Check if this is a delegation method — forward assignment to delegate
        if let Some((ref attr_var_name, ref target_method)) = method_def.delegation
            && !attr_var_name.starts_with('&')
        {
            let attr_key = attr_var_name
                .trim_start_matches('.')
                .trim_start_matches('!');
            let delegate = attributes
                .as_map()
                .get(attr_key)
                .cloned()
                .unwrap_or(Value::Nil);
            if delegate != Value::Nil {
                // Temporarily bind the delegate to an env variable for update tracking
                let temp_var = "__mutsu_delegation_tmp__".to_string();
                self.env.insert(temp_var.clone(), delegate.clone());
                // Forward the assignment to the delegate
                let result = self.assign_method_lvalue_with_values(
                    Some(&temp_var),
                    delegate,
                    target_method,
                    method_args,
                    value,
                )?;
                // Read back the potentially-updated delegate
                let updated_delegate = self.env.get(&temp_var).cloned().unwrap_or(Value::Nil);
                self.env.remove(&temp_var);
                // Write the updated delegate back into the frontend's live cell.
                let mut updated = attributes.to_map();
                updated.insert(attr_key.to_string(), updated_delegate);
                if let Some(var_name) = target_var {
                    self.env.insert(
                        var_name.to_string(),
                        Value::write_back_sharing(&attributes, class_name, updated, target_id),
                    );
                }
                return Ok(result);
            }
        }

        // The method body doesn't directly expose an attribute — run it and check for Proxy.
        // Set in_lvalue_assignment so the VM skips Proxy auto-fetching on method call
        // results, allowing the raw Proxy to flow back for STORE dispatch.
        let was_lvalue = self.in_lvalue_assignment;
        self.in_lvalue_assignment = true;
        let method_result = self.run_instance_method(
            &class_name.resolve(),
            attributes.to_map(),
            method,
            method_args,
            None,
        );
        self.in_lvalue_assignment = was_lvalue;
        let (method_result, updated_attrs) = method_result?;
        if let Value::Proxy { storer, .. } = &method_result {
            return self.proxy_store(
                storer,
                target_var,
                class_name,
                &updated_attrs,
                &attributes,
                value,
            );
        }

        Err(RuntimeError::new(format!(
            "X::Assignment::RO: rw method '{}' does not expose an assignable attribute",
            method
        )))
    }
}
