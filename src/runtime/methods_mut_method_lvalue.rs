use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Whether the role attribute backing a mixin override key
    /// (`__mutsu_attr__{method}`) is declared `is rw`. Scans the mixin's
    /// `__mutsu_role__*` entries for a role that declares the attribute; an
    /// ad-hoc mixin with no declaring role stays writable (matching the
    /// lenient ad-hoc branch of the Instance mixin path).
    fn mixin_attr_is_rw(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        method: &str,
    ) -> bool {
        let mut found_attr = false;
        for role_name in mixins
            .keys()
            .filter_map(|k| k.strip_prefix("__mutsu_role__"))
        {
            let base = role_name.split('[').next().unwrap_or(role_name);
            for candidate in [role_name, base] {
                for (attr_name, is_public, _default, is_rw, _is_required, sigil, ..) in
                    self.collect_role_attributes_for_class(candidate)
                {
                    if attr_name == method && is_public {
                        if is_rw || sigil == '@' || sigil == '%' {
                            return true;
                        }
                        found_attr = true;
                    }
                }
            }
        }
        !found_attr
    }

    /// `Pair.freeze`: decontainerize the pair's value (severing any
    /// Scalar-container alias to an outer variable), mark the fresh cell
    /// read-only, rebind the pair variable, and return the value. After this a
    /// `$pair.value = X` raises `X::Assignment::RO`. Deprecated as of 6.d, but
    /// still specified (Pair.rakudoc).
    pub(crate) fn pair_freeze(&mut self, target: &Value, target_name: &str) -> Value {
        let current = match target.view() {
            ValueView::Pair(_, v) => v.clone(),
            ValueView::ValuePair(_, v) => v.clone(),
            _ => return target.clone(),
        };
        let deref = current.deref_container();
        // Preserve a typed container's `of`-constraint across the freeze, so a
        // later (rejected) assignment still reports the right type.
        let constraint = match current.view() {
            ValueView::ContainerRef(old) => crate::value::lookup_container_constraint(&old),
            _ => None,
        };
        let frozen_value =
            crate::value::make_frozen_container(deref.clone(), constraint.as_deref());
        let new_pair = match target.view() {
            ValueView::Pair(k, _) => Value::pair(k.clone(), frozen_value),
            ValueView::ValuePair(k, _) => Value::value_pair(k.clone(), frozen_value),
            _ => return deref,
        };
        if !target_name.is_empty() {
            self.env.insert(target_name.to_string(), new_pair);
        }
        deref
    }

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
            if matches!(
                self.env.get(&deep_key).map(Value::view),
                Some(ValueView::Bool(true))
            ) {
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
            && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "IO::Path")
        {
            let cur = self
                .call_method_with_values(target.clone(), method, vec![])
                .unwrap_or(Value::NIL);
            let typename = crate::value::what_type_name(&cur);
            let repr = cur.to_string_value();
            return Err(RuntimeError::assignment_ro_typename(&typename, &repr));
        }
        // Handle AT-POS lvalue assignment: @arr.AT-POS(idx...) = v  =>  ASSIGN-POS(idx..., v)
        if method == "AT-POS"
            && !method_args.is_empty()
            && matches!(target.view(), ValueView::Array(..))
        {
            let mut assign_args = method_args.clone();
            assign_args.push(value.clone());
            self.call_method_with_values(target, "ASSIGN-POS", assign_args)?;
            return Ok(value);
        }
        // Handle AT-KEY assignment on Hash: h.AT-KEY("k") = v  =>  ASSIGN-KEY("k", v)
        if method == "AT-KEY" && method_args.len() == 1 {
            let inner = match target.view() {
                ValueView::Scalar(inner) => inner,
                _ => &target,
            };
            if matches!(inner.view(), ValueView::Hash(_) | ValueView::Nil)
                || matches!(inner.view(), ValueView::Package(n) if matches!(n.resolve().as_str(), "Any" | "Mu"))
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
                    && !value.is_nil()
                    && !self.type_matches_value(vt, &value)
                {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        target_var.unwrap_or("%"),
                        vt,
                        &value,
                    ));
                }
                let key = method_args[0].to_string_value();
                // An attribute-backed hash (`%!h.AT-KEY($k) = $v` inside a method)
                // has no live env binding under its twigil name — the attribute
                // lives in `self`'s shared cell. Read the current hash from that
                // cell, insert the element, and write it straight back, so the
                // element write reaches the instance the same way `%!h{$k} = $v`
                // does. Without this the AT-KEY lvalue builtin only mutated a
                // read-copy and the write was lost.
                if let Some(var_name) = target_var
                    && Self::attr_twigil_base(var_name).is_some()
                    && let Some(cell_val) = self.read_self_attr_cell(var_name)
                    && matches!(cell_val.view(), ValueView::Hash(_) | ValueView::Nil)
                {
                    let mut map = match cell_val.view() {
                        ValueView::Hash(h) => h.map.clone(),
                        _ => std::collections::HashMap::new(),
                    };
                    map.insert(key, value.clone());
                    let mut new_hash = Value::hash_with_data(Value::hash_arc(map));
                    if let Some(m) = old_meta.clone() {
                        new_hash = self.tag_container_metadata(new_hash, m);
                    }
                    self.write_self_attr_cell(var_name, new_hash);
                    return Ok(value);
                }
                let mut hash = match inner.view() {
                    ValueView::Hash(map) => map.map.clone(),
                    _ => std::collections::HashMap::new(),
                };
                hash.insert(key, value.clone());
                let mut new_hash = Value::hash_with_data(Value::hash_arc(hash));
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
        if let ValueView::Instance { class_name, .. } = target.view()
            && (class_name == "Date" || class_name == "DateTime")
        {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: {} is immutable",
                class_name.resolve()
            )));
        }
        // Handle .first rw write-back: @a.first(matcher).++ etc.
        if method == "first"
            && let ValueView::Array(items, kind) = target.view()
        {
            // Re-run .first to find the matching index
            let func = method_args.first().cloned();
            if let Some((idx, _)) =
                self.find_first_match_over_items(func, &items.to_vec(), false)?
            {
                let mut updated = items.to_vec();
                if idx < updated.len() {
                    updated[idx] = value.clone();
                    let replacement = Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(updated)),
                        kind,
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
            && let ValueView::Array(items, kind) = target.view()
            && !items.is_empty()
        {
            let idx = if method == "head" { 0 } else { items.len() - 1 };
            let mut updated = items.to_vec();
            updated[idx] = value.clone();
            let replacement = Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(updated)),
                kind,
            );
            if let Some(var_name) = target_var {
                self.env.insert(var_name.to_string(), replacement);
            }
            return Ok(value);
        }
        // Handle class-level attribute assignment (our $.x / my $.x)
        {
            let class_name_for_lookup = match target.view() {
                ValueView::Package(name) => Some(name.resolve()),
                ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
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
            && let ValueView::Instance { class_name, id, .. } = target.view()
            && class_name.resolve() == "Failure"
        {
            let handled = value.truthy();
            crate::value::set_failure_handled(id, handled);
            return Ok(Value::truth(handled));
        }
        if method == "substr-rw" {
            return self.assign_substr_rw(target_var, target, method_args, value);
        }
        if method == "subbuf-rw" {
            return self.assign_subbuf_rw(target_var, target, method_args, value);
        }
        if method == "out-buffer"
            && let ValueView::Instance { class_name, .. } = target.view()
            && class_name == "IO::Handle"
            && method_args.is_empty()
        {
            let _ = self.call_method_with_values(target.clone(), method, vec![value.clone()])?;
            return Ok(value);
        }
        // nl-in setter for IO::Socket::INET and IO::Handle
        if method == "nl-in"
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && (class_name == "IO::Socket::INET" || class_name == "IO::Handle")
            && let Some(ValueView::Int(handle_id)) =
                attributes.as_map().get("handle").map(Value::view)
        {
            let id = handle_id as usize;
            let new_seps = match value.view() {
                ValueView::Str(s) => vec![s.as_bytes().to_vec()],
                ValueView::Array(items, ..) => items
                    .iter()
                    .map(|v| v.to_string_value().into_bytes())
                    .collect(),
                _ => vec![value.to_string_value().into_bytes()],
            };
            if let Some(state) = self.io_handles_mut().map.get_mut(&id) {
                state.line_separators = new_seps;
            }
            return Ok(value);
        }
        // nl-in / nl-out setter for a USER SUBCLASS of IO::Handle (overrides
        // READ/WRITE/EOF, no OS handle): store the value in the instance attribute
        // through the shared cell so the user IO read path (`.lines`/`.get`) and
        // output path see it. See `try_user_io_handle_method`.
        if matches!(method, "nl-in" | "nl-out" | "encoding")
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && attributes.as_map().get("handle").is_none()
            && self
                .class_mro(&class_name.resolve())
                .iter()
                .any(|c| c == "IO::Handle")
            && class_name.resolve() != "IO::Handle"
        {
            attributes.insert(method.to_string(), value.clone());
            return Ok(value);
        }
        // nl-out setter for IO::Handle
        if method == "nl-out"
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && class_name == "IO::Handle"
            && let Some(ValueView::Int(handle_id)) =
                attributes.as_map().get("handle").map(Value::view)
        {
            let id = handle_id as usize;
            let new_nl_out = value.to_string_value();
            if let Some(state) = self.io_handles_mut().map.get_mut(&id) {
                state.nl_out = new_nl_out;
            }
            return Ok(value);
        }
        // chomp setter for IO::Handle
        if method == "chomp"
            && let ValueView::Instance {
                class_name,
                attributes,
                id: inst_id,
            } = target.view()
            && class_name == "IO::Handle"
            && let Some(ValueView::Int(handle_id)) =
                attributes.as_map().get("handle").map(Value::view)
        {
            let hid = handle_id as usize;
            let new_chomp = value.truthy();
            if let Some(state) = self.io_handles_mut().map.get_mut(&hid) {
                state.line_chomp = new_chomp;
            }
            // Also update instance attribute so .open can inherit it
            let mut new_attrs = attributes.to_map();
            new_attrs.insert("chomp".to_string(), Value::truth(new_chomp));
            let tid = inst_id;
            attributes.commit_attrs(new_attrs);
            if let Some(var_name) = target_var {
                self.env.insert(
                    var_name.to_string(),
                    Value::instance_sharing_cell(&attributes, class_name, tid),
                );
            }
            return Ok(value);
        }
        if method == "value"
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && class_name == "Pair"
            && let Some(ValueView::Str(key)) = attributes.as_map().get("key").map(Value::view)
            && let Some(ValueView::Hash(source_hash)) =
                attributes.as_map().get("__mutsu_hash_ref").map(Value::view)
        {
            let mut updated = (**source_hash).clone();
            updated.insert(key.to_string(), value.clone());
            let replacement = Value::hash(updated);
            self.overwrite_hash_bindings_by_identity(&source_hash, replacement);
            return Ok(value);
        }
        if method == "value" {
            let pair_data = match target.view() {
                ValueView::Pair(key, current_value) => {
                    Some((key.clone(), Box::new(current_value.clone())))
                }
                ValueView::ValuePair(key, current_value) => {
                    Some((key.to_string_value(), Box::new(current_value.clone())))
                }
                _ => None,
            };
            if let Some((key, current_value)) = pair_data {
                // A Pair whose value is a live `HashEntryRef` (a `for %h -> $p`
                // loop pair) writes `.value` straight through to the shared hash
                // node in place, so `$p.value = X` updates `%h{$p.key}` and the
                // ref keeps reading the new value.
                if let ValueView::HashEntryRef { .. } = current_value.as_ref().view()
                    && let Some((node, last_key)) = current_value.hash_entry_terminal()
                {
                    // SAFETY: aliased in-place mutation of a shared hash node;
                    // mirrors `hash_entry_terminal`'s own interior mutation. No
                    // borrow into the map is held across the write.
                    let data = unsafe { crate::value::gc_contents_mut(&node) };
                    data.map.insert(last_key, value.clone());
                    return Ok(value);
                }
                // A Pair whose value is a shared `ContainerRef` (built by `key =>
                // $var`) aliases the source variable's container. Writing `.value`
                // updates the cell in place, so `$pair.value = X` writes through to
                // `$var` (S02:1704). The type constraint, if any, is enforced by
                // the cell itself on assignment.
                if let ValueView::ContainerRef(cell) = current_value.as_ref().view() {
                    // A frozen Pair value (after `.freeze`) is read-only: any
                    // assignment raises X::Assignment::RO (Pair.rakudoc).
                    if crate::value::is_container_frozen(&cell) {
                        let guard = cell.lock().unwrap();
                        let type_name = crate::value::what_type_name(&guard);
                        let repr = guard.to_string_value();
                        return Err(RuntimeError::assignment_ro_typename(&type_name, &repr));
                    }
                    // Enforce a typed container's `of`-type constraint, so
                    // `Pair.new("foo", my Int $).value = "bar"` raises
                    // X::TypeCheck::Assignment (S02-types/pair.t).
                    if let Some(constraint) = crate::value::lookup_container_constraint(&cell)
                        && !matches!(constraint.as_str(), "Any" | "Mu")
                        && !value.is_nil()
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
                        self.env.get(&source).map(Value::view),
                        Some(
                            ValueView::Bag(_, true)
                                | ValueView::Mix(_, true)
                                | ValueView::Set(_, true)
                        )
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
                let mut selected_hash: Option<crate::gc::Gc<crate::value::HashData>> = None;
                // Track the source array's `ArrayKind` so the rebuilt array keeps
                // its Array/Shaped identity (`for @a.pairs { .value = X }` must not
                // demote `@a` to a bare List).
                let mut selected_array: Option<(
                    crate::gc::Gc<crate::value::ArrayData>,
                    ArrayKind,
                )> = None;

                if let Some(var_name) = target_var
                    && let Some(ValueView::Hash(candidate)) =
                        self.env.get(var_name).map(Value::view)
                    && candidate.contains_key(&key)
                {
                    selected_hash = Some(candidate.clone());
                }
                if selected_hash.is_none()
                    && let Ok(i) = key.parse::<usize>()
                    && let Some(var_name) = target_var
                    && let Some(ValueView::Array(candidate, kind)) =
                        self.env.get(var_name).map(Value::view)
                    && candidate.get(i) == Some(current_value.as_ref())
                {
                    selected_array = Some((candidate.clone(), kind));
                }

                if selected_hash.is_none() {
                    let mut candidates = self.env.values().filter_map(|bound| match bound.view() {
                        ValueView::Hash(map)
                            if map
                                .get(&key)
                                .is_some_and(|existing| existing == current_value.as_ref()) =>
                        {
                            Some(map.clone())
                        }
                        _ => None,
                    });
                    if let Some(first) = candidates.next()
                        && candidates.all(|other| crate::gc::Gc::ptr_eq(&first, &other))
                    {
                        selected_hash = Some(first);
                    }
                }
                if selected_array.is_none()
                    && let Ok(i) = key.parse::<usize>()
                {
                    let mut candidates = self.env.values().filter_map(|bound| match bound.view() {
                        ValueView::Array(arr, kind)
                            if arr.get(i) == Some(current_value.as_ref()) =>
                        {
                            Some((arr.clone(), kind))
                        }
                        _ => None,
                    });
                    if let Some(first) = candidates.next()
                        && candidates.all(|(other, _)| crate::gc::Gc::ptr_eq(&first.0, &other))
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
                if let Some((source_array, source_kind)) = selected_array
                    && let Ok(i) = key.parse::<usize>()
                {
                    let mut updated = (*source_array).clone();
                    if i < updated.len() {
                        updated[i] = value.clone();
                        // Preserve the source array's kind (Array/Shaped/…) so the
                        // writeback does not demote it to a List.
                        let replacement =
                            Value::array_with_kind(crate::gc::Gc::new(updated), source_kind);
                        self.overwrite_array_bindings_by_identity(&source_array, replacement);
                        return Ok(value);
                    }
                }

                // If the pair value is Bool and the pair is NOT directly backed
                // by a user-visible hash variable, the Bool is immutable.
                // This handles Set.pairs[0].value = 0 which should die.
                if matches!(current_value.as_ref().view(), ValueView::Bool(_)) {
                    let has_backing_hash = target_var.is_some_and(|vn| {
                        matches!(self.env.get(vn).map(Value::view), Some(ValueView::Hash(_)))
                    });
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
                            let matches = match val.view() {
                                ValueView::Pair(k, v) => k == &key && v == &old_value,
                                ValueView::ValuePair(k, v) => {
                                    k.to_string_value() == key && v == &old_value
                                }
                                _ => false,
                            };
                            if matches { Some(*name) } else { None }
                        })
                        .collect();

                    if !vars_to_update.is_empty() {
                        for var_name in &vars_to_update {
                            let current = self.env.get_sym(*var_name).cloned();
                            let new_pair = match current.as_ref().map(Value::view) {
                                Some(ValueView::Pair(k, _)) => {
                                    Value::pair(k.clone(), value.clone())
                                }
                                Some(ValueView::ValuePair(k, _)) => {
                                    Value::value_pair(k.clone(), value.clone())
                                }
                                _ => continue,
                            };
                            self.env.insert_sym(*var_name, new_pair);
                        }
                        return Ok(value);
                    } else if let Some(var_name) = target_var {
                        let new_pair = match target.view() {
                            ValueView::Pair(k, _) => Value::pair(k.clone(), value.clone()),
                            ValueView::ValuePair(k, _) => {
                                Value::value_pair(k.clone(), value.clone())
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
        if let ValueView::Proxy {
            subclass: Some((_, subclass_attrs)),
            ..
        } = target.view()
        {
            let attrs = subclass_attrs.clone();
            if attrs.lock().unwrap().contains_key(method) {
                // Coerce to array if the existing attribute is an array
                let new_val = {
                    let guard = attrs.lock().unwrap();
                    if matches!(
                        guard.get(method).map(Value::view),
                        Some(ValueView::Array(..))
                    ) {
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
            && let ValueView::Instance {
                class_name,
                attributes,
                id: target_id,
            } = target.view()
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
            let cn = class_name;
            attributes.commit_attrs(updated);
            if let Some(var_name) = target_var {
                self.env.insert(
                    var_name.to_string(),
                    Value::instance_sharing_cell(&attributes, cn, target_id),
                );
            }
            return Ok(value);
        }

        // Handle qualified method names early: Class::method (e.g., $o.Parent::x = 5)
        // Must be before call_method_mut_with_values which can't handle qualified names.
        if method.contains("::")
            && !method.starts_with('!')
            && let ValueView::Instance {
                class_name,
                attributes,
                id: target_id,
            } = target.view()
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
                    if assigned_value.is_nil()
                        && let Some(def) = self.class_attribute_default(qualifier, &attr_name)
                    {
                        assigned_value = def;
                    }
                    let cn = class_name;
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
                            Value::write_back_sharing(&attributes, cn, updated, target_id),
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
                    if assigned_value.is_nil()
                        && let Some(def) = self.class_attribute_default(qualifier, actual_method)
                    {
                        assigned_value = def;
                    }
                    let cn = class_name;
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
                            Value::write_back_sharing(&attributes, cn, updated, target_id),
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

        // Preserve existing accessor/setter assignment behavior for concrete
        // variables. `AT-KEY`/`AT-POS` are element accessors, never setters:
        // `$obj.AT-KEY($k) = $v` means "set element $k", so skip this setter
        // convention and let the raw-accessor recursion / element handling below
        // take it (otherwise it would mis-call `AT-KEY($v)` with the value).
        if let Some(var_name) = target_var
            && !method_args.is_empty()
            && !matches!(method, "AT-KEY" | "AT-POS")
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

        // Assignment through a first-class container (`$obj.attr.VAR.name = v`
        // after `$obj.attr.VAR does Role`): the target is the attribute slot's
        // `ContainerRef` cell. A role-mixin attribute write updates the mixin
        // override in place through the cell (visible to every alias); any
        // other method-lvalue delegates to the inner value.
        if let ValueView::ContainerRef(cell) = target.view() {
            let inner = cell.lock().unwrap().clone();
            if let ValueView::Mixin(minner, mixins) = inner.view() {
                let mixin_attr_key = format!("__mutsu_attr__{}", method);
                if mixins.contains_key(&mixin_attr_key) {
                    if !self.mixin_attr_is_rw(mixins, method) {
                        return Err(RuntimeError::new(format!(
                            "X::Assignment::RO: method '{}' is not rw",
                            method
                        )));
                    }
                    let mut updated = (**mixins).clone();
                    updated.insert(mixin_attr_key, value.clone());
                    *cell.lock().unwrap() =
                        Value::mixin_parts(minner.clone(), std::sync::Arc::new(updated));
                    return Ok(value);
                }
            }
            return self.assign_method_lvalue_with_values(
                target_var,
                inner,
                method,
                method_args,
                value,
            );
        }

        // Handle Mixin-wrapped instances (e.g. from role punning) by updating
        // the mixin attribute entry directly.
        if let ValueView::Mixin(inner, mixins) = target.view()
            && let ValueView::Instance { class_name, .. } = inner.as_ref().view()
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
                    let new_mixin =
                        Value::mixin_parts(inner.clone(), std::sync::Arc::new(updated_mixins));
                    if let Some(var_name) = target_var {
                        self.env.insert(var_name.to_string(), new_mixin.clone());
                    }
                    // Inside a trait_mod the rebuilt Mixin must also refresh the
                    // writeback capture (same convention as DoesVar): the value
                    // DoesVar captured predates this assignment and would hand
                    // the trait's caller a mixin missing it (JSON::Name's
                    // `$a.json-name = $json-name` right after `$a does ...`).
                    if self.trait_mod_writeback_key.is_some()
                        && self.trait_mod_writeback_value.is_some()
                    {
                        self.trait_mod_writeback_value = Some(new_mixin);
                    }
                    return Ok(value);
                }
            }
            // If we have the mixin key but didn't find a matching role attribute,
            // still allow the update (e.g. for ad-hoc mixins)
            if mixins.contains_key(&mixin_attr_key) {
                let mut updated_mixins = (**mixins).clone();
                updated_mixins.insert(mixin_attr_key, value.clone());
                let new_mixin =
                    Value::mixin_parts(inner.clone(), std::sync::Arc::new(updated_mixins));
                if let Some(var_name) = target_var {
                    self.env.insert(var_name.to_string(), new_mixin.clone());
                }
                // See the role-attribute branch above: refresh the trait_mod
                // writeback capture so the assignment survives past the trait.
                if self.trait_mod_writeback_key.is_some()
                    && self.trait_mod_writeback_value.is_some()
                {
                    self.trait_mod_writeback_value = Some(new_mixin);
                }
                return Ok(value);
            }
        }

        let (class_name, attributes, target_id) = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } => (class_name, attributes.clone(), id),
            _ => {
                return Err(RuntimeError::new(format!(
                    "X::Assignment::RO: cannot assign through .{} on non-instance",
                    method
                )));
            }
        };

        // When the public attribute accessor wins the per-MRO-level race
        // against user methods of this name (a child's accessor shadows a
        // parent's explicit method), assignment must go through the accessor
        // branch below, not the shadowed method (which would reject with
        // "not rw" when the parent method lacks `is rw`).
        let accessor_wins = method_args.is_empty()
            && matches!(
                self.resolve_user_method_or_accessor(&class_name.resolve(), method),
                Some(UserMethodOrAccessor::Accessor)
            );
        let method_def = if let Some(def) = (!accessor_wins)
            .then(|| self.resolve_method(&class_name.resolve(), method, &method_args))
            .flatten()
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
                let nil_has_default = value.is_nil()
                    && self
                        .class_attribute_default(&class_name.resolve(), method)
                        .is_some();
                // Nil assigned to a typed attribute restores the type object default
                let nil_restores_type = value.is_nil() && !nil_has_default;
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
                    && let ValueView::Array(items, ..) = value.view()
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
                    // An object hash (`%.h{Str:D}`) declares `ValueType{KeyType}`;
                    // its elements are checked against the value type only.
                    let (value_type, _) =
                        crate::runtime::types::split_object_hash_constraint(&type_constraint);
                    let hash_vals: Vec<Value> = match value.view() {
                        ValueView::Hash(h) => h.values().cloned().collect(),
                        _ => Vec::new(),
                    };
                    for v in &hash_vals {
                        if !v.is_nil() && !self.type_matches_value(value_type, v) {
                            return Err(RuntimeError::new(format!(
                                "Type check failed for an element of %{}; expected {} but got {}",
                                method,
                                value_type,
                                super::utils::value_type_name(v),
                            )));
                        }
                    }
                }
                let attr_key = if attributes.contains_key(method) {
                    method.to_string()
                } else if attributes.contains_key(format!("@{}", method)) {
                    format!("@{}", method)
                } else if attributes.contains_key(format!("%{}", method)) {
                    format!("%{}", method)
                } else if attributes.contains_key(format!("${}", method)) {
                    format!("${}", method)
                } else if attributes.contains_key(format!("!{}", method)) {
                    format!("!{}", method)
                } else {
                    method.to_string()
                };
                let mut updated = attributes.to_map();
                // A slot promoted to a `ContainerRef` cell (a `:=` bind / `.VAR`
                // mixin took the attribute's container identity) is transparent
                // to the accessor write: normalize against the inner value and
                // (below) write through the cell instead of replacing it.
                let mut assigned_value = Self::normalize_rw_accessor_assignment(
                    updated.get(&attr_key).cloned().map(|v| v.deref_container()),
                    value,
                );
                // When Nil is assigned to an attribute with `is default(...)`,
                // restore the default value instead of setting Nil.
                if assigned_value.is_nil()
                    && let Some(def) = self.class_attribute_default(&class_name.resolve(), method)
                {
                    assigned_value = def;
                }
                // When Nil is assigned to a typed attribute without `is default`,
                // restore the type object (e.g., Nil -> Int for `has Int $.a`).
                if assigned_value.is_nil()
                    && attr_sigil == '$'
                    && let Some(tc) = self.get_attr_type_constraint(&class_name.resolve(), method)
                {
                    assigned_value = Value::package(crate::symbol::Symbol::intern(&tc));
                }
                // Embed the attribute's declared element type into the stored
                // container so it survives later reads (`$o.h.of`, `.push` type
                // enforcement). Hash metadata lives in `HashData`; without this
                // an assignment to a typed `%`/`@` attribute would drop the type
                // (the old side table re-derived it on every read).
                if matches!(attr_sigil, '@' | '%')
                    && matches!(
                        assigned_value.view(),
                        ValueView::Hash(_) | ValueView::Array(..)
                    )
                    && let Some(tc) = self.get_attr_type_constraint(&class_name.resolve(), method)
                    && !matches!(tc.as_str(), "Mu" | "Any")
                {
                    // Split an object-hash `ValueType{KeyType}` so the container
                    // records the value and key constraints separately.
                    let (value_type, key_type) =
                        crate::runtime::types::split_object_hash_constraint(&tc);
                    let info = ContainerTypeInfo {
                        value_type: value_type.to_string(),
                        key_type: key_type.map(|k| k.to_string()),
                        declared_type: None,
                    };
                    assigned_value = self.tag_container_metadata(assigned_value, info);
                }
                // Write through an existing `ContainerRef` slot (preserving any
                // `:=`-bound alias of the attribute container); otherwise replace
                // the entry as a bare value.
                updated.insert_through(attr_key.as_str(), assigned_value.clone());
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
        // `is raw` container-accessor return (`method AT-KEY($k) is raw {
        // %!hash.AT-KEY($k) }`): assigning through it (`$obj.AT-KEY($k) = $v`,
        // e.g. Hash::Agnostic's `self.AT-KEY($key) = value`) must reach the
        // container the body yields. Bind the method's params + `self`, then
        // recurse the lvalue assignment into the body's accessor expression.
        if let Some(result) =
            self.try_raw_accessor_method_lvalue(&target, &method_def, &method_args, &value)?
        {
            return Ok(result);
        }
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
                .unwrap_or(Value::NIL);
            if delegate != Value::NIL {
                let sigil = match delegate.view() {
                    ValueView::Array(..) => "@",
                    ValueView::Hash(_) => "%",
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
                let updated_delegate = self.env.get(&temp_var).cloned().unwrap_or(Value::NIL);
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
        // A method whose body is `return-rw $!attr` exposes a writable
        // container even without an `is rw` trait (URI's `multi method
        // fragment(URI:D: --> Fragment) { return-rw $!fragment }`).
        let returns_rw_attr =
            Self::rw_method_attribute_target(&method_def.body).is_some_and(|_| {
                method_def
                    .body
                    .iter()
                    .any(Self::stmt_contains_return_rw_call)
            });
        if !method_def.is_rw && !returns_rw_attr {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: method '{}' is not rw",
                method
            )));
        }
        if let Some(attr_name) = Self::rw_method_attribute_target(&method_def.body) {
            let mut updated = attributes.to_map();
            let current = if method_args.is_empty() {
                self.call_method_with_values(
                    Value::instance_parts(class_name, attributes.clone(), target_id),
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
            if assigned_value.is_nil()
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

        // `method z() is rw { self[2] }` on an `is Array` subclass — assign into
        // the invocant's backing array storage element. The index is a literal
        // or a positional parameter of the method.
        if attributes.contains_key("__mutsu_array_storage")
            && let Some((index_expr, is_pos)) = Self::rw_method_self_index_target(&method_def.body)
        {
            let idx_val = match &index_expr {
                Expr::Literal(v) => v.clone(),
                Expr::Var(param) => match method_def
                    .params
                    .iter()
                    .position(|p| p == param)
                    .and_then(|pos| method_args.get(pos).cloned())
                {
                    Some(v) => v,
                    None => self.eval_block_value(&[Stmt::Expr(index_expr.clone())])?,
                },
                other => self.eval_block_value(&[Stmt::Expr(other.clone())])?,
            };
            return self.assign_rw_indexed_attr(
                &attributes,
                class_name,
                target_id,
                target_var,
                "__mutsu_array_storage",
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
                .unwrap_or(Value::NIL);
            if delegate != Value::NIL {
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
                let updated_delegate = self.env.get(&temp_var).cloned().unwrap_or(Value::NIL);
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
        if let ValueView::Proxy { storer, .. } = method_result.view() {
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

    /// The variable name (with sigil) a target expression assigns through, or
    /// `None` for a non-variable target. `%!hash` -> `Some("%!hash")`.
    fn expr_lvalue_var_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(n) => Some(format!("${}", n)),
            Expr::ArrayVar(n) => Some(format!("@{}", n)),
            Expr::HashVar(n) => Some(format!("%{}", n)),
            _ => None,
        }
    }

    /// Assign through an `is raw` container-accessor method whose body is a
    /// single `EXPR.AT-KEY(...)` / `EXPR.AT-POS(...)` call: bind the method's
    /// positional params and `self`, evaluate the body's receiver + index in
    /// that scope, and recurse the lvalue assignment into it. Returns `None`
    /// (leaving the caller to fall through) when the body is not such an
    /// accessor. This is what makes Hash::Agnostic's role `ASSIGN-KEY`
    /// (`self.AT-KEY($key) = value`) reach the consumer's backing store.
    fn try_raw_accessor_method_lvalue(
        &mut self,
        target: &Value,
        method_def: &MethodDef,
        method_args: &[Value],
        value: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let stmts: Vec<&Stmt> = method_def
            .body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        if stmts.len() != 1 {
            return Ok(None);
        }
        let Stmt::Expr(Expr::MethodCall {
            target: inner_target,
            name,
            args: inner_args,
            ..
        }) = stmts[0]
        else {
            return Ok(None);
        };
        let inner_method = name.as_str();
        // Only recurse through the two positional/associative element accessors.
        if !matches!(inner_method, "AT-KEY" | "AT-POS") {
            return Ok(None);
        }
        // Save and rebind `self` + the method's positional (non-invocant,
        // non-slurpy, non-named) params so the body's receiver/index evaluate
        // in the method's scope, then restore afterwards.
        let mut saved: Vec<(String, Option<Value>)> = Vec::new();
        let mut bind = |this: &mut Self, key: String, val: Value| {
            saved.push((key.clone(), this.env.get(&key).cloned()));
            this.env.insert(key, val);
        };
        bind(self, "self".to_string(), target.clone());
        let mut ai = 0;
        for pd in &method_def.param_defs {
            if pd.is_invocant || pd.named || pd.slurpy {
                continue;
            }
            if let Some(a) = method_args.get(ai) {
                // The body may read the param under its sigil'd name (`$key`) or
                // its bare slot name (`key`); bind both so evaluation resolves it
                // regardless of which form the reader uses.
                let bare = pd.name.trim_start_matches(['$', '@', '%', '&']);
                let sigil = pd
                    .name
                    .chars()
                    .next()
                    .filter(|c| matches!(c, '$' | '@' | '%' | '&'))
                    .unwrap_or('$');
                bind(self, format!("{}{}", sigil, bare), a.clone());
                bind(self, bare.to_string(), a.clone());
            }
            ai += 1;
        }
        let restore = |this: &mut Self, saved: Vec<(String, Option<Value>)>| {
            for (key, prev) in saved.into_iter().rev() {
                match prev {
                    Some(v) => {
                        this.env.insert(key, v);
                    }
                    None => {
                        this.env.remove(&key);
                    }
                }
            }
        };
        let inner_var = Self::expr_lvalue_var_name(inner_target);
        let recur = (|| {
            let inner_target_val =
                self.eval_block_value(&[Stmt::Expr((**inner_target).clone())])?;
            let mut inner_arg_vals = Vec::with_capacity(inner_args.len());
            for e in inner_args {
                inner_arg_vals.push(self.eval_block_value(&[Stmt::Expr(e.clone())])?);
            }
            self.assign_method_lvalue_with_values(
                inner_var.as_deref(),
                inner_target_val,
                inner_method,
                inner_arg_vals,
                value.clone(),
            )
        })();
        restore(self, saved);
        Ok(Some(recur?))
    }
}
