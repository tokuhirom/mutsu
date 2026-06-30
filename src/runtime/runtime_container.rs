use super::*;

impl Interpreter {
    /// Autovivify a typed array element when a mutating array method (push/append/
    /// unshift/prepend) is called on its type object, e.g. `my Array of Int @x;
    /// @x[0].push(3)`. `type_name` is the element type object name (e.g.
    /// `Array[Int]`). Returns `Some(new_array)` when `type_name` is a positional
    /// container type, `None` otherwise. The result carries type metadata so the
    /// elements stay constrained.
    pub(crate) fn autoviv_typed_array_push(
        &mut self,
        type_name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let base = type_name.split('[').next().unwrap_or(type_name);
        if !matches!(base, "Array" | "List") {
            return Ok(None);
        }
        let inner = type_name
            .strip_prefix(base)
            .and_then(|s| s.strip_prefix('['))
            .and_then(|s| s.strip_suffix(']'))
            .map(|s| s.trim().to_string());
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Slip(vals) => items.extend(vals.iter().cloned()),
                other => items.push(other.clone()),
            }
        }
        if let Some(ref constraint) = inner
            && constraint.starts_with(char::is_uppercase)
        {
            for item in &items {
                if !self.type_matches_value(constraint, item) {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        "", constraint, item,
                    ));
                }
            }
        }
        let result = Value::real_array(items);
        if let Some(constraint) = inner {
            let info = ContainerTypeInfo {
                value_type: constraint,
                key_type: None,
                declared_type: Some(type_name.to_string()),
            };
            self.register_container_type_metadata(&result, info);
        }
        Ok(Some(result))
    }

    /// Build a `ContainerTypeInfo` from a hash's embedded type metadata, or
    /// `None` when the hash carries no type metadata. This is the authoritative
    /// source of hash type metadata, replacing the old `hash_type_metadata`
    /// Arc-pointer-keyed side table: the metadata travels WITH the `HashData`
    /// through copy-on-write and rebuilds, so a freshly-built hash literal can
    /// never inherit a stale typed-hash entry via pointer reuse.
    pub(crate) fn hashdata_type_info(hd: &crate::value::HashData) -> Option<ContainerTypeInfo> {
        if !hd.has_type_meta() {
            return None;
        }
        Some(ContainerTypeInfo {
            value_type: hd.value_type.clone().unwrap_or_default(),
            key_type: hd.key_type.clone(),
            declared_type: hd.declared_type.clone(),
        })
    }

    /// Attach container type metadata to `value`, returning the (possibly
    /// rebuilt) value. For hashes and Set/Bag/Mix the metadata is embedded in
    /// the backing data struct (via copy-on-write `Arc::make_mut`), so callers
    /// MUST use the returned value — store it back into the env/local slot it
    /// came from. For other container types this defers to the Arc-pointer
    /// side tables (unchanged) and returns the value untouched.
    pub(crate) fn tag_container_metadata(
        &mut self,
        value: Value,
        info: ContainerTypeInfo,
    ) -> Value {
        // Skip the copy-on-write clone when the metadata is already present.
        // `Arc::make_mut` on a shared container clones the backing data, which
        // changes the container's identity (`.WHICH` is pointer-based) — so a
        // no-op re-tag of e.g. `$map.Map` would otherwise break
        // `$map.Map === $map`.
        macro_rules! embed_type_info {
            ($arc:ident, $info:ident) => {{
                let new_vt = (!$info.value_type.is_empty()).then(|| $info.value_type.clone());
                if $arc.value_type != new_vt
                    || $arc.key_type != $info.key_type
                    || $arc.declared_type != $info.declared_type
                {
                    let data = Arc::make_mut(&mut $arc);
                    data.value_type = new_vt;
                    data.key_type = $info.key_type.clone();
                    data.declared_type = $info.declared_type.clone();
                }
            }};
        }
        match value {
            Value::Array(mut arc, kind) => {
                embed_type_info!(arc, info);
                Value::Array(arc, kind)
            }
            Value::Hash(mut arc) => {
                embed_type_info!(arc, info);
                Value::Hash(arc)
            }
            Value::Set(mut arc, m) => {
                embed_type_info!(arc, info);
                Value::Set(arc, m)
            }
            Value::Bag(mut arc, m) => {
                embed_type_info!(arc, info);
                Value::Bag(arc, m)
            }
            Value::Mix(mut arc, m) => {
                embed_type_info!(arc, info);
                Value::Mix(arc, m)
            }
            Value::Mixin(inner, m)
                if matches!(
                    inner.as_ref(),
                    Value::Array(..)
                        | Value::Hash(_)
                        | Value::Set(..)
                        | Value::Bag(..)
                        | Value::Mix(..)
                ) =>
            {
                let tagged = self.tag_container_metadata((*inner).clone(), info);
                Value::Mixin(Arc::new(tagged), m)
            }
            other => {
                self.register_container_type_metadata(&other, info);
                other
            }
        }
    }

    pub(crate) fn register_container_type_metadata(
        &mut self,
        value: &Value,
        info: ContainerTypeInfo,
    ) {
        match value {
            Value::Array(..)
            | Value::Hash(_)
            | Value::Set(..)
            | Value::Bag(..)
            | Value::Mix(..) => {
                // Array/Hash/Set/Bag/Mix type metadata is embedded in the backing
                // data struct — see `tag_container_metadata`. Reaching here
                // means such a value flowed through the by-reference register
                // path, which cannot embed (it has no owning slot to write
                // back). Such sites are migrated to `tag_container_metadata`;
                // this arm is intentionally a no-op.
                debug_assert!(
                    false,
                    "register_container_type_metadata called on a {}; use tag_container_metadata",
                    crate::value::what_type_name(value)
                );
            }
            Value::Instance { id, .. } => {
                self.instance_type_metadata
                    .write()
                    .unwrap()
                    .insert(*id, info);
            }
            Value::Mixin(inner, _) => self.register_container_type_metadata(inner, info),
            _ => {}
        }
    }

    /// Re-attach previously-captured container type metadata to whatever array
    /// is now bound to `key`. In-place array mutators (pop/shift/splice/append/
    /// prepend/unshift) call `Arc::make_mut`, which reallocates the backing
    /// buffer whenever the Arc is shared (e.g. the method receiver still holds a
    /// clone). The fresh heap pointer has no entry in the pointer-keyed
    /// `array_type_metadata` map, which silently demotes a typed `array[int]` /
    /// `Array[Int]` to a plain `Array`. Re-registering the saved metadata under
    /// the new pointer preserves the declared type across the mutation. No-op
    /// when the array was untyped (`saved` is `None`).
    pub(crate) fn reattach_array_type_metadata(
        &mut self,
        key: &str,
        saved: &Option<ContainerTypeInfo>,
    ) {
        if let Some(info) = saved
            && let Some(arr @ Value::Array(..)) = self.env.get(key).cloned()
        {
            let tagged = self.tag_container_metadata(arr, info.clone());
            self.env.insert(key.to_string(), tagged);
        }
    }

    /// Clear a hash's embedded type metadata, returning the rebuilt value.
    /// Callers must store the returned value back into its slot.
    pub(crate) fn clear_hash_type_metadata(value: Value) -> Value {
        if let Value::Hash(mut arc) = value {
            if arc.has_type_meta() {
                Arc::make_mut(&mut arc).clear_type_meta();
            }
            return Value::Hash(arc);
        }
        if let Value::Array(mut arc, kind) = value {
            if arc.has_type_meta() {
                let data = Arc::make_mut(&mut arc);
                data.value_type = None;
                data.key_type = None;
                data.declared_type = None;
            }
            return Value::Array(arc, kind);
        }
        value
    }

    pub(crate) fn container_type_metadata(&self, value: &Value) -> Option<ContainerTypeInfo> {
        container_type_metadata_with(value, &self.instance_type_metadata)
    }

    // Object-hash original keys are embedded in `HashData.original_keys`
    // (see `runtime::utils::set_hash_original_keys` / `hash_original_keys_snapshot`),
    // so the `hash_object_keys` side table and its pointer-migration helpers are
    // gone — the map now travels with the hash across copy-on-write.

    /// Check if a hash is an object hash (has a key_type constraint).
    pub(crate) fn is_object_hash(&self, hash: &Value) -> bool {
        self.hash_key_type(hash).is_some()
    }

    /// Get the key type constraint for an object hash, if any.
    pub(crate) fn hash_key_type(&self, hash: &Value) -> Option<String> {
        if let Value::Hash(arc) = hash {
            return arc.key_type.clone();
        }
        None
    }

    pub(crate) fn register_var_container_type_metadata(
        &mut self,
        name: &str,
        info: &ContainerTypeInfo,
    ) {
        if let Some(value) = self.env.get(name).cloned() {
            // Hashes embed metadata in `HashData`, so the tagged value must be
            // stored back; non-hash containers use the Arc-pointer side tables
            // and `tag_container_metadata` returns the same Arc (re-insert is a
            // no-op for them).
            let tagged = self.tag_container_metadata(value, info.clone());
            self.env.insert(name.to_string(), tagged);
        }
    }

    pub(crate) fn parse_container_constraint(name: &str, raw: &str) -> ContainerTypeInfo {
        let raw = raw.trim();
        // Note: For %-sigil variables, Hash[X] means "elements are Hash[X]",
        // NOT "elements are X". We do NOT unwrap Hash[...] here.
        // The only special case for % is the `TypeName{KeyType}` syntax
        // (e.g. `my Int %h{Str}`) which specifies both value and key types.
        if name.starts_with('%')
            && let Some((value_type, key_part)) = raw.split_once('{')
            && let Some(key_type) = key_part.strip_suffix('}')
        {
            return ContainerTypeInfo {
                value_type: value_type.trim().to_string(),
                key_type: Some(key_type.trim().to_string()),
                declared_type: Some(format!("Hash[{},{}]", value_type.trim(), key_type.trim())),
            };
        }
        // Note: For @-sigil variables, Array[X] means "elements are Array[X]",
        // NOT "elements are X". We do NOT unwrap Array[...] here.
        // `my Int @a` has raw="Int" and `my Array[Int] @a` has raw="Array[Int]".
        ContainerTypeInfo {
            value_type: raw.to_string(),
            key_type: None,
            declared_type: if name.starts_with('@')
                && crate::runtime::native_types::is_native_array_element_type(raw)
            {
                Some(format!("array[{raw}]"))
            } else {
                None
            },
        }
    }

    pub(crate) fn snapshot_var_type_constraints(&self) -> HashMap<String, String> {
        self.var_type_constraints.clone()
    }

    pub(crate) fn restore_var_type_constraints(&mut self, snapshot: HashMap<String, String>) {
        self.var_type_constraints = snapshot;
    }

    /// Check that all values satisfy the element type constraint for a
    /// container variable (e.g. `my Int @a`). Returns Ok(()) if no constraint
    /// exists or all values pass; returns a type-check error otherwise.
    pub(crate) fn check_container_element_types(
        &mut self,
        var_name: &str,
        values: &[Value],
    ) -> Result<(), RuntimeError> {
        if let Some(constraint) = self.var_type_constraint(var_name) {
            for val in values {
                if !matches!(val, Value::Nil) && !self.type_matches_value(&constraint, val) {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        var_name,
                        &constraint,
                        val,
                    ));
                }
            }
        }
        Ok(())
    }

    /// Check element type constraints using container metadata (for typed arrays
    /// obtained from instance attributes or other non-variable sources).
    pub(crate) fn check_array_value_element_types(
        &mut self,
        target: &Value,
        values: &[Value],
    ) -> Result<(), RuntimeError> {
        if let Some(info) = self.container_type_metadata(target) {
            let constraint = &info.value_type;
            if constraint != "Mu" && constraint != "Any" {
                for val in values {
                    if !matches!(val, Value::Nil) && !self.type_matches_value(constraint, val) {
                        return Err(crate::runtime::utils::type_check_element_typed_error(
                            "@_", constraint, val,
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    /// Finalize a typed `@`/`%` constructor attribute (`has Int @.nums` /
    /// `has Int %.map`): type-check each element against the declared element
    /// type (matching the variable path `my Int @a`, and raku's "Type check
    /// failed for an element of @!nums") and tag the container with element-type
    /// metadata so `.of` / `.WHAT` / `~~ Array[Int]` reflect the declared type.
    ///
    /// `value` must be the exact `Value` (Array/Hash) that will be stored in the
    /// instance: the metadata is keyed by the backing `Arc`'s pointer, so it has
    /// to be registered against the Arc that survives into `make_instance`.
    pub(crate) fn finalize_typed_container_attr(
        &mut self,
        attr_name: &str,
        sigil: char,
        elem_type: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // An object-hash attribute (`%.a{Str:D}`) carries its declared type as
        // `ValueType{KeyType}`; the element type is the value part, and the key
        // part is tagged onto the container as the key constraint.
        let (elem_type, key_type) = crate::runtime::types::split_object_hash_constraint(elem_type);
        if elem_type != "Mu" && elem_type != "Any" {
            let display = format!("{}!{}", sigil, attr_name);
            // Collect the values to type-check. A shaped array (`has Int @.g[2;2]`)
            // nests its leaves inside per-dimension sub-arrays, so descend to the
            // leaves; a plain array checks its direct elements (a `has Array @.x`
            // legitimately holds array elements, so do not descend into those).
            fn collect_leaves<'a>(v: &'a Value, descend: bool, out: &mut Vec<&'a Value>) {
                match v {
                    Value::Array(items, kind) if descend || matches!(kind, ArrayKind::Shaped) => {
                        for it in items.iter() {
                            collect_leaves(it, true, out);
                        }
                    }
                    _ => out.push(v),
                }
            }
            let mut elems: Vec<&Value> = Vec::new();
            match &value {
                Value::Array(items, ArrayKind::Shaped) => {
                    for it in items.iter() {
                        collect_leaves(it, true, &mut elems);
                    }
                }
                Value::Array(items, _) => elems.extend(items.iter()),
                Value::Hash(map) => elems.extend(map.values()),
                _ => {}
            }
            for it in elems {
                if !matches!(it, Value::Nil) && !self.type_matches_value(elem_type, it) {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        &display, elem_type, it,
                    ));
                }
            }
        }
        let info = ContainerTypeInfo {
            value_type: elem_type.to_string(),
            key_type: key_type.map(|k| k.to_string()),
            declared_type: None,
        };
        Ok(self.tag_container_metadata(value, info))
    }

    pub(crate) fn is_var_dynamic(&self, name: &str) -> bool {
        let bare = Self::normalize_var_meta_name(name);
        // The implicit special variables `$_`, `$/`, `$!` are dynamic by nature
        // (no `*` twigil, but `.VAR.dynamic` is True and they are visible through
        // CALLER::), so report them as dynamic even without an explicit flag.
        if matches!(bare, "_" | "/" | "!") {
            return true;
        }
        self.var_dynamic_flags.get(bare).copied().unwrap_or(false)
    }
}
