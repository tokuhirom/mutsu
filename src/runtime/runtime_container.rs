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
            match arg.view() {
                ValueView::Slip(vals) => items.extend(vals.iter().cloned()),
                _ => items.push(arg.clone()),
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
        let mut result = Value::real_array(items);
        if let Some(constraint) = inner {
            let info = ContainerTypeInfo {
                value_type: constraint,
                key_type: None,
                declared_type: Some(type_name.to_string()),
            };
            // Array metadata is embedded in `ArrayData`; the pointer-keyed
            // `register_container_type_metadata` path panics for arrays.
            result = self.tag_container_metadata(result, info);
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
        mut value: Value,
        info: ContainerTypeInfo,
    ) -> Value {
        // Skip the copy-on-write clone when the metadata is already present.
        // `Arc::make_mut` on a shared container clones the backing data, which
        // changes the container's identity (`.WHICH` is pointer-based) — so a
        // no-op re-tag of e.g. `$map.Map` would otherwise break
        // `$map.Map === $map`.
        macro_rules! embed_type_info {
            ($arc:ident) => {{
                let new_vt = (!info.value_type.is_empty()).then(|| info.value_type.clone());
                if $arc.value_type != new_vt
                    || $arc.key_type != info.key_type
                    || $arc.declared_type != info.declared_type
                {
                    let data = crate::gc::ContainerMakeMut::container_make_mut($arc);
                    data.value_type = new_vt;
                    data.key_type = info.key_type.clone();
                    data.declared_type = info.declared_type.clone();
                }
            }};
        }
        if value
            .with_array_mut(|arc, _kind| embed_type_info!(arc))
            .is_some()
        {
            return value;
        }
        if value
            .with_hash_mut(|arc| {
                embed_type_info!(arc);
                // An object hash (`my %h{Any}` / `:{...}`) keys by the `.WHICH`
                // of the key *object*. The list→hash coercion that built the
                // value is key-type-blind and stringified the keys (recording
                // the key objects in `original_keys`), so enforce the keying
                // invariant here — the chokepoint every declared-constraint
                // assignment and `SetVarType` tagging flows through.
                if info.key_type.is_some() {
                    crate::runtime::utils::ensure_object_hash_which_keys(arc);
                }
            })
            .is_some()
        {
            return value;
        }
        if value
            .with_set_mut(|arc, _m| embed_type_info!(arc))
            .is_some()
        {
            return value;
        }
        if value
            .with_bag_mut(|arc, _m| embed_type_info!(arc))
            .is_some()
        {
            return value;
        }
        if value
            .with_mix_mut(|arc, _m| embed_type_info!(arc))
            .is_some()
        {
            return value;
        }
        let mixin_parts = match value.view() {
            ValueView::Mixin(inner, m)
                if matches!(
                    inner.as_ref().view(),
                    ValueView::Array(..)
                        | ValueView::Hash(_)
                        | ValueView::Set(..)
                        | ValueView::Bag(..)
                        | ValueView::Mix(..)
                ) =>
            {
                Some((Arc::clone(inner), Arc::clone(m)))
            }
            _ => None,
        };
        if let Some((inner, m)) = mixin_parts {
            let tagged = self.tag_container_metadata((*inner).clone(), info);
            return Value::mixin_parts(Arc::new(tagged), m);
        }
        self.register_container_type_metadata(&value, info);
        value
    }

    pub(crate) fn register_container_type_metadata(
        &mut self,
        value: &Value,
        info: ContainerTypeInfo,
    ) {
        match value.view() {
            ValueView::Array(..)
            | ValueView::Hash(_)
            | ValueView::Set(..)
            | ValueView::Bag(..)
            | ValueView::Mix(..) => {
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
            ValueView::Instance { id, .. } => {
                self.instance_type_metadata
                    .write()
                    .unwrap()
                    .insert(id, info);
            }
            ValueView::Mixin(inner, _) => self.register_container_type_metadata(inner, info),
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
            && let Some(arr) = self.env.get(key).cloned()
            && matches!(arr.view(), ValueView::Array(..))
        {
            let tagged = self.tag_container_metadata(arr, info.clone());
            self.env.insert(key.to_string(), tagged);
        }
    }

    /// Clear a hash's embedded type metadata, returning the rebuilt value.
    /// Callers must store the returned value back into its slot.
    pub(crate) fn clear_hash_type_metadata(mut value: Value) -> Value {
        if value
            .with_hash_mut(|arc| {
                if arc.has_type_meta() {
                    crate::gc::Gc::make_mut(arc).clear_type_meta();
                }
            })
            .is_some()
        {
            return value;
        }
        if value
            .with_array_mut(|arc, _kind| {
                if arc.has_type_meta() {
                    let data = crate::gc::Gc::make_mut(arc);
                    data.value_type = None;
                    data.key_type = None;
                    data.declared_type = None;
                }
            })
            .is_some()
        {
            return value;
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
        if let ValueView::Hash(arc) = hash.view() {
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

    /// Element type constraint for a container variable, preferring the
    /// metadata embedded in the value itself over the name-keyed
    /// `var_type_constraints` side table. The embedded metadata travels with
    /// the value through frame save/restore, so it stays correct when a
    /// recursive call re-binds a same-named variable to a differently-typed
    /// container (`my @ret := Array[T].new` in a recursive sub) — the
    /// name-keyed store is clobbered by the inner frame in that case.
    pub(crate) fn element_constraint_for(&self, var_name: &str, value: &Value) -> Option<String> {
        if let Some(info) = self.container_type_metadata(value) {
            if info.value_type.is_empty() {
                return None;
            }
            return Some(info.value_type);
        }
        self.var_type_constraint(var_name)
    }

    /// Check that all values satisfy the element type constraint for a
    /// container variable (e.g. `my Int @a`). Returns Ok(()) if no constraint
    /// exists or all values pass; returns a type-check error otherwise.
    /// `target` is the variable's current value: its embedded metadata takes
    /// priority over the name-keyed constraint (see `element_constraint_for`).
    pub(crate) fn check_container_element_types(
        &mut self,
        var_name: &str,
        target: &Value,
        values: &[Value],
    ) -> Result<(), RuntimeError> {
        if let Some(constraint) = self.element_constraint_for(var_name, target) {
            for val in values {
                if !val.is_nil() && !self.type_matches_value(&constraint, val) {
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
                    if !val.is_nil() && !self.type_matches_value(constraint, val) {
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
            fn collect_leaves(v: &Value, descend: bool, out: &mut Vec<Value>) {
                match v.view() {
                    ValueView::Array(items, kind)
                        if descend || matches!(kind, ArrayKind::Shaped) =>
                    {
                        for it in items.iter() {
                            collect_leaves(it, true, out);
                        }
                    }
                    _ => out.push(v.clone()),
                }
            }
            // A shaped array's unset cell seed (the Any type object) re-seeds
            // as the element type object, like the variable path's typed
            // coercion (`has Int @.g[2;2]` cells read as `Int`).
            fn reseed_shaped_cells(v: &Value, elem: &Value) -> Value {
                let ValueView::Array(items, kind) = v.view() else {
                    return v.clone();
                };
                if !items
                    .iter()
                    .any(|it| it.is_any_type_object() || matches!(it.view(), ValueView::Array(..)))
                {
                    return v.clone();
                }
                let rebuilt: Vec<Value> = items
                    .iter()
                    .map(|it| {
                        if it.is_any_type_object() {
                            elem.clone()
                        } else if matches!(it.view(), ValueView::Array(..)) {
                            reseed_shaped_cells(it, elem)
                        } else {
                            it.clone()
                        }
                    })
                    .collect();
                Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(rebuilt)),
                    kind,
                )
            }
            let value = if matches!(value.view(), ValueView::Array(_, ArrayKind::Shaped)) {
                let elem = Value::package(crate::symbol::Symbol::intern(elem_type));
                let shape = crate::runtime::utils::shaped_array_shape(&value);
                let reseeded = reseed_shaped_cells(&value, &elem);
                crate::runtime::utils::mark_shaped_array(&reseeded, shape.as_deref());
                reseeded
            } else {
                value
            };
            let mut elems: Vec<Value> = Vec::new();
            match value.view() {
                ValueView::Array(items, ArrayKind::Shaped) => {
                    for it in items.iter() {
                        collect_leaves(it, true, &mut elems);
                    }
                }
                ValueView::Array(items, _) => elems.extend(items.iter().cloned()),
                ValueView::Hash(map) => elems.extend(map.values().cloned()),
                _ => {}
            }
            for it in &elems {
                if !it.is_nil()
                    && !self.type_matches_value(elem_type, it)
                    // The freshly re-seeded element type object is type-legal.
                    && !(matches!(it.view(), ValueView::Package(p) if p.resolve() == elem_type))
                {
                    return Err(crate::runtime::utils::type_check_element_typed_error(
                        &display, elem_type, it,
                    ));
                }
            }
            let info = ContainerTypeInfo {
                value_type: elem_type.to_string(),
                key_type: key_type.map(|k| k.to_string()),
                declared_type: None,
            };
            return Ok(self.tag_container_metadata(value, info));
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
