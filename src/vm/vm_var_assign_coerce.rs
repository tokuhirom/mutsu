use super::*;

impl Interpreter {
    /// Type-check a `:=` bind to a typed-hash variable.
    ///
    /// Binding (unlike assignment) does not coerce — it aliases the RHS
    /// container directly. Raku therefore requires the RHS to *be* an
    /// `Associative[T]` matching the variable's declared value type `T`:
    ///
    /// ```text
    /// my Int %a; my Int  %b := %a;            # ok   (Int does Int)
    /// my Int %a; my Cool %b := %a;            # ok   (Int does Cool)
    /// my Int %h := :42foo.Set.Hash;           # dies (Hash[Any,Any])
    /// ```
    ///
    /// mutsu tracks a hash's element type via `container_type_metadata`; a
    /// plain/coerced hash has no metadata, so its value type defaults to `Mu`,
    /// which only conforms to an `Any`/`Mu`-typed target.
    pub(crate) fn check_hash_bind_value_type(
        &mut self,
        name: &str,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        if !name.starts_with('%') {
            return Ok(());
        }
        let Some(constraint) = loan_env!(self, var_type_constraint(name)) else {
            return Ok(());
        };
        // Only enforce for a plain value-type constraint (`my Int %h`). Skip
        // container-trait declarations (`is Set`/`is Bag`/`is Map`/...), key-typed
        // forms (`%h{Int}`), and the trivial Any/Mu types.
        if constraint.contains('{')
            || matches!(constraint.as_str(), "" | "Any" | "Mu")
            || Self::quant_hash_trait_from_constraint(&constraint).is_some()
            || matches!(
                constraint.split('[').next().unwrap_or(&constraint),
                "Set"
                    | "SetHash"
                    | "Bag"
                    | "BagHash"
                    | "Mix"
                    | "MixHash"
                    | "Map"
                    | "Hash"
                    | "Associative"
                    | "QuantHash"
            )
        {
            return Ok(());
        }
        // The bound container's element type (defaults to Mu when untyped).
        let rhs_value_type = self
            .container_type_metadata(value)
            .map(|info| info.value_type)
            .filter(|vt| !vt.is_empty())
            .unwrap_or_else(|| "Mu".to_string());
        if crate::runtime::Interpreter::type_matches(&constraint, &rhs_value_type) {
            return Ok(());
        }
        let got = crate::runtime::utils::value_type_name(value);
        let message = format!(
            "Type check failed in binding; expected Associative[{}] but got {}",
            constraint, got
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("got".to_string(), value.clone());
        attrs.insert(
            "expected".to_string(),
            Value::package(crate::symbol::Symbol::intern(&format!(
                "Associative[{}]",
                constraint
            ))),
        );
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(
            crate::symbol::Symbol::intern("X::TypeCheck::Binding"),
            attrs,
        );
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        Err(err)
    }

    /// Identity-preserving STORE for a declared QuantHash variable:
    /// `%s = <a b>` on a `my %s is SetHash` writes the coerced contents INTO
    /// the variable's existing container node (keeping the node's embedded
    /// type metadata), so every holder of that node — the env mirror the
    /// trait op installed, `:=` binds, by-value captures — observes the
    /// reassignment (container identity §3). Without this, the coercion
    /// returns a fresh node that `SetLocal` stores only into the local slot,
    /// leaving the env mirror stale (a later name-based subscript assign then
    /// mutates the stale empty container and clobbers the slot on writeback).
    /// Falls through when the variable has no existing same-kind container.
    fn quanthash_store_preserving_identity(&mut self, name: &str, coerced: Value) -> Value {
        let existing = self.env().get(name).cloned();
        match (existing.as_ref().map(Value::view), coerced.view()) {
            (Some(ValueView::Set(old, mutable)), ValueView::Set(new, _))
                if !crate::gc::Gc::ptr_eq(&old, &new) =>
            {
                let mut data = (**new).clone();
                data.value_type = old.value_type.clone();
                data.key_type = old.key_type.clone();
                data.declared_type = old.declared_type.clone();
                // SAFETY: audited aliased in-place container write; see
                // `value::aliased_mut` (no other borrow live, single write).
                unsafe {
                    *crate::value::gc_contents_mut(&old) = data;
                }
                Value::set_parts(old.clone(), mutable)
            }
            (Some(ValueView::Bag(old, mutable)), ValueView::Bag(new, _))
                if !crate::gc::Gc::ptr_eq(&old, &new) =>
            {
                let mut data = (**new).clone();
                data.value_type = old.value_type.clone();
                data.key_type = old.key_type.clone();
                data.declared_type = old.declared_type.clone();
                // SAFETY: as above.
                unsafe {
                    *crate::value::gc_contents_mut(&old) = data;
                }
                Value::bag_parts(old.clone(), mutable)
            }
            (Some(ValueView::Mix(old, mutable)), ValueView::Mix(new, _))
                if !crate::gc::Gc::ptr_eq(&old, &new) =>
            {
                let mut data = (**new).clone();
                data.value_type = old.value_type.clone();
                data.key_type = old.key_type.clone();
                data.declared_type = old.declared_type.clone();
                // SAFETY: as above.
                unsafe {
                    *crate::value::gc_contents_mut(&old) = data;
                }
                Value::mix_parts(old.clone(), mutable)
            }
            _ => coerced,
        }
    }

    /// True when a `%` variable stores plain `Str` keys — no object-hash key
    /// constraint, or an explicit `Str` one. A bare type-object key coerces to
    /// "" (with the string-context warning) only for these; an object hash
    /// (`%h{Any}`, `%h{Int}`, ...) keeps its keys distinct.
    fn is_str_keyed_hash_var(&self, name: &str) -> bool {
        match self.var_hash_key_constraint(name) {
            None => true,
            Some(kc) => {
                let (base, _) = crate::runtime::types::strip_type_smiley(&kc);
                base == "Str"
            }
        }
    }

    pub(super) fn coerce_hash_var_value(
        &mut self,
        name: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(constraint) = self.var_type_constraint_fast(name).cloned()
            && let Some(trait_name) = Self::quant_hash_trait_from_constraint(&constraint)
        {
            // Only coerce if the variable IS a QuantHash container (declared via `is`),
            // not when the constraint is a value type (declared via `of`).
            // Check: if the current value is already a QuantHash, it's an `is` trait.
            let current = self.env().get(name).cloned();
            let is_quanthash_container = matches!(
                current.as_ref().map(Value::view),
                Some(ValueView::Bag(_, _) | ValueView::Mix(_, _) | ValueView::Set(_, _))
            );
            if is_quanthash_container {
                let coerced = self.try_compiled_method_or_interpret(value, trait_name, vec![])?;
                return Ok(self.quanthash_store_preserving_identity(name, coerced));
            }
        }
        if self.check_readonly_for_modify(name).is_err()
            && matches!(
                value.view(),
                ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _)
            )
        {
            return Ok(value);
        }
        // A multi-param for-loop `%`-binding keeps its QuantHash identity (Raku
        // binds params; it does not assign-coerce them). A Set/Bag/Mix value
        // passes through unchanged; any other value (e.g. a Seq of pairs from a
        // `%a = %reset.pairs` reset) is coerced to the binding's *current*
        // QuantHash type rather than collapsing to a plain Hash.
        if self.quanthash_bind_params.iter().any(|n| n == name) {
            if matches!(
                value.view(),
                ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _)
            ) {
                return Ok(value);
            }
            let trait_name = match self.env().get(name).map(Value::view) {
                Some(ValueView::Set(_, _)) => Some("SetHash"),
                Some(ValueView::Bag(_, _)) => Some("BagHash"),
                Some(ValueView::Mix(_, _)) => Some("MixHash"),
                _ => None,
            };
            if let Some(tn) = trait_name {
                return self.try_compiled_method_or_interpret(value, tn, vec![]);
            }
        }
        if let Some(constraint) = loan_env!(self, var_type_constraint(name))
            && constraint.starts_with("SetHash")
        {
            let result = runtime::utils::coerce_value_to_quanthash(&value);
            // SetHash should be mutable
            if let ValueView::Set(items, _) = result.view() {
                let coerced = Value::set_parts(items.clone(), true);
                return Ok(self.quanthash_store_preserving_identity(name, coerced));
            }
            return Ok(result);
        }
        // A bare type-object key stringifies to "" with a warning only for a
        // plain (`Str`-keyed) hash. An object hash (`%h{Any}`, `%h{Int}`, ...)
        // keeps its keys distinct (by WHICH / reconstructed type) via
        // `build_hash_from_items`'s `original_keys` recording, so the coercion
        // must NOT run there — it would collapse distinct type-object keys to a
        // single "" and drop the object-hash key entirely.
        let build_items = |this: &mut Self, items: Vec<Value>| {
            if this.is_str_keyed_hash_var(name) {
                this.build_hash_from_items_warning(items)
            } else {
                runtime::utils::build_hash_from_items(items)
            }
        };
        // An object hash (`.WHICH`-keyed) assigned to a `%` variable
        // materializes as fresh entries: a plain target sees the stringified
        // original keys (raku: `my %p = %o` stringifies), while the key
        // objects are recorded in `original_keys` so a key-constrained target
        // re-keys by `.WHICH` when `tag_container_metadata` runs below.
        let value = match value.view() {
            ValueView::Hash(h) if h.has_typed_keys() => {
                let mut map = std::collections::HashMap::with_capacity(h.len());
                let mut orig = std::collections::HashMap::new();
                for (k, v) in h.iter() {
                    let key_obj = h.typed_key(k);
                    let str_key = Value::hash_key_encode(&key_obj);
                    if !matches!(key_obj.view(), ValueView::Str(_)) {
                        orig.insert(str_key.clone(), key_obj);
                    }
                    map.insert(str_key, v.clone());
                }
                runtime::utils::set_hash_original_keys(Value::hash(map), orig)
            }
            _ => value,
        };
        // For Array/Seq/Slip values, use `build_hash_from_items` which
        // raises "Odd number of elements" when appropriate. Hash values from
        // scalar containers (`$h`) are NOT pre-flattened, so they appear as
        // opaque items (triggering the odd-number check when expected).
        let hash_val = match value.view() {
            ValueView::Array(items, _) => {
                // `build_hash_from_items` flattens a bare `%h` element into its
                // pairs (`%m = %h, a => 42` and the single-element `%m = (%h,)`),
                // while a hash sourced from a `$` scalar carries
                // `HashData.itemized` and stays opaque (`%m = ($hashitem,)` →
                // "Odd number") — matching Raku.
                build_items(self, items.iter().cloned().collect())?
            }
            ValueView::Seq(items) | ValueView::Slip(items) => {
                build_items(self, items.iter().cloned().collect())?
            }
            // A single bare scalar assigned to a hash is a one-element (odd)
            // initializer: `my %h = 1` is X::Hash::Store::OddNumber. Hashes,
            // pairs, sets, instances, Nil, etc. keep their existing coercion.
            _ if matches!(
                value.clone().into_descalarized().view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Rat(..)
                    | ValueView::FatRat(..)
                    | ValueView::BigRat(..)
            ) =>
            {
                build_items(self, vec![value])?
            }
            _ => self.coerce_object_to_hash(value),
        };
        // Resolve hash sentinel entries (self-refs) and decont `:=`-bound
        // `ContainerRef` cells when assigning to a new hash variable:
        // assignment creates new containers, so the copy snapshots values
        // instead of sharing cells.
        if let ValueView::Hash(items) = hash_val.view()
            && (Self::hash_has_sentinels(&items) || items.values().any(Value::is_container_ref))
        {
            return Ok(self.resolve_hash_for_iteration(&items));
        }
        Ok(hash_val)
    }

    /// Coerce a value for `constant %x = ...` assignment.
    /// Associative values (Hash, Pair, Bag, Set, Mix) are preserved.
    /// Non-Associative values (Lists, Arrays) are coerced to Map.
    /// Instance objects that don't do Associative get `.Map` called.
    pub(crate) fn coerce_constant_hash_value(
        &mut self,
        name: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        match value.view() {
            // Associative types are preserved as-is
            ValueView::Hash(_)
            | ValueView::Pair(..)
            | ValueView::Set(..)
            | ValueView::Bag(..)
            | ValueView::Mix(..) => Ok(value),
            // Instance objects: check if they do Associative
            ValueView::Instance { class_name, .. } => {
                let cn = class_name.resolve();
                let does_associative = matches!(
                    cn.as_str(),
                    "Hash"
                        | "Map"
                        | "Pair"
                        | "Set"
                        | "Bag"
                        | "Mix"
                        | "SetHash"
                        | "BagHash"
                        | "MixHash"
                ) || self
                    .class_composed_roles(&cn)
                    .is_some_and(|roles| roles.iter().any(|r| r == "Associative"));
                if does_associative {
                    Ok(value)
                } else {
                    // Call .Map on non-Associative to coerce.
                    // Skip native methods so user-defined .Map is called.
                    let mapped = self.call_method_all_with_fallback(&value, "Map", &[], true)?;
                    let mapped_val = mapped.into_iter().next().unwrap_or(Value::NIL);
                    // Check that .Map returned an Associative
                    let is_assoc = matches!(
                        mapped_val.view(),
                        ValueView::Hash(_)
                            | ValueView::Pair(..)
                            | ValueView::Set(..)
                            | ValueView::Bag(..)
                            | ValueView::Mix(..)
                    );
                    if !is_assoc {
                        let got_type = crate::runtime::utils::value_type_name(&mapped_val);
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("got".to_string(), mapped_val);
                        attrs.insert(
                            "expected".to_string(),
                            Value::package(crate::symbol::Symbol::intern("Associative")),
                        );
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Type check failed in assignment to {}; expected Associative but got {}",
                                name, got_type
                            )),
                        );
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::TypeCheck"),
                            attrs,
                        );
                        let mut err = RuntimeError::new(format!(
                            "Type check failed in assignment to {}; expected Associative but got {}",
                            name, got_type
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    // Register as Map
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type: String::new(),
                        key_type: None,
                        declared_type: Some("Map".to_string()),
                    };
                    Ok(self.tag_container_metadata(mapped_val, info))
                }
            }
            // Non-Associative values: coerce to Map
            ValueView::Array(items, _) => {
                let hash = self.build_hash_from_items_warning(items.iter().cloned().collect())?;
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                Ok(self.tag_container_metadata(hash, info))
            }
            ValueView::Seq(items) | ValueView::Slip(items) => {
                let hash = self.build_hash_from_items_warning(items.iter().cloned().collect())?;
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                Ok(self.tag_container_metadata(hash, info))
            }
            _ => {
                // For other types (Int, Str, etc.), coerce to Map via
                // build_hash_from_items which raises X::Hash::Store::OddNumber
                // for odd element counts (e.g., `constant %h = 42`).
                let hash = self.build_hash_from_items_warning(vec![value])?;
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                Ok(self.tag_container_metadata(hash, info))
            }
        }
    }

    pub(super) fn mix_assignment_weight(value: &Value) -> Result<f64, RuntimeError> {
        match value.view() {
            ValueView::Int(i) => Ok(i as f64),
            ValueView::Num(n) => Ok(n),
            ValueView::Rat(n, d) if d != 0 => Ok(n as f64 / d as f64),
            ValueView::Bool(flag) => Ok(if flag { 1.0 } else { 0.0 }),
            ValueView::Str(s) => {
                // Try to parse as numeric; throw X::Str::Numeric on failure
                if let Ok(n) = s.parse::<f64>() {
                    Ok(n)
                } else {
                    Err(RuntimeError::str_numeric(
                        &s,
                        "base-10 number must begin with valid digits or '.'",
                    ))
                }
            }
            _ => {
                if value.truthy() {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
        }
    }

    /// Coerce a value assigned to a Bag/BagHash weight to an `i64` count, with
    /// the same numeric validation as Mix: a non-numeric `Str` raises
    /// X::Str::Numeric rather than being silently treated as truthy (1).
    pub(super) fn bag_assignment_count(value: &Value) -> Result<num_bigint::BigInt, RuntimeError> {
        use num_bigint::BigInt;
        match value.view() {
            ValueView::Int(i) => Ok(BigInt::from(i)),
            // Weights can exceed i64::MAX (e.g. `%h<k> = 10**19`), so a BigInt
            // weight is preserved verbatim rather than truncated to a native int.
            ValueView::BigInt(n) => Ok((**n).clone()),
            ValueView::Num(n) => Ok(BigInt::from(n as i64)),
            ValueView::Rat(n, d) if d != 0 => Ok(BigInt::from(n / d)),
            ValueView::Bool(flag) => Ok(BigInt::from(i64::from(flag))),
            ValueView::Str(s) => {
                if let Ok(n) = s.parse::<BigInt>() {
                    Ok(n)
                } else if let Ok(n) = s.parse::<f64>() {
                    Ok(BigInt::from(n as i64))
                } else {
                    Err(RuntimeError::str_numeric(
                        &s,
                        "base-10 number must begin with valid digits or '.'",
                    ))
                }
            }
            _ => {
                if value.truthy() {
                    Ok(BigInt::from(1))
                } else {
                    Ok(BigInt::from(0))
                }
            }
        }
    }

    pub(crate) const MAX_ASSIGN_SLICE_EXPAND: i64 = 100_000;

    pub(crate) fn assignment_rhs_values(
        &mut self,
        val: &Value,
    ) -> Result<Vec<Value>, RuntimeError> {
        Ok(match val.view() {
            ValueView::Array(v, ..) => v.as_ref().clone().items,
            ValueView::Seq(v) | ValueView::Slip(v) => v.iter().cloned().collect(),
            ValueView::Range(a, b) => {
                let end = b.min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < a {
                    Vec::new()
                } else {
                    (a..=end).map(Value::int).collect()
                }
            }
            ValueView::RangeExcl(a, b) => {
                let end = b.min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= a {
                    Vec::new()
                } else {
                    (a..end).map(Value::int).collect()
                }
            }
            ValueView::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let end = b.min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    Vec::new()
                } else {
                    (start..=end).map(Value::int).collect()
                }
            }
            ValueView::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let end = b.min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= start {
                    Vec::new()
                } else {
                    (start..end).map(Value::int).collect()
                }
            }
            // Non-i64 ranges (string `"a".."z"`, Rat, `.succ`-driven) must also
            // distribute across a slice, e.g. `@a[^10] = 'a'..'z'`. The i64
            // variants above stay inline for speed; everything else expands via
            // the shared `value_to_list` range walker.
            ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(val),
            // A live gather coroutine may be infinite (`@a[0,1,2] =
            // (loop { 42 })`): bounded pull so the slice fills without
            // hanging (a finite gather completes below the cap). Every other
            // lazy kind keeps the strict force (a lazy pipe's own bounded
            // machinery handles it).
            ValueView::LazyList(list) => {
                if list.coroutine.is_some() && list.cache.lock().unwrap().is_none() {
                    self.force_lazy_list_vm_n(&list, Self::MAX_ASSIGN_SLICE_EXPAND as usize)?
                } else {
                    self.force_lazy_list_vm(&list)?
                }
            }
            _ => vec![val.clone()],
        })
    }

    /// Track whether a scalar variable is bound (`:=`) to a Positional value.
    /// A bound scalar is NOT a Scalar container, so `@a = $bound` must flatten
    /// rather than itemize. The `ItemizeVar` opcode reads this marker. Plain
    /// assignment to a scalar clears any stale marker (guarded by
    /// `bound_decont_active` so the common no-bind case stays cheap).
    pub(super) fn update_bound_decont_marker(&mut self, name: &str, is_bind: bool, val: &Value) {
        // Scalar local names carry no sigil (e.g. "y"); `@`/`%`/`&` vars do.
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return;
        }
        if is_bind {
            // A scalar bound to a container (`$x := @a` / `:= %h` / `:= (1,2,3)`)
            // is not a Scalar container of its own: it aliases the container, so
            // `@a = $bound` flattens (ItemizeVar) and `$bound.VAR.^name` reflects
            // the container type (List/Array/Hash/Set/Bag/Mix) rather than Scalar.
            let is_container = match val.view() {
                ValueView::Array(_, k) => !k.is_itemized(),
                ValueView::Seq(_)
                | ValueView::Slip(_)
                | ValueView::LazyList(_)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::Hash(_)
                | ValueView::Set(..)
                | ValueView::Bag(..)
                | ValueView::Mix(..) => true,
                // A `:=` bind to a whole-container `@`/`%` variable holds a
                // shared cell whose inner value is the container.
                ValueView::ContainerRef(cell) => matches!(
                    cell.lock().unwrap().view(),
                    ValueView::Array(..)
                        | ValueView::Hash(_)
                        | ValueView::Set(..)
                        | ValueView::Bag(..)
                        | ValueView::Mix(..)
                ),
                _ => false,
            };
            if is_container {
                let key = format!("__mutsu_bound_decont::{}", name);
                self.env_mut().insert(key, Value::TRUE);
                self.bound_decont_active = true;
                return;
            }
        }
        if self.bound_decont_active {
            let key = format!("__mutsu_bound_decont::{}", name);
            self.env_mut().remove(&key);
        }
    }

    /// Slice 2a (`docs/scalar-array-sharing.md`): is `name` a `$` scalar that
    /// currently shares an array/hash by reference (`$n = @z`)? Such a scalar's
    /// whole reassignment REPLACES its slot instead of writing through the cell.
    pub(super) fn is_array_share_scalar(&self, name: &str) -> bool {
        self.env()
            .get(&format!("__mutsu_array_share::{}", name))
            .is_some()
    }

    pub(crate) fn clear_array_share_marker(&mut self, name: &str) {
        self.env_mut()
            .remove(&format!("__mutsu_array_share::{}", name));
    }

    /// Slice 2a: `$n = @z` / `$n = %h`. Promote the source container variable to
    /// a shared `ContainerRef` cell and store that same cell in the scalar
    /// target, so structural mutations (`.push`) through either name are seen by
    /// both (raku reference semantics) — a snapshotting copy would COW-detach on
    /// the first `.push`. Marks the scalar `__mutsu_array_share::` so a later
    /// whole reassignment (`$n = 5`) replaces the slot instead of mutating the
    /// shared cell. Does NOT set the bound-decont marker: the scalar stays
    /// itemized (`@a = $n` itemizes, unlike a `:=` bind which flattens).
    pub(crate) fn array_share_assign(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        val: Value,
        source_name: String,
    ) -> Result<(), RuntimeError> {
        let resolved_source = self.resolve_sigilless_alias_source_name(&source_name);
        let name = code.locals[idx].clone();
        // Build (or reuse) the shared cell: reuse an existing cell carried by the
        // value or already held by the source variable, else wrap the snapshot.
        let cell = match val.view() {
            ValueView::ContainerRef(arc) => arc.clone(),
            _ => match self.env().get(&resolved_source).map(Value::view) {
                Some(ValueView::ContainerRef(arc)) => arc.clone(),
                _ => crate::gc::Gc::new(std::sync::Mutex::new(val.clone())),
            },
        };
        let container = Value::container_ref(cell);
        // Promote the SOURCE container variable to the same cell so its own
        // `.push` / whole-reassign (`@z = (...)`) mutate through and stay visible
        // via the scalar.
        if let Some(source_idx) = code.locals.iter().rposition(|n| n == &resolved_source) {
            self.locals[source_idx] = container.clone();
            self.flush_local_to_env(code, source_idx);
        }
        self.set_env_with_main_alias(&resolved_source, container.clone());
        // Propagate the shared cell into saved call frames so the sharing
        // survives method returns (env restore).
        for frame in self.call_frames.iter_mut().rev() {
            // `code.locals` is this frame's slot layout, not the parent's; only
            // write a parent frame's `saved_locals` when that frame owns the source
            // lexical (its saved env holds the name), else the callee slot index
            // clobbers an unrelated same-index local.
            if frame.saved_env.contains_key(&resolved_source) {
                frame
                    .saved_env
                    .insert(resolved_source.clone(), container.clone());
                for (i, local_name) in code.locals.iter().enumerate() {
                    if local_name == &resolved_source && i < frame.saved_locals.len() {
                        frame.saved_locals[i] = container.clone();
                    }
                }
            }
        }
        // Store the shared cell in the scalar target (itemized scalar).
        self.locals[idx] = container.clone();
        // Clear any stale bound-decont marker inherited from an earlier bind of
        // the same name (this `=` share is itemized, not a `:=` decont alias).
        self.update_bound_decont_marker(&name, false, &val);
        // Mark the scalar so a later whole reassignment replaces the slot.
        self.env_mut()
            .insert(format!("__mutsu_array_share::{}", name), Value::TRUE);
        self.array_share_active = true;
        self.set_env_with_main_alias(&name, container.clone());
        self.flush_local_to_env(code, idx);
        Ok(())
    }
}
