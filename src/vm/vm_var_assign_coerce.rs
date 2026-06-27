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
    pub(super) fn check_hash_bind_value_type(
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
            Value::Package(crate::symbol::Symbol::intern(&format!(
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
                current,
                Some(Value::Bag(_, _) | Value::Mix(_, _) | Value::Set(_, _))
            );
            if is_quanthash_container {
                return self.try_compiled_method_or_interpret(value, trait_name, vec![]);
            }
        }
        if self.check_readonly_for_modify(name).is_err()
            && matches!(
                value,
                Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)
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
                value,
                Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)
            ) {
                return Ok(value);
            }
            let trait_name = match self.env().get(name) {
                Some(Value::Set(_, _)) => Some("SetHash"),
                Some(Value::Bag(_, _)) => Some("BagHash"),
                Some(Value::Mix(_, _)) => Some("MixHash"),
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
            if let Value::Set(items, _) = result {
                return Ok(Value::Set(items, true));
            }
            return Ok(result);
        }
        // For Array/Seq/Slip values, use `build_hash_from_items` which
        // raises "Odd number of elements" when appropriate. Hash values from
        // scalar containers (`$h`) are NOT pre-flattened, so they appear as
        // opaque items (triggering the odd-number check when expected).
        let hash_val = match value {
            Value::Array(ref items, _) => {
                // `build_hash_from_items` flattens a bare `%h` element into its
                // pairs (`%m = %h, a => 42` and the single-element `%m = (%h,)`),
                // while a hash sourced from a `$` scalar carries
                // `HashData.itemized` and stays opaque (`%m = ($hashitem,)` →
                // "Odd number") — matching Raku.
                runtime::utils::build_hash_from_items(items.iter().cloned().collect())?
            }
            Value::Seq(ref items) | Value::Slip(ref items) => {
                runtime::utils::build_hash_from_items(items.iter().cloned().collect())?
            }
            // A single bare scalar assigned to a hash is a one-element (odd)
            // initializer: `my %h = 1` is X::Hash::Store::OddNumber. Hashes,
            // pairs, sets, instances, Nil, etc. keep their existing coercion.
            ref scalar
                if matches!(
                    scalar.clone().into_descalarized(),
                    Value::Int(_)
                        | Value::BigInt(_)
                        | Value::Num(_)
                        | Value::Str(_)
                        | Value::Bool(_)
                        | Value::Rat(..)
                        | Value::FatRat(..)
                        | Value::BigRat(..)
                ) =>
            {
                runtime::utils::build_hash_from_items(vec![value])?
            }
            _ => runtime::coerce_to_hash(value),
        };
        // Resolve hash sentinel entries (self-refs) and decont `:=`-bound
        // `ContainerRef` cells when assigning to a new hash variable:
        // assignment creates new containers, so the copy snapshots values
        // instead of sharing cells.
        if let Value::Hash(ref items) = hash_val
            && (Self::hash_has_sentinels(items) || items.values().any(Value::is_container_ref))
        {
            return Ok(self.resolve_hash_for_iteration(items));
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
        match &value {
            // Associative types are preserved as-is
            Value::Hash(_) | Value::Pair(..) | Value::Set(..) | Value::Bag(..) | Value::Mix(..) => {
                Ok(value)
            }
            // Instance objects: check if they do Associative
            Value::Instance { class_name, .. } => {
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
                    let mapped_val = mapped.into_iter().next().unwrap_or(Value::Nil);
                    // Check that .Map returned an Associative
                    let is_assoc = matches!(
                        &mapped_val,
                        Value::Hash(_)
                            | Value::Pair(..)
                            | Value::Set(..)
                            | Value::Bag(..)
                            | Value::Mix(..)
                    );
                    if !is_assoc {
                        let got_type = crate::runtime::utils::value_type_name(&mapped_val);
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("got".to_string(), mapped_val);
                        attrs.insert(
                            "expected".to_string(),
                            Value::Package(crate::symbol::Symbol::intern("Associative")),
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
            Value::Array(items, _) => {
                let hash = runtime::utils::build_hash_from_items(items.iter().cloned().collect())?;
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                Ok(self.tag_container_metadata(hash, info))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let hash = runtime::utils::build_hash_from_items(items.iter().cloned().collect())?;
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
                let hash = runtime::utils::build_hash_from_items(vec![value])?;
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
        match value {
            Value::Int(i) => Ok(*i as f64),
            Value::Num(n) => Ok(*n),
            Value::Rat(n, d) if *d != 0 => Ok(*n as f64 / *d as f64),
            Value::Bool(flag) => Ok(if *flag { 1.0 } else { 0.0 }),
            Value::Str(s) => {
                // Try to parse as numeric; throw X::Str::Numeric on failure
                if let Ok(n) = s.parse::<f64>() {
                    Ok(n)
                } else {
                    Err(RuntimeError::str_numeric(
                        s,
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
        match value {
            Value::Int(i) => Ok(BigInt::from(*i)),
            // Weights can exceed i64::MAX (e.g. `%h<k> = 10**19`), so a BigInt
            // weight is preserved verbatim rather than truncated to a native int.
            Value::BigInt(n) => Ok((**n).clone()),
            Value::Num(n) => Ok(BigInt::from(*n as i64)),
            Value::Rat(n, d) if *d != 0 => Ok(BigInt::from(*n / *d)),
            Value::Bool(flag) => Ok(BigInt::from(i64::from(*flag))),
            Value::Str(s) => {
                if let Ok(n) = s.parse::<BigInt>() {
                    Ok(n)
                } else if let Ok(n) = s.parse::<f64>() {
                    Ok(BigInt::from(n as i64))
                } else {
                    Err(RuntimeError::str_numeric(
                        s,
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

    pub(super) const LAZY_ASSIGN_PRESERVE_MARKER: &str = "__mutsu_preserve_lazy_on_array_assign";
    pub(super) const MAX_ASSIGN_SLICE_EXPAND: i64 = 100_000;

    pub(super) fn assignment_rhs_values(
        &mut self,
        val: &Value,
    ) -> Result<Vec<Value>, RuntimeError> {
        Ok(match val {
            Value::Array(v, ..) => v.as_ref().clone().items,
            Value::Seq(v) | Value::Slip(v) => v.iter().cloned().collect(),
            Value::Range(a, b) => {
                let end = (*b).min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < *a {
                    Vec::new()
                } else {
                    (*a..=end).map(Value::Int).collect()
                }
            }
            Value::RangeExcl(a, b) => {
                let end = (*b).min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= *a {
                    Vec::new()
                } else {
                    (*a..end).map(Value::Int).collect()
                }
            }
            Value::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    Vec::new()
                } else {
                    (start..=end).map(Value::Int).collect()
                }
            }
            Value::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= start {
                    Vec::new()
                } else {
                    (start..end).map(Value::Int).collect()
                }
            }
            Value::LazyList(list) => self.force_lazy_list_vm(list)?,
            _ => vec![val.clone()],
        })
    }
}
