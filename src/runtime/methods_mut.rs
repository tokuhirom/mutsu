use super::*;
use crate::symbol::Symbol;
use num_bigint::BigInt;

impl Interpreter {
    fn apply_hash_assignment_entry(
        updated: &mut std::collections::HashMap<String, Value>,
        item: Value,
    ) -> bool {
        match item.into_descalarized() {
            Value::Pair(key, boxed) => {
                updated.insert(key, *boxed);
                true
            }
            Value::ValuePair(key, boxed) => {
                updated.insert(key.to_string_value(), *boxed);
                true
            }
            Value::Hash(map) => {
                updated.extend(map.map.clone());
                true
            }
            _ => false,
        }
    }

    fn normalize_hash_like_assignment(
        existing_hash: std::collections::HashMap<String, Value>,
        value: Value,
    ) -> Value {
        let normalized_value = value.into_descalarized();
        match normalized_value {
            Value::Pair(..) | Value::ValuePair(..) | Value::Hash(..) => {
                let mut updated = existing_hash;
                let _ = Self::apply_hash_assignment_entry(&mut updated, normalized_value);
                Value::hash(updated)
            }
            Value::Array(items, _) => {
                let mut updated = existing_hash;
                let mut iter = items.iter().cloned();
                if let Some(first) = iter.next()
                    && !Self::apply_hash_assignment_entry(&mut updated, first)
                {
                    // Preserve existing hash when comma assignment returns [<hash>, <pair>].
                }
                for item in iter {
                    if !Self::apply_hash_assignment_entry(&mut updated, item) {
                        return Value::Array(items.clone(), ArrayKind::List);
                    }
                }
                Value::hash(updated)
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut updated = existing_hash;
                let mut iter = items.iter().cloned();
                if let Some(first) = iter.next()
                    && !Self::apply_hash_assignment_entry(&mut updated, first)
                {
                    // Preserve existing hash when comma assignment returns [<hash>, <pair>].
                }
                for item in iter {
                    if !Self::apply_hash_assignment_entry(&mut updated, item) {
                        return Value::Array(
                            crate::value::Value::array_arc(items.clone().to_vec()),
                            ArrayKind::List,
                        );
                    }
                }
                Value::hash(updated)
            }
            other => other,
        }
    }

    /// The class in `class_name`'s MRO (most-derived first) that *locally*
    /// declares `attr`. For a same-named attribute redeclared in a child, this is
    /// the child; the instance's bare attribute key mirrors this class's value
    /// (what a public `$.attr` accessor reads). Returns `None` if undeclared.
    fn most_derived_attr_declarer(&mut self, class_name: &str, attr: &str) -> Option<String> {
        for cn in self.class_mro(class_name) {
            if let Some(cd) = self.registry().classes.get(&cn)
                && cd.attributes.iter().any(|a| a.0 == attr)
            {
                return Some(cn);
            }
        }
        None
    }

    /// Write `value` for a class-qualified attribute assignment (`$o.C::attr = …`).
    /// When `attr` is stored per-class (a same-named attribute redeclared across
    /// the hierarchy, so the instance carries `"C\0attr"` keys), target the
    /// qualifier's own key so sibling classes' copies are untouched, and keep the
    /// bare key in sync only when `qualifier` is the most-derived declarer (the
    /// bare key is what the public `$.attr` accessor reads). Otherwise (the common
    /// single-class case, no qualified key) fall back to the bare key as before.
    pub(crate) fn store_qualified_attr(
        &mut self,
        updated: &mut std::collections::HashMap<String, Value>,
        instance_class: &str,
        qualifier: &str,
        attr: &str,
        value: Value,
    ) {
        let qualified_key = format!("{}\0{}", qualifier, attr);
        if updated.contains_key(&qualified_key) {
            updated.insert(qualified_key, value.clone());
            if self
                .most_derived_attr_declarer(instance_class, attr)
                .as_deref()
                == Some(qualifier)
            {
                updated.insert(attr.to_string(), value);
            }
        } else {
            updated.insert(attr.to_string(), value);
        }
    }

    pub(crate) fn normalize_rw_accessor_assignment(current: Option<Value>, value: Value) -> Value {
        let current = current.map(Value::into_descalarized);
        match current {
            Some(Value::Hash(existing_hash)) => {
                Self::normalize_hash_like_assignment(existing_hash.map.clone(), value)
            }
            Some(Value::Array(..)) => super::coerce_to_array(value),
            _ => value,
        }
    }

    fn normalize_push_unshift_arg(arg: Value) -> Value {
        match arg {
            Value::Scalar(inner) => *inner,
            Value::Array(items, kind) if kind.is_itemized() => {
                Value::Array(items, kind.decontainerize())
            }
            other => other,
        }
    }

    pub(crate) fn normalize_push_unshift_args(args: Vec<Value>) -> Vec<Value> {
        let needs_normalize = args.iter().any(|arg| match arg {
            Value::Scalar(_) => true,
            Value::Array(_, kind) => kind.is_itemized(),
            Value::Slip(_) => true,
            _ => false,
        });
        if !needs_normalize {
            return args;
        }
        args.into_iter()
            .flat_map(|arg| match arg {
                Value::Slip(items) => items.to_vec(),
                other => vec![Self::normalize_push_unshift_arg(other)],
            })
            .collect()
    }

    pub(crate) fn normalize_incdec_source_for_mut(value: Value) -> Value {
        match value {
            Value::Nil => Value::Int(0),
            Value::Package(name) => match name.resolve().as_str() {
                "Num" | "num" => Value::Num(0.0),
                "Rat" => crate::value::make_rat(0, 1),
                "Complex" => Value::Complex(0.0, 0.0),
                _ => Value::Int(0),
            },
            other => other,
        }
    }

    pub(crate) fn increment_mut_target_value(value: &Value) -> Value {
        match value {
            Value::Int(i) => i
                .checked_add(1)
                .map(Value::Int)
                .unwrap_or_else(|| Value::from_bigint(BigInt::from(*i) + 1)),
            Value::BigInt(n) => Value::from_bigint(n.as_ref() + 1),
            Value::Bool(_) => Value::Bool(true),
            Value::Rat(n, d) => make_rat(n + d, *d),
            Value::FatRat(n, d) => match make_rat(n + d, *d) {
                Value::Rat(nn, dd) => Value::FatRat(nn, dd),
                other => other,
            },
            Value::Str(s) => Value::str(Self::string_succ(s)),
            _ => Value::Int(1),
        }
    }

    pub(crate) fn decrement_mut_target_value(value: &Value) -> Value {
        match value {
            Value::Int(i) => i
                .checked_sub(1)
                .map(Value::Int)
                .unwrap_or_else(|| Value::from_bigint(BigInt::from(*i) - 1)),
            Value::BigInt(n) => Value::from_bigint(n.as_ref() - 1),
            Value::Bool(_) => Value::Bool(false),
            Value::Rat(n, d) => make_rat(n - d, *d),
            Value::FatRat(n, d) => match make_rat(n - d, *d) {
                Value::Rat(nn, dd) => Value::FatRat(nn, dd),
                other => other,
            },
            Value::Str(s) => match Self::string_pred(s) {
                Ok(prev) => Value::str(prev),
                Err(_) => Value::Str(s.clone()),
            },
            _ => Value::Int(-1),
        }
    }

    pub(crate) fn value_to_non_negative_i64(value: &Value) -> Option<i64> {
        match value {
            Value::Int(i) => Some(*i),
            Value::Num(f) => Some(*f as i64),
            Value::BigInt(i) => num_traits::ToPrimitive::to_i64(i.as_ref()),
            _ => None,
        }
    }

    pub(crate) fn overwrite_array_bindings_by_identity(
        &mut self,
        needle: &std::sync::Arc<crate::value::ArrayData>,
        replacement: Value,
    ) {
        let mut keys: Vec<Symbol> = Vec::new();
        // Slice 2a: a `=`-array-shared scalar (`my $n = @z`) holds the array
        // inside a shared `ContainerRef` cell, so its inner Arc — not the
        // variable's own value — matches `needle`. Write the replacement THROUGH
        // the cell (never replace the var's value, which would sever the share)
        // so every alias of the cell observes the element write.
        let mut cells: Vec<std::sync::Arc<std::sync::Mutex<Value>>> = Vec::new();
        for (name, value) in self.env.iter() {
            match value {
                Value::Array(existing, ..) if std::sync::Arc::ptr_eq(existing, needle) => {
                    keys.push(*name);
                }
                Value::ContainerRef(cell) => {
                    if let Value::Array(existing, ..) = &*cell.lock().unwrap()
                        && std::sync::Arc::ptr_eq(existing, needle)
                    {
                        cells.push(cell.clone());
                    }
                }
                _ => {}
            }
        }
        for key in keys {
            // Slice F (env<->locals coherence): this writes the replacement into
            // `env` by name, but the caller's local slot still holds the old Arc.
            // Record the name so the call-site `apply_pending_rw_writeback` drains
            // it into the local slot — otherwise a mutation through a Pair value
            // (`$p = ($a => $a); $p.value[0] = x`) updates `env` `$a` but leaves
            // the locals `$a` stale without the reverse pull.
            self.pending_rw_writeback_sources
                .push(key.resolve().to_string());
            self.env.insert_sym(key, replacement.clone());
        }
        for cell in cells {
            *cell.lock().unwrap() = replacement.clone();
        }
    }

    pub(crate) fn overwrite_hash_bindings_by_identity(
        &mut self,
        needle: &std::sync::Arc<crate::value::HashData>,
        replacement: Value,
    ) {
        let mut keys: Vec<Symbol> = Vec::new();
        let mut cells: Vec<std::sync::Arc<std::sync::Mutex<Value>>> = Vec::new();
        for (name, value) in self.env.iter() {
            match value {
                Value::Hash(existing) if std::sync::Arc::ptr_eq(existing, needle) => {
                    keys.push(*name);
                }
                Value::ContainerRef(cell) => {
                    if let Value::Hash(existing) = &*cell.lock().unwrap()
                        && std::sync::Arc::ptr_eq(existing, needle)
                    {
                        cells.push(cell.clone());
                    }
                }
                _ => {}
            }
        }
        for key in keys {
            // Slice F (env<->locals coherence): see the array counterpart above.
            self.pending_rw_writeback_sources
                .push(key.resolve().to_string());
            self.env.insert_sym(key, replacement.clone());
        }
        for cell in cells {
            *cell.lock().unwrap() = replacement.clone();
        }
    }

    /// Find all Instance values in the env whose attributes contain an array
    /// with the same Arc pointer as `needle`, and replace that attribute with
    /// `replacement`. This handles clone semantics where multiple instances
    /// share the same array container.
    pub(crate) fn propagate_shared_array_in_instances(
        &mut self,
        needle: &std::sync::Arc<crate::value::ArrayData>,
        replacement: &Value,
    ) {
        let mut updates: Vec<(Symbol, String)> = Vec::new();
        for (var_name, value) in self.env.iter() {
            if let Value::Instance { attributes, .. } = value {
                for (attr_key, attr_val) in attributes.as_map().iter() {
                    if let Value::Array(arc, ..) = attr_val
                        && std::sync::Arc::ptr_eq(arc, needle)
                    {
                        updates.push((*var_name, attr_key.clone()));
                    }
                }
            }
        }
        for (var_name, attr_key) in updates {
            if let Some(Value::Instance { attributes, .. }) = self.env.get_sym(var_name) {
                // The env binding shares this instance's live cell, so the in-place
                // insert is visible without rebuilding/re-inserting the value.
                attributes.insert(attr_key, replacement.clone());
            }
        }
    }

    /// Same as `propagate_shared_array_in_instances` but for Hash attributes.
    pub(crate) fn propagate_shared_hash_in_instances(
        &mut self,
        needle: &std::sync::Arc<crate::value::HashData>,
        replacement: &Value,
    ) {
        let mut updates: Vec<(Symbol, String)> = Vec::new();
        for (var_name, value) in self.env.iter() {
            if let Value::Instance { attributes, .. } = value {
                for (attr_key, attr_val) in attributes.as_map().iter() {
                    if let Value::Hash(arc) = attr_val
                        && std::sync::Arc::ptr_eq(arc, needle)
                    {
                        updates.push((*var_name, attr_key.clone()));
                    }
                }
            }
        }
        for (var_name, attr_key) in updates {
            if let Some(Value::Instance { attributes, .. }) = self.env.get_sym(var_name) {
                // Shared live cell — in-place insert is visible to the env binding.
                attributes.insert(attr_key, replacement.clone());
            }
        }
    }

    pub(crate) fn propagate_mixin_update_by_arc(
        &mut self,
        old_mixins: &std::sync::Arc<std::collections::HashMap<String, Value>>,
        new_mixin: &Value,
    ) {
        // Update top-level env bindings
        let top_keys: Vec<Symbol> = self
            .env
            .iter()
            .filter_map(|(sym, val)| match val {
                Value::Mixin(_, existing_mixins)
                    if std::sync::Arc::ptr_eq(existing_mixins, old_mixins) =>
                {
                    Some(*sym)
                }
                _ => None,
            })
            .collect();
        for key in top_keys {
            self.env.insert_sym(key, new_mixin.clone());
        }
        // Update captured envs inside Sub/Block values stored in the env
        let sub_keys: Vec<Symbol> = self
            .env
            .iter()
            .filter_map(|(sym, val)| {
                if let Value::Sub(_) = val {
                    Some(*sym)
                } else {
                    None
                }
            })
            .collect();
        for key in sub_keys {
            if let Some(Value::Sub(data)) = self.env.get_sym(key) {
                let sub_data = data.clone();
                let closure_keys: Vec<Symbol> = sub_data
                    .env
                    .iter()
                    .filter_map(|(sym, val)| match val {
                        Value::Mixin(_, existing_mixins)
                            if std::sync::Arc::ptr_eq(existing_mixins, old_mixins) =>
                        {
                            Some(*sym)
                        }
                        _ => None,
                    })
                    .collect();
                if !closure_keys.is_empty()
                    && let Some(Value::Sub(data_arc)) = self.env.get_mut_sym(key)
                {
                    let data_mut = std::sync::Arc::make_mut(data_arc);
                    for closure_key in closure_keys {
                        data_mut.env.insert_sym(closure_key, new_mixin.clone());
                    }
                }
            }
        }
        // Also update wrappers in wrap_chains that captured the old mixin
        for chain in self.wrap_chains.values_mut() {
            for (_handle_id, wrapper) in chain.iter_mut() {
                Self::update_mixin_in_sub(old_mixins, new_mixin, wrapper);
            }
        }
        for wrapper in self.wrap_name_to_sub.values_mut() {
            Self::update_mixin_in_sub(old_mixins, new_mixin, wrapper);
        }
    }

    fn update_mixin_in_sub(
        old_mixins: &std::sync::Arc<std::collections::HashMap<String, Value>>,
        new_mixin: &Value,
        sub_val: &mut Value,
    ) {
        if let Value::Sub(data_arc) = sub_val {
            let closure_keys: Vec<Symbol> = data_arc
                .env
                .iter()
                .filter_map(|(sym, val)| match val {
                    Value::Mixin(_, existing_mixins)
                        if std::sync::Arc::ptr_eq(existing_mixins, old_mixins) =>
                    {
                        Some(*sym)
                    }
                    _ => None,
                })
                .collect();
            if !closure_keys.is_empty() {
                let data_mut = std::sync::Arc::make_mut(data_arc);
                for key in closure_keys {
                    data_mut.env.insert_sym(key, new_mixin.clone());
                }
            }
        }
    }
}
