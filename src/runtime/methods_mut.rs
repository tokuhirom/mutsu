use super::methods_signature::make_x_immutable_error;
use super::*;
use crate::symbol::Symbol;
use crate::value::InstanceAttrs;
use num_bigint::BigInt;
use num_traits::Signed;
use std::sync::Arc;

fn value_to_bigint(value: &Value) -> BigInt {
    match value {
        Value::Int(i) => BigInt::from(*i),
        Value::BigInt(n) => (**n).clone(),
        Value::Num(f) => BigInt::from(*f as i64),
        Value::Rat(n, d) | Value::FatRat(n, d) => {
            if *d == 0 {
                BigInt::from(0)
            } else {
                BigInt::from(*n / *d)
            }
        }
        Value::Bool(b) => BigInt::from(i64::from(*b)),
        Value::Str(s) => s
            .trim()
            .parse::<i64>()
            .map(BigInt::from)
            .unwrap_or_else(|_| BigInt::from(0)),
        _ => BigInt::from(0),
    }
}

fn normalize_twos_complement(mut value: BigInt, bits: usize) -> BigInt {
    if bits == 0 {
        return BigInt::from(0);
    }
    let modulus = BigInt::from(1u8) << bits;
    value %= &modulus;
    if value.is_negative() {
        value += modulus;
    }
    value
}

fn write_bits_into_bytes(bytes: &mut [u8], from: usize, bits: usize, value: &BigInt) {
    for i in 0..bits {
        let bit_index = from + i;
        let byte_index = bit_index / 8;
        let bit_in_byte = 7 - (bit_index % 8);
        let src_shift = bits - 1 - i;
        let bit_is_set = ((value >> src_shift) & BigInt::from(1u8)) == BigInt::from(1u8);
        if bit_is_set {
            bytes[byte_index] |= 1 << bit_in_byte;
        } else {
            bytes[byte_index] &= !(1 << bit_in_byte);
        }
    }
}

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
    fn store_qualified_attr(
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

    fn normalize_rw_accessor_assignment(current: Option<Value>, value: Value) -> Value {
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

    fn normalize_incdec_source_for_mut(value: Value) -> Value {
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

    fn increment_mut_target_value(value: &Value) -> Value {
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

    fn decrement_mut_target_value(value: &Value) -> Value {
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

    /// Call a Proxy callback (FETCH or STORE) in the context of an instance's attributes.
    /// Sets up `!attr` bindings, runs the callback, and reads back attribute changes.
    pub(crate) fn call_proxy_callback(
        &mut self,
        callback: &Value,
        args: Vec<Value>,
        instance_attrs: &HashMap<String, Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        if let Value::Sub(data) = callback {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            // Merge captured env. For user variables that already exist in the
            // current env, prefer the current (live) value over the captured
            // snapshot. This approximates Raku's shared-binding closure semantics
            // for Proxy callbacks (FETCH should see changes made by STORE).
            // Internal/special variables always use the captured value so that
            // the callback's lexical context is properly restored.
            for (k, v) in &data.env {
                let is_user_var = !k.starts_with("!")
                    && !k.starts_with(".")
                    && !k.starts_with("*")
                    && !k.starts_with("?")
                    && !k.starts_with("__")
                    && *k != "self"
                    && *k != "_";
                if is_user_var && new_env.contains_key_sym(*k) {
                    // Keep current env value (live binding)
                } else {
                    new_env.insert_sym(*k, v.clone());
                }
            }
            // Override !attr bindings with current instance attributes
            for (attr_name, attr_val) in instance_attrs {
                new_env.insert(format!("!{}", attr_name), attr_val.clone());
                new_env.insert(format!(".{}", attr_name), attr_val.clone());
            }
            self.env = new_env;
            if let Err(e) = self.bind_function_args_values(&data.param_defs, &data.params, &args) {
                self.env = saved_env;
                return Err(e);
            }
            let result = self.run_block(&data.body);
            let implicit_return = self.env.get("_").cloned();
            // Read back !attr changes
            let mut updated_attrs = instance_attrs.clone();
            for attr_name in instance_attrs.keys() {
                if let Some(val) = self.env.get(&format!("!{}", attr_name)) {
                    updated_attrs.insert(attr_name.clone(), val.clone());
                }
            }
            // Propagate outer variable changes (e.g., our variables, closured scalars).
            // Only propagate values that were genuinely modified by the callback body,
            // not values merely loaded from the captured env. This prevents stale
            // captured env entries (e.g. an old instance snapshot) from overwriting
            // current env values.
            let mut restored = saved_env;
            for (k, v) in &self.env {
                if restored.contains_key_sym(*k)
                    && !k.starts_with("!")
                    && !k.starts_with(".")
                    && *k != "self"
                    && *k != "_"
                {
                    // Skip if the value is unchanged from the captured env —
                    // it was merely loaded, not modified by the callback.
                    if let Some(captured_v) = data.env.get_sym(*k)
                        && v == captured_v
                    {
                        continue;
                    }
                    restored.insert_sym(*k, v.clone());
                }
            }
            self.env = restored;
            let value = match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                Err(e) => Err(e),
                Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
            }?;
            Ok((value, updated_attrs))
        } else {
            // Non-sub callback
            let result = self.call_sub_value(callback.clone(), args, false)?;
            Ok((result, instance_attrs.clone()))
        }
    }

    /// Call Proxy FETCH and return the fetched value, propagating attribute updates to the instance.
    pub(crate) fn proxy_fetch(
        &mut self,
        fetcher: &Value,
        target_var: Option<&str>,
        class_name: &str,
        attributes: &HashMap<String, Value>,
        target_id: u64,
    ) -> Result<Value, RuntimeError> {
        let proxy_val = Value::Proxy {
            fetcher: Box::new(fetcher.clone()),
            storer: Box::new(Value::Nil),
            subclass: None,
            decontainerized: false,
        };
        let (result, _updated) = self.call_proxy_callback(fetcher, vec![proxy_val], attributes)?;
        // For FETCH we don't propagate attribute changes (reads shouldn't mutate)
        let _ = target_var;
        let _ = class_name;
        let _ = target_id;
        Ok(result)
    }

    /// Call Proxy STORE with a new value, propagating attribute updates to the instance.
    ///
    /// `attributes` is the (post-method-run) snapshot fed to the STORE callback;
    /// `attrs_cell` is the receiver instance's live shared cell that the updated
    /// attributes are written back into in place (Phase 3 registry-removal).
    pub(crate) fn proxy_store(
        &mut self,
        storer: &Value,
        target_var: Option<&str>,
        class_name: Symbol,
        attributes: &HashMap<String, Value>,
        attrs_cell: &Arc<InstanceAttrs>,
        new_value: Value,
    ) -> Result<Value, RuntimeError> {
        let proxy_val = Value::Proxy {
            fetcher: Box::new(Value::Nil),
            storer: Box::new(storer.clone()),
            subclass: None,
            decontainerized: false,
        };
        let (_result, updated) =
            self.call_proxy_callback(storer, vec![proxy_val, new_value.clone()], attributes)?;
        // Propagate attribute changes back to the instance's live cell.
        if let Some(var_name) = target_var {
            attrs_cell.commit_attrs(updated);
            self.env.insert(
                var_name.to_string(),
                Value::instance_sharing_cell(attrs_cell, class_name, attrs_cell.instance_id()),
            );
        }
        // Assignment returns the assigned value, not the STORE callback's return value
        Ok(new_value)
    }

    fn rw_method_attribute_target(body: &[Stmt]) -> Option<String> {
        let first = body.iter().find(|s| !matches!(s, Stmt::SetLine(_)))?;
        let extract_attr = |expr: &Expr| -> Option<String> {
            match expr {
                Expr::Var(name) if name.starts_with('!') && name.len() > 1 => {
                    Some(name[1..].to_string())
                }
                Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => {
                    if let Expr::Var(attr) = &args[0]
                        && attr.starts_with('!')
                        && attr.len() > 1
                    {
                        return Some(attr[1..].to_string());
                    }
                    None
                }
                _ => None,
            }
        };
        match first {
            Stmt::Expr(expr) | Stmt::Return(expr) => extract_attr(expr),
            _ => None,
        }
    }

    /// Detect an `is rw` method whose body returns an *indexed* attribute
    /// element — `@!attr[$param]` or `%!attr{$param}` — where the index/key is
    /// a single positional parameter. Returns `(attr_name, param_name,
    /// is_positional)`. Such a method exposes the element as a writable lvalue,
    /// so `$obj.at($i) = v` assigns into the attribute container's element.
    /// (The simpler `{ $!attr }` form is handled by
    /// `rw_method_attribute_target`.)
    fn rw_method_indexed_attr_target(body: &[Stmt]) -> Option<(String, String, bool)> {
        let first = body.iter().find(|s| !matches!(s, Stmt::SetLine(_)))?;
        let expr = match first {
            Stmt::Expr(e) | Stmt::Return(e) => e,
            _ => return None,
        };
        // Unwrap `return-rw EXPR`.
        let expr = match expr {
            Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => &args[0],
            other => other,
        };
        if let Expr::Index {
            target,
            index,
            is_positional,
        } = expr
            && let Expr::Var(param) = index.as_ref()
        {
            // `@!attr[...]` parses as `ArrayVar("!attr")`, `%!attr{...}` as
            // `HashVar("!attr")`.
            let attr = match target.as_ref() {
                Expr::ArrayVar(a) | Expr::HashVar(a) if a.starts_with('!') && a.len() > 1 => {
                    &a[1..]
                }
                _ => return None,
            };
            return Some((attr.to_string(), param.clone(), *is_positional));
        }
        None
    }

    /// Assign `value` into element `index_value` of the array/hash attribute
    /// `attr_name` on the instance, then write the updated instance back through
    /// `target_var`. Backs `$obj.rw-method(idx) = value` where the method
    /// returns `@!attr[idx]` / `%!attr{key}`.
    #[allow(clippy::too_many_arguments)]
    fn assign_rw_indexed_attr(
        &mut self,
        attributes: &std::sync::Arc<crate::value::InstanceAttrs>,
        class_name: Symbol,
        target_id: u64,
        target_var: Option<&str>,
        attr_name: &str,
        index_value: Value,
        is_positional: bool,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let mut updated = attributes.to_map();
        let container = updated.get(attr_name).cloned().unwrap_or(Value::Nil);
        let new_container = if is_positional {
            let idx = crate::runtime::utils::to_int(&index_value);
            let (mut data, kind) = match container {
                Value::Array(items, kind) => ((*items).clone(), kind),
                Value::Nil => (
                    crate::value::ArrayData::new(Vec::new()),
                    crate::value::ArrayKind::Array,
                ),
                other => {
                    return Err(RuntimeError::assignment_ro_typename(
                        &crate::value::what_type_name(&other),
                        &value.to_string_value(),
                    ));
                }
            };
            if idx < 0 {
                return Err(RuntimeError::new(format!(
                    "Index {} out of range for rw element assignment",
                    idx
                )));
            }
            let idx = idx as usize;
            if idx >= data.items.len() {
                data.items.resize(idx + 1, Value::Nil);
            }
            data.items[idx] = value.clone();
            Value::Array(std::sync::Arc::new(data), kind)
        } else {
            let key = index_value.to_string_value();
            let mut data = match container {
                Value::Hash(items) => (*items).clone(),
                Value::Nil => crate::value::HashData::new(std::collections::HashMap::new()),
                other => {
                    return Err(RuntimeError::assignment_ro_typename(
                        &crate::value::what_type_name(&other),
                        &value.to_string_value(),
                    ));
                }
            };
            data.map.insert(key, value.clone());
            Value::Hash(std::sync::Arc::new(data))
        };
        updated.insert(attr_name.to_string(), new_container);
        if let Some(var_name) = target_var {
            self.env.insert(
                var_name.to_string(),
                Value::write_back_sharing(attributes, class_name, updated, target_id),
            );
        }
        Ok(value)
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
            if matches!(self.env.get(&deep_key), Some(Value::Bool(true))) {
                let repr = value.to_string_value();
                return Err(RuntimeError::assignment_ro_typename(
                    &crate::value::what_type_name(&value),
                    &repr,
                ));
            }
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
            return Err(super::methods_signature::make_multi_no_match_error(method));
        } else {
            return Err(super::methods_signature::make_multi_no_match_error(method));
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

    pub(crate) fn call_method_mut_with_values(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", target_var);
        let alias_key = format!("__mutsu_sigilless_alias::{}", target_var);
        let has_sigilless_meta =
            self.env.contains_key(&readonly_key) || self.env.contains_key(&alias_key);
        let scalar_like_target = target_var.starts_with('$')
            || (!target_var.starts_with('@')
                && !target_var.starts_with('%')
                && !target_var.starts_with('&')
                && !has_sigilless_meta);
        if scalar_like_target
            && let Value::Array(_, kind) = &target
            && !kind.is_real_array()
            && matches!(
                method,
                "push" | "append" | "pop" | "shift" | "unshift" | "prepend" | "splice"
            )
        {
            return Err(make_x_immutable_error(method, "List"));
        }
        if scalar_like_target
            && args.is_empty()
            && matches!(method, "postfix:<++>" | "postfix:<-->")
        {
            self.check_readonly_for_increment(target_var)?;
            let current = self.env.get(target_var).cloned().unwrap_or(target);
            let current = Self::normalize_incdec_source_for_mut(current);
            let updated = if method == "postfix:<++>" {
                Self::increment_mut_target_value(&current)
            } else {
                Self::decrement_mut_target_value(&current)
            };
            self.env.insert(target_var.to_string(), updated);
            return Ok(current);
        }
        // .keyof on Mix/Set/Bag variables: check type constraint for parameterized type
        if method == "keyof"
            && args.is_empty()
            && matches!(
                &target,
                Value::Mix(_, _) | Value::Set(_, _) | Value::Bag(_, _)
            )
        {
            if let Some(constraint) = self.var_type_constraint(target_var)
                && let Some(bracket_pos) = constraint.find('[')
            {
                let param = &constraint[bracket_pos + 1..constraint.len() - 1];
                return Ok(Value::Package(Symbol::intern(param)));
            }
            return Ok(Value::Package(Symbol::intern("Mu")));
        }
        if method == "VAR" && args.is_empty() {
            // Proxy (including subclasses): .VAR returns the proxy wrapped as a
            // ProxyObject so that subsequent method calls don't auto-FETCH.
            if matches!(&target, Value::Proxy { .. }) {
                return Ok(Value::proxy_var_object(target, target_var.to_string()));
            }
            if let Value::Instance { attributes, .. } = &target
                && matches!(
                    attributes.as_map().get("__mutsu_var_target"),
                    Some(Value::Str(_))
                )
            {
                return Ok(target);
            }
            if let Some(existing) = self.var_meta_value(target_var) {
                return Ok(existing);
            }
            let readonly_key = format!("__mutsu_sigilless_readonly::{}", target_var);
            let alias_key = format!("__mutsu_sigilless_alias::{}", target_var);
            let has_sigilless_meta =
                self.env.contains_key(&readonly_key) || self.env.contains_key(&alias_key);
            if has_sigilless_meta {
                let readonly = matches!(self.env.get(&readonly_key), Some(Value::Bool(true)));
                let itemized_array =
                    matches!(target, Value::Array(_, kind) if kind.is_real_array());
                if readonly && !itemized_array {
                    return Ok(target);
                }
            }
            // A scalar `:=`-bound to a container (`my $r := @a` / `:= %h` /
            // `:= (1,2,3)`) has no Scalar container of its own — the binding
            // aliases the container directly — so `.VAR` returns the bound value
            // itself and `.VAR.^name` reflects the container type (List/Array/
            // Hash/...), not Scalar. The `__mutsu_bound_decont` marker records
            // such binds.
            if !target_var.starts_with('@')
                && !target_var.starts_with('%')
                && !target_var.starts_with('&')
            {
                let decont_key = format!("__mutsu_bound_decont::{}", target_var);
                if matches!(self.env.get(&decont_key), Some(Value::Bool(true))) {
                    return Ok(target);
                }
            }
            let class_name = if target_var.starts_with('@') {
                "Array"
            } else if target_var.starts_with('%') {
                "Hash"
            } else if target_var.starts_with('&') {
                "Sub"
            } else {
                "Scalar"
            };
            let display_name = if target_var.starts_with('$')
                || target_var.starts_with('@')
                || target_var.starts_with('%')
                || target_var.starts_with('&')
            {
                target_var.to_string()
            } else {
                format!("${}", target_var)
            };
            let mut attributes = HashMap::new();
            attributes.insert("name".to_string(), Value::str(display_name));
            attributes.insert(
                "__mutsu_var_target".to_string(),
                Value::str(target_var.to_string()),
            );
            attributes.insert(
                "dynamic".to_string(),
                Value::Bool(self.is_var_dynamic(target_var)),
            );
            // Add .default: explicit `is default(...)` value, or type object
            // for typed variables, or (Any) for untyped.
            let default_val = if let Some(def) = self.var_default(target_var) {
                def.clone()
            } else if let Some(tc) = self.var_type_constraint(target_var) {
                Value::Package(Symbol::intern(&tc))
            } else {
                Value::Package(Symbol::intern("Any"))
            };
            attributes.insert("default".to_string(), default_val);
            // Add .of: type constraint of the variable (Mu for unconstrained)
            let of_val = if let Some(tc) = self.var_type_constraint(target_var) {
                Value::Package(Symbol::intern(&tc))
            } else {
                Value::Package(Symbol::intern("Mu"))
            };
            attributes.insert("of".to_string(), of_val);
            let meta = Value::make_instance(Symbol::intern(class_name), attributes);
            self.set_var_meta_value(target_var, meta.clone());
            return Ok(meta);
        }
        // .of returns the element type constraint of a container
        if method == "of"
            && args.is_empty()
            && (target_var.starts_with('@') || target_var.starts_with('%'))
        {
            let type_name = self
                .var_type_constraint(target_var)
                .or_else(|| {
                    self.container_type_metadata(&target)
                        .map(|info| info.value_type)
                })
                .unwrap_or_else(|| "Mu".to_string());
            return Ok(Value::Package(Symbol::intern(&type_name)));
        }

        // Collation.set — mutates the Collation instance in the variable
        if method == "set"
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Collation")
        {
            let result = self.dispatch_collation_method(target, method, &args)?;
            // Update the variable in the environment to reflect the mutation
            self.env.insert(target_var.to_string(), result.clone());
            return Ok(result);
        }

        if let Value::Instance {
            class_name,
            attributes,
            id,
        } = &target
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            let bytes = attributes
                .as_map()
                .get("bytes")
                .and_then(|v| match v {
                    Value::Array(items, ..) => Some(
                        items
                            .iter()
                            .map(|v| match v {
                                Value::Int(i) => (*i).clamp(0, 255) as u8,
                                Value::Num(f) => (*f as i64).clamp(0, 255) as u8,
                                Value::BigInt(bi) => num_traits::ToPrimitive::to_i64(bi.as_ref())
                                    .unwrap_or(0)
                                    .clamp(0, 255)
                                    as u8,
                                _ => 0u8,
                            })
                            .collect::<Vec<u8>>(),
                    ),
                    _ => None,
                })
                .unwrap_or_default();

            if (method == "read-ubits" || method == "read-bits") && args.len() == 2 {
                let Some(from) = Self::value_to_non_negative_i64(&args[0]) else {
                    return Err(RuntimeError::new("read-ubits/read-bits expects Int offset"));
                };
                let Some(bits) = Self::value_to_non_negative_i64(&args[1]) else {
                    return Err(RuntimeError::new(
                        "read-ubits/read-bits expects Int bit count",
                    ));
                };
                return crate::builtins::buf_bits::read_bits(
                    &bytes,
                    from,
                    bits,
                    method == "read-bits",
                );
            }

            if (method == "write-ubits" || method == "write-bits") && args.len() == 3 {
                if class_name == "Blob" {
                    return Err(RuntimeError::new(
                        "Cannot modify immutable Blob with write-bits/write-ubits",
                    ));
                }
                let Some(from) = Self::value_to_non_negative_i64(&args[0]) else {
                    return Err(RuntimeError::new(
                        "write-ubits/write-bits expects Int offset",
                    ));
                };
                let Some(bits) = Self::value_to_non_negative_i64(&args[1]) else {
                    return Err(RuntimeError::new(
                        "write-ubits/write-bits expects Int bit count",
                    ));
                };
                let written = crate::builtins::buf_bits::write_bits(&bytes, from, bits, &args[2])?;
                let mut updated_attrs = attributes.to_map();
                updated_attrs.insert(
                    "bytes".to_string(),
                    Value::array(
                        written
                            .iter()
                            .map(|b| Value::Int(*b as i64))
                            .collect::<Vec<_>>(),
                    ),
                );
                return Ok(Value::write_back_sharing(
                    attributes,
                    *class_name,
                    updated_attrs,
                    *id,
                ));
            }
        }

        // Buf/Blob write-num32 / write-num64 — mutate an existing instance.
        if crate::builtins::buf_write_num::write_num_size(method).is_some()
            && let Value::Instance {
                class_name,
                attributes,
                id,
            } = &target
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            let cn = class_name.resolve();
            if cn == "Blob" || cn.starts_with("Blob[") || cn.starts_with("blob") {
                return Err(RuntimeError::new(format!(
                    "Cannot modify immutable {} with {}",
                    cn, method
                )));
            }
            if args.len() < 2 || args.len() > 3 {
                return Err(RuntimeError::new(format!(
                    "{} expects 2 or 3 arguments, got {}",
                    method,
                    args.len()
                )));
            }
            let offset_val = &args[0];
            let value_val = &args[1];
            let endian_val = if args.len() == 3 {
                crate::builtins::buf_write_num::decode_endian(&args[2])
            } else {
                0
            };
            let offset_i64 = match offset_val {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                _ => 0,
            };
            let mut bytes = {
                let mut v: Vec<u8> = Vec::new();
                if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                    v.reserve(items.len());
                    for it in items.iter() {
                        v.push(match it {
                            Value::Int(i) => (*i).clamp(0, 255) as u8,
                            Value::Num(f) => (*f as i64).clamp(0, 255) as u8,
                            _ => 0,
                        });
                    }
                }
                v
            };
            crate::builtins::buf_write_num::apply_write_num(
                &mut bytes, method, offset_i64, value_val, endian_val,
            )?;
            let mut updated_attrs = attributes.to_map();
            updated_attrs.insert(
                "bytes".to_string(),
                Value::array(bytes.into_iter().map(|b| Value::Int(b as i64)).collect()),
            );
            let updated = Value::write_back_sharing(attributes, *class_name, updated_attrs, *id);
            self.env.insert(target_var.to_string(), updated.clone());
            return Ok(updated);
        }

        // Buf/Blob write-num on type object: returns a fresh buf.
        if crate::builtins::buf_write_num::write_num_size(method).is_some()
            && let Value::Package(name) = &target
        {
            let cn = name.resolve();
            if crate::runtime::utils::is_buf_or_blob_class(&cn) {
                if args.len() < 2 || args.len() > 3 {
                    return Err(RuntimeError::new(format!(
                        "{} expects 2 or 3 arguments, got {}",
                        method,
                        args.len()
                    )));
                }
                let offset_i64 = match &args[0] {
                    Value::Int(i) => *i,
                    Value::Num(f) => *f as i64,
                    _ => 0,
                };
                let endian_val = if args.len() == 3 {
                    crate::builtins::buf_write_num::decode_endian(&args[2])
                } else {
                    0
                };
                let mut bytes: Vec<u8> = Vec::new();
                crate::builtins::buf_write_num::apply_write_num(
                    &mut bytes, method, offset_i64, &args[1], endian_val,
                )?;
                let normalized = crate::runtime::utils::normalize_buf_type_name(&cn);
                return Ok(crate::builtins::buf_write_num::make_buf_value(
                    &normalized,
                    bytes,
                ));
            }
        }

        // Buf/Blob write-int / write-uint -- mutate an existing instance.
        if crate::builtins::buf_write_int::write_int_info(method).is_some()
            && let Value::Instance {
                class_name,
                attributes,
                id,
            } = &target
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            let cn = class_name.resolve();
            if cn == "Blob" || cn.starts_with("Blob[") || cn.starts_with("blob") {
                return Err(RuntimeError::new(format!(
                    "Cannot modify immutable {} with {}",
                    cn, method
                )));
            }
            if args.len() < 2 || args.len() > 3 {
                return Err(RuntimeError::new(format!(
                    "{} expects 2 or 3 arguments, got {}",
                    method,
                    args.len()
                )));
            }
            let offset_val = &args[0];
            let value_val = &args[1];
            let endian_val = if args.len() == 3 {
                crate::builtins::buf_write_num::decode_endian(&args[2])
            } else {
                0
            };
            let offset_i64 = match offset_val {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                _ => 0,
            };
            let mut bytes = {
                let mut v: Vec<u8> = Vec::new();
                if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                    v.reserve(items.len());
                    for it in items.iter() {
                        v.push(match it {
                            Value::Int(i) => (*i).clamp(0, 255) as u8,
                            Value::Num(f) => (*f as i64).clamp(0, 255) as u8,
                            _ => 0,
                        });
                    }
                }
                v
            };
            crate::builtins::buf_write_int::apply_write_int(
                &mut bytes, method, offset_i64, value_val, endian_val,
            )?;
            let mut updated_attrs = attributes.to_map();
            updated_attrs.insert(
                "bytes".to_string(),
                Value::array(bytes.into_iter().map(|b| Value::Int(b as i64)).collect()),
            );
            let updated = Value::write_back_sharing(attributes, *class_name, updated_attrs, *id);
            self.env.insert(target_var.to_string(), updated.clone());
            return Ok(updated);
        }

        // Buf/Blob write-int on type object: returns a fresh buf.
        if crate::builtins::buf_write_int::write_int_info(method).is_some()
            && let Value::Package(name) = &target
        {
            let cn = name.resolve();
            if crate::runtime::utils::is_buf_or_blob_class(&cn) {
                if args.len() < 2 || args.len() > 3 {
                    return Err(RuntimeError::new(format!(
                        "{} expects 2 or 3 arguments, got {}",
                        method,
                        args.len()
                    )));
                }
                let offset_i64 = match &args[0] {
                    Value::Int(i) => *i,
                    Value::Num(f) => *f as i64,
                    _ => 0,
                };
                let endian_val = if args.len() == 3 {
                    crate::builtins::buf_write_num::decode_endian(&args[2])
                } else {
                    0
                };
                let mut bytes: Vec<u8> = Vec::new();
                crate::builtins::buf_write_int::apply_write_int(
                    &mut bytes, method, offset_i64, &args[1], endian_val,
                )?;
                let normalized = crate::runtime::utils::normalize_buf_type_name(&cn);
                return Ok(crate::builtins::buf_write_num::make_buf_value(
                    &normalized,
                    bytes,
                ));
            }
        }

        // Buf/Blob mutating methods: append, push, prepend, unshift, reallocate, pop, shift, splice
        if matches!(
            method,
            "append" | "push" | "prepend" | "unshift" | "reallocate" | "pop" | "shift" | "splice"
        ) && Self::is_buf_like_value(&target)
        {
            if method == "reallocate" {
                return self.buf_reallocate(target_var, target, &args);
            }
            if method == "pop" || method == "shift" || method == "splice" {
                return self.buf_pop_shift_splice(target_var, target, method, args);
            }
            return self.buf_mutate_method(target_var, target, method, args);
        }

        if target_var.starts_with('@') {
            // Check for shaped (multidimensional) arrays - these don't support
            // mutating operations like push/pop/shift/unshift/splice/append/prepend
            if matches!(
                method,
                "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
            ) && is_shaped_array(&target)
            {
                return Err(RuntimeError::illegal_on_fixed_dimension_array(method));
            }
            let key = target_var.to_string();
            // Capture the declared container type before mutating. The mutators
            // below reallocate the backing buffer via `Arc::make_mut` when the
            // Arc is shared, which orphans the pointer-keyed type metadata; we
            // re-attach it to the post-mutation array (see
            // `reattach_array_type_metadata`).
            let saved_meta = self.container_type_metadata(&target);
            // Container description for X::Cannot::Empty (`array[num]` for a
            // native typed array, otherwise `Array`).
            let empty_what = match self.var_type_constraint(&key) {
                Some(c)
                    if crate::runtime::native_types::is_native_array_element_type(&c)
                        || matches!(c.as_str(), "num" | "num32" | "num64" | "str") =>
                {
                    format!("array[{c}]")
                }
                _ => "Array".to_string(),
            };
            match method {
                "push" => {
                    let normalized_args = Self::normalize_push_unshift_args(args);
                    self.check_container_element_types(&key, &normalized_args)?;
                    let result = self.push_to_shared_var(&key, normalized_args, &target);
                    self.reattach_array_type_metadata(&key, &saved_meta);
                    return Ok(result);
                }
                "append" => {
                    // Raku's append uses the "one-arg rule": if exactly one
                    // non-itemized Array/List argument is passed, its elements
                    // are flattened. With multiple arguments, each is appended
                    // as-is (no recursive flattening).
                    let flat_values = flatten_append_args(args);
                    self.check_container_element_types(&key, &flat_values)?;
                    let result = if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key)
                    {
                        let kind = *kind;
                        let items = Arc::make_mut(arc_items);
                        items.extend(flat_values);
                        Value::Array(Arc::clone(arc_items), kind)
                    } else {
                        let mut items = match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        };
                        items.extend(flat_values);
                        self.env
                            .insert(key.clone(), Value::real_array(items.clone()));
                        Value::real_array(items)
                    };
                    self.reattach_array_type_metadata(&key, &saved_meta);
                    return Ok(result);
                }
                "unshift" => {
                    let normalized_args = Self::normalize_push_unshift_args(args);
                    self.check_container_element_types(&key, &normalized_args)?;
                    let result = if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key)
                    {
                        let kind = *kind;
                        let items = Arc::make_mut(arc_items);
                        for (i, arg) in normalized_args.iter().enumerate() {
                            items.insert(i, arg.clone());
                        }
                        Value::Array(Arc::clone(arc_items), kind)
                    } else {
                        let items = match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        };
                        let mut pref: Vec<Value> = normalized_args;
                        pref.extend(items);
                        self.env
                            .insert(key.clone(), Value::real_array(pref.clone()));
                        Value::real_array(pref)
                    };
                    self.reattach_array_type_metadata(&key, &saved_meta);
                    return Ok(result);
                }
                "prepend" => {
                    let flat_values = flatten_append_args(args);
                    self.check_container_element_types(&key, &flat_values)?;
                    let result = if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key)
                    {
                        let kind = *kind;
                        let items = Arc::make_mut(arc_items);
                        for (i, arg) in flat_values.iter().enumerate() {
                            items.insert(i, arg.clone());
                        }
                        Value::Array(Arc::clone(arc_items), kind)
                    } else {
                        let items = match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        };
                        let mut pref: Vec<Value> = flat_values;
                        pref.extend(items);
                        self.env
                            .insert(key.clone(), Value::real_array(pref.clone()));
                        Value::real_array(pref)
                    };
                    self.reattach_array_type_metadata(&key, &saved_meta);
                    return Ok(result);
                }
                "pop" => {
                    if !args.is_empty() {
                        return Err(RuntimeError::new(format!(
                            "Too many positionals passed; expected 1 argument but got {}",
                            args.len() + 1
                        )));
                    }
                    if let Some(Value::Array(_, kind)) = self.env.get(&key)
                        && kind.is_lazy()
                    {
                        return Err(RuntimeError::cannot_lazy("pop"));
                    }
                    let out = if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        // Avoid `Arc::make_mut` on an empty array: it would clone a
                        // shared Arc and drop the native type metadata keyed by the
                        // old pointer, demoting `array[num]` to a plain Array.
                        if arc_items.is_empty() {
                            return Ok(make_empty_array_failure_what("pop", &empty_what));
                        }
                        let items = Arc::make_mut(arc_items);
                        items.pop().unwrap_or(Value::Nil)
                    } else {
                        let mut items = match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        };
                        if items.is_empty() {
                            return Ok(make_empty_array_failure_what("pop", &empty_what));
                        }
                        let out = items.pop().unwrap_or(Value::Nil);
                        self.env.insert(key.clone(), Value::real_array(items));
                        out
                    };
                    self.reattach_array_type_metadata(&key, &saved_meta);
                    return Ok(out);
                }
                "shift" => {
                    if !args.is_empty() {
                        return Err(RuntimeError::new(format!(
                            "Too many positionals passed; expected 1 argument but got {}",
                            args.len() + 1
                        )));
                    }
                    let out = if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        if arc_items.is_empty() {
                            return Ok(make_empty_array_failure_what("shift", &empty_what));
                        }
                        let items = Arc::make_mut(arc_items);
                        items.remove(0)
                    } else {
                        let mut items = match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        };
                        if items.is_empty() {
                            return Ok(make_empty_array_failure_what("shift", &empty_what));
                        }
                        let out = items.remove(0);
                        self.env.insert(key.clone(), Value::real_array(items));
                        out
                    };
                    self.reattach_array_type_metadata(&key, &saved_meta);
                    return Ok(out);
                }
                "splice" => {
                    // Resolve a splice position argument to a usize.
                    // Whatever => array length, Callable => call with length, etc.
                    /// Resolve a splice position to a signed integer for validation.
                    fn resolve_splice_raw(v: &Value, len: usize) -> Option<i64> {
                        match v {
                            Value::Int(i) => Some(*i),
                            Value::Whatever => Some(len as i64),
                            Value::Str(s) => s.parse::<i64>().ok(),
                            Value::Num(n) => Some(*n as i64),
                            // Handle Mixin (allomorphic types like IntStr)
                            Value::Mixin(inner, _) => resolve_splice_raw(inner, len),
                            _ => None,
                        }
                    }
                    fn do_splice(items: &mut Vec<Value>, args: &[Value]) -> Vec<Value> {
                        let len = items.len();
                        let start = args
                            .first()
                            .and_then(|v| resolve_splice_raw(v, len))
                            .unwrap_or(0)
                            .max(0) as usize;
                        let start = start.min(len);
                        let count = args
                            .get(1)
                            .and_then(|v| resolve_splice_raw(v, len))
                            .unwrap_or(len.saturating_sub(start) as i64)
                            .max(0) as usize;
                        let end = (start + count).min(len);
                        let removed: Vec<Value> = items.drain(start..end).collect();
                        // Collect all replacement values from args[2..]
                        let mut new_items: Vec<Value> = Vec::new();
                        for arg in args.iter().skip(2) {
                            match arg {
                                Value::Array(arr, ..) => {
                                    new_items.extend(arr.iter().cloned());
                                }
                                other => new_items.push(other.clone()),
                            }
                        }
                        for (i, item) in new_items.into_iter().enumerate() {
                            items.insert(start + i, item);
                        }
                        removed
                    }
                    // Pre-resolve callable arguments (WhateverCode like *-3)
                    // before borrowing the array mutably.
                    let arr_len = match self.env.get(&key) {
                        Some(Value::Array(v, ..)) => v.len(),
                        _ => match &target {
                            Value::Array(v, ..) => v.len(),
                            _ => 0,
                        },
                    };
                    // Check for lazy values in splice replacement args
                    {
                        let type_name = if let Some(constraint) = self.var_type_constraint(&key)
                            && crate::runtime::native_types::is_native_array_element_type(
                                &constraint,
                            ) {
                            format!("array[{}]", constraint)
                        } else {
                            "Array".to_string()
                        };
                        for arg in args.iter().skip(2) {
                            let has_lazy = match arg {
                                Value::Array(items, _) => items
                                    .iter()
                                    .any(crate::builtins::methods_0arg::is_value_lazy),
                                other => crate::builtins::methods_0arg::is_value_lazy(other),
                            };
                            if has_lazy {
                                return Err(RuntimeError::typed(
                                    "X::Cannot::Lazy",
                                    [
                                        (
                                            "message".to_string(),
                                            Value::str(format!(
                                                "Cannot splice a lazy list into a {}",
                                                type_name
                                            )),
                                        ),
                                        ("action".to_string(), Value::str_from("splice in")),
                                    ]
                                    .into_iter()
                                    .collect(),
                                ));
                            }
                        }
                    }
                    let mut resolved_args = args.clone();
                    // Resolve callable for offset (arg 0) with array length
                    if let Some(arg @ (Value::Sub(..) | Value::WeakSub(..))) = args.first()
                        && let Ok(result) =
                            self.call_sub_value(arg.clone(), vec![Value::Int(arr_len as i64)], true)
                    {
                        resolved_args[0] = result;
                    }
                    // Resolve callable for count (arg 1) with (array_len - offset)
                    if let Some(arg @ (Value::Sub(..) | Value::WeakSub(..))) = args.get(1) {
                        let resolved_start = resolved_args
                            .first()
                            .and_then(|v| match v {
                                Value::Int(i) => Some((*i).max(0) as usize),
                                Value::Whatever => Some(arr_len),
                                _ => None,
                            })
                            .unwrap_or(0)
                            .min(arr_len);
                        let remaining = arr_len.saturating_sub(resolved_start) as i64;
                        if let Ok(result) =
                            self.call_sub_value(arg.clone(), vec![Value::Int(remaining)], true)
                        {
                            resolved_args[1] = result;
                        }
                    }
                    // Validate offset range
                    if let Some(offset_val) = resolved_args.first()
                        && let Some(raw_offset) = resolve_splice_raw(offset_val, arr_len)
                        && (raw_offset < 0 || raw_offset as usize > arr_len)
                    {
                        return Err(RuntimeError::typed(
                            "X::OutOfRange",
                            [
                                (
                                    "message".to_string(),
                                    Value::str(format!(
                                        "Offset argument to splice out of range. Is: {}, should be in 0..{}",
                                        raw_offset, arr_len
                                    )),
                                ),
                                (
                                    "what".to_string(),
                                    Value::str_from("Offset argument to splice"),
                                ),
                                ("got".to_string(), Value::Int(raw_offset)),
                                (
                                    "range".to_string(),
                                    Value::str(format!("0..{}", arr_len)),
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        ));
                    }
                    // Validate size range
                    if let Some(size_val) = resolved_args.get(1)
                        && let Some(raw_size) = resolve_splice_raw(size_val, arr_len)
                        && raw_size < 0
                    {
                        let resolved_start = resolved_args
                            .first()
                            .and_then(|v| resolve_splice_raw(v, arr_len))
                            .unwrap_or(0)
                            .max(0) as usize;
                        let remaining = arr_len.saturating_sub(resolved_start);
                        return Err(RuntimeError::typed(
                            "X::OutOfRange",
                            [
                                (
                                    "message".to_string(),
                                    Value::str(format!(
                                        "Size argument to splice out of range. Is: {}, should be in 0..^{}",
                                        raw_size, remaining
                                    )),
                                ),
                                (
                                    "what".to_string(),
                                    Value::str_from("Size argument to splice"),
                                ),
                                ("got".to_string(), Value::Int(raw_size)),
                                (
                                    "range".to_string(),
                                    Value::str(format!("0..^{}", remaining)),
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        ));
                    }
                    let removed = if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        do_splice(items, &resolved_args)
                    } else {
                        let mut items = match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        };
                        let removed = do_splice(&mut items, &resolved_args);
                        self.env.insert(key.clone(), Value::real_array(items));
                        removed
                    };
                    self.reattach_array_type_metadata(&key, &saved_meta);
                    // Raku's `splice` returns the removed elements as the *same*
                    // typed container as the receiver (`array[int]` splices to
                    // `array[int]`), so propagate the declared type onto the
                    // returned slice too.
                    let mut removed_arr = Value::real_array(removed);
                    if let Some(info) = &saved_meta {
                        removed_arr = self.tag_container_metadata(removed_arr, info.clone());
                    }
                    return Ok(removed_arr);
                }
                "squish" => {
                    let current = self.env.get(&key).cloned().unwrap_or(target.clone());
                    let squished = self.dispatch_squish(current, &args)?;
                    if self.in_lvalue_assignment {
                        let squished_items = match &squished {
                            Value::Array(items, ..) => items.to_vec(),
                            Value::Seq(items) => items.to_vec(),
                            other => vec![other.clone()],
                        };
                        self.env.insert(key, Value::real_array(squished_items));
                    }
                    return Ok(squished);
                }
                _ => {}
            }
        }

        // Handle push/append on hash variables
        if target_var.starts_with('%') {
            let key = target_var.to_string();
            match method {
                "push" | "append" => {
                    let is_push = method == "push";

                    // Typed / object hashes (`my Int %h{Rat}`) must type-check both
                    // the pushed key (against the object-hash key type) and value
                    // (against the element type), and reject a push that would turn a
                    // scalar-typed value into an array (duplicate key). Read the
                    // constraints off the hash's own metadata (authoritative,
                    // travels with COW) falling back to the variable's declared
                    // constraints.
                    let (key_constraint, value_constraint, is_object_hash) =
                        match self.env.get(&key) {
                            Some(Value::Hash(h)) => (
                                h.key_type
                                    .clone()
                                    .or_else(|| self.var_hash_key_constraint(&key)),
                                h.value_type
                                    .clone()
                                    .or_else(|| self.var_type_constraint(&key)),
                                h.key_type.is_some()
                                    || self.var_hash_key_constraint(&key).is_some(),
                            ),
                            _ => (
                                self.var_hash_key_constraint(&key),
                                self.var_type_constraint(&key),
                                self.var_hash_key_constraint(&key).is_some(),
                            ),
                        };
                    let needs_typed_push = is_object_hash
                        || value_constraint
                            .as_deref()
                            .is_some_and(|c| !matches!(c, "" | "Any" | "Mu"));
                    if needs_typed_push {
                        let kv_pairs = Self::hash_push_collect_pairs_kv(args);
                        // Snapshot the existing stored value for each pushed key so
                        // the duplicate-key array-conflict check can run before the
                        // mutable borrow (type_matches_value needs `&mut self`).
                        let existing: Vec<Option<Value>> = {
                            let h = match self.env.get(&key) {
                                Some(Value::Hash(h)) => Some(h),
                                _ => None,
                            };
                            kv_pairs
                                .iter()
                                .map(|(k, _)| {
                                    let wk = if is_object_hash {
                                        crate::runtime::utils::value_which_key(k)
                                    } else {
                                        k.to_string_value()
                                    };
                                    h.and_then(|h| h.map.get(&wk).cloned())
                                })
                                .collect()
                        };
                        for (i, (k, v)) in kv_pairs.iter().enumerate() {
                            if let Some(kc) = &key_constraint
                                && !matches!(kc.as_str(), "" | "Any" | "Mu")
                                && !self.type_matches_value(kc, k)
                            {
                                return Err(crate::runtime::utils::type_check_element_typed_error(
                                    &key, kc, k,
                                ));
                            }
                            if let Some(vc) = &value_constraint
                                && !matches!(vc.as_str(), "" | "Any" | "Mu")
                            {
                                if !matches!(v, Value::Nil) && !self.type_matches_value(vc, v) {
                                    return Err(
                                        crate::runtime::utils::type_check_element_typed_error(
                                            &key, vc, v,
                                        ),
                                    );
                                }
                                // A duplicate key turns the scalar value into an
                                // array; reject it when the element type does not
                                // accept that array.
                                if let Some(ex) = &existing[i] {
                                    let resulting = match ex {
                                        Value::Array(arr, ..) => {
                                            let mut items = arr.to_vec();
                                            items.push(v.clone());
                                            Value::real_array(items)
                                        }
                                        other => Value::real_array(vec![other.clone(), v.clone()]),
                                    };
                                    if !self.type_matches_value(vc, &resulting) {
                                        return Err(
                                            crate::runtime::utils::type_check_element_typed_error(
                                                &key, vc, &resulting,
                                            ),
                                        );
                                    }
                                }
                            }
                        }
                        if let Some(Value::Hash(arc_hash)) = self.env.get_mut(&key) {
                            let hash = Arc::make_mut(arc_hash);
                            for (k, v) in kv_pairs {
                                let wk = if is_object_hash {
                                    crate::runtime::utils::value_which_key(&k)
                                } else {
                                    k.to_string_value()
                                };
                                if is_object_hash {
                                    hash.original_keys
                                        .get_or_insert_with(std::collections::HashMap::new)
                                        .insert(wk.clone(), k);
                                }
                                Self::hash_push_insert(hash, wk, v, is_push);
                            }
                            return Ok(Value::Hash(Arc::clone(arc_hash)));
                        }
                        // No existing hash in the variable: build a fresh typed
                        // hash from the pushed pairs (preserving the constraints).
                        let mut map = std::collections::HashMap::new();
                        let mut orig = std::collections::HashMap::new();
                        for (k, v) in kv_pairs {
                            let wk = if is_object_hash {
                                crate::runtime::utils::value_which_key(&k)
                            } else {
                                k.to_string_value()
                            };
                            if is_object_hash {
                                orig.insert(wk.clone(), k);
                            }
                            Self::hash_push_insert(&mut map, wk, v, is_push);
                        }
                        let mut hd = crate::value::HashData::new(map);
                        if is_object_hash {
                            hd.original_keys = Some(orig);
                            hd.key_type = key_constraint;
                        }
                        hd.value_type = value_constraint;
                        let result = Value::Hash(Arc::new(hd));
                        self.env.insert(key, result.clone());
                        return Ok(result);
                    }

                    // Fast path: COW via Arc::make_mut (O(1) when refcount=1)
                    if let Some(Value::Hash(arc_hash)) = self.env.get_mut(&key) {
                        let hash = Arc::make_mut(arc_hash);
                        let pairs = Self::hash_push_collect_pairs(args);
                        for (k, v) in pairs {
                            Self::hash_push_insert(hash, k, v, is_push);
                        }
                        return Ok(Value::Hash(Arc::clone(arc_hash)));
                    }

                    // Fallback: create from target value
                    let mut hash: std::collections::HashMap<String, Value> = match &target {
                        Value::Hash(h, ..) => h.map.clone(),
                        _ => std::collections::HashMap::new(),
                    };
                    let pairs = Self::hash_push_collect_pairs(args);
                    for (k, v) in pairs {
                        Self::hash_push_insert(&mut hash, k, v, is_push);
                    }
                    let result = Value::Hash(Value::hash_arc(hash));
                    self.env.insert(key, result.clone());
                    return Ok(result);
                }
                _ => {}
            }
        }

        // Handle push/append/pop/shift/unshift on sigilless array bindings
        if !target_var.starts_with('@') && matches!(&target, Value::Array(..)) {
            let key = target_var.to_string();
            let empty_what = match self.var_type_constraint(&key) {
                Some(c)
                    if crate::runtime::native_types::is_native_array_element_type(&c)
                        || matches!(c.as_str(), "num" | "num32" | "num64" | "str") =>
                {
                    format!("array[{c}]")
                }
                _ => "Array".to_string(),
            };
            let array_flag = match self.env.get(&key) {
                Some(Value::Array(_, kind)) => *kind,
                _ => match &target {
                    Value::Array(_, kind) => *kind,
                    _ => ArrayKind::List,
                },
            };
            match method {
                "push" | "append" => {
                    let normalized_args = if method == "push" {
                        Self::normalize_push_unshift_args(args)
                    } else {
                        args
                    };
                    // Check element type constraints from container metadata
                    self.check_array_value_element_types(&target, &normalized_args)?;
                    if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        if method == "append" {
                            items.extend(flatten_append_args(normalized_args));
                        } else {
                            items.extend(normalized_args);
                        }
                        return Ok(Value::Array(Arc::clone(arc_items), *kind));
                    }
                    // Interior mutation: if the target Array has shared references
                    // (Arc refcount > 1), mutate in-place so all references see the
                    // change. This matches Raku's container semantics.
                    if let Value::Array(ref arc_items, _) = target
                        && Arc::strong_count(arc_items) > 1
                    {
                        let vals = if method == "append" {
                            flatten_append_args(normalized_args)
                        } else {
                            normalized_args
                        };
                        for v in vals {
                            target.array_push_in_place(v);
                        }
                        return Ok(target);
                    }
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    if method == "append" {
                        items.extend(flatten_append_args(normalized_args));
                    } else {
                        items.extend(normalized_args);
                    }
                    let result =
                        Value::Array(Arc::new(crate::value::ArrayData::new(items)), array_flag);
                    self.env.insert(key, result.clone());
                    return Ok(result);
                }
                "pop" => {
                    if !args.is_empty() {
                        return Err(RuntimeError::new(format!(
                            "Too many positionals passed; expected 1 argument but got {}",
                            args.len() + 1
                        )));
                    }
                    if let Some(Value::Array(_, kind)) = self.env.get(&key)
                        && kind.is_lazy()
                    {
                        return Err(RuntimeError::cannot_lazy("pop"));
                    }
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        let out = if items.is_empty() {
                            make_empty_array_failure_what("pop", &empty_what)
                        } else {
                            items.pop().unwrap_or(Value::Nil)
                        };
                        return Ok(out);
                    }
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let out = if items.is_empty() {
                        make_empty_array_failure_what("pop", &empty_what)
                    } else {
                        items.pop().unwrap_or(Value::Nil)
                    };
                    self.env.insert(
                        key,
                        Value::Array(Arc::new(crate::value::ArrayData::new(items)), array_flag),
                    );
                    return Ok(out);
                }
                "unshift" => {
                    let normalized_args = Self::normalize_push_unshift_args(args);
                    if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        for (i, arg) in normalized_args.iter().enumerate() {
                            items.insert(i, arg.clone());
                        }
                        return Ok(Value::Array(Arc::clone(arc_items), *kind));
                    }
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    for (i, arg) in normalized_args.iter().enumerate() {
                        items.insert(i, arg.clone());
                    }
                    let result =
                        Value::Array(Arc::new(crate::value::ArrayData::new(items)), array_flag);
                    self.env.insert(key, result.clone());
                    return Ok(result);
                }
                "prepend" => {
                    let flat_values = flatten_append_args(args);
                    if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        for (i, arg) in flat_values.iter().enumerate() {
                            items.insert(i, arg.clone());
                        }
                        return Ok(Value::Array(Arc::clone(arc_items), *kind));
                    }
                    let items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let mut pref: Vec<Value> = flat_values;
                    pref.extend(items);
                    let result = Value::Array(
                        Arc::new(crate::value::ArrayData::new(pref.clone())),
                        array_flag,
                    );
                    self.env.insert(
                        key,
                        Value::Array(Arc::new(crate::value::ArrayData::new(pref)), array_flag),
                    );
                    return Ok(result);
                }
                "shift" => {
                    if !args.is_empty() {
                        return Err(RuntimeError::new(format!(
                            "Too many positionals passed; expected 1 argument but got {}",
                            args.len() + 1
                        )));
                    }
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        let out = if items.is_empty() {
                            make_empty_array_failure_what("shift", &empty_what)
                        } else {
                            items.remove(0)
                        };
                        return Ok(out);
                    }
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let out = if items.is_empty() {
                        make_empty_array_failure_what("shift", &empty_what)
                    } else {
                        items.remove(0)
                    };
                    self.env.insert(
                        key,
                        Value::Array(Arc::new(crate::value::ArrayData::new(items)), array_flag),
                    );
                    return Ok(out);
                }
                _ => {}
            }
        }

        // map with rw binding: mutations to $_ inside map should write back to the
        // source array elements (Raku semantics: $_ is rw-bound in map).
        // The rw-map fast path materializes the source (for `$_`-mutating blocks
        // like `@a.map({ $_++ })`). An infinite sequence/closure spec must stay
        // lazy — fall through to the lazy `map` pipeline in `call_method_with_values`
        // (L2b). Writeback is meaningless on an unbounded array anyway.
        if method == "map"
            && target_var.starts_with('@')
            && !matches!(&target, Value::LazyList(ll) if ll.is_infinite_spec())
        {
            let mut items = if crate::runtime::utils::is_shaped_array(&target) {
                crate::runtime::utils::shaped_array_leaves(&target)
            } else {
                Self::value_to_list(&target)
            };
            let result = self.eval_map_over_items_rw(args.first().cloned(), &mut items)?;
            // Write mutated elements back to the source array (skip for shaped arrays
            // since map shouldn't mutate the shaped structure)
            if !crate::runtime::utils::is_shaped_array(&target) {
                let key = target_var.to_string();
                self.env.insert(key, Value::real_array(items));
            }
            return Ok(result);
        }

        // SetHash.grab / SetHash.grabpairs: remove random elements, mutating the Set
        if matches!(&target, Value::Set(_, true)) && matches!(method, "grab" | "grabpairs") {
            // Resolve Callable args: call with .elems to get count
            let args = if !args.is_empty() && args[0].as_sub().is_some() {
                let callable = args[0].clone();
                let method_sym = crate::symbol::Symbol::intern("elems");
                let input = crate::builtins::native_method_0arg(&target, method_sym)
                    .unwrap_or(Ok(Value::Int(0)))?;
                let count = self.call_sub_value(callable, vec![input], false)?;
                let count_int = match &count {
                    Value::Int(n) => Value::Int(*n),
                    Value::Num(f) => Value::Int(*f as i64),
                    Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                    _ => count.clone(),
                };
                vec![count_int]
            } else {
                args
            };
            // NaN check for grab count
            if !args.is_empty()
                && let Value::Num(f) = &args[0]
                && f.is_nan()
            {
                return Err(RuntimeError::new(
                    "Cannot .grab from a SetHash with NaN elements",
                ));
            }
            let set_data = match &target {
                Value::Set(s, _) => (**s).clone(),
                _ => unreachable!(),
            };
            let mut elements: Vec<String> = set_data.elements.iter().cloned().collect();
            let count = if args.is_empty() {
                1usize
            } else {
                match &args[0] {
                    Value::Whatever => elements.len(),
                    v => v.to_f64().max(0.0) as usize,
                }
            };
            if elements.is_empty() || count == 0 {
                if method == "grab" && args.is_empty() {
                    return Ok(Value::Nil);
                }
                return Ok(Value::Seq(Arc::new(Vec::new())));
            }
            use crate::builtins::rng::builtin_rand;
            let mut grabbed = Vec::new();
            for _ in 0..count {
                if elements.is_empty() {
                    break;
                }
                let idx = (builtin_rand() * elements.len() as f64) as usize % elements.len();
                let key = elements.remove(idx);
                if method == "grabpairs" {
                    grabbed.push(Value::Pair(key, Box::new(Value::Bool(true))));
                } else {
                    grabbed.push(Value::str(key));
                }
            }
            let new_elements: std::collections::HashSet<String> = elements.into_iter().collect();
            let new_set = Value::Set(Arc::new(crate::value::SetData::new(new_elements)), true);
            self.env.insert(target_var.to_string(), new_set);
            return Ok(
                if grabbed.len() == 1 && args.is_empty() && method == "grab" {
                    grabbed.into_iter().next().unwrap()
                } else {
                    Value::Seq(Arc::new(grabbed))
                },
            );
        }

        // BagHash.grab / BagHash.grabpairs: remove random elements, mutating the Bag
        if matches!(&target, Value::Bag(_, true)) && matches!(method, "grab" | "grabpairs") {
            // Resolve Callable args: call with .total (grab) or .elems (grabpairs)
            let args = if !args.is_empty() && args[0].as_sub().is_some() {
                let callable = args[0].clone();
                let input = if method == "grabpairs" {
                    let method_sym = crate::symbol::Symbol::intern("elems");
                    crate::builtins::native_method_0arg(&target, method_sym)
                        .unwrap_or(Ok(Value::Int(0)))?
                } else {
                    let method_sym = crate::symbol::Symbol::intern("total");
                    crate::builtins::native_method_0arg(&target, method_sym)
                        .unwrap_or(Ok(Value::Int(0)))?
                };
                let count = self.call_sub_value(callable, vec![input], false)?;
                let count_int = match &count {
                    Value::Int(n) => Value::Int(*n),
                    Value::Num(f) => Value::Int(*f as i64),
                    Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                    _ => count.clone(),
                };
                vec![count_int]
            } else {
                args
            };
            // NaN check for grab/grabpairs count
            if !args.is_empty()
                && let Value::Num(f) = &args[0]
                && f.is_nan()
            {
                return Err(RuntimeError::new("Cannot convert NaN to Int"));
            }
            let bag = match &target {
                Value::Bag(b, _) => b.counts.clone(),
                _ => unreachable!(),
            };
            let count = if args.is_empty() {
                1usize
            } else {
                match &args[0] {
                    Value::Whatever => {
                        if method == "grabpairs" {
                            bag.len()
                        } else {
                            crate::runtime::utils::bigint_to_i128_sat(&bag.values().sum::<BigInt>())
                                .max(0) as usize
                        }
                    }
                    v => v.to_f64().max(0.0) as usize,
                }
            };
            let keys: Vec<String> = bag.keys().cloned().collect();
            if keys.is_empty() || count == 0 {
                if method == "grab" && args.is_empty() {
                    return Ok(Value::Nil);
                }
                return Ok(Value::Seq(Arc::new(Vec::new())));
            }
            use crate::builtins::rng::builtin_rand;
            let mut grabbed = Vec::new();
            let mut remaining = bag;
            if method == "grabpairs" {
                for _ in 0..count {
                    if remaining.is_empty() {
                        break;
                    }
                    let ks: Vec<String> = remaining.keys().cloned().collect();
                    let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
                    let key = ks[idx].clone();
                    let val = remaining.remove(&key).unwrap_or_default();
                    grabbed.push(Value::Pair(key, Box::new(Value::from_bigint(val))));
                }
            } else {
                // grab: pick weighted random elements one at a time
                for _ in 0..count {
                    if remaining.is_empty() {
                        break;
                    }
                    let total: i128 = remaining
                        .values()
                        .map(crate::runtime::utils::bigint_to_i128_sat)
                        .sum();
                    if total <= 0 {
                        break;
                    }
                    let r = (builtin_rand() * total as f64) as i128;
                    let mut cumulative = 0i128;
                    let mut chosen_key = String::new();
                    for (k, v) in &remaining {
                        cumulative += crate::runtime::utils::bigint_to_i128_sat(v);
                        if r < cumulative {
                            chosen_key = k.clone();
                            break;
                        }
                    }
                    if let Some(c) = remaining.get_mut(&chosen_key) {
                        *c -= BigInt::from(1);
                        if !c.is_positive() {
                            remaining.remove(&chosen_key);
                        }
                    }
                    grabbed.push(Value::str(chosen_key));
                }
            }
            // Update the original variable
            let new_bag = Value::Bag(Arc::new(crate::value::BagData::new(remaining)), true);
            self.env.insert(target_var.to_string(), new_bag);
            return Ok(if grabbed.len() == 1 && args.is_empty() {
                grabbed.into_iter().next().unwrap()
            } else {
                Value::Seq(Arc::new(grabbed))
            });
        }

        // MixHash.grabpairs: remove random pairs and return them, mutating the Mix
        if matches!(&target, Value::Mix(_, _)) && matches!(method, "grabpairs" | "grab") {
            // Resolve Callable args
            let args = if !args.is_empty() && args[0].as_sub().is_some() {
                let callable = args[0].clone();
                let input = if method == "grabpairs" {
                    let method_sym = crate::symbol::Symbol::intern("elems");
                    crate::builtins::native_method_0arg(&target, method_sym)
                        .unwrap_or(Ok(Value::Int(0)))?
                } else {
                    let method_sym = crate::symbol::Symbol::intern("total");
                    crate::builtins::native_method_0arg(&target, method_sym)
                        .unwrap_or(Ok(Value::Int(0)))?
                };
                let count = self.call_sub_value(callable, vec![input], false)?;
                let count_int = match &count {
                    Value::Int(n) => Value::Int(*n),
                    Value::Num(f) => Value::Int(*f as i64),
                    Value::Rat(n, d) if *d != 0 => Value::Int(*n / *d),
                    _ => count.clone(),
                };
                vec![count_int]
            } else {
                args
            };
            let mix = match &target {
                Value::Mix(m, _) => (**m).clone(),
                _ => unreachable!(),
            };
            let count = if method == "grabpairs" {
                if args.is_empty() {
                    1usize
                } else {
                    match &args[0] {
                        Value::Whatever => mix.len(),
                        v => v.to_f64().max(0.0) as usize,
                    }
                }
            } else {
                // grab
                if args.is_empty() {
                    1usize
                } else {
                    match &args[0] {
                        Value::Whatever => mix.len(),
                        v => v.to_f64().max(0.0) as usize,
                    }
                }
            };
            let keys: Vec<String> = mix.keys().cloned().collect();
            if keys.is_empty() || count == 0 {
                return Ok(Value::Seq(Arc::new(Vec::new())));
            }
            use crate::builtins::rng::builtin_rand;
            let mut grabbed = Vec::new();
            let mut remaining = mix;
            for _ in 0..count {
                if remaining.is_empty() {
                    break;
                }
                let ks: Vec<String> = remaining.keys().cloned().collect();
                let idx = (builtin_rand() * ks.len() as f64) as usize % ks.len();
                let key = ks[idx].clone();
                let weight = remaining.remove(&key).unwrap_or(0.0);
                if method == "grabpairs" {
                    let weight_val = crate::value::mix_weight_to_value(weight);
                    grabbed.push(Value::Pair(key, Box::new(weight_val)));
                } else {
                    // grab: return the key
                    grabbed.push(Value::str(key));
                }
            }
            // Update the original variable
            let new_mix = Value::Mix(Arc::new(remaining), true);
            self.env.insert(target_var.to_string(), new_mix);
            return Ok(if grabbed.len() == 1 && args.is_empty() {
                grabbed.into_iter().next().unwrap()
            } else {
                Value::Seq(Arc::new(grabbed))
            });
        }

        // SharedPromise/SharedChannel are internally mutable — delegate to immutable dispatch
        if matches!(target, Value::Promise(_) | Value::Channel(_)) {
            return self.call_method_with_values(target, method, args);
        }

        if let Value::Instance {
            class_name,
            attributes,
            id: target_id,
        } = target.clone()
        {
            if crate::runtime::utils::is_buf_like_class(&class_name.resolve())
                && matches!(method, "write-ubits" | "write-bits")
                && args.len() == 3
            {
                let from = super::to_int(&args[0]);
                let bits = super::to_int(&args[1]);
                if from < 0 || bits < 0 {
                    return Err(RuntimeError::new("bit offset/length must be non-negative"));
                }
                let from = from as usize;
                let bits = bits as usize;
                let mut updated = attributes.to_map();
                let mut bytes = if let Some(Value::Array(items, ..)) = updated.get("bytes") {
                    items
                        .iter()
                        .map(|v| match v {
                            Value::Int(i) => *i as u8,
                            _ => 0,
                        })
                        .collect::<Vec<u8>>()
                } else {
                    Vec::new()
                };
                let required_bits = from.saturating_add(bits);
                let required_len = required_bits.div_ceil(8);
                if bytes.len() < required_len {
                    bytes.resize(required_len, 0);
                }
                let value = normalize_twos_complement(value_to_bigint(&args[2]), bits);
                if bits > 0 {
                    write_bits_into_bytes(&mut bytes, from, bits, &value);
                }
                updated.insert(
                    "bytes".to_string(),
                    Value::array(bytes.into_iter().map(|b| Value::Int(b as i64)).collect()),
                );
                let updated_instance =
                    Value::write_back_sharing(&attributes, class_name, updated, target_id);
                self.env
                    .insert(target_var.to_string(), updated_instance.clone());
                return Ok(updated_instance);
            }

            if class_name == "Iterator" {
                // A detached working copy of the attribute map; written back into
                // the instance's live shared cell at the end.
                let mut updated = attributes.to_map();
                let squish_source = updated.get("squish_source").cloned();
                if let Some(Value::Array(source, ..)) = squish_source {
                    let mut scan_index = match updated.get("squish_scan_index") {
                        Some(Value::Int(i)) if *i >= 0 => *i as usize,
                        _ => 0,
                    };
                    let mut prev_key = updated
                        .get("squish_prev_key")
                        .cloned()
                        .unwrap_or(Value::Nil);
                    let mut initialized =
                        matches!(updated.get("squish_initialized"), Some(Value::Bool(true)));
                    let as_func = updated
                        .get("squish_as")
                        .cloned()
                        .filter(|v| !matches!(v, Value::Nil));
                    let with_func = updated
                        .get("squish_with")
                        .cloned()
                        .filter(|v| !matches!(v, Value::Nil));

                    let mut pull_one_squish = |this: &mut Self| -> Result<Value, RuntimeError> {
                        if !initialized {
                            let Some(first) = source.first().cloned() else {
                                return Ok(Value::str_from("IterationEnd"));
                            };
                            prev_key = if let Some(func) = as_func.clone() {
                                this.call_sub_value(func, vec![first.clone()], true)?
                            } else {
                                first.clone()
                            };
                            initialized = true;
                            scan_index = 1;
                            return Ok(first);
                        }

                        while scan_index < source.len() {
                            let item = source[scan_index].clone();
                            let key = if let Some(func) = as_func.clone() {
                                this.call_sub_value(func, vec![item.clone()], true)?
                            } else {
                                item.clone()
                            };

                            let duplicate = if let Some(func) = with_func.clone() {
                                this.call_sub_value(
                                    func,
                                    vec![prev_key.clone(), key.clone()],
                                    true,
                                )?
                                .truthy()
                            } else {
                                crate::runtime::values_identical(&prev_key, &key)
                            };
                            prev_key = key;
                            scan_index += 1;
                            if !duplicate {
                                return Ok(item);
                            }
                        }
                        Ok(Value::str_from("IterationEnd"))
                    };

                    let ret = match method {
                        "count-only" => self
                            .iterator_count_only_from_attrs(&updated)?
                            .unwrap_or_else(|| Value::Int(0)),
                        "bool-only" => self
                            .iterator_bool_only_from_attrs(&updated)?
                            .unwrap_or(Value::Bool(false)),
                        "pull-one" => pull_one_squish(self)?,
                        "push-all" | "push-until-lazy" => {
                            let mut collected = Vec::new();
                            loop {
                                let next = pull_one_squish(self)?;
                                if matches!(next, Value::Str(ref s) if s.as_str() == "IterationEnd")
                                {
                                    break;
                                }
                                collected.push(next);
                            }
                            if !collected.is_empty()
                                && let Some(Value::Array(existing, arr_kind)) = args.first()
                            {
                                let mut next = existing.to_vec();
                                next.extend(collected);
                                let updated_array = Value::Array(
                                    std::sync::Arc::new(crate::value::ArrayData::new(next)),
                                    *arr_kind,
                                );
                                self.overwrite_array_bindings_by_identity(existing, updated_array);
                            }
                            Value::str_from("IterationEnd")
                        }
                        "skip-one" => {
                            let next = pull_one_squish(self)?;
                            Value::Bool(
                                !matches!(next, Value::Str(ref s) if s.as_str() == "IterationEnd"),
                            )
                        }
                        "skip-at-least" => {
                            let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                            let mut ok = true;
                            for _ in 0..want {
                                let next = pull_one_squish(self)?;
                                if matches!(next, Value::Str(ref s) if s.as_str() == "IterationEnd")
                                {
                                    ok = false;
                                    break;
                                }
                            }
                            Value::Bool(ok)
                        }
                        "skip-at-least-pull-one" => {
                            let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                            for _ in 0..want {
                                let next = pull_one_squish(self)?;
                                if matches!(next, Value::Str(ref s) if s.as_str() == "IterationEnd")
                                {
                                    updated.insert(
                                        "squish_scan_index".to_string(),
                                        Value::Int(scan_index as i64),
                                    );
                                    updated.insert("squish_prev_key".to_string(), prev_key.clone());
                                    updated.insert(
                                        "squish_initialized".to_string(),
                                        Value::Bool(initialized),
                                    );
                                    attributes.commit_attrs(updated.clone());
                                    return Ok(Value::str_from("IterationEnd"));
                                }
                            }
                            pull_one_squish(self)?
                        }
                        "push-exactly" | "push-at-least" => {
                            let want = args.get(1).map(super::to_int).unwrap_or(1).max(0) as usize;
                            let mut collected = Vec::new();
                            for _ in 0..want {
                                let next = pull_one_squish(self)?;
                                if matches!(next, Value::Str(ref s) if s.as_str() == "IterationEnd")
                                {
                                    break;
                                }
                                collected.push(next);
                            }
                            if !collected.is_empty()
                                && let Some(Value::Array(existing, arr_kind)) = args.first()
                            {
                                let mut next = existing.to_vec();
                                next.extend(collected.clone());
                                let updated_array = Value::Array(
                                    std::sync::Arc::new(crate::value::ArrayData::new(next)),
                                    *arr_kind,
                                );
                                self.overwrite_array_bindings_by_identity(existing, updated_array);
                            }
                            if collected.len() >= want {
                                Value::Nil
                            } else {
                                Value::str_from("IterationEnd")
                            }
                        }
                        "sink-all" => {
                            loop {
                                let next = pull_one_squish(self)?;
                                if matches!(next, Value::Str(ref s) if s.as_str() == "IterationEnd")
                                {
                                    break;
                                }
                            }
                            Value::str_from("IterationEnd")
                        }
                        "can" => {
                            let method_name = args
                                .first()
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let supported = matches!(
                                method_name.as_str(),
                                "pull-one"
                                    | "count-only"
                                    | "bool-only"
                                    | "push-exactly"
                                    | "push-at-least"
                                    | "push-all"
                                    | "push-until-lazy"
                                    | "sink-all"
                                    | "skip-one"
                                    | "skip-at-least"
                                    | "skip-at-least-pull-one"
                            );
                            if supported {
                                Value::array(vec![Value::str(method_name)])
                            } else {
                                Value::array(Vec::new())
                            }
                        }
                        _ => self.call_method_with_values(target, method, args)?,
                    };

                    updated.insert(
                        "squish_scan_index".to_string(),
                        Value::Int(scan_index as i64),
                    );
                    updated.insert("squish_prev_key".to_string(), prev_key);
                    updated.insert("squish_initialized".to_string(), Value::Bool(initialized));
                    let updated_instance =
                        Value::write_back_sharing(&attributes, class_name, updated, target_id);
                    self.env
                        .insert(target_var.to_string(), updated_instance.clone());
                    return Ok(ret);
                }

                let items = match updated.get("items") {
                    Some(Value::Array(values, ..)) => values.to_vec(),
                    _ => Vec::new(),
                };
                let mut index = match updated.get("index") {
                    Some(Value::Int(i)) if *i >= 0 => *i as usize,
                    _ => 0,
                };
                let len = items.len();
                // A known logical count (set for `LHS xx N` lazy repeats) overrides
                // the materialized prefix length, so `.count-only` / `.bool-only`
                // on a stored iterator report the true (possibly infinite) count.
                let known_count = updated.get("known_count").cloned();

                let mut append_to_first_array_arg = |vals: &[Value]| {
                    if vals.is_empty() {
                        return;
                    }
                    if let Some(Value::Array(existing, arr_kind)) = args.first() {
                        let mut next = existing.to_vec();
                        next.extend(vals.iter().cloned());
                        let updated_array = Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new(next)),
                            *arr_kind,
                        );
                        self.overwrite_array_bindings_by_identity(existing, updated_array);
                    }
                };

                let ret = match method {
                    "count-only" => known_count
                        .clone()
                        .unwrap_or_else(|| Value::Int(len.saturating_sub(index) as i64)),
                    "bool-only" => match &known_count {
                        Some(c) => Value::Bool(c.to_f64() > 0.0),
                        None => Value::Bool(index < len),
                    },
                    "pull-one" => {
                        if index < len {
                            let out = items[index].clone();
                            index += 1;
                            out
                        } else {
                            Value::str_from("IterationEnd")
                        }
                    }
                    "push-exactly" | "push-at-least" => {
                        let want = args.get(1).map(super::to_int).unwrap_or(1).max(0) as usize;
                        let available = len.saturating_sub(index);
                        let take = available.min(want);
                        if take > 0 {
                            append_to_first_array_arg(&items[index..index + take]);
                            index += take;
                        }
                        if index >= len {
                            Value::str_from("IterationEnd")
                        } else {
                            Value::Nil
                        }
                    }
                    "push-all" | "push-until-lazy" => {
                        if index < len {
                            append_to_first_array_arg(&items[index..]);
                            index = len;
                        }
                        Value::str_from("IterationEnd")
                    }
                    "sink-all" => {
                        index = len;
                        Value::str_from("IterationEnd")
                    }
                    "skip-one" => {
                        if index < len {
                            index += 1;
                            Value::Bool(true)
                        } else {
                            Value::Bool(false)
                        }
                    }
                    "skip-at-least" => {
                        let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                        let available = len.saturating_sub(index);
                        if available >= want {
                            index += want;
                            Value::Bool(true)
                        } else {
                            index = len;
                            Value::Bool(false)
                        }
                    }
                    "skip-at-least-pull-one" => {
                        let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                        let available = len.saturating_sub(index);
                        if available >= want {
                            index += want;
                            if index < len {
                                let out = items[index].clone();
                                index += 1;
                                out
                            } else {
                                Value::str_from("IterationEnd")
                            }
                        } else {
                            index = len;
                            Value::str_from("IterationEnd")
                        }
                    }
                    "can" => {
                        let method_name = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let supported = matches!(
                            method_name.as_str(),
                            "pull-one"
                                | "count-only"
                                | "bool-only"
                                | "push-exactly"
                                | "push-at-least"
                                | "push-all"
                                | "push-until-lazy"
                                | "sink-all"
                                | "skip-one"
                                | "skip-at-least"
                                | "skip-at-least-pull-one"
                        );
                        if supported {
                            return Ok(Value::array(vec![Value::str(method_name)]));
                        } else {
                            return Ok(Value::array(Vec::new()));
                        }
                    }
                    _ => self.call_method_with_values(target, method, args)?,
                };

                updated.insert("index".to_string(), Value::Int(index as i64));
                self.env.insert(
                    target_var.to_string(),
                    Value::write_back_sharing(&attributes, class_name, updated, target_id),
                );
                return Ok(ret);
            }

            // Handle delegation methods: forward the call to the delegate
            if let Some(method_def) = self.resolve_method(&class_name.resolve(), method, &args)
                && method_def.delegation.is_some()
            {
                // Clear skip_pseudo_method_native so the inner delegate dispatch
                // does not inherit the outer call's bypass flag (which was set
                // for the delegator's own method name).
                let saved_skip_pseudo = self.skip_pseudo_method_native.take();
                let (attr_var_name, target_method) = method_def.delegation.as_ref().unwrap();
                let is_method_based = attr_var_name.starts_with('&');
                let attr_key = attr_var_name
                    .trim_start_matches('&')
                    .trim_start_matches('.')
                    .trim_start_matches('!');
                let delegate = if is_method_based {
                    let source_method = attr_var_name.trim_start_matches('&').to_string();
                    let invocant_val =
                        Value::instance_sharing_cell(&attributes, class_name, target_id);
                    self.call_method_with_values(invocant_val, &source_method, Vec::new())?
                } else {
                    attributes
                        .as_map()
                        .get(attr_key)
                        .cloned()
                        .unwrap_or(Value::Nil)
                };
                if delegate == Value::Nil {
                    return Err(RuntimeError::new(format!(
                        "No such method '{}' for invocant of type '{}'",
                        target_method,
                        class_name.resolve()
                    )));
                }
                // Determine sigil for temp var based on delegate type
                let sigil = match &delegate {
                    Value::Array(..) => "@",
                    Value::Hash(_) => "%",
                    _ => "$",
                };
                let temp_var = format!("{}__mutsu_delegation_tmp__", sigil);
                self.env.insert(temp_var.clone(), delegate.clone());
                let result =
                    self.call_method_mut_with_values(&temp_var, delegate, target_method, args)?;
                // Read back the potentially-updated delegate
                let updated_delegate = self.env.get(&temp_var).cloned().unwrap_or(Value::Nil);
                self.env.remove(&temp_var);
                if !is_method_based {
                    // Write the updated delegate back into the frontend's live cell.
                    let mut updated = attributes.to_map();
                    updated.insert(attr_key.to_string(), updated_delegate);
                    self.env.insert(
                        target_var.to_string(),
                        Value::write_back_sharing(&attributes, class_name, updated, target_id),
                    );
                }
                // Restore skip_pseudo for the outer caller.
                self.skip_pseudo_method_native = saved_skip_pseudo;
                return Ok(result);
            }

            if args.len() == 1 && !self.is_native_method(&class_name.resolve(), method) {
                let class_attrs = self.collect_class_attributes(&class_name.resolve());
                let is_public_rw_accessor = if class_attrs.is_empty() {
                    attributes.contains_key(method)
                } else {
                    class_attrs
                        .iter()
                        .any(|(attr_name, is_public, _, is_rw, ..)| {
                            *is_public && attr_name == method && *is_rw
                        })
                };
                if is_public_rw_accessor {
                    // User-defined rw method takes priority over simple accessor
                    let has_rw_method = self
                        .resolve_method(&class_name.resolve(), method, &[])
                        .is_some_and(|m| m.is_rw);
                    if !has_rw_method {
                        let mut updated = attributes.to_map();
                        let assigned = args[0].clone();
                        updated.insert(method.to_string(), assigned.clone());
                        self.env.insert(
                            target_var.to_string(),
                            Value::write_back_sharing(&attributes, class_name, updated, target_id),
                        );
                        return Ok(assigned);
                    }
                    // Signal to assign_method_lvalue to handle via Proxy
                    return Err(super::methods_signature::make_multi_no_match_error(method));
                } else {
                    // Check if there's a user-defined method with is_rw
                    let has_rw_method = self
                        .resolve_method(&class_name.resolve(), method, &[])
                        .is_some_and(|m| m.is_rw);
                    if has_rw_method {
                        // Signal to assign_method_lvalue to handle via Proxy
                        return Err(super::methods_signature::make_multi_no_match_error(method));
                    }
                    // Public accessor exists but is not rw — reject assignment
                    let is_public_accessor = if class_attrs.is_empty() {
                        false
                    } else {
                        class_attrs
                            .iter()
                            .any(|(attr_name, is_public, ..)| *is_public && attr_name == method)
                    };
                    if is_public_accessor {
                        let current = attributes
                            .as_map()
                            .get(method)
                            .cloned()
                            .unwrap_or(Value::Nil);
                        return Err(RuntimeError::assignment_ro_typename(
                            super::utils::value_type_name(&current),
                            &current.to_string_value(),
                        ));
                    }
                }
            }

            if self.is_native_method(&class_name.resolve(), method) {
                // Try mutable dispatch first; if no mutable handler, fall back to immutable
                match self.call_native_instance_method_mut(
                    &class_name.resolve(),
                    attributes.to_map(),
                    method,
                    args.clone(),
                ) {
                    Ok((result, updated)) => {
                        self.env.insert(
                            target_var.to_string(),
                            Value::write_back_sharing(&attributes, class_name, updated, target_id),
                        );
                        return Ok(result);
                    }
                    Err(err) => {
                        if err.message.starts_with("No native mutable method") {
                            return self.call_native_instance_method(
                                &class_name.resolve(),
                                &attributes.as_map(),
                                method,
                                args,
                            );
                        }
                        return Err(err);
                    }
                }
            }
            let skip_pseudo = self
                .skip_pseudo_method_native
                .as_ref()
                .is_some_and(|m| m == method);
            if skip_pseudo {
                self.skip_pseudo_method_native = None;
            }
            let is_pseudo_method = matches!(
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            );
            if self.has_user_method(&class_name.resolve(), method)
                && (!is_pseudo_method || skip_pseudo)
            {
                let (result, updated) = self.run_instance_method(
                    &class_name.resolve(),
                    attributes.to_map(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                let updated_clone = updated.clone();
                attributes.commit_attrs(updated);
                self.env.insert(
                    target_var.to_string(),
                    Value::instance_sharing_cell(&attributes, class_name, target_id),
                );
                // Auto-FETCH if the method returned a Proxy
                if !self.in_lvalue_assignment
                    && let Value::Proxy { ref fetcher, .. } = result
                {
                    return self.proxy_fetch(
                        fetcher,
                        Some(target_var),
                        &class_name.resolve(),
                        &updated_clone,
                        target_id,
                    );
                }
                return Ok(result);
            }
        }
        self.call_method_with_values(target, method, args)
    }

    /// Collect key-value pairs from Hash.push/append arguments.
    /// Arguments can be Pair values or alternating key, value flat lists.
    fn hash_push_collect_pairs(args: Vec<Value>) -> Vec<(String, Value)> {
        let mut pairs = Vec::new();
        let mut iter = args.into_iter().peekable();
        while let Some(arg) = iter.next() {
            match &arg {
                Value::Pair(k, v) => {
                    pairs.push((k.clone(), (**v).clone()));
                }
                Value::ValuePair(k, v) => {
                    pairs.push((k.to_string_value(), (**v).clone()));
                }
                Value::Array(items, ..) => {
                    // Recursively collect pairs from array elements
                    let inner_pairs = Self::hash_push_collect_pairs(items.to_vec());
                    pairs.extend(inner_pairs);
                }
                Value::Seq(items) | Value::Slip(items) => {
                    // A Seq/Slip of pairs (e.g. from `%h.push: %x.invert`) is
                    // flattened like an array, not stringified as one key.
                    let inner_pairs = Self::hash_push_collect_pairs(items.to_vec());
                    pairs.extend(inner_pairs);
                }
                Value::Hash(h, ..) => {
                    for (k, v) in h.iter() {
                        pairs.push((k.clone(), v.clone()));
                    }
                }
                _ => {
                    // Alternating key, value
                    let key = arg.to_string_value();
                    let val = iter.next().unwrap_or(Value::Nil);
                    pairs.push((key, val));
                }
            }
        }
        pairs
    }

    /// Like [`hash_push_collect_pairs`] but preserves the original key *value*
    /// (not its stringification), so typed object hashes can type-check the key
    /// and store it under its `.WHICH` key. The first element of each tuple is
    /// the key value, the second the value.
    fn hash_push_collect_pairs_kv(args: Vec<Value>) -> Vec<(Value, Value)> {
        let mut pairs = Vec::new();
        let mut iter = args.into_iter().peekable();
        while let Some(arg) = iter.next() {
            match arg {
                Value::Pair(k, v) => {
                    pairs.push((Value::str(k), *v));
                }
                Value::ValuePair(k, v) => {
                    pairs.push((*k, *v));
                }
                Value::Array(items, ..) => {
                    pairs.extend(Self::hash_push_collect_pairs_kv(items.to_vec()));
                }
                Value::Seq(items) | Value::Slip(items) => {
                    pairs.extend(Self::hash_push_collect_pairs_kv(items.to_vec()));
                }
                Value::Hash(h, ..) => {
                    for (k, v) in h.map.iter() {
                        let key = h
                            .original_keys
                            .as_ref()
                            .and_then(|o| o.get(k).cloned())
                            .unwrap_or_else(|| Value::str(k.clone()));
                        pairs.push((key, v.clone()));
                    }
                }
                other => {
                    let val = iter.next().unwrap_or(Value::Nil);
                    pairs.push((other, val));
                }
            }
        }
        pairs
    }

    /// Insert a key-value pair into a hash with push/append semantics.
    /// push: if key exists, stack the new value (existing becomes [existing, new])
    /// append: if key exists, flatten arrays when appending
    fn hash_push_insert(
        hash: &mut std::collections::HashMap<String, Value>,
        key: String,
        value: Value,
        is_push: bool,
    ) {
        if let Some(existing) = hash.get(&key) {
            let new_val = match existing {
                Value::Array(arr, ..) => {
                    let mut items = arr.to_vec();
                    if is_push {
                        // push: add value as-is (could be nested array)
                        items.push(value);
                    } else {
                        // append: flatten arrays
                        match value {
                            Value::Array(new_items, ..) => {
                                items.extend(new_items.iter().cloned());
                            }
                            other => items.push(other),
                        }
                    }
                    Value::real_array(items)
                }
                _ => {
                    // First duplicate: create array [existing, new]
                    if is_push {
                        Value::real_array(vec![existing.clone(), value])
                    } else {
                        // append: flatten arrays
                        let mut items = vec![existing.clone()];
                        match value {
                            Value::Array(new_items, ..) => {
                                items.extend(new_items.iter().cloned());
                            }
                            other => items.push(other),
                        }
                        Value::real_array(items)
                    }
                }
            };
            hash.insert(key, new_val);
        } else {
            hash.insert(key, value);
        }
    }

    fn assign_substr_rw(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let str_len = chars.len();

        // Resolve start and end using the same logic as dispatch_substr
        let (start, end) = self.resolve_substr_rw_range(&method_args, str_len)?;

        let replacement = value.to_string_value();

        // Build new string: prefix + replacement + suffix
        let prefix: String = chars[..start].iter().collect();
        let suffix: String = chars[end..].iter().collect();
        let new_str = format!("{}{}{}", prefix, replacement, suffix);

        let result = Value::str(new_str);
        if let Some(var) = target_var {
            self.env.insert(var.to_string(), result.clone());
        }
        Ok(result)
    }

    /// Resolve substr-rw start and end positions from arguments.
    /// Reuses the same logic as dispatch_substr for consistency.
    pub(crate) fn resolve_substr_rw_range(
        &mut self,
        method_args: &[Value],
        str_len: usize,
    ) -> Result<(usize, usize), RuntimeError> {
        // Check if first arg is a Range
        if let Some(first_arg) = method_args.first()
            && let Some((range_start, range_end)) = self.substr_extract_range(first_arg, str_len)?
        {
            let rs: usize = range_start.min(str_len);
            let re: usize = range_end.min(str_len);
            return Ok((rs, re));
        }

        // First arg: start position
        let start_raw: i64 = if let Some(pos) = method_args.first() {
            self.substr_resolve_position(pos, str_len)?
        } else {
            0
        };

        let start = if start_raw < 0 {
            (str_len as i64 + start_raw).max(0) as usize
        } else {
            (start_raw as usize).min(str_len)
        };

        // Second arg: length
        let end = if let Some(len_val) = method_args.get(1) {
            match len_val {
                Value::Int(i) => {
                    let len = (*i).max(0) as usize;
                    (start + len).min(str_len)
                }
                Value::Num(f) if f.is_infinite() && *f > 0.0 => str_len,
                Value::Num(f) => {
                    let len = (*f as i64).max(0) as usize;
                    (start + len).min(str_len)
                }
                Value::Rat(n, d) if *d != 0 => {
                    let len = (*n / *d).max(0) as usize;
                    (start + len).min(str_len)
                }
                Value::Whatever => str_len,
                Value::Sub { .. } => {
                    // WhateverCode/Callable: call with remaining length
                    let remaining = if start <= str_len {
                        (str_len - start) as i64
                    } else {
                        0
                    };
                    let result =
                        self.eval_call_on_value(len_val.clone(), vec![Value::Int(remaining)])?;
                    let len = match &result {
                        Value::Int(i) => (*i).max(0) as usize,
                        Value::Num(f) => (*f as i64).max(0) as usize,
                        Value::Rat(n, d) if *d != 0 => (*n / *d).max(0) as usize,
                        _ => 0,
                    };
                    (start + len).min(str_len)
                }
                _ => str_len,
            }
        } else {
            str_len
        };

        Ok((start, end))
    }

    fn assign_subbuf_rw(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let mut items = if let Value::Instance { attributes, .. } = &target
            && let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes")
        {
            items.to_vec()
        } else {
            Vec::new()
        };

        let from = method_args.first().map(crate::runtime::to_int).unwrap_or(0) as usize;
        let len = method_args.get(1).map(crate::runtime::to_int).unwrap_or(0) as usize;

        let new_bytes = if let Value::Instance { attributes, .. } = &value
            && let Some(Value::Array(new_items, ..)) = attributes.as_map().get("bytes")
        {
            new_items.to_vec()
        } else {
            Vec::new()
        };

        // splice: remove `len` items at `from`, insert `new_bytes`
        let end = (from + len).min(items.len());
        items.splice(from..end, new_bytes);

        let class_name = if let Value::Instance { class_name, .. } = &target {
            class_name.resolve().to_string()
        } else {
            "Buf".to_string()
        };
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(items));
        let new_buf = Value::make_instance(Symbol::intern(&class_name), attrs);

        if let Some(var) = target_var {
            self.env.insert(var.to_string(), new_buf.clone());
        }
        Ok(new_buf)
    }

    fn buf_reallocate(
        &mut self,
        target_var: &str,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let (class_name_sym, mut bytes, orig_id, attrs_cell) = if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = &target
        {
            // Blob is immutable — cannot reallocate
            let cn = class_name.resolve();
            if cn == "Blob" || cn.starts_with("Blob[") || cn.starts_with("blob") {
                return Err(RuntimeError::new(format!(
                    "Cannot reallocate an immutable {}",
                    cn
                )));
            }
            let items = if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id, attributes.clone())
        } else {
            return Err(RuntimeError::new("Not a Buf".to_string()));
        };
        let new_size = match args.first() {
            Some(v) => super::to_int(v) as usize,
            None => 0,
        };
        bytes.resize(new_size, Value::Int(0));
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(bytes));
        let updated = Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
        self.env.insert(target_var.to_string(), updated.clone());
        Ok(updated)
    }

    /// Recursively flatten arguments for Buf mutate methods.
    /// Handles nested arrays, Seq, Slip, and Buf/Blob instances.
    fn flatten_buf_args(args: Vec<Value>) -> Vec<Value> {
        let mut result = Vec::new();
        for a in args {
            match &a {
                Value::Int(_) => result.push(a),
                Value::Array(items, ..) => {
                    // Recursively flatten
                    result.extend(Self::flatten_buf_args(items.to_vec()));
                }
                Value::Seq(items) | Value::Slip(items) => {
                    // Recursively flatten
                    result.extend(Self::flatten_buf_args(items.to_vec()));
                }
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                        result.extend(items.to_vec());
                    }
                }
                _ => result.push(a),
            }
        }
        result
    }

    fn is_buf_like_value(val: &Value) -> bool {
        if let Value::Instance { class_name, .. } = val {
            let cn = class_name.resolve();
            cn == "Buf"
                || cn == "Blob"
                || cn == "utf8"
                || cn == "utf16"
                || cn.starts_with("Buf[")
                || cn.starts_with("Blob[")
                || cn.starts_with("buf")
                || cn.starts_with("blob")
        } else {
            false
        }
    }

    fn buf_mutate_method(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let (class_name_sym, mut bytes, orig_id, attrs_cell) = if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = &target
        {
            let items = if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id, attributes.clone())
        } else {
            return Err(RuntimeError::new("Not a Buf".to_string()));
        };

        // Validate and flatten args to int values
        // String args should throw X::TypeCheck
        for a in &args {
            if matches!(a, Value::Str(_)) {
                let msg = "Type check failed in assignment; expected Int but got Str".to_string();
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("message".to_string(), Value::str(msg.clone()));
                ex_attrs.insert("got".to_string(), a.clone());
                ex_attrs.insert("expected".to_string(), Value::str("Int".to_string()));
                let exception =
                    Value::make_instance(crate::symbol::Symbol::intern("X::TypeCheck"), ex_attrs);
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(exception));
                return Err(err);
            }
        }
        let new_items: Vec<Value> = Self::flatten_buf_args(args);

        match method {
            "append" | "push" => {
                bytes.extend(new_items);
            }
            "prepend" | "unshift" => {
                let mut combined = new_items;
                combined.extend(bytes);
                bytes = combined;
            }
            _ => unreachable!(),
        }

        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(bytes));
        let updated = Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
        self.env.insert(target_var.to_string(), updated.clone());
        Ok(updated)
    }

    fn buf_pop_shift_splice(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let (class_name_sym, mut bytes, orig_id, attrs_cell) = if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = &target
        {
            let cn = class_name.resolve();
            // Blob is immutable
            if crate::runtime::utils::is_blob_like_class(&cn) {
                return Err(RuntimeError::new(format!(
                    "Cannot modify immutable {} with {}",
                    cn, method
                )));
            }
            let items = if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id, attributes.clone())
        } else {
            return Err(RuntimeError::new("Not a Buf".to_string()));
        };

        match method {
            "pop" => {
                if bytes.is_empty() {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("action".to_string(), Value::str("pop".to_string()));
                    ex_attrs.insert("what".to_string(), Value::str("Buf".to_string()));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str("Cannot pop from an empty Buf".to_string()),
                    );
                    let exception = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Cannot::Empty"),
                        ex_attrs,
                    );
                    let mut err = RuntimeError::new("Cannot pop from an empty Buf".to_string());
                    err.exception = Some(Box::new(exception));
                    return Err(err);
                }
                let popped = bytes.pop().unwrap();
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                let updated =
                    Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
                self.env.insert(target_var.to_string(), updated);
                Ok(popped)
            }
            "shift" => {
                if bytes.is_empty() {
                    let mut ex_attrs = HashMap::new();
                    ex_attrs.insert("action".to_string(), Value::str("shift".to_string()));
                    ex_attrs.insert("what".to_string(), Value::str("Buf".to_string()));
                    ex_attrs.insert(
                        "message".to_string(),
                        Value::str("Cannot shift from an empty Buf".to_string()),
                    );
                    let exception = Value::make_instance(
                        crate::symbol::Symbol::intern("X::Cannot::Empty"),
                        ex_attrs,
                    );
                    let mut err = RuntimeError::new("Cannot shift from an empty Buf".to_string());
                    err.exception = Some(Box::new(exception));
                    return Err(err);
                }
                let shifted = bytes.remove(0);
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                let updated =
                    Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
                self.env.insert(target_var.to_string(), updated);
                Ok(shifted)
            }
            "splice" => {
                // .splice() with no args: returns all elements, empties the Buf
                let spliced_bytes = bytes.clone();
                bytes.clear();
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes));
                let updated =
                    Value::write_back_sharing(&attrs_cell, class_name_sym, attrs, orig_id);
                self.env.insert(target_var.to_string(), updated);
                // Return a Buf with the spliced elements
                let mut result_attrs = HashMap::new();
                result_attrs.insert("bytes".to_string(), Value::array(spliced_bytes));
                Ok(Value::make_instance(class_name_sym, result_attrs))
            }
            _ => unreachable!(),
        }
    }
}
