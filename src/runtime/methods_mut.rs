use super::methods_signature::make_x_immutable_error;
use super::*;
use crate::symbol::Symbol;
use num_bigint::BigInt;
use num_traits::Signed;

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
    fn normalize_push_unshift_arg(arg: Value) -> Value {
        match arg {
            Value::Scalar(inner) => *inner,
            Value::Array(items, kind) if kind.is_itemized() => {
                Value::Array(items, kind.decontainerize())
            }
            other => other,
        }
    }

    fn normalize_push_unshift_args(args: Vec<Value>) -> Vec<Value> {
        let needs_normalize = args.iter().any(|arg| match arg {
            Value::Scalar(_) => true,
            Value::Array(_, kind) => kind.is_itemized(),
            _ => false,
        });
        if !needs_normalize {
            return args;
        }
        args.into_iter()
            .map(Self::normalize_push_unshift_arg)
            .collect()
    }

    fn normalize_incdec_source_for_mut(value: Value) -> Value {
        match value {
            Value::Nil | Value::Package(_) => Value::Int(0),
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
            Value::Rat(n, d) | Value::FatRat(n, d) => make_rat(n + d, *d),
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
            Value::Rat(n, d) | Value::FatRat(n, d) => make_rat(n - d, *d),
            Value::Str(s) => match Self::string_pred(s) {
                Ok(prev) => Value::str(prev),
                Err(_) => Value::Str(s.clone()),
            },
            _ => Value::Int(-1),
        }
    }

    fn value_to_non_negative_i64(value: &Value) -> Option<i64> {
        match value {
            Value::Int(i) => Some(*i),
            Value::Num(f) => Some(*f as i64),
            Value::BigInt(i) => num_traits::ToPrimitive::to_i64(i.as_ref()),
            _ => None,
        }
    }

    fn buf_blob_read_bits(
        bytes: &[u8],
        from: i64,
        bits: i64,
        signed: bool,
    ) -> Result<Value, RuntimeError> {
        if from < 0 || bits < 0 {
            return Err(RuntimeError::new(
                "read from out of range. Is: negative offset/length",
            ));
        }
        let from_u = from as usize;
        let bits_u = bits as usize;
        let total_bits = bytes.len().saturating_mul(8);
        if from_u
            .checked_add(bits_u)
            .is_none_or(|end| end > total_bits)
        {
            return Err(RuntimeError::new(format!(
                "read from out of range. Is: {}, should be in 0..{}",
                from, total_bits
            )));
        }
        let mut out = num_bigint::BigInt::from(0u8);
        for i in 0..bits_u {
            let bit_index = from_u + i;
            let byte_index = bit_index / 8;
            let bit_in_byte = 7 - (bit_index % 8);
            let bit = (bytes[byte_index] >> bit_in_byte) & 1;
            out = (out << 1) + num_bigint::BigInt::from(bit);
        }
        if !signed || bits_u == 0 {
            return Ok(Value::from_bigint(out));
        }
        let sign_mask = num_bigint::BigInt::from(1u8) << (bits_u - 1);
        if (&out & &sign_mask) != num_bigint::BigInt::from(0u8) {
            let modulus = num_bigint::BigInt::from(1u8) << bits_u;
            Ok(Value::from_bigint(out - modulus))
        } else {
            Ok(Value::from_bigint(out))
        }
    }

    fn buf_blob_write_bits(
        bytes: &[u8],
        from: i64,
        bits: i64,
        value: &Value,
    ) -> Result<Vec<u8>, RuntimeError> {
        if from < 0 || bits < 0 {
            return Err(RuntimeError::new(
                "write out of range. Is: negative offset/length",
            ));
        }
        let from_u = from as usize;
        let bits_u = bits as usize;
        let required_bits = from_u.saturating_add(bits_u);
        let required_bytes = required_bits.div_ceil(8);

        let mut out = bytes.to_vec();
        if out.len() < required_bytes {
            out.resize(required_bytes, 0);
        }

        if bits_u == 0 {
            return Ok(out);
        }

        let modulus = num_bigint::BigInt::from(1u8) << bits_u;
        let mut encoded = value.to_bigint();
        encoded = ((encoded % &modulus) + &modulus) % &modulus;

        for i in 0..bits_u {
            let shift = bits_u - 1 - i;
            let bit_is_set = ((&encoded >> shift) & num_bigint::BigInt::from(1u8))
                != num_bigint::BigInt::from(0u8);
            let bit_index = from_u + i;
            let byte_index = bit_index / 8;
            let bit_in_byte = 7 - (bit_index % 8);
            if bit_is_set {
                out[byte_index] |= 1u8 << bit_in_byte;
            } else {
                out[byte_index] &= !(1u8 << bit_in_byte);
            }
        }
        Ok(out)
    }

    pub(super) fn overwrite_array_bindings_by_identity(
        &mut self,
        needle: &std::sync::Arc<Vec<Value>>,
        replacement: Value,
    ) {
        let keys: Vec<String> = self
            .env
            .iter()
            .filter_map(|(name, value)| match value {
                Value::Array(existing, ..) if std::sync::Arc::ptr_eq(existing, needle) => {
                    Some(name.clone())
                }
                _ => None,
            })
            .collect();
        for key in keys {
            self.env.insert(key, replacement.clone());
        }
    }

    fn overwrite_hash_bindings_by_identity(
        &mut self,
        needle: &std::sync::Arc<std::collections::HashMap<String, Value>>,
        replacement: Value,
    ) {
        let keys: Vec<String> = self
            .env
            .iter()
            .filter_map(|(name, value)| match value {
                Value::Hash(existing) if std::sync::Arc::ptr_eq(existing, needle) => {
                    Some(name.clone())
                }
                _ => None,
            })
            .collect();
        for key in keys {
            self.env.insert(key, replacement.clone());
        }
    }

    pub(crate) fn overwrite_instance_bindings_by_identity(
        &mut self,
        class_name: &str,
        id: u64,
        updated: std::collections::HashMap<String, Value>,
    ) {
        for bound in self.env.values_mut() {
            let should_replace = match bound {
                Value::Instance {
                    class_name: existing_class,
                    id: existing_id,
                    ..
                } => existing_class == class_name && *existing_id == id,
                _ => false,
            };
            if should_replace {
                *bound = Value::Instance {
                    class_name: Symbol::intern(class_name),
                    attributes: std::sync::Arc::new(updated.clone()),
                    id,
                };
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
            // Merge captured env
            for (k, v) in &data.env {
                new_env.insert(k.clone(), v.clone());
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
            // Propagate outer variable changes (e.g., our variables, closured scalars)
            let mut restored = saved_env;
            for (k, v) in &self.env {
                if restored.contains_key(k)
                    && !k.starts_with('!')
                    && !k.starts_with('.')
                    && k != "self"
                    && k != "_"
                {
                    restored.insert(k.clone(), v.clone());
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
    pub(crate) fn proxy_store(
        &mut self,
        storer: &Value,
        target_var: Option<&str>,
        class_name: &str,
        attributes: &HashMap<String, Value>,
        target_id: u64,
        new_value: Value,
    ) -> Result<Value, RuntimeError> {
        let proxy_val = Value::Proxy {
            fetcher: Box::new(Value::Nil),
            storer: Box::new(storer.clone()),
            subclass: None,
            decontainerized: false,
        };
        let (result, updated) =
            self.call_proxy_callback(storer, vec![proxy_val, new_value.clone()], attributes)?;
        // Propagate attribute changes back to the instance
        if let Some(var_name) = target_var {
            self.overwrite_instance_bindings_by_identity(class_name, target_id, updated.clone());
            self.env.insert(
                var_name.to_string(),
                Value::Instance {
                    class_name: Symbol::intern(class_name),
                    attributes: std::sync::Arc::new(updated),
                    id: target_id,
                },
            );
        }
        Ok(result)
    }

    fn rw_method_attribute_target(body: &[Stmt]) -> Option<String> {
        let first = body.first()?;
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

    pub(crate) fn assign_method_lvalue_with_values(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method: &str,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
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
            && let Some(Value::Int(handle_id)) = attributes.get("handle")
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
            if let Some(state) = self.handles.get_mut(&id) {
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
            && let Some(Value::Int(handle_id)) = attributes.get("handle")
        {
            let id = *handle_id as usize;
            let new_nl_out = value.to_string_value();
            if let Some(state) = self.handles.get_mut(&id) {
                state.nl_out = new_nl_out;
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
            && let Some(Value::Str(key)) = attributes.get("key")
            && let Some(Value::Hash(source_hash)) = attributes.get("__mutsu_hash_ref")
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
                let mut selected_hash: Option<
                    std::sync::Arc<std::collections::HashMap<String, Value>>,
                > = None;
                let mut selected_array: Option<std::sync::Arc<Vec<Value>>> = None;

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
                        let replacement = Value::array(updated);
                        self.overwrite_array_bindings_by_identity(&source_array, replacement);
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
                    let mut updated = (**attributes).clone();
                    updated.insert(attr_name, value.clone());
                    let cn = *class_name;
                    if let Some(var_name) = target_var {
                        self.overwrite_instance_bindings_by_identity(
                            &cn.resolve(),
                            target_id,
                            updated.clone(),
                        );
                        self.env.insert(
                            var_name.to_string(),
                            Value::Instance {
                                class_name: cn,
                                attributes: std::sync::Arc::new(updated),
                                id: target_id,
                            },
                        );
                    }
                    return Ok(value);
                }
            } else {
                // No explicit method found — try auto-accessor for public `is rw` attributes
                let class_attrs = self.collect_class_attributes(qualifier);
                let mut found_rw = false;
                for (attr_name, is_public, _default, is_rw, ..) in &class_attrs {
                    if attr_name == actual_method && *is_public {
                        if !is_rw {
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
                    let mut updated = (**attributes).clone();
                    updated.insert(actual_method.to_string(), value.clone());
                    let cn = *class_name;
                    if let Some(var_name) = target_var {
                        self.overwrite_instance_bindings_by_identity(
                            &cn.resolve(),
                            target_id,
                            updated.clone(),
                        );
                        self.env.insert(
                            var_name.to_string(),
                            Value::Instance {
                                class_name: cn,
                                attributes: std::sync::Arc::new(updated),
                                id: target_id,
                            },
                        );
                    }
                    return Ok(value);
                }
            }
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: rw method '{}' does not expose an assignable attribute",
                actual_method
            )));
        }

        // Preserve existing accessor/setter assignment behavior for concrete variables.
        if let Some(var_name) = target_var {
            match self.call_method_mut_with_values(
                var_name,
                target.clone(),
                method,
                vec![value.clone()],
            ) {
                Ok(result) => return Ok(result),
                Err(err) => {
                    if !err
                        .message
                        .starts_with("No matching candidates for method:")
                        && !err.message.starts_with("X::Method::NotFound:")
                    {
                        return Err(err);
                    }
                }
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

        let method_def = self
            .resolve_method(&class_name.resolve(), method, &method_args)
            .ok_or_else(|| super::methods_signature::make_multi_no_match_error(method))?;
        if !method_def.is_rw {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: method '{}' is not rw",
                method
            )));
        }
        if let Some(attr_name) = Self::rw_method_attribute_target(&method_def.body) {
            let mut updated = (*attributes).clone();
            updated.insert(attr_name, value.clone());
            if let Some(var_name) = target_var {
                self.overwrite_instance_bindings_by_identity(
                    &class_name.resolve(),
                    target_id,
                    updated.clone(),
                );
                self.env.insert(
                    var_name.to_string(),
                    Value::Instance {
                        class_name,
                        attributes: std::sync::Arc::new(updated),
                        id: target_id,
                    },
                );
            }
            return Ok(value);
        }

        // The method body doesn't directly expose an attribute — run it and check for Proxy
        let attrs_map = (*attributes).clone();
        let (method_result, updated_attrs) =
            self.run_instance_method(&class_name.resolve(), attrs_map, method, method_args, None)?;
        if let Value::Proxy { storer, .. } = &method_result {
            return self.proxy_store(
                storer,
                target_var,
                &class_name.resolve(),
                &updated_attrs,
                target_id,
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
        if method == "VAR" && args.is_empty() {
            // Proxy (including subclasses): .VAR returns the proxy wrapped as a
            // ProxyObject so that subsequent method calls don't auto-FETCH.
            if matches!(&target, Value::Proxy { .. }) {
                return Ok(Value::proxy_var_object(target, target_var.to_string()));
            }
            if let Value::Instance { attributes, .. } = &target
                && matches!(attributes.get("__mutsu_var_target"), Some(Value::Str(_)))
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
                .unwrap_or_else(|| "Mu".to_string());
            return Ok(Value::Package(Symbol::intern(&type_name)));
        }

        if let Value::Instance {
            class_name,
            attributes,
            id,
        } = &target
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            let bytes = attributes
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
                return Self::buf_blob_read_bits(&bytes, from, bits, method == "read-bits");
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
                let written = Self::buf_blob_write_bits(&bytes, from, bits, &args[2])?;
                let mut updated_attrs = attributes.as_ref().clone();
                updated_attrs.insert(
                    "bytes".to_string(),
                    Value::array(
                        written
                            .iter()
                            .map(|b| Value::Int(*b as i64))
                            .collect::<Vec<_>>(),
                    ),
                );
                self.overwrite_instance_bindings_by_identity(
                    &class_name.resolve(),
                    *id,
                    updated_attrs.clone(),
                );
                return Ok(Value::Instance {
                    class_name: *class_name,
                    attributes: std::sync::Arc::new(updated_attrs),
                    id: *id,
                });
            }
        }

        // Buf/Blob mutating methods: append, push, prepend, unshift, reallocate
        if matches!(
            method,
            "append" | "push" | "prepend" | "unshift" | "reallocate"
        ) && Self::is_buf_like_value(&target)
        {
            if method == "reallocate" {
                return self.buf_reallocate(target_var, target, &args);
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
            match method {
                "push" => {
                    let normalized_args = Self::normalize_push_unshift_args(args);
                    self.check_container_element_types(&key, &normalized_args)?;
                    let result = self.push_to_shared_var(&key, normalized_args, &target);
                    return Ok(result);
                }
                "append" => {
                    // Raku's append uses the "one-arg rule": if exactly one
                    // non-itemized Array/List argument is passed, its elements
                    // are flattened. With multiple arguments, each is appended
                    // as-is (no recursive flattening).
                    let flat_values = flatten_append_args(args);
                    self.check_container_element_types(&key, &flat_values)?;
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        items.extend(flat_values);
                        return Ok(Value::Nil);
                    }
                    let mut items = match target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    items.extend(flat_values);
                    self.env.insert(key, Value::real_array(items));
                    return Ok(Value::Nil);
                }
                "unshift" | "prepend" => {
                    let normalized_args = Self::normalize_push_unshift_args(args);
                    self.check_container_element_types(&key, &normalized_args)?;
                    if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        for (i, arg) in normalized_args.iter().enumerate() {
                            items.insert(i, arg.clone());
                        }
                        let result = Value::Array(Arc::clone(arc_items), *kind);
                        return Ok(result);
                    }
                    let items = match target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let mut pref: Vec<Value> = normalized_args;
                    pref.extend(items);
                    let result = Value::real_array(pref.clone());
                    self.env.insert(key, Value::real_array(pref));
                    return Ok(result);
                }
                "pop" => {
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        let out = if items.is_empty() {
                            make_empty_array_failure("pop")
                        } else {
                            items.pop().unwrap_or(Value::Nil)
                        };
                        return Ok(out);
                    }
                    let mut items = match target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let out = if items.is_empty() {
                        make_empty_array_failure("pop")
                    } else {
                        items.pop().unwrap_or(Value::Nil)
                    };
                    self.env.insert(key, Value::real_array(items));
                    return Ok(out);
                }
                "shift" => {
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        let out = if items.is_empty() {
                            Value::Nil
                        } else {
                            items.remove(0)
                        };
                        return Ok(out);
                    }
                    let mut items = match target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let out = if items.is_empty() {
                        Value::Nil
                    } else {
                        items.remove(0)
                    };
                    self.env.insert(key, Value::real_array(items));
                    return Ok(out);
                }
                "splice" => {
                    fn do_splice(items: &mut Vec<Value>, args: &[Value]) -> Vec<Value> {
                        let start = args
                            .first()
                            .and_then(|v| match v {
                                Value::Int(i) => Some((*i).max(0) as usize),
                                _ => None,
                            })
                            .unwrap_or(0)
                            .min(items.len());
                        let count = args
                            .get(1)
                            .and_then(|v| match v {
                                Value::Int(i) => Some((*i).max(0) as usize),
                                _ => None,
                            })
                            .unwrap_or(items.len().saturating_sub(start));
                        let end = (start + count).min(items.len());
                        let removed: Vec<Value> = items.drain(start..end).collect();
                        if let Some(new_val) = args.get(2) {
                            match new_val {
                                Value::Array(new_items, ..) => {
                                    for (i, item) in new_items.iter().enumerate() {
                                        items.insert(start + i, item.clone());
                                    }
                                }
                                other => items.insert(start, other.clone()),
                            }
                        }
                        removed
                    }
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        let removed = do_splice(items, &args);
                        return Ok(Value::real_array(removed));
                    }
                    let mut items = match target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let removed = do_splice(&mut items, &args);
                    self.env.insert(key, Value::real_array(items));
                    return Ok(Value::real_array(removed));
                }
                "squish" => {
                    let current = self.env.get(&key).cloned().unwrap_or(target.clone());
                    let squished = self.dispatch_squish(current, &args)?;
                    if self.in_lvalue_assignment {
                        let squished_items = match &squished {
                            Value::Array(items, ..) | Value::Seq(items) => items.to_vec(),
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
                        Value::Hash(h, ..) => (**h).clone(),
                        _ => std::collections::HashMap::new(),
                    };
                    let pairs = Self::hash_push_collect_pairs(args);
                    for (k, v) in pairs {
                        Self::hash_push_insert(&mut hash, k, v, is_push);
                    }
                    let result = Value::Hash(Arc::new(hash));
                    self.env.insert(key, result.clone());
                    return Ok(result);
                }
                _ => {}
            }
        }

        // Handle push/append/pop/shift/unshift on sigilless array bindings
        if !target_var.starts_with('@') && matches!(&target, Value::Array(..)) {
            let key = target_var.to_string();
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
                    if let Some(Value::Array(arc_items, kind)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        if method == "append" {
                            items.extend(flatten_append_args(normalized_args));
                        } else {
                            items.extend(normalized_args);
                        }
                        return Ok(Value::Array(Arc::clone(arc_items), *kind));
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
                    let result = Value::Array(Arc::new(items), array_flag);
                    self.env.insert(key, result.clone());
                    return Ok(result);
                }
                "pop" => {
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        let out = if items.is_empty() {
                            make_empty_array_failure("pop")
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
                        make_empty_array_failure("pop")
                    } else {
                        items.pop().unwrap_or(Value::Nil)
                    };
                    self.env
                        .insert(key, Value::Array(Arc::new(items), array_flag));
                    return Ok(out);
                }
                "unshift" | "prepend" => {
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
                    let result = Value::Array(Arc::new(items), array_flag);
                    self.env.insert(key, result.clone());
                    return Ok(result);
                }
                "shift" => {
                    if let Some(Value::Array(arc_items, _)) = self.env.get_mut(&key) {
                        let items = Arc::make_mut(arc_items);
                        let out = if items.is_empty() {
                            Value::Nil
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
                        Value::Nil
                    } else {
                        items.remove(0)
                    };
                    self.env
                        .insert(key, Value::Array(Arc::new(items), array_flag));
                    return Ok(out);
                }
                _ => {}
            }
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
                let mut updated = (*attributes).clone();
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
                self.overwrite_instance_bindings_by_identity(
                    &class_name.resolve(),
                    target_id,
                    updated.clone(),
                );
                let updated_instance = Value::Instance {
                    class_name,
                    attributes: std::sync::Arc::new(updated),
                    id: target_id,
                };
                self.env
                    .insert(target_var.to_string(), updated_instance.clone());
                return Ok(updated_instance);
            }

            if class_name == "Iterator" {
                let mut updated = (*attributes).clone();
                if let Some(Value::Array(source, ..)) = updated.get("squish_source").cloned() {
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
                                let updated_array =
                                    Value::Array(std::sync::Arc::new(next), *arr_kind);
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
                                    self.overwrite_instance_bindings_by_identity(
                                        &class_name.resolve(),
                                        target_id,
                                        updated.clone(),
                                    );
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
                                let updated_array =
                                    Value::Array(std::sync::Arc::new(next), *arr_kind);
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
                    self.overwrite_instance_bindings_by_identity(
                        &class_name.resolve(),
                        target_id,
                        updated.clone(),
                    );
                    let updated_instance = Value::Instance {
                        class_name,
                        attributes: std::sync::Arc::new(updated),
                        id: target_id,
                    };
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

                let mut append_to_first_array_arg = |vals: &[Value]| {
                    if vals.is_empty() {
                        return;
                    }
                    if let Some(Value::Array(existing, arr_kind)) = args.first() {
                        let mut next = existing.to_vec();
                        next.extend(vals.iter().cloned());
                        let updated_array = Value::Array(std::sync::Arc::new(next), *arr_kind);
                        self.overwrite_array_bindings_by_identity(existing, updated_array);
                    }
                };

                let ret = match method {
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
                self.overwrite_instance_bindings_by_identity(
                    &class_name.resolve(),
                    target_id,
                    updated.clone(),
                );
                self.env.insert(
                    target_var.to_string(),
                    Value::Instance {
                        class_name,
                        attributes: std::sync::Arc::new(updated),
                        id: target_id,
                    },
                );
                return Ok(ret);
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
                        let mut updated = (*attributes).clone();
                        let assigned = args[0].clone();
                        updated.insert(method.to_string(), assigned.clone());
                        self.overwrite_instance_bindings_by_identity(
                            &class_name.resolve(),
                            target_id,
                            updated.clone(),
                        );
                        self.env.insert(
                            target_var.to_string(),
                            Value::Instance {
                                class_name,
                                attributes: std::sync::Arc::new(updated),
                                id: target_id,
                            },
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
                        let current = attributes.get(method).cloned().unwrap_or(Value::Nil);
                        return Err(RuntimeError::new(format!(
                            "Cannot modify an immutable {} ({})",
                            super::utils::value_type_name(&current),
                            current.to_string_value()
                        )));
                    }
                }
            }

            if self.is_native_method(&class_name.resolve(), method) {
                // Try mutable dispatch first; if no mutable handler, fall back to immutable
                match self.call_native_instance_method_mut(
                    &class_name.resolve(),
                    (*attributes).clone(),
                    method,
                    args.clone(),
                ) {
                    Ok((result, updated)) => {
                        self.overwrite_instance_bindings_by_identity(
                            &class_name.resolve(),
                            target_id,
                            updated.clone(),
                        );
                        self.env.insert(
                            target_var.to_string(),
                            Value::Instance {
                                class_name,
                                attributes: std::sync::Arc::new(updated),
                                id: target_id,
                            },
                        );
                        return Ok(result);
                    }
                    Err(err) => {
                        if err.message.starts_with("No native mutable method") {
                            return self.call_native_instance_method(
                                &class_name.resolve(),
                                &attributes,
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
                    (*attributes).clone(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                self.overwrite_instance_bindings_by_identity(
                    &class_name.resolve(),
                    target_id,
                    updated.clone(),
                );
                let updated_clone = updated.clone();
                self.env.insert(
                    target_var.to_string(),
                    Value::Instance {
                        class_name,
                        attributes: std::sync::Arc::new(updated),
                        id: target_id,
                    },
                );
                // Auto-FETCH if the method returned a Proxy
                if let Value::Proxy { ref fetcher, .. } = result {
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

    fn assign_subbuf_rw(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let mut items = if let Value::Instance { attributes, .. } = &target
            && let Some(Value::Array(items, ..)) = attributes.get("bytes")
        {
            items.to_vec()
        } else {
            Vec::new()
        };

        let from = method_args.first().map(crate::runtime::to_int).unwrap_or(0) as usize;
        let len = method_args.get(1).map(crate::runtime::to_int).unwrap_or(0) as usize;

        let new_bytes = if let Value::Instance { attributes, .. } = &value
            && let Some(Value::Array(new_items, ..)) = attributes.get("bytes")
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
        let (class_name_sym, mut bytes, orig_id) = if let Value::Instance {
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
            let items = if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id)
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
        let updated = Value::Instance {
            class_name: class_name_sym,
            attributes: std::sync::Arc::new(attrs),
            id: orig_id,
        };
        self.env.insert(target_var.to_string(), updated.clone());
        Ok(updated)
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
        let (class_name_sym, mut bytes, orig_id) = if let Value::Instance {
            class_name,
            attributes,
            id,
            ..
        } = &target
        {
            let items = if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                items.to_vec()
            } else {
                Vec::new()
            };
            (*class_name, items, *id)
        } else {
            return Err(RuntimeError::new("Not a Buf".to_string()));
        };

        // Flatten args to int values
        let new_items: Vec<Value> = args
            .into_iter()
            .flat_map(|a| match a {
                Value::Int(_) => vec![a],
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) => items.to_vec(),
                Value::Slip(items) => items.to_vec(),
                _ => vec![a],
            })
            .collect();

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
        let updated = Value::Instance {
            class_name: class_name_sym,
            attributes: std::sync::Arc::new(attrs),
            id: orig_id,
        };
        self.env.insert(target_var.to_string(), updated.clone());
        Ok(updated)
    }
}
