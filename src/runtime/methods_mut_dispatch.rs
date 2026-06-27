use super::methods_signature_errors::make_x_immutable_error;
use super::*;
use crate::symbol::Symbol;
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
            // for typed variables, or (Any) for untyped. Prefer the value-carried
            // default (HashData/ArrayData) so it survives raw-parameter binding
            // and list construction, where the name-keyed `var_defaults` lookup
            // (the variable's original name) no longer resolves.
            let default_val = if let Some(def) = Self::value_carried_default(&target) {
                def
            } else if let Some(def) = self.var_default(target_var) {
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

        // `.splice` on a *scalar* variable holding a real array
        // (`my $n = [1,2,3]; $n.splice(1,1)`) writes back through the same array
        // path as `@a.splice`. The plain scalar array-mutator fast path covers
        // push/pop/shift/unshift/append/prepend but not splice, so those reached
        // here only via an `@`-sigiled name; route a scalar-held splice through
        // the `@` block too. (`env.insert(key, …)` below keys on the bare
        // `target_var`, so a scalar name is written back correctly; the metadata
        // lookups return `None` for an untyped scalar, which is a no-op.) This
        // also fixes `is Array`-instance splice, which delegates through a
        // scalar-named (`__mutsu_array_tmp`) temp binding.
        let scalar_holds_real_array = !target_var.starts_with('@')
            && !target_var.starts_with('%')
            && !target_var.starts_with('&')
            && matches!(&target, Value::Array(_, crate::value::ArrayKind::Array));
        if target_var.starts_with('@') || (method == "splice" && scalar_holds_real_array) {
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
                        let kind = *kind;
                        let vals = if method == "append" {
                            flatten_append_args(normalized_args)
                        } else {
                            normalized_args
                        };
                        if Arc::strong_count(arc_items) > 1 {
                            // Shared backing array: mutate the interior in place so
                            // every alias observes the push. `Arc::make_mut` would
                            // detach a private copy and lose the write for those
                            // aliases — the bug behind `$t.push` inside a `for`/`when`
                            // body where `$t` is a by-ref param: entering the block
                            // flushes a shared clone of `$t` into env, and a
                            // make_mut here would write only that env copy, leaving
                            // the caller's binding stale. SAFETY: same contract as
                            // `array_push_in_place` — no live borrow into the items,
                            // and we do not re-enter the VM while the borrow is held.
                            let data = unsafe { crate::value::arc_contents_mut(arc_items) };
                            data.items.extend(vals);
                        } else {
                            Arc::make_mut(arc_items).extend(vals);
                        }
                        return Ok(Value::Array(Arc::clone(arc_items), kind));
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
                        // Shared backing array: in-place interior mutation (see `push`).
                        let items = if Arc::strong_count(arc_items) > 1 {
                            // SAFETY: same contract as `array_push_in_place`.
                            unsafe { &mut crate::value::arc_contents_mut(arc_items).items }
                        } else {
                            Arc::make_mut(arc_items)
                        };
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
                        let kind = *kind;
                        // Shared backing array: mutate the interior in place so
                        // every alias (a by-ref param, a captured-outer slot)
                        // observes the change. See the `push` branch above.
                        let items = if Arc::strong_count(arc_items) > 1 {
                            // SAFETY: same contract as `array_push_in_place`.
                            unsafe { &mut crate::value::arc_contents_mut(arc_items).items }
                        } else {
                            Arc::make_mut(arc_items)
                        };
                        for (i, arg) in normalized_args.iter().enumerate() {
                            items.insert(i, arg.clone());
                        }
                        return Ok(Value::Array(Arc::clone(arc_items), kind));
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
                        let kind = *kind;
                        // Shared backing array: in-place interior mutation (see `push`).
                        let items = if Arc::strong_count(arc_items) > 1 {
                            // SAFETY: same contract as `array_push_in_place`.
                            unsafe { &mut crate::value::arc_contents_mut(arc_items).items }
                        } else {
                            Arc::make_mut(arc_items)
                        };
                        for (i, arg) in flat_values.iter().enumerate() {
                            items.insert(i, arg.clone());
                        }
                        return Ok(Value::Array(Arc::clone(arc_items), kind));
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
                        // Shared backing array: in-place interior mutation (see `push`).
                        let items = if Arc::strong_count(arc_items) > 1 {
                            // SAFETY: same contract as `array_push_in_place`.
                            unsafe { &mut crate::value::arc_contents_mut(arc_items).items }
                        } else {
                            Arc::make_mut(arc_items)
                        };
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
                    return Err(super::methods_signature_errors::make_multi_no_match_error(
                        method,
                    ));
                } else {
                    // Check if there's a user-defined method with is_rw
                    let has_rw_method = self
                        .resolve_method(&class_name.resolve(), method, &[])
                        .is_some_and(|m| m.is_rw);
                    if has_rw_method {
                        // Signal to assign_method_lvalue to handle via Proxy
                        return Err(super::methods_signature_errors::make_multi_no_match_error(
                            method,
                        ));
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
}
