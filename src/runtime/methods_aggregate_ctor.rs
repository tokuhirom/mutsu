//! VM-native construction for the aggregate built-in constructors `Array`/`List`/
//! `Positional`/`array` and `Hash`/`Map`.
//!
//! These are `&mut self` (shaped-dimension parsing, parameterized type checks, and
//! container-metadata tagging), so unlike the pure-data static constructors they
//! cannot live in `try_native_builtin_construct`. Following the QuantHash pattern
//! (`methods_quanthash_ctor.rs`), the construction logic is extracted here once and
//! called from *both* the interpreter's `dispatch_new` arms and the VM fast path
//! (`try_native_aggregate_construct_for_package`) — a true single implementation,
//! byte-identical between the two.

use std::collections::HashMap;

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

impl Interpreter {
    /// Construct an `Array`/`List`/`Positional`/`array` from `.new` arguments.
    ///
    /// Handles the native `array` type-parameter requirement, shaped arrays
    /// (`:shape(...)` + `:data`/positional population, with the X::Assignment shape
    /// errors), Slip/Range/Seq flattening, parameterized type checks
    /// (`Array[Int].new` → `X::TypeCheck::Assignment`) and container-metadata
    /// tagging for typed arrays. The caller has already resolved any parametric
    /// type name into `base_class_name` + `type_args`.
    pub(crate) fn try_native_array_construct(
        &mut self,
        class_name: Symbol,
        base_class_name: &str,
        type_args: &Option<Vec<String>>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // native `array` requires a type parameter (e.g. array[int].new)
        if base_class_name == "array" && type_args.is_none() {
            return Err(RuntimeError::new(
                "Must use a native array with a type parameter (e.g. array[int].new)",
            ));
        }
        if let Some(dims) = self.shaped_dims_from_new_args(args)? {
            // Check for :data argument to populate the shaped array
            let data = args.iter().find_map(|arg| match arg {
                Value::Pair(name, value) if name == "data" => Some(value.as_ref().clone()),
                _ => None,
            });
            // If no :data, collect positional (non-Pair) args as data
            let data = if data.is_none() {
                let positional: Vec<Value> = args
                    .iter()
                    .filter(|arg| !matches!(arg, Value::Pair(..)))
                    .cloned()
                    .collect();
                if positional.is_empty() {
                    None
                } else {
                    Some(Value::array(positional))
                }
            } else {
                data
            };
            let mut shaped = Self::make_shaped_array(&dims);
            if let Some(ref data_val) = data
                && let Some(source_shape) = crate::runtime::utils::shaped_array_shape(data_val)
                && source_shape != dims
            {
                return Err(RuntimeError::new(
                    "X::Assignment::ArrayShapeMismatch: Cannot assign a shaped array to another shaped array with a different shape",
                ));
            }
            if let Some(data_val) = data {
                // Populate the shaped array with data
                let data_items = match data_val {
                    Value::Array(items, ..) => items.to_vec(),
                    Value::Seq(items) | Value::Slip(items) => items.to_vec(),
                    Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. } => crate::runtime::value_to_list(&data_val),
                    other => vec![other],
                };
                // For 1D shaped arrays, flatten nested arrays in the data
                // e.g. Array.new(:shape(5,), [1,2,3,4,0]) should produce [1,2,3,4,0]
                let data_items = if dims.len() == 1 {
                    let mut flat = Vec::new();
                    for item in data_items {
                        match item {
                            Value::Array(inner, ..) => {
                                flat.extend(inner.iter().cloned());
                            }
                            Value::Seq(inner) | Value::Slip(inner) => {
                                flat.extend(inner.iter().cloned());
                            }
                            Value::Range(..)
                            | Value::RangeExcl(..)
                            | Value::RangeExclStart(..)
                            | Value::RangeExclBoth(..)
                            | Value::GenericRange { .. } => {
                                flat.extend(crate::runtime::value_to_list(&item));
                            }
                            other => flat.push(other),
                        }
                    }
                    flat
                } else {
                    data_items
                };
                if let Value::Array(ref items, is_arr) = shaped {
                    let dim_size = dims[0];
                    // For multi-dim arrays, check if data has flat items
                    // (X::Assignment::ToShaped) or too many items for the
                    // first dimension.
                    if dims.len() > 1
                        && data_items
                            .iter()
                            .any(|v| !matches!(v, Value::Array(..) | Value::Nil))
                    {
                        return Err(RuntimeError::new(
                            "X::Assignment::ToShaped: Cannot assign a flat list to a shaped array",
                        ));
                    }
                    if data_items.len() > dim_size {
                        return Err(RuntimeError::new(format!(
                            "Index {} for dimension 1 out of range (must be 0..{})",
                            data_items.len() - 1,
                            dim_size - 1
                        )));
                    }
                    let mut new_items = items.as_ref().clone();
                    for (i, val) in data_items.into_iter().enumerate() {
                        // For multi-dim: check sub-array size
                        if dims.len() > 1
                            && let Value::Array(sub_items, ..) = &val
                            && sub_items.len() > dims[1]
                        {
                            return Err(RuntimeError::new(format!(
                                "Index {} for dimension 2 out of range (must be 0..{})",
                                sub_items.len() - 1,
                                dims[1] - 1
                            )));
                        }
                        if i < new_items.len() {
                            new_items[i] = val;
                        }
                    }
                    let mut result = Value::Array(std::sync::Arc::new(new_items), is_arr);
                    crate::runtime::utils::mark_shaped_array(&result, Some(&dims));
                    // Register type metadata for typed shaped arrays (e.g. array[str].new(:shape(5), ...))
                    if let Some(ta) = type_args
                        && let Some(constraint) = ta.first()
                    {
                        let info = crate::runtime::ContainerTypeInfo {
                            value_type: constraint.clone(),
                            key_type: None,
                            declared_type: Some(if base_class_name == "array" {
                                format!("array[{constraint}]")
                            } else {
                                class_name.resolve()
                            }),
                        };
                        result = self.tag_container_metadata(result, info);
                    }
                    return Ok(result);
                }
            }
            // Register type metadata for typed shaped arrays (e.g. array[str].new(:shape(5)))
            if let Some(ta) = type_args
                && let Some(constraint) = ta.first()
            {
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: constraint.clone(),
                    key_type: None,
                    declared_type: Some(if base_class_name == "array" {
                        format!("array[{constraint}]")
                    } else {
                        class_name.resolve()
                    }),
                };
                shaped = self.tag_container_metadata(shaped, info);
            }
            return Ok(shaped);
        }
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Slip(vals) => items.extend(vals.iter().cloned()),
                // Ranges and sequences passed to `.new` flatten into
                // their elements (e.g. `array[num].new(1e0..10e0)`).
                Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }
                | Value::Seq(_)
                | Value::LazyList(_) => {
                    items.extend(crate::runtime::utils::value_to_list(arg));
                }
                other => items.push(other.clone()),
            }
        }
        // Type check for typed arrays (e.g. Array[Int].new(...))
        // Skip for native types (int8, num32, etc.) which coerce rather than check
        if let Some(ta) = type_args
            && let Some(constraint) = ta.first()
            && constraint.starts_with(char::is_uppercase)
        {
            for item in &items {
                if !self.type_matches_value(constraint, item) {
                    let got_type = crate::value::what_type_name(item);
                    let got_repr = item.to_string_value();
                    let msg = format!(
                        "Type check failed in assignment to ; expected {} but got {} ({})",
                        constraint, got_type, got_repr,
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("operation".to_string(), Value::str_from("assignment"));
                    attrs.insert("got".to_string(), item.clone());
                    attrs.insert(
                        "expected".to_string(),
                        Value::Package(Symbol::intern(constraint)),
                    );
                    let ex =
                        Value::make_instance(Symbol::intern("X::TypeCheck::Assignment"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }
        let mut result = if matches!(base_class_name, "Array" | "array") {
            Value::real_array(items)
        } else {
            Value::array(items)
        };
        // Register type metadata for typed arrays (e.g. Array[Int].new)
        if let Some(ta) = type_args
            && let Some(constraint) = ta.first()
        {
            let info = crate::runtime::ContainerTypeInfo {
                value_type: constraint.clone(),
                key_type: None,
                declared_type: Some(if base_class_name == "array" {
                    format!("array[{constraint}]")
                } else {
                    class_name.resolve()
                }),
            };
            result = self.tag_container_metadata(result, info);
        }
        Ok(result)
    }

    /// Construct a `Hash`/`Map` from `.new` arguments.
    ///
    /// Flattens the args into key/value pairs (named args become data — unlike the
    /// QuantHash constructors, Hash/Map do NOT strip them, matching rakudo issue
    /// #3211 / roast hash.t), applies the parameterized value-type check
    /// (`Hash[Int].new` → `X::TypeCheck::Assignment`), and tags container metadata
    /// for typed hashes and for `Map` (always records the `Map` declared type).
    pub(crate) fn try_native_hash_construct(
        &mut self,
        class_name: Symbol,
        type_args: &Option<Vec<String>>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // NOTE: Hash/Map deliberately do NOT strip named args here.
        // rakudo eats them (`Hash.new(:42a) eqv {}`), but roast
        // hash.t (rakudo issue #3211) asserts `Hash.new(:42a, :666b)`
        // equals the positional `Hash.new((:42a, :666b))` — i.e. the
        // named args should become data. mutsu already does that, so
        // keep it (only the QuantHash constructors eat named args).
        let mut flat = Vec::new();
        for arg in args {
            flat.extend(Self::value_to_list(arg));
        }
        let mut map = HashMap::new();
        let mut iter = flat.into_iter();
        while let Some(item) = iter.next() {
            match item {
                Value::Pair(k, v) => {
                    map.insert(k, *v);
                }
                Value::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), *v);
                }
                other => {
                    let key = other.to_string_value();
                    let value = iter.next().unwrap_or(Value::Nil);
                    map.insert(key, value);
                }
            }
        }
        // Type check for typed hashes (e.g. Hash[Int].new("a","b") should die)
        if let Some(ta) = type_args
            && let Some(constraint) = ta.first()
            && !constraint.is_empty()
            && constraint.starts_with(char::is_uppercase)
        {
            for value in map.values() {
                if !self.type_matches_value(constraint, value) {
                    let got_type = crate::value::what_type_name(value);
                    let got_repr = value.to_string_value();
                    let msg = format!(
                        "Type check failed in assignment to ; expected {} but got {} (\"{}\")",
                        constraint, got_type, got_repr,
                    );
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("operation".to_string(), Value::str_from("assignment"));
                    attrs.insert("got".to_string(), value.clone());
                    attrs.insert(
                        "expected".to_string(),
                        Value::Package(Symbol::intern(constraint)),
                    );
                    let ex =
                        Value::make_instance(Symbol::intern("X::TypeCheck::Assignment"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }
        let result = Value::hash(map);
        // Register type metadata for typed hashes (e.g. Hash[Int].new)
        // or Map.new (always register Map declared_type)
        let is_map = class_name.resolve() == "Map";
        if type_args.is_some() || is_map {
            let (value_type, key_type) = if let Some(ta) = type_args {
                (ta.first().cloned().unwrap_or_default(), ta.get(1).cloned())
            } else {
                (String::new(), None)
            };
            let info = crate::runtime::ContainerTypeInfo {
                value_type,
                key_type,
                declared_type: Some(class_name.resolve()),
            };
            return Ok(self.tag_container_metadata(result, info));
        }
        Ok(result)
    }

    /// VM fast-path wrapper: build an aggregate (`Array`/`List`/`Positional`/
    /// `array`/`Hash`/`Map`) from a `Package` receiver and `.new` args, resolving
    /// any parametric type name into base + type arguments. Returns `Some` when the
    /// class is an aggregate constructor (so the VM uses the native result), else
    /// `None` so the caller falls through. The interpreter's `dispatch_new` arms
    /// delegate to the same per-type helpers, so the two paths are byte-identical.
    pub(crate) fn try_native_aggregate_construct_for_package(
        &mut self,
        class_name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn_resolved = class_name.resolve();
        let parametric = Self::parse_parametric_type_name(&cn_resolved);
        let (base, type_args) = if let Some((base, ta)) = &parametric {
            (base.as_str(), Some(ta.clone()))
        } else {
            (cn_resolved.as_str(), None)
        };
        match base {
            "Array" | "List" | "Positional" | "array" => {
                Some(self.try_native_array_construct(class_name, base, &type_args, args))
            }
            "Hash" | "Map" => Some(self.try_native_hash_construct(class_name, &type_args, args)),
            _ => None,
        }
    }
}
