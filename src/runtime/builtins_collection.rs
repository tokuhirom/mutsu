use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap as StdHashMap;

/// Public wrapper for `collect_minmax_candidates` usable from builtins crate.
pub(crate) fn collect_minmax_candidates_pub(value: &Value, out: &mut Vec<Value>) {
    Interpreter::collect_minmax_candidates(value, out);
}

/// Public wrapper for `make_inclusive_range_value` usable from builtins crate.
pub(crate) fn make_inclusive_range_pub(left: Value, right: Value) -> Value {
    Interpreter::make_inclusive_range_value(left, right)
}

/// Format the result of `first()` according to adverb flags (:k, :kv, :p).
pub(super) fn format_first_result(
    idx: usize,
    value: Value,
    has_k: bool,
    has_kv: bool,
    has_p: bool,
) -> Value {
    if has_k {
        Value::Int(idx as i64)
    } else if has_kv {
        Value::array(vec![Value::Int(idx as i64), value])
    } else if has_p {
        Value::ValuePair(Box::new(Value::Int(idx as i64)), Box::new(value))
    } else {
        value
    }
}

/// Raku `val()` builtin: convert a string into an allomorphic type.
pub(crate) fn builtin_val(args: &[Value]) -> Value {
    let arg = match args.first() {
        Some(v) => v,
        None => return Value::Nil,
    };
    // val() on non-Str types (List, Slip, Array) returns the value unchanged.
    match arg {
        Value::Array(..) | Value::Seq(_) | Value::Slip(_) => return arg.clone(),
        _ => {}
    }
    let original = arg.to_string_value();
    let word = original.trim();

    fn make_allomorphic(val: Value, original: &str) -> Value {
        let mut mixins = StdHashMap::new();
        // Store the original string (with whitespace) as the Str component
        mixins.insert("Str".to_string(), Value::str(original.to_string()));
        Value::mixin(val, mixins)
    }

    // Try Unicode vulgar fractions (single character like ½, ⅓, ¼, etc.)
    if let Some(rat_val) = try_parse_unicode_fraction(word) {
        return make_allomorphic(rat_val, &original);
    }

    // Use the comprehensive Raku numeric string parser
    if let Some(numeric) = crate::runtime::str_numeric::parse_raku_str_to_numeric(word) {
        return make_allomorphic(numeric, &original);
    }

    // Plain string (not parseable as a number)
    Value::str(original.to_string())
}

/// Try to parse a single Unicode vulgar fraction character (½, ⅓, ¼, etc.)
fn try_parse_unicode_fraction(s: &str) -> Option<Value> {
    let mut chars = s.chars();
    let ch = chars.next()?;
    // Must be exactly one character
    if chars.next().is_some() {
        return None;
    }
    let (n, d) = crate::builtins::unicode::unicode_rat_value(ch)?;
    if d == 0 {
        return None;
    }
    Some(crate::value::make_rat(n, d))
}

impl Interpreter {
    pub(super) fn builtin_end(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            let msg = format!(
                "Calling end({}) will never work with signature of the proto ($, *%)",
                std::iter::repeat_n("Int", args.len())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        let elems = self.builtin_elems(args)?;
        match elems {
            Value::Int(n) => Ok(Value::Int(n - 1)),
            _ => Ok(Value::Int(0)),
        }
    }

    pub(super) fn builtin_elems(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            let msg = format!(
                "Calling elems({}) will never work with signature of the proto ($, *%)",
                std::iter::repeat_n("Int", args.len())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Argument"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        // `elems($x)` is defined as `$x.elems`; delegate to the single `.elems`
        // method impl rather than keep a second copy that drifted (it counted Str
        // chars instead of 1, missed Seq, and force-counted lazy lists that raku
        // rejects with X::Cannot::Lazy). The method dispatch still forces
        // gather-sourced lazy lists via its interpreter slow path.
        self.call_method_with_values(args[0].clone(), "elems", vec![])
    }

    pub(super) fn builtin_set(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Check for lazy inputs
        for arg in args {
            if Self::is_lazy_for_coerce(arg) {
                return Err(RuntimeError::cannot_lazy_what("set"));
            }
        }
        let mut elems = HashSet::new();
        let mut original_keys = HashMap::new();
        let mut has_typed_keys = false;

        let insert_value = |val: &Value,
                            elems: &mut HashSet<String>,
                            original_keys: &mut HashMap<String, Value>,
                            has_typed_keys: &mut bool| {
            let key = val.to_string_value();
            if !elems.contains(&key) {
                // Track original type for non-Str values
                if !matches!(val, Value::Str(_)) {
                    original_keys.insert(key.clone(), val.clone());
                    *has_typed_keys = true;
                }
                elems.insert(key);
            }
        };

        for arg in args {
            match arg {
                // Itemized arrays ($[...]) are treated as a single element
                Value::Array(_, kind) if kind.is_itemized() => {
                    insert_value(arg, &mut elems, &mut original_keys, &mut has_typed_keys);
                }
                // Regular arrays are flattened
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        insert_value(item, &mut elems, &mut original_keys, &mut has_typed_keys);
                    }
                }
                // Hashes are decomposed into their pairs
                Value::Hash(map) => {
                    for (k, v) in map.iter() {
                        let pair = Value::Pair(k.clone(), Box::new(v.clone()));
                        insert_value(&pair, &mut elems, &mut original_keys, &mut has_typed_keys);
                    }
                }
                other => {
                    insert_value(other, &mut elems, &mut original_keys, &mut has_typed_keys);
                }
            }
        }
        if has_typed_keys {
            Ok(Value::set_typed(elems, original_keys))
        } else {
            Ok(Value::set(elems))
        }
    }

    pub(super) fn builtin_bag(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Check for lazy inputs
        for arg in args {
            if Self::is_lazy_for_coerce(arg) {
                return Err(RuntimeError::cannot_lazy_what("bag"));
            }
        }
        // The `bag` function counts occurrences of each element.
        // Unlike .Bag coercion, `bag` does NOT decompose pairs into key=>count.
        // Each element (including pairs) is treated as an opaque value to count.
        let mut counts: HashMap<String, i64> = HashMap::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();
        let mut has_non_str_keys = false;

        fn add_item(
            counts: &mut HashMap<String, i64>,
            original_keys: &mut HashMap<String, Value>,
            has_non_str: &mut bool,
            item: &Value,
        ) {
            let str_key = item.to_string_value();
            if !matches!(item, Value::Str(_)) {
                *has_non_str = true;
                original_keys
                    .entry(str_key.clone())
                    .or_insert_with(|| item.clone());
            }
            *counts.entry(str_key).or_insert(0) += 1;
        }

        for arg in args {
            match arg {
                // Itemized arrays/hashes are single elements
                Value::Array(_, kind) if kind.is_itemized() => {
                    add_item(&mut counts, &mut original_keys, &mut has_non_str_keys, arg);
                }
                // Regular arrays are flattened
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        add_item(&mut counts, &mut original_keys, &mut has_non_str_keys, item);
                    }
                }
                // Hashes are flattened into their pairs (each pair is a single element)
                Value::Hash(map) => {
                    for (k, v) in map.iter() {
                        let pair = Value::Pair(k.clone(), Box::new(v.clone()));
                        add_item(
                            &mut counts,
                            &mut original_keys,
                            &mut has_non_str_keys,
                            &pair,
                        );
                    }
                }
                // QuantHash types are single elements
                Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _) => {
                    add_item(&mut counts, &mut original_keys, &mut has_non_str_keys, arg);
                }
                other => {
                    add_item(
                        &mut counts,
                        &mut original_keys,
                        &mut has_non_str_keys,
                        other,
                    );
                }
            }
        }
        if has_non_str_keys {
            Ok(Value::bag_typed(counts, original_keys))
        } else {
            Ok(Value::bag(counts))
        }
    }

    pub(super) fn builtin_mix(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Check for lazy inputs
        for arg in args {
            if Self::is_lazy_for_coerce(arg) {
                return Err(RuntimeError::cannot_lazy_what("mix"));
            }
        }
        let mut weights: HashMap<String, f64> = HashMap::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();
        let mut has_typed_keys = false;

        let insert_value = |val: &Value,
                            weights: &mut HashMap<String, f64>,
                            original_keys: &mut HashMap<String, Value>,
                            has_typed_keys: &mut bool| {
            let key = val.to_string_value();
            // Track original type for non-Str values
            if !matches!(val, Value::Str(_)) {
                original_keys.insert(key.clone(), val.clone());
                *has_typed_keys = true;
            }
            *weights.entry(key).or_insert(0.0) += 1.0;
        };

        for arg in args {
            match arg {
                // Itemized arrays ($[...]) are treated as a single element
                Value::Array(_, kind) if kind.is_itemized() => {
                    insert_value(arg, &mut weights, &mut original_keys, &mut has_typed_keys);
                }
                // Regular arrays are flattened; each element becomes a key
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        insert_value(item, &mut weights, &mut original_keys, &mut has_typed_keys);
                    }
                }
                // Hashes are flattened into their pairs; each pair becomes a key
                Value::Hash(map) => {
                    for (k, v) in map.iter() {
                        let pair = Value::Pair(k.clone(), Box::new(v.clone()));
                        insert_value(&pair, &mut weights, &mut original_keys, &mut has_typed_keys);
                    }
                }
                other => {
                    insert_value(other, &mut weights, &mut original_keys, &mut has_typed_keys);
                }
            }
        }
        if has_typed_keys {
            Ok(Value::mix_with_original_keys(weights, original_keys))
        } else {
            Ok(Value::mix(weights))
        }
    }

    /// VM-native dispatch for the pure list/coercion builtin *functions*
    /// (`val`/`list`/`slip`/`hash`) — collection constructors that reached the
    /// interpreter only via the generic `call_function` name-match fallback. They are
    /// pure / `&self` (no tree-walk, no mutable interpreter state beyond reading
    /// `self`), so the VM dispatches them straight to the existing `builtin_*` impls.
    /// Mirrors the `call_function` arms 1:1 — same args, same `self` => byte-identical.
    /// Dispatched after all user-sub resolution (so a user `sub list` still wins).
    pub(crate) fn try_native_collection_function(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let r = match name {
            "val" => Ok(builtin_val(args)),
            "list" => self.builtin_list(args),
            "slip" | "Slip" => self.builtin_slip(args),
            "hash" => self.builtin_hash(args),
            _ => return None,
        };
        Some(r)
    }

    pub(super) fn builtin_hash(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut flat_values = Vec::new();
        for arg in args {
            flat_values.extend(Self::value_to_list(arg));
        }
        crate::runtime::utils::build_hash_from_items(flat_values)
    }

    pub(super) fn builtin_junction(
        &self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Delegate to the single shared implementation in `builtins` (the same
        // one the VM-native dispatch uses) so this is no longer a duplicate
        // tree-walk copy. See `crate::builtins::functions::build_junction`.
        Ok(crate::builtins::build_junction(name, args))
    }

    pub(super) fn builtin_pair(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let key = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let val = args.get(1).cloned().unwrap_or(Value::Nil);
        Ok(Value::Pair(key, Box::new(val)))
    }

    pub(super) fn builtin_keys(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "keys")
    }

    pub(super) fn builtin_values(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "values")
    }

    pub(super) fn builtin_kv(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "kv")
    }

    pub(super) fn builtin_pairs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "pairs")
    }

    fn builtin_unary_collection_method(
        &self,
        args: &[Value],
        method: &str,
    ) -> Result<Value, RuntimeError> {
        let target = args.first().cloned().unwrap_or(Value::Nil);
        crate::builtins::native_method_0arg(&target, crate::symbol::Symbol::intern(method))
            .unwrap_or_else(|| Ok(Value::array(Vec::new())))
    }
}
