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
        Value::int(idx as i64)
    } else if has_kv {
        Value::array(vec![Value::int(idx as i64), value])
    } else if has_p {
        Value::value_pair(Value::int(idx as i64), value)
    } else {
        value
    }
}

/// Raku `val()` builtin: convert a string into an allomorphic type.
pub(crate) fn builtin_val(args: &[Value]) -> Value {
    let arg = match args.first() {
        Some(v) => v,
        None => return Value::NIL,
    };
    // val() on non-Str types (List, Slip, Array) returns the value unchanged.
    match arg.view() {
        ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_) => return arg.clone(),
        _ => {}
    }
    let original = arg.to_string_value();
    let word = original.trim();
    // Whitespace *around* a number is fine (`val(" 42 ")` is `IntStr.new(42, " 42 ")`),
    // but a non-empty all-whitespace string is not numeric at all — rakudo
    // returns a plain `Str` for `val(" ")`, `val("\t")`, `val("\n")`. Only the
    // genuinely EMPTY string numifies, to `IntStr.new(0, "")`. Without this
    // guard the trim above turned every whitespace argument into that same
    // `0`, which `sub MAIN(:$y)` then reported for `-y= ` (roast
    // S06-other/main-usage.t).
    if word.is_empty() && !original.is_empty() {
        return Value::str(original.to_string());
    }

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
        match elems.view() {
            ValueView::Int(n) => Ok(Value::int(n - 1)),
            _ => Ok(Value::int(0)),
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

    fn reify_finite_closure_args(&mut self, args: &[Value]) -> Result<Vec<Value>, RuntimeError> {
        args.iter()
            .map(|arg| match arg.view() {
                ValueView::LazyList(list) if list.has_finite_closure_endpoint() => {
                    self.force_lazy_list_vm(&list).map(Value::seq)
                }
                _ => Ok(arg.clone()),
            })
            .collect()
    }

    pub(super) fn builtin_set(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let args = self.reify_finite_closure_args(args)?;
        // Check for lazy inputs
        for arg in &args {
            if Self::is_lazy_for_coerce(arg) {
                return Err(RuntimeError::cannot_lazy_what("set"));
            }
        }
        let mut elems = HashSet::new();
        let mut original_keys = HashMap::new();

        let insert_value = |val: &Value,
                            elems: &mut HashSet<String>,
                            original_keys: &mut HashMap<String, Value>| {
            crate::runtime::utils::quanthash_insert_set(elems, original_keys, val);
        };

        for arg in &args {
            match arg.view() {
                // Itemized arrays ($[...]) are treated as a single element
                ValueView::Array(_, kind) if kind.is_itemized() => {
                    insert_value(arg, &mut elems, &mut original_keys);
                }
                // Regular arrays are flattened
                ValueView::Array(items, ..) => {
                    for item in items.iter() {
                        insert_value(item, &mut elems, &mut original_keys);
                    }
                }
                // Hashes are decomposed into their pairs
                ValueView::Hash(map) => {
                    for (k, v) in map.iter() {
                        // ADR-0021 I2: data-minted pairs default positional.
                        let pair = Value::value_pair(Value::str(k.clone()), v.clone());
                        insert_value(&pair, &mut elems, &mut original_keys);
                    }
                }
                // A Seq (e.g. from `.map`/`.comb`) is a list of elements, not one
                // opaque value — flatten it like a regular array.
                ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                    for item in items.iter() {
                        insert_value(item, &mut elems, &mut original_keys);
                    }
                }
                _ => {
                    insert_value(arg, &mut elems, &mut original_keys);
                }
            }
        }
        Ok(Value::set_typed(elems, original_keys))
    }

    /// The *capitalised* QuantHash coercion functions — `Set(...)`,
    /// `SetHash(...)`, `Bag(...)`, `BagHash(...)`, `Mix(...)`, `MixHash(...)`.
    ///
    /// Rakudo spells these `multi sub Mix(+@a) { @a.Mix }`: the arguments are
    /// slurped into a list and that *list is coerced*, so a positional `Pair`
    /// argument contributes `key => weight` and a nested QuantHash spills its
    /// own pairs. That is deliberately NOT the lowercase `mix(+@a) {
    /// Mix.new(@a) }` family, where every element (`Pair`s included) stays an
    /// opaque key of weight 1 — which is what `builtin_set` / `builtin_bag` /
    /// `builtin_mix` implement, and which stays correct for `set`/`bag`/`mix`.
    ///
    /// Sharing the `new`-flavoured builders between both spellings is what made
    /// `MixHash(2 => 2, 4)` read as the two opaque keys `2 => 2` and `4`
    /// instead of `2(2) 4(1)` — which in turn made every weighted set operator
    /// over such an operand produce garbage. Routing the coercion spelling
    /// through the same `quanthash_coerce` builders the `.Mix`/`.Bag`/`.Set`
    /// *methods* already use keeps one implementation per operation.
    pub(super) fn builtin_quanthash_coerce(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let args = self.reify_finite_closure_args(args)?;
        // `+@a` slurps the arguments into a List, and it is that List which is
        // coerced — so a lone `Mix(@a)` still flattens `@a` (a List element
        // spills in list context) while `Mix($p)` keeps an itemized Pair whole.
        let list = Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(args)),
            crate::value::ArrayKind::List,
        );
        use crate::builtins::quanthash_coerce;
        match name {
            "Set" => quanthash_coerce::to_set(list, "Set"),
            "SetHash" => quanthash_coerce::to_set(list, "SetHash")
                .map(|v| crate::runtime::utils::with_set_mutability(v, true)),
            "Bag" => quanthash_coerce::to_bag(list, "Bag"),
            "BagHash" => quanthash_coerce::to_bag(list, "BagHash")
                .map(|v| crate::runtime::utils::with_set_mutability(v, true)),
            "Mix" => quanthash_coerce::to_mix(list, "Mix"),
            _ => quanthash_coerce::to_mixhash(list),
        }
    }

    pub(super) fn builtin_bag(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let args = self.reify_finite_closure_args(args)?;
        // Check for lazy inputs
        for arg in &args {
            if Self::is_lazy_for_coerce(arg) {
                return Err(RuntimeError::cannot_lazy_what("bag"));
            }
        }
        // The `bag` function counts occurrences of each element.
        // Unlike .Bag coercion, `bag` does NOT decompose pairs into key=>count.
        // Each element (including pairs) is treated as an opaque value to count.
        let mut counts: HashMap<String, i64> = HashMap::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();

        fn add_item(
            counts: &mut HashMap<String, i64>,
            original_keys: &mut HashMap<String, Value>,
            item: &Value,
        ) {
            let (key, elem) = crate::runtime::utils::quanthash_elem_entry(item);
            crate::runtime::utils::record_quanthash_original(original_keys, &key, &elem);
            *counts.entry(key).or_insert(0) += 1;
        }

        for arg in &args {
            match arg.view() {
                // Itemized arrays/hashes are single elements
                ValueView::Array(_, kind) if kind.is_itemized() => {
                    add_item(&mut counts, &mut original_keys, arg);
                }
                // Regular arrays are flattened
                ValueView::Array(items, ..) => {
                    for item in items.iter() {
                        add_item(&mut counts, &mut original_keys, item);
                    }
                }
                // Hashes are flattened into their pairs (each pair is a single element)
                ValueView::Hash(map) => {
                    for (k, v) in map.iter() {
                        // ADR-0021 I2: data-minted pairs default positional.
                        let pair = Value::value_pair(Value::str(k.clone()), v.clone());
                        add_item(&mut counts, &mut original_keys, &pair);
                    }
                }
                // QuantHash types are single elements
                ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _) => {
                    add_item(&mut counts, &mut original_keys, arg);
                }
                // A Seq (e.g. from `.map`/`.comb`) is a list of elements, not one
                // opaque value — flatten it like a regular array.
                ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                    for item in items.iter() {
                        add_item(&mut counts, &mut original_keys, item);
                    }
                }
                _ => {
                    add_item(&mut counts, &mut original_keys, arg);
                }
            }
        }
        Ok(Value::bag_typed(counts, original_keys))
    }

    pub(super) fn builtin_mix(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let args = self.reify_finite_closure_args(args)?;
        // Check for lazy inputs
        for arg in &args {
            if Self::is_lazy_for_coerce(arg) {
                return Err(RuntimeError::cannot_lazy_what("mix"));
            }
        }
        let mut weights: HashMap<String, f64> = HashMap::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();

        let insert_value = |val: &Value,
                            weights: &mut HashMap<String, f64>,
                            original_keys: &mut HashMap<String, Value>| {
            let (key, elem) = crate::runtime::utils::quanthash_elem_entry(val);
            crate::runtime::utils::record_quanthash_original(original_keys, &key, &elem);
            *weights.entry(key).or_insert(0.0) += 1.0;
        };

        for arg in &args {
            match arg.view() {
                // Itemized arrays ($[...]) are treated as a single element
                ValueView::Array(_, kind) if kind.is_itemized() => {
                    insert_value(arg, &mut weights, &mut original_keys);
                }
                // Regular arrays are flattened; each element becomes a key
                ValueView::Array(items, ..) => {
                    for item in items.iter() {
                        insert_value(item, &mut weights, &mut original_keys);
                    }
                }
                // Hashes are flattened into their pairs; each pair becomes a key
                ValueView::Hash(map) => {
                    for (k, v) in map.iter() {
                        // ADR-0021 I2: data-minted pairs default positional.
                        let pair = Value::value_pair(Value::str(k.clone()), v.clone());
                        insert_value(&pair, &mut weights, &mut original_keys);
                    }
                }
                // A Seq (e.g. from `.map`/`.comb`) is a list of elements, not one
                // opaque value — flatten it like a regular array.
                ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                    for item in items.iter() {
                        insert_value(item, &mut weights, &mut original_keys);
                    }
                }
                _ => {
                    insert_value(arg, &mut weights, &mut original_keys);
                }
            }
        }
        Ok(Value::mix_with_original_keys(weights, original_keys))
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
            "__object_hash" => self.builtin_object_hash(args),
            _ => return None,
        };
        Some(r)
    }

    pub(super) fn builtin_hash(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut flat_values = Vec::new();
        for arg in args {
            flat_values.extend(Self::value_to_list(arg));
        }
        self.build_hash_from_items_warning(flat_values)
    }

    /// `:{ ... }` — a `Mu`-keyed object hash. Builds like `hash(...)` (which
    /// records the key objects in `original_keys`), then tags the `Mu` key type
    /// and re-keys by `.WHICH`. Uses the warning-free builder: an object hash
    /// KEEPS a type-object key distinct (no ""-stringification warning).
    pub(super) fn builtin_object_hash(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut flat_values = Vec::new();
        for arg in args {
            flat_values.extend(Self::value_to_list(arg));
        }
        let hash = crate::runtime::utils::build_hash_from_items(flat_values)?;
        Ok(crate::runtime::utils::into_object_hash(hash, "Mu"))
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
        let val = args.get(1).cloned().unwrap_or(Value::NIL);
        // ADR-0021 I2: `pair(...)` is a data constructor, not argument-list
        // syntax — the result is a plain (positional-flavour) Pair.
        Ok(Value::value_pair(Value::str(key), val))
    }

    pub(super) fn builtin_keys(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "keys")
    }

    pub(super) fn builtin_values(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "values")
    }

    pub(super) fn builtin_kv(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "kv")
    }

    pub(super) fn builtin_pairs(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_unary_collection_method(args, "pairs")
    }

    // ADR-0019 Phase E box E11: route through the canonical resolver entry
    // point (`call_method_with_values`) instead of calling `native_method_0arg`
    // directly. The E2 catalog existence check
    // (`Interpreter::e2_native_method_exists`) stands in for the old
    // `Option`-based "did the native cascade recognize this name at all"
    // probe, preserving the exact fallback: an unrecognized `(target, method)`
    // pair (e.g. a bare `keys()` call, `target` defaulting to `Value::NIL`)
    // still yields an empty list rather than a dispatch error.
    fn builtin_unary_collection_method(
        &mut self,
        args: &[Value],
        method: &'static str,
    ) -> Result<Value, RuntimeError> {
        let target = args.first().cloned().unwrap_or(Value::NIL);
        if self.e2_native_method_exists(&target, method) {
            self.call_method_with_values(target, method, Vec::new())
        } else {
            Ok(Value::array(Vec::new()))
        }
    }
}
