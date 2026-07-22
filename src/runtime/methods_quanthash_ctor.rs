//! Native construction of the QuantHash family (`Set`/`SetHash`/`Bag`/`BagHash`/
//! `Mix`/`MixHash`) `.new`.
//!
//! These constructors are pure value assembly — element counting plus optional
//! parameterized-element type checking and container-type metadata tagging — with
//! no env / registry / user-code dependency. The VM's `.new` dispatch
//! (`vm_call_method_compiled.rs`) calls [`Interpreter::try_native_quanthash_construct`]
//! as a focused native fast path, and the interpreter's generic `dispatch_new`
//! delegates to the same single impl, so the two stay byte-identical (the
//! "1 operation = 1 implementation" rule). §D state-ownership.

use std::collections::{HashMap, HashSet};

use crate::runtime::{ContainerTypeInfo, Interpreter};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// True for the six QuantHash type names this module constructs natively.
    pub(crate) fn is_quanthash_ctor_type(base_class_name: &str) -> bool {
        matches!(
            base_class_name,
            "Set" | "SetHash" | "Bag" | "BagHash" | "Mix" | "MixHash"
        )
    }

    /// Flatten `.new` positional args into the element list a QuantHash counts:
    /// a lone QuantHash argument is a single opaque element, any other single
    /// argument is iterated, and multiple arguments are each one element.
    fn quanthash_flatten_items(args: &[Value]) -> Vec<Value> {
        if args.len() == 1 {
            let arg = &args[0];
            if matches!(
                arg.view(),
                ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _)
            ) {
                vec![arg.clone()]
            } else {
                Self::value_to_list(arg)
            }
        } else {
            args.to_vec()
        }
    }

    /// Apply a parameterized QuantHash's element type to its flattened element
    /// list. A coercion parameter (`Int()`, `Date()`) coerces each element,
    /// propagating the coercion's own exception on failure (`X::Str::Numeric`,
    /// `X::Temporal::InvalidFormat`, ...); a plain nominal parameter type-checks
    /// each element and throws `X::TypeCheck::Binding` on a mismatch. `Any`/`Mu`
    /// and lowercase (type-capture) parameters are no-ops. `unwrap_pair` checks a
    /// `Pair` element by its value (Set semantics) rather than as a whole.
    fn apply_quanthash_element_param(
        &mut self,
        type_args: &Option<Vec<String>>,
        items: Vec<Value>,
        unwrap_pair: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        let Some(constraint) = type_args.as_ref().and_then(|ta| ta.first()) else {
            return Ok(items);
        };
        if !constraint.starts_with(char::is_uppercase) || constraint == "Any" || constraint == "Mu"
        {
            return Ok(items);
        }
        if let Some((target, _source)) = crate::runtime::types::parse_coercion_type(constraint) {
            // A coercion parameter `T(...)` coerces each element with `.T`, the
            // same as `T($x)`. Using the coercion *method* (not the lenient
            // internal `coerce_value`) makes a bad element throw the coercion's
            // own exception — `X::Str::Numeric` for `Int()`, `X::Temporal::
            // InvalidFormat` for `Date()` — instead of silently yielding a
            // fallback value.
            let target = target.to_string();
            let mut out = Vec::with_capacity(items.len());
            for item in items {
                let coerced = self.call_method_with_values(item, &target, vec![])?;
                // A soft-failing coercion (e.g. `"a".Int`) returns a Failure; in
                // the `.new` construction context it must throw its wrapped
                // exception, so sink it into an Err.
                out.push(Self::sink_failure_to_error(coerced)?);
            }
            return Ok(out);
        }
        for item in &items {
            let check_val = if unwrap_pair {
                match item.view() {
                    ValueView::Pair(_, v) => v.clone(),
                    _ => item.clone(),
                }
            } else {
                item.clone()
            };
            if !self.type_matches_value(constraint, &check_val) {
                let got_type = crate::value::what_type_name(&check_val);
                let got_repr = check_val.to_string_value();
                let msg = format!(
                    "Type check failed in binding; expected {} but got {} (\"{}\")",
                    constraint, got_type, got_repr,
                );
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                attrs.insert("operation".to_string(), Value::str_from("bind"));
                attrs.insert("got".to_string(), check_val.clone());
                attrs.insert(
                    "expected".to_string(),
                    Value::package(Symbol::intern(constraint)),
                );
                let ex = Value::make_instance(Symbol::intern("X::TypeCheck::Binding"), attrs);
                let mut err = RuntimeError::new(msg);
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }
        Ok(items)
    }

    /// VM `.new` fast-path entry: if `class_name` (parametric name stripped to
    /// its base) is a QuantHash type, construct it natively and return
    /// `Some(result)`; otherwise return `None` so the caller falls through
    /// unchanged. `args` is borrowed and only cloned when the type actually
    /// matches, so a non-QuantHash `Package.new` pays nothing.
    pub(crate) fn try_native_quanthash_construct_for_package(
        &mut self,
        class_name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let cn = class_name.resolve();
        let (base, type_args) = match Self::parse_parametric_type_name(&cn) {
            Some((base, ta)) => (base, Some(ta)),
            None => (cn.clone(), None),
        };
        if !Self::is_quanthash_ctor_type(&base) {
            return None;
        }
        Some(self.try_native_quanthash_construct(class_name, &base, &type_args, args.to_vec()))
    }

    /// Construct a QuantHash (`Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash`)
    /// `.new(...)` natively. `base_class_name` is the parametric-stripped class
    /// name (so `MixHash[Int]` arrives as `MixHash`); `class_name` is the full
    /// resolved `Symbol` (used for the `declared_type` metadata of a parameterized
    /// variant). Must only be called for a QuantHash type (see
    /// [`is_quanthash_ctor_type`](Self::is_quanthash_ctor_type)).
    pub(crate) fn try_native_quanthash_construct(
        &mut self,
        class_name: Symbol,
        base_class_name: &str,
        type_args: &Option<Vec<String>>,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Clone so the moved arm bodies (lifted verbatim from `dispatch_new`) keep
        // their owned-`Option` `type_args` references unchanged.
        let type_args = type_args.clone();
        match base_class_name {
            "Set" | "SetHash" => {
                let args = Self::strip_named_pair_args(args);
                // Check for lazy inputs
                for a in &args {
                    if Self::is_lazy_for_coerce(a) {
                        return Err(RuntimeError::cannot_lazy_what(base_class_name));
                    }
                }
                // Set/SetHash.new treats each element as an opaque value
                // (does NOT decompose Pairs like the .Set/.SetHash coercion does).
                // Single arg: if QuantHash, treat as single element; otherwise iterate.
                // Multiple args: each is a single element.
                // Flatten the elements, then apply the parameterized element
                // type (coercion or nominal check) before counting.
                let items = Self::quanthash_flatten_items(&args);
                let items = self.apply_quanthash_element_param(&type_args, items, true)?;
                let mut elems = HashSet::new();
                let mut original_keys: HashMap<String, Value> = HashMap::new();
                let mut has_non_str_keys = false;
                let add_item = |elems: &mut HashSet<String>,
                                original_keys: &mut HashMap<String, Value>,
                                has_non_str: &mut bool,
                                item: &Value| {
                    let str_key = item.to_string_value();
                    if !matches!(item.view(), ValueView::Str(_)) {
                        *has_non_str = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| item.clone());
                    }
                    elems.insert(str_key);
                };
                for item in &items {
                    add_item(&mut elems, &mut original_keys, &mut has_non_str_keys, item);
                }
                let is_mutable = base_class_name == "SetHash";
                let result = if has_non_str_keys {
                    if is_mutable {
                        Value::set_hash_typed(elems, original_keys)
                    } else {
                        Value::set_typed(elems, original_keys)
                    }
                } else if is_mutable {
                    Value::set_hash(elems)
                } else {
                    Value::set(elems)
                };
                // Embed type metadata for parameterized SetHash[T]
                let result = if let Some(ref ta) = type_args
                    && let Some(constraint) = ta.first()
                {
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type: constraint.clone(),
                        key_type: Some(constraint.clone()),
                        declared_type: Some(class_name.resolve()),
                    };
                    self.tag_container_metadata(result, info)
                } else {
                    result
                };
                Ok(result)
            }
            "Bag" | "BagHash" => {
                let args = Self::strip_named_pair_args(args);
                // Check for lazy inputs
                for a in &args {
                    if Self::is_lazy_for_coerce(a) {
                        return Err(RuntimeError::cannot_lazy_what(base_class_name));
                    }
                }
                // BagHash.new(|c) takes a Capture:
                // - Single arg: iterate over it (flatten lists/arrays/hashes,
                //   but NOT QuantHash types which are single elements)
                // - Multiple args: each arg is a single element (no flattening)
                // Check for lazy/infinite arguments
                let is_lazy_arg = |v: &Value| -> bool {
                    match v.view() {
                        ValueView::LazyList(_) => true,
                        ValueView::Array(_, kind) if kind.is_lazy() => true,
                        ValueView::Range(_, end)
                        | ValueView::RangeExcl(_, end)
                        | ValueView::RangeExclStart(_, end)
                        | ValueView::RangeExclBoth(_, end) => end == i64::MAX,
                        ValueView::GenericRange { end, .. } => match end.as_ref().view() {
                            ValueView::HyperWhatever => true,
                            ValueView::Num(n) => n.is_infinite() && n.is_sign_positive(),
                            _ => {
                                let n = end.to_f64();
                                n.is_infinite() && n.is_sign_positive()
                            }
                        },
                        _ => false,
                    }
                };
                if args.iter().any(is_lazy_arg) {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "action".to_string(),
                        Value::str(".new-from-pairs".to_string()),
                    );
                    attrs.insert("what".to_string(), Value::str(base_class_name.to_string()));
                    return Err(RuntimeError::typed("X::Cannot::Lazy", attrs));
                }
                // Flatten the elements, then apply the parameterized element type.
                let items = Self::quanthash_flatten_items(&args);
                let items = self.apply_quanthash_element_param(&type_args, items, false)?;
                let mut counts: HashMap<String, i64> = HashMap::new();
                let mut original_keys: HashMap<String, Value> = HashMap::new();
                let mut has_non_str_keys = false;
                let add_item = |counts: &mut HashMap<String, i64>,
                                original_keys: &mut HashMap<String, Value>,
                                has_non_str: &mut bool,
                                item: &Value| {
                    let str_key = item.to_string_value();
                    if !matches!(item.view(), ValueView::Str(_)) {
                        *has_non_str = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| item.clone());
                    }
                    *counts.entry(str_key).or_insert(0) += 1;
                };
                for item in &items {
                    add_item(&mut counts, &mut original_keys, &mut has_non_str_keys, item);
                }
                let result = if has_non_str_keys {
                    if base_class_name == "BagHash" {
                        Value::bag_hash_typed(counts, original_keys)
                    } else {
                        Value::bag_typed(counts, original_keys)
                    }
                } else if base_class_name == "BagHash" {
                    Value::bag_hash(counts)
                } else {
                    Value::bag(counts)
                };
                // Embed type metadata for parameterized Bag[T]/BagHash[T] so
                // later element binding (`$b{42} = 1`) can type-check the key.
                let result = if let Some(ref ta) = type_args
                    && let Some(constraint) = ta.first()
                {
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type: constraint.clone(),
                        key_type: Some(constraint.clone()),
                        declared_type: Some(class_name.resolve()),
                    };
                    self.tag_container_metadata(result, info)
                } else {
                    result
                };
                Ok(result)
            }
            "Mix" | "MixHash" => {
                let args = Self::strip_named_pair_args(args);
                // MixHash.new treats all positional arguments as elements
                // to count. Pairs are NOT decomposed into key=>weight;
                // they are stringified and treated as individual elements.
                // Only .MixHash coercion decomposes pairs.
                // QuantHash types (Set, Bag, Mix) are treated as single
                // elements, not flattened.
                // Check for lazy iterables
                for arg in &args {
                    if Self::is_lazy_for_set_ops(arg) {
                        return Err(RuntimeError::cannot_lazy_what(base_class_name));
                    }
                }
                // Flatten the elements, then apply the parameterized element type.
                let items = Self::quanthash_flatten_items(&args);
                let items = self.apply_quanthash_element_param(&type_args, items, false)?;
                let mut weights: HashMap<String, f64> = HashMap::new();
                let mut original_keys: HashMap<String, Value> = HashMap::new();
                let mut has_non_str_keys = false;
                let add_item = |weights: &mut HashMap<String, f64>,
                                original_keys: &mut HashMap<String, Value>,
                                has_non_str: &mut bool,
                                item: &Value| {
                    let str_key = item.to_string_value();
                    if !matches!(item.view(), ValueView::Str(_)) {
                        *has_non_str = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| item.clone());
                    }
                    *weights.entry(str_key).or_insert(0.0) += 1.0;
                };
                for item in &items {
                    add_item(
                        &mut weights,
                        &mut original_keys,
                        &mut has_non_str_keys,
                        item,
                    );
                }
                // Use `base_class_name` (not `class_name.resolve()`) so a
                // parameterized `MixHash[T]` is still recognized as the mutable
                // variant — `class_name.resolve()` would be "MixHash[T]".
                let is_hash_variant = base_class_name == "MixHash";
                let result = if has_non_str_keys {
                    if is_hash_variant {
                        Value::mix_hash_with_original_keys(weights, original_keys)
                    } else {
                        Value::mix_with_original_keys(weights, original_keys)
                    }
                } else if is_hash_variant {
                    Value::mix_hash(weights)
                } else {
                    Value::mix(weights)
                };
                // For a parameterized Mix[T]/MixHash[T] embed the element type so
                // later element binding (`$m{42} = 1`) can type-check the key.
                let param_constraint = type_args
                    .as_ref()
                    .and_then(|ta| ta.first())
                    .filter(|c| c.starts_with(char::is_uppercase) && *c != "Any" && *c != "Mu");
                let result = if let Some(constraint) = param_constraint {
                    // keyof is the element (key) type; the weight value type is
                    // always Real, so keep `value_type` as "Real" and carry the
                    // parameterized element type in `key_type`.
                    self.tag_container_metadata(
                        result,
                        ContainerTypeInfo {
                            value_type: "Real".to_string(),
                            key_type: Some(constraint.clone()),
                            declared_type: Some(class_name.resolve()),
                        },
                    )
                } else if is_hash_variant {
                    self.tag_container_metadata(
                        result,
                        ContainerTypeInfo {
                            value_type: "Real".to_string(),
                            key_type: None,
                            declared_type: Some("MixHash".to_string()),
                        },
                    )
                } else {
                    result
                };
                Ok(result)
            }
            other => unreachable!(
                "try_native_quanthash_construct called with non-QuantHash type {other:?}"
            ),
        }
    }
}
