//! Hyper operators over Set/Bag/Mix operands.
//!
//! Rakudo hypers a QuantHash as the Associative it is: over its keys, with
//! the dwim arrows picking the key set exactly as for a Hash, and the result
//! rebuilt as the type of the operand that donates the structure. mutsu
//! used to walk a QuantHash as a list of Pairs and nest the per-pair results
//! (`set(1,2) «∪» set(3)` answered `(1 => 3 => Set.new(True), ...)`, #9482).
//! Here a QuantHash operand is projected to its `.Hash` -- the one coercion
//! `builtins::map_hash_coerce::to_hash` implements -- the Hash hyper runs, and
//! the result is folded back into the donor's QuantHash type.

use super::vm_string_regex_ops::QuantKind;
use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// The QuantHash kind and mutability a hyper result is rebuilt as, when a
    /// Set/Bag/Mix operand takes part: the left operand's when it is
    /// Associative, else the right one's. So `bag(...) »+« %h` is a Bag,
    /// `%h »+« bag(...)` a Hash and `1 «+« bag(...)` a Bag, as in rakudo.
    // Cost: O(1).
    pub(super) fn hyper_quanthash_result(left: &Value, right: &Value) -> Option<(QuantKind, bool)> {
        match left.view() {
            ValueView::Hash(..) => None,
            _ => Self::quanthash_kind(left).or_else(|| Self::quanthash_kind(right)),
        }
    }

    /// The QuantHash kind and mutability of a value, if it is a Set/Bag/Mix.
    // Cost: O(1).
    pub(super) fn quanthash_kind(v: &Value) -> Option<(QuantKind, bool)> {
        match v.view() {
            ValueView::Set(_, m) => Some((QuantKind::Set, m)),
            ValueView::Bag(_, m) => Some((QuantKind::Bag, m)),
            ValueView::Mix(_, m) => Some((QuantKind::Mix, m)),
            _ => None,
        }
    }

    /// A QuantHash operand as the `element => weight` Hash its `.Hash`
    /// coercion gives (Set membership `True`, Bag/Mix weights); any other
    /// operand is returned unchanged (a scalar still broadcasts).
    // Cost: O(e), e = elements of a QuantHash operand; O(1) otherwise.
    pub(super) fn quanthash_to_hash(v: &Value) -> Result<Value, RuntimeError> {
        if Self::quanthash_kind(v).is_some() {
            crate::builtins::map_hash_coerce::to_hash(v.clone(), false)
        } else {
            Ok(v.clone())
        }
    }

    /// Hyper `op` over a pair of operands one of which is a QuantHash; `None`
    /// when neither is, so the caller carries on with its own cases.
    // Cost: O(e_l + e_r) operator applications, e = keys of each operand.
    pub(super) fn hyper_quanthash_pair(
        &mut self,
        op: crate::compiled_operator::InfixRef<'_>,
        left: &Value,
        right: &Value,
        dwim_left: bool,
        dwim_right: bool,
    ) -> Result<Option<Value>, RuntimeError> {
        if Self::quanthash_kind(left).is_none() && Self::quanthash_kind(right).is_none() {
            return Ok(None);
        }
        let result_kind = Self::hyper_quanthash_result(left, right);
        // A key a QuantHash lacks reads as its absent weight -- `False` for a
        // Set, 0 for a Bag/Mix -- the way rakudo's hyper reads it through
        // AT-KEY: `mix(<a b>) »*« mix(<a>)` is `("a"=>1).Mix`, not
        // `("a"=>1,"b"=>1).Mix`. A plain Hash side reads its own AT-KEY
        // default (`Any`), so `bag(<a b>) »*« {a=>2}` is `("a"=>2).Bag`.
        let absent = |v: &Value| match (Self::quanthash_kind(v), v.view()) {
            (Some((QuantKind::Set, _)), _) => Value::FALSE,
            (Some(_), _) => Value::int(0),
            (None, ValueView::Hash(map)) => Self::hash_absent_value(&map),
            (None, _) => Value::package(Symbol::intern("Any")),
        };
        let missing = [absent(left), absent(right)];
        let left = Self::quanthash_to_hash(left)?;
        let right = Self::quanthash_to_hash(right)?;
        let result = match (left.view(), right.view()) {
            (ValueView::Hash(la), ValueView::Hash(ra)) => self.hyper_hash_pair(
                op,
                &la,
                &ra,
                dwim_left,
                dwim_right,
                [&missing[0], &missing[1]],
            )?,
            // A QuantHash against a scalar broadcasts the scalar over its keys.
            _ => self.hyper_op_pair(op, &left, &right, dwim_left, dwim_right)?,
        };
        Ok(Some(match result_kind {
            Some((kind, mutable)) => Self::hash_to_quanthash(result, kind, mutable),
            None => result,
        }))
    }

    /// Rebuild a QuantHash of the given kind/mutability from a hyper result
    /// Hash, applying Rakudo's QuantHash coercion: a Set keeps the truthy
    /// keys, a Bag the strictly-positive integer weights, a Mix the non-zero
    /// weights. A key the Hash records as a typed element (`set(1)`'s `Int`
    /// 1, kept in its `original_keys`) stays that element.
    // Cost: O(k), k = keys of the result Hash.
    pub(super) fn hash_to_quanthash(v: Value, kind: QuantKind, mutable: bool) -> Value {
        let ValueView::Hash(map) = v.view() else {
            return v;
        };
        let mut original_keys = ValueMap::default();
        // Each surviving key as its QuantHash store key (the element's
        // `.WHICH`), recording a non-Str element so it keeps its type.
        let mut store_key = |display: &str| -> String {
            let element = map
                .original_keys
                .as_ref()
                .and_then(|orig| orig.get(display))
                .cloned()
                .unwrap_or_else(|| Value::str(display.to_string()));
            let key = crate::runtime::utils::value_which_key(&element);
            if !matches!(element.view(), ValueView::Str(_)) {
                original_keys.insert(key.clone(), element);
            }
            key
        };
        match kind {
            QuantKind::Set => {
                let elems: std::collections::HashSet<String> = map
                    .iter()
                    .filter(|(_, val)| val.truthy())
                    .map(|(k, _)| store_key(k))
                    .collect();
                match (original_keys.is_empty(), mutable) {
                    (true, false) => Value::set(elems),
                    (true, true) => Value::set_hash(elems),
                    (false, false) => Value::set_typed(elems, original_keys),
                    (false, true) => Value::set_hash_typed(elems, original_keys),
                }
            }
            QuantKind::Bag => {
                let counts: std::collections::HashMap<String, i64> = map
                    .iter()
                    .filter_map(|(k, val)| {
                        let c = crate::runtime::utils::to_int(val);
                        (c > 0).then(|| (store_key(k), c))
                    })
                    .collect();
                match (original_keys.is_empty(), mutable) {
                    (true, false) => Value::bag(counts),
                    (true, true) => Value::bag_hash(counts),
                    (false, false) => Value::bag_typed(counts, original_keys),
                    (false, true) => Value::bag_hash_typed(counts, original_keys),
                }
            }
            QuantKind::Mix => {
                let weights: std::collections::HashMap<String, f64> = map
                    .iter()
                    .filter_map(|(k, val)| {
                        crate::runtime::utils::to_float_value(val).map(|w| (store_key(k), w))
                    })
                    .collect();
                match (original_keys.is_empty(), mutable) {
                    (true, false) => Value::mix(weights),
                    (true, true) => Value::mix_hash(weights),
                    (false, false) => Value::mix_with_original_keys(weights, original_keys),
                    (false, true) => Value::mix_hash_with_original_keys(weights, original_keys),
                }
            }
        }
    }
}
