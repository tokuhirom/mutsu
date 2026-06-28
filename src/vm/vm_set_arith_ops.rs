//! Set arithmetic/comparison ops (intersect/multiply/diff/subset/...),
//! Bag/Mix item coercion, and junction ops — split from `vm_set_ops` (§7-8).
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Determine the set type level, including Package type objects.
    /// 0 = Set/SetHash, 1 = Bag/BagHash, 2 = Mix/MixHash
    fn set_type_level_full(val: &Value) -> u8 {
        match val {
            Value::Mix(_, _) => 2,
            Value::Bag(_, _) => 1,
            Value::Package(sym) => {
                let name = sym.resolve();
                match name.as_str() {
                    "Mix" | "MixHash" => 2,
                    "Bag" | "BagHash" => 1,
                    _ => 0,
                }
            }
            _ => 0,
        }
    }

    /// Baggy addition: adds weights of bags/mixes (as opposed to union which takes max).
    pub(super) fn exec_set_addition_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }
        if Self::is_lazy_value(&left) || Self::is_lazy_value(&right) {
            return Err(Self::lazy_list_error());
        }

        // For (+), result is at least Bag (level 1), promoted to Mix if either side is Mix-ish
        let left_level = Self::set_type_level_full(&left);
        let right_level = Self::set_type_level_full(&right);
        let result_level = left_level.max(right_level).max(1); // minimum Bag
        let result_mutable = runtime::set_result_mutability(&left);

        let result = match result_level {
            2 => {
                // Result is Mix
                let mut a = Self::coerce_to_mix(&left);
                let b = Self::coerce_to_mix(&right);
                for (k, v) in b {
                    let e = a.entry(k).or_insert(0.0);
                    *e += v;
                }
                Value::mix(a)
            }
            _ => {
                // Result is Bag (level 1 is the minimum for (+))
                let mut a = Self::coerce_to_bag(&left);
                let b = Self::coerce_to_bag(&right);
                for (k, v) in b {
                    let e = a.entry(k).or_insert(0);
                    *e += v;
                }
                Value::bag(a)
            }
        };
        self.stack
            .push(runtime::with_set_mutability(result, result_mutable));
        Ok(())
    }

    fn is_lazy_value(value: &Value) -> bool {
        match value {
            Value::LazyList(_) => true,
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            Value::GenericRange { end, .. } => match end.as_ref() {
                Value::HyperWhatever => true,
                Value::Num(n) => n.is_infinite() && n.is_sign_positive(),
                _ => {
                    let n = end.to_f64();
                    n.is_infinite() && n.is_sign_positive()
                }
            },
            _ => false,
        }
    }

    /// Helper: extract bag-like weights from a list item (Pair or plain value)
    fn bag_insert_item(result: &mut HashMap<String, i64>, item: &Value) {
        match item {
            Value::Pair(k, v) => {
                let weight = v.to_f64() as i64;
                *result.entry(k.clone()).or_insert(0) += weight;
            }
            Value::ValuePair(k, v) => {
                let weight = v.to_f64() as i64;
                *result.entry(k.to_string_value()).or_insert(0) += weight;
            }
            other => {
                let key = other.to_string_value();
                if !key.is_empty() {
                    *result.entry(key).or_insert(0) += 1;
                }
            }
        }
    }

    /// Helper: extract mix-like weights from a list item (Pair or plain value)
    fn mix_insert_item(result: &mut HashMap<String, f64>, item: &Value) {
        match item {
            Value::Pair(k, v) => {
                let weight = v.to_f64();
                *result.entry(k.clone()).or_insert(0.0) += weight;
            }
            Value::ValuePair(k, v) => {
                let weight = v.to_f64();
                *result.entry(k.to_string_value()).or_insert(0.0) += weight;
            }
            other => {
                let key = other.to_string_value();
                if !key.is_empty() {
                    *result.entry(key).or_insert(0.0) += 1.0;
                }
            }
        }
    }

    /// Coerce a value to a Bag (HashMap<String, i64>)
    fn coerce_to_bag(val: &Value) -> HashMap<String, i64> {
        match val {
            Value::Bag(b, _) => crate::runtime::utils::bag_counts_as_i64(&b.counts),
            Value::Set(s, _) => s.iter().map(|k| (k.clone(), 1)).collect(),
            Value::Mix(m, _) => m.iter().map(|(k, v)| (k.clone(), *v as i64)).collect(),
            Value::Hash(map) => {
                let mut result = HashMap::new();
                for (k, v) in map.iter() {
                    let weight = v.to_f64() as i64;
                    if weight != 0 {
                        result.insert(k.clone(), weight);
                    }
                }
                result
            }
            _ if val.as_list_items().is_some() => {
                let mut result = HashMap::new();
                for item in val.as_list_items().unwrap().iter() {
                    Self::bag_insert_item(&mut result, item);
                }
                // Remove entries with 0 weight
                result.retain(|_, v| *v != 0);
                result
            }
            Value::Pair(k, v) => {
                let mut result = HashMap::new();
                let weight = v.to_f64() as i64;
                if weight != 0 {
                    result.insert(k.clone(), weight);
                }
                result
            }
            _ => {
                let set = runtime::coerce_to_set(val);
                set.into_iter().map(|k| (k, 1)).collect()
            }
        }
    }

    /// Coerce a value to a Mix (HashMap<String, f64>)
    fn coerce_to_mix(val: &Value) -> HashMap<String, f64> {
        match val {
            Value::Mix(m, _) => m.weights.clone(),
            Value::Bag(b, _) => b
                .iter()
                .map(|(k, v)| (k.clone(), crate::runtime::utils::bigint_to_f64_sat(v)))
                .collect(),
            Value::Set(s, _) => s.iter().map(|k| (k.clone(), 1.0)).collect(),
            Value::Hash(map) => {
                let mut result = HashMap::new();
                for (k, v) in map.iter() {
                    let weight = v.to_f64();
                    if weight != 0.0 {
                        result.insert(k.clone(), weight);
                    }
                }
                result
            }
            _ if val.as_list_items().is_some() => {
                let mut result = HashMap::new();
                for item in val.as_list_items().unwrap().iter() {
                    Self::mix_insert_item(&mut result, item);
                }
                // Remove entries with 0 weight
                result.retain(|_, v| *v != 0.0);
                result
            }
            Value::Pair(k, v) => {
                let mut result = HashMap::new();
                let weight = v.to_f64();
                if weight != 0.0 {
                    result.insert(k.clone(), weight);
                }
                result
            }
            _ => {
                let set = runtime::coerce_to_set(val);
                set.into_iter().map(|k| (k, 1.0)).collect()
            }
        }
    }

    /// Determine the result type level for set operations:
    /// 0 = Set, 1 = Bag, 2 = Mix
    fn set_type_level(val: &Value) -> u8 {
        match val {
            Value::Mix(_, _) => 2,
            Value::Bag(_, _) => 1,
            _ => 0,
        }
    }

    pub(super) fn exec_set_intersect_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        // Check for Failure values
        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }

        // Check for lazy lists
        if Self::is_lazy_value(&left) || Self::is_lazy_value(&right) {
            let mut attrs = HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str_from("Cannot coerce a lazy list onto a Set"),
            );
            let ex = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), attrs);
            let mut err = RuntimeError::new("Cannot coerce a lazy list onto a Set");
            err.exception = Some(Box::new(ex));
            return Err(err);
        }

        // Determine result type based on the "highest" type
        let left_level = Self::set_type_level(&left);
        let right_level = Self::set_type_level(&right);
        let result_level = left_level.max(right_level);
        let result_mutable = runtime::set_result_mutability(&left);

        let result = match result_level {
            2 => {
                // Result is Mix
                let a = Self::coerce_to_mix(&left);
                let b = Self::coerce_to_mix(&right);
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), v.min(*bv));
                    }
                }
                Value::mix(result)
            }
            1 => {
                // Result is Bag
                let a = Self::coerce_to_bag(&left);
                let b = Self::coerce_to_bag(&right);
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), (*v).min(*bv));
                    }
                }
                Value::bag(result)
            }
            _ => {
                // Result is Set
                let a = runtime::coerce_to_set(&left);
                let b = runtime::coerce_to_set(&right);
                Value::set(a.intersection(&b).cloned().collect())
            }
        };
        self.stack
            .push(runtime::with_set_mutability(result, result_mutable));
        Ok(())
    }

    pub(super) fn exec_set_multiply_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();

        if Self::is_failure_value(&left) || Self::is_failure_value(&right) {
            return Err(RuntimeError::new("Exception"));
        }
        if Self::is_lazy_value(&left) || Self::is_lazy_value(&right) {
            let mut attrs = HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str_from("Cannot coerce a lazy list onto a Set"),
            );
            let ex = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), attrs);
            let mut err = RuntimeError::new("Cannot coerce a lazy list onto a Set");
            err.exception = Some(Box::new(ex));
            return Err(err);
        }

        let result_mutable = runtime::set_result_mutability(&left);
        let result = runtime::Interpreter::apply_reduction_op("(.)", &left, &right)?;
        self.stack
            .push(runtime::with_set_mutability(result, result_mutable));
        Ok(())
    }

    pub(super) fn exec_set_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result_mutable = runtime::set_result_mutability(&left);
        let result = runtime::set_diff_values(&left, &right);
        self.stack
            .push(runtime::with_set_mutability(result, result_mutable));
    }

    pub(super) fn exec_set_sym_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result_mutable = runtime::set_sym_diff_mutability(&left, &right);
        let result = runtime::set_sym_diff_values(&left, &right);
        self.stack
            .push(runtime::with_set_mutability(result, result_mutable));
    }

    pub(super) fn exec_set_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::Bool(Self::quant_hash_subset(&left, &right)));
    }

    pub(super) fn exec_set_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::Bool(Self::quant_hash_subset(&right, &left)));
    }

    pub(super) fn exec_set_strict_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::Bool(Self::quant_hash_strict_subset(&left, &right)));
    }

    pub(super) fn exec_set_strict_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(Value::Bool(Self::quant_hash_strict_subset(&right, &left)));
    }

    pub(super) fn exec_junction_any_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::Any, left, right));
    }

    pub(super) fn exec_junction_all_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::All, left, right));
    }

    pub(super) fn exec_junction_one_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        self.stack
            .push(runtime::merge_junction(JunctionKind::One, left, right));
    }

    /// Execute a multi-operand junction opcode. Pops `count` values from
    /// the stack. If a user-defined infix operator exists in scope, calls
    /// it once with all operands (list-associative). Otherwise builds the
    /// junction from all values.
    pub(super) fn exec_junction_n_op(
        &mut self,
        count: u32,
        kind: JunctionKind,
        infix_name: &str,
    ) -> Result<(), RuntimeError> {
        let n = count as usize;
        let mut values: Vec<Value> = Vec::with_capacity(n);
        for _ in 0..n {
            values.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        values.reverse();

        // Check for user-defined override
        if let Some(def) = loan_env!(self, resolve_function_with_types(infix_name, &values)) {
            let empty_fns = HashMap::new();
            let result = self.compile_and_call_function_def(&def, values.clone(), &empty_fns)?;
            self.stack.push(result);
            return Ok(());
        }

        // No user override: build junction from all values
        let result = Value::Junction {
            kind,
            values: values.into(),
        };
        self.stack.push(result);
        Ok(())
    }
}
