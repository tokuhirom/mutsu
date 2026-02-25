use super::*;
use std::collections::HashMap as StdHashMap;

impl Interpreter {
    pub(super) fn builtin_elems(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            let msg = format!(
                "Calling elems({}) will never work with signature of the proto ($, *%)",
                std::iter::repeat_n("Int", args.len())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            let mut attrs = StdHashMap::new();
            attrs.insert("message".to_string(), Value::Str(msg.clone()));
            let ex = Value::make_instance("X::TypeCheck::Argument".to_string(), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        let val = &args[0];
        Ok(match val {
            Value::Array(items, ..) => Value::Int(items.len() as i64),
            Value::LazyList(list) => Value::Int(self.force_lazy_list(list)?.len() as i64),
            Value::Hash(items) => Value::Int(items.len() as i64),
            Value::Str(s) => Value::Int(s.chars().count() as i64),
            _ => Value::Int(1),
        })
    }

    pub(super) fn builtin_set(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut elems = HashSet::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        elems.insert(item.to_string_value());
                    }
                }
                other => {
                    elems.insert(other.to_string_value());
                }
            }
        }
        Ok(Value::set(elems))
    }

    pub(super) fn builtin_bag(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut counts: HashMap<String, i64> = HashMap::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        *counts.entry(item.to_string_value()).or_insert(0) += 1;
                    }
                }
                other => {
                    *counts.entry(other.to_string_value()).or_insert(0) += 1;
                }
            }
        }
        Ok(Value::bag(counts))
    }

    pub(super) fn builtin_mix(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut weights: HashMap<String, f64> = HashMap::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                    }
                }
                other => {
                    *weights.entry(other.to_string_value()).or_insert(0.0) += 1.0;
                }
            }
        }
        Ok(Value::mix(weights))
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
        let kind = match name {
            "any" => JunctionKind::Any,
            "all" => JunctionKind::All,
            "one" => JunctionKind::One,
            _ => JunctionKind::None,
        };
        let mut elems = Vec::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => elems.extend(items.iter().cloned()),
                other => elems.push(other),
            }
        }
        Ok(Value::junction(kind, elems))
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
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Hash(items)) => {
                Value::array(items.keys().map(|k| Value::Str(k.clone())).collect())
            }
            Some(Value::Pair(key, _)) => Value::array(vec![Value::Str(key)]),
            Some(Value::ValuePair(key, _)) => Value::array(vec![*key]),
            Some(Value::Nil) | None => Value::array(Vec::new()),
            _ => Value::array(Vec::new()),
        })
    }

    pub(super) fn builtin_values(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Hash(items)) => Value::array(items.values().cloned().collect()),
            Some(Value::Pair(_, value)) | Some(Value::ValuePair(_, value)) => {
                Value::array(vec![*value])
            }
            Some(Value::Nil) | None => Value::array(Vec::new()),
            _ => Value::array(Vec::new()),
        })
    }

    pub(super) fn builtin_abs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Int(i)) => Value::Int(i.abs()),
            Some(Value::Num(f)) => Value::Num(f.abs()),
            _ => Value::Int(0),
        })
    }

    pub(super) fn builtin_min(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(args
            .iter()
            .cloned()
            .min_by(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => a.to_string_value().cmp(&b.to_string_value()),
            })
            .unwrap_or(Value::Nil))
    }

    pub(super) fn builtin_max(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(args
            .iter()
            .cloned()
            .max_by(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                _ => a.to_string_value().cmp(&b.to_string_value()),
            })
            .unwrap_or(Value::Nil))
    }

    pub(super) fn builtin_shift(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                if items.is_empty() {
                    Value::Nil
                } else {
                    Arc::make_mut(&mut items).remove(0)
                }
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_pop(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        Ok(match args.first().cloned() {
            Some(Value::Array(mut items, ..)) => {
                Arc::make_mut(&mut items).pop().unwrap_or(Value::Nil)
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_join(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let sep = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let list = args.get(1).cloned();
        Ok(match list {
            Some(Value::Array(items, ..)) => {
                let joined = items
                    .iter()
                    .map(|v| v.to_string_value())
                    .collect::<Vec<_>>()
                    .join(&sep);
                Value::Str(joined)
            }
            _ => Value::Str(String::new()),
        })
    }

    pub(super) fn builtin_list(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();
        for arg in args {
            result.extend(Self::value_to_list(arg));
        }
        Ok(Value::array(result))
    }

    pub(super) fn builtin_flat(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();
        for arg in args {
            Self::flat_into(arg, &mut result);
        }
        Ok(Value::array(result))
    }

    pub(crate) fn flat_into(val: &Value, out: &mut Vec<Value>) {
        match val {
            Value::Array(items, ..) | Value::Slip(items) | Value::Seq(items) => {
                for item in items.iter() {
                    Self::flat_into(item, out);
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                out.extend(Self::value_to_list(val));
            }
            other => out.push(other.clone()),
        }
    }

    pub(super) fn builtin_slip(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut items = Vec::new();
        for arg in args {
            match arg {
                Value::Array(elems, ..) => items.extend(elems.iter().cloned()),
                Value::Seq(elems) => items.extend(elems.iter().cloned()),
                Value::Slip(elems) => items.extend(elems.iter().cloned()),
                other => items.push(other.clone()),
            }
        }
        Ok(Value::slip(items))
    }

    pub(super) fn builtin_reverse(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Array(mut items, ..)) => {
                Arc::make_mut(&mut items).reverse();
                Value::Array(items, false)
            }
            Some(Value::Str(s)) => Value::Str(s.chars().rev().collect()),
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_sort(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // sort(comparator, list) or sort(list)
        if args.len() >= 2 {
            let first = &args[0];
            // Check if first arg is a callable (comparator function)
            if matches!(first, Value::Sub(_) | Value::Routine { .. }) {
                let comparator = first.clone();
                let mut items: Vec<Value> = Vec::new();
                for arg in args.iter().skip(1) {
                    match arg {
                        Value::Array(elems, ..) => items.extend(elems.iter().cloned()),
                        Value::Seq(elems) => items.extend(elems.iter().cloned()),
                        other => items.push(other.clone()),
                    }
                }
                // Delegate to dispatch_sort which handles all callable types
                return self.dispatch_sort(Value::array(items), &[comparator]);
            }
        }
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Array(mut items, ..)) => {
                Arc::make_mut(&mut items).sort_by_key(|a| a.to_string_value());
                Value::Array(items, false)
            }
            _ => Value::Nil,
        })
    }

    pub(super) fn builtin_map(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                Value::Array(items, ..) => list_items.extend(items.iter().cloned()),
                Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                Value::RangeExclStart(a, b) => list_items.extend((*a + 1..=*b).map(Value::Int)),
                Value::RangeExclBoth(a, b) => list_items.extend((*a + 1..*b).map(Value::Int)),
                v if v.is_range() => {
                    list_items.extend(crate::runtime::utils::value_to_list(v));
                }
                other => list_items.push(other.clone()),
            }
        }
        self.eval_map_over_items(func, list_items)
    }

    pub(super) fn builtin_grep(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let func = args.first().cloned();
        let mut list_items = Vec::new();
        for arg in args.iter().skip(1) {
            match arg {
                Value::Array(items, ..) => list_items.extend(items.iter().cloned()),
                Value::Range(a, b) => list_items.extend((*a..=*b).map(Value::Int)),
                Value::RangeExcl(a, b) => list_items.extend((*a..*b).map(Value::Int)),
                v if v.is_range() => {
                    list_items.extend(crate::runtime::utils::value_to_list(v));
                }
                other => list_items.push(other.clone()),
            }
        }
        self.eval_grep_over_items(func, list_items)
    }

    pub(super) fn builtin_classify(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let func = match args.first() {
            Some(value) => value.clone(),
            None => return Ok(Value::hash(HashMap::new())),
        };
        let mut buckets: HashMap<String, Vec<Value>> = HashMap::new();
        for item in args.iter().skip(1) {
            let keys = match self.call_lambda_with_arg(&func, item.clone()) {
                Ok(Value::Array(values, ..)) => values.to_vec(),
                Ok(value) => vec![value],
                Err(_) => vec![Value::Nil],
            };
            let target_keys = if name == "classify" {
                if keys.is_empty() {
                    vec![Value::Nil]
                } else {
                    vec![keys[0].clone()]
                }
            } else {
                keys
            };
            for key in target_keys {
                let bucket_key = key.to_string_value();
                buckets.entry(bucket_key).or_default().push(item.clone());
            }
        }
        let hash_map = buckets
            .into_iter()
            .map(|(k, v)| (k, Value::array(v)))
            .collect();
        Ok(Value::hash(hash_map))
    }

    /// `cross(@a, @b, ...)` — Cartesian product of lists.
    /// With `with => &op`, applies the operator to each pair instead of making tuples.
    pub(super) fn builtin_cross(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut lists: Vec<Vec<Value>> = Vec::new();
        let mut with_func: Option<Value> = None;

        for arg in &args {
            match arg {
                Value::Pair(k, v) if k.as_str() == "with" => {
                    with_func = Some(v.as_ref().clone());
                }
                _ => {
                    lists.push(super::utils::value_to_list(arg));
                }
            }
        }

        if lists.is_empty() {
            return Ok(Value::array(vec![]));
        }

        // Compute Cartesian product iteratively
        let mut result: Vec<Vec<Value>> = vec![vec![]];
        for list in &lists {
            let mut new_result = Vec::new();
            for combo in &result {
                for item in list {
                    let mut new_combo = combo.clone();
                    new_combo.push(item.clone());
                    new_result.push(new_combo);
                }
            }
            result = new_result;
        }

        // Apply `with` function or create tuples
        if let Some(func) = with_func {
            let mut final_result = Vec::new();
            for combo in result {
                let val = self.call_sub_value(func.clone(), combo, false)?;
                final_result.push(val);
            }
            Ok(Value::array(final_result))
        } else {
            // Return as list of lists (tuples)
            let tuples: Vec<Value> = result.into_iter().map(Value::array).collect();
            Ok(Value::array(tuples))
        }
    }

    pub(super) fn builtin_roundrobin(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        if args.len() == 1 {
            return Ok(args[0].clone());
        }

        let streams: Vec<Vec<Value>> = args
            .iter()
            .map(|arg| match arg {
                Value::Capture { positional, named }
                    if named.is_empty() && positional.len() == 1 =>
                {
                    vec![arg.clone()]
                }
                Value::Array(items, _) => items.iter().cloned().collect(),
                Value::Seq(items) | Value::Slip(items) => items.iter().cloned().collect(),
                Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),
                Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
                v if v.is_range() => crate::runtime::utils::value_to_list(v),
                other => vec![other.clone()],
            })
            .collect();

        let mut indices = vec![0usize; streams.len()];
        let mut rounds = Vec::new();
        loop {
            let mut tuple = Vec::new();
            let mut progressed = false;
            for (i, stream) in streams.iter().enumerate() {
                if indices[i] < stream.len() {
                    tuple.push(stream[indices[i]].clone());
                    indices[i] += 1;
                    progressed = true;
                }
            }
            if !progressed {
                break;
            }
            rounds.push(Value::array(tuple));
        }

        Ok(Value::array(rounds))
    }
}
