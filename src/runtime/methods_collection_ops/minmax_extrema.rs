use super::*;

impl Interpreter {
    pub(in crate::runtime) fn dispatch_minmaxpairs(
        &mut self,
        target: Value,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        if matches!(target.view(), ValueView::Instance { .. })
            && let Ok(pairs) = self.call_method_with_values(target.clone(), "pairs", Vec::new())
        {
            return Ok(pairs);
        }
        let want_max = method == "maxpairs";
        let to_pairs = |items: &[Value]| -> Value {
            let mut best: Option<Value> = None;
            let mut out: Vec<Value> = Vec::new();
            for (idx, item) in items.iter().enumerate() {
                if matches!(item.view(), ValueView::Nil)
                    || matches!(item.view(), ValueView::Package(n) if n == "Any")
                {
                    continue;
                }
                let ord = if let Some(current) = &best {
                    // Use `cmp` semantics: numeric comparison for numeric
                    // pairs, string comparison otherwise
                    match (item.view(), current.view()) {
                        (ValueView::Int(a), ValueView::Int(b)) => a.cmp(&b),
                        (ValueView::Num(a), ValueView::Num(b)) => {
                            a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal)
                        }
                        (ValueView::Int(a), ValueView::Num(b)) => (a as f64)
                            .partial_cmp(&b)
                            .unwrap_or(std::cmp::Ordering::Equal),
                        (ValueView::Num(a), ValueView::Int(b)) => a
                            .partial_cmp(&(b as f64))
                            .unwrap_or(std::cmp::Ordering::Equal),
                        (ValueView::Rat(..), _) | (_, ValueView::Rat(..)) => {
                            if let (Some((an, ad)), Some((bn, bd))) = (
                                crate::runtime::to_rat_parts(item),
                                crate::runtime::to_rat_parts(current),
                            ) {
                                crate::runtime::compare_rat_parts((an, ad), (bn, bd))
                            } else {
                                item.to_string_value().cmp(&current.to_string_value())
                            }
                        }
                        _ => item.to_string_value().cmp(&current.to_string_value()),
                    }
                } else {
                    std::cmp::Ordering::Equal
                };
                let replace = best.is_none()
                    || (want_max && ord == std::cmp::Ordering::Greater)
                    || (!want_max && ord == std::cmp::Ordering::Less);
                if replace {
                    best = Some(item.clone());
                    out.clear();
                    out.push(Value::value_pair(Value::int(idx as i64), item.clone()));
                } else if ord == std::cmp::Ordering::Equal {
                    out.push(Value::value_pair(Value::int(idx as i64), item.clone()));
                }
            }
            Value::seq(out)
        };
        Ok(match target.view() {
            ValueView::Array(items, ..) => to_pairs(&items),
            ValueView::Set(set, ..) => {
                if set.elements.is_empty() {
                    Value::seq(Vec::new())
                } else {
                    // All Set weights are True, so min == max == all elements
                    let out: Vec<Value> = set
                        .elements
                        .iter()
                        .map(|k| Value::pair(k.clone(), Value::TRUE))
                        .collect();
                    Value::seq(out)
                }
            }
            ValueView::Bag(bag, ..) => {
                if bag.is_empty() {
                    Value::seq(Vec::new())
                } else {
                    let mut best_count: Option<num_bigint::BigInt> = None;
                    let mut out: Vec<Value> = Vec::new();
                    for (key, count) in bag.iter() {
                        let ord = if let Some(ref current) = best_count {
                            count.cmp(current)
                        } else {
                            std::cmp::Ordering::Equal
                        };
                        let replace = best_count.is_none()
                            || (want_max && ord == std::cmp::Ordering::Greater)
                            || (!want_max && ord == std::cmp::Ordering::Less);
                        if replace {
                            best_count = Some(count.clone());
                            out.clear();
                            out.push(Value::pair(key.clone(), Value::from_bigint(count.clone())));
                        } else if ord == std::cmp::Ordering::Equal {
                            out.push(Value::pair(key.clone(), Value::from_bigint(count.clone())));
                        }
                    }
                    Value::seq(out)
                }
            }
            ValueView::Mix(mix, ..) => {
                if mix.is_empty() {
                    Value::seq(Vec::new())
                } else {
                    let mut best_weight: Option<f64> = None;
                    let mut out: Vec<Value> = Vec::new();
                    for (key, weight) in mix.iter() {
                        let ord = if let Some(current) = best_weight {
                            weight
                                .partial_cmp(&current)
                                .unwrap_or(std::cmp::Ordering::Equal)
                        } else {
                            std::cmp::Ordering::Equal
                        };
                        let replace = best_weight.is_none()
                            || (want_max && ord == std::cmp::Ordering::Greater)
                            || (!want_max && ord == std::cmp::Ordering::Less);
                        if replace {
                            best_weight = Some(*weight);
                            out.clear();
                            out.push(Value::pair(
                                key.clone(),
                                crate::value::mix_weight_to_value(*weight),
                            ));
                        } else if ord == std::cmp::Ordering::Equal {
                            out.push(Value::pair(
                                key.clone(),
                                crate::value::mix_weight_to_value(*weight),
                            ));
                        }
                    }
                    Value::seq(out)
                }
            }
            ValueView::Hash(hash) => {
                if hash.is_empty() {
                    Value::seq(Vec::new())
                } else {
                    // For Hash, compare values
                    let mut best: Option<&Value> = None;
                    let mut out: Vec<Value> = Vec::new();
                    for (key, value) in hash.iter() {
                        let ord = if let Some(current) = best {
                            match (value.view(), current.view()) {
                                (ValueView::Int(a), ValueView::Int(b)) => a.cmp(&b),
                                _ => value.to_string_value().cmp(&current.to_string_value()),
                            }
                        } else {
                            std::cmp::Ordering::Equal
                        };
                        let replace = best.is_none()
                            || (want_max && ord == std::cmp::Ordering::Greater)
                            || (!want_max && ord == std::cmp::Ordering::Less);
                        if replace {
                            best = Some(value);
                            out.clear();
                            out.push(hash.typed_pair(key, value.clone()));
                        } else if ord == std::cmp::Ordering::Equal {
                            out.push(hash.typed_pair(key, value.clone()));
                        }
                    }
                    Value::seq(out)
                }
            }
            _ => Value::seq(vec![Value::value_pair(Value::int(0), target.clone())]),
        })
    }

    pub(in crate::runtime) fn dispatch_supply_running_extrema(
        &mut self,
        target: Value,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let ValueView::Instance { attributes, .. } = target.view() else {
            return Err(RuntimeError::new("Expected Supply instance"));
        };
        let values = if let Some(ValueView::Array(items, ..)) =
            attributes.as_map().get("values").map(Value::view)
        {
            items.to_vec()
        } else {
            Vec::new()
        };
        let want_max = method == "max";

        let build_supply = |vals: Vec<Value>| {
            let mut attrs = HashMap::new();
            attrs.insert("values".to_string(), Value::array(vals));
            attrs.insert("taps".to_string(), Value::array(Vec::new()));
            attrs.insert("live".to_string(), Value::FALSE);
            Value::make_instance(Symbol::intern("Supply"), attrs)
        };

        let compare_or_key_fn = args.first().cloned();
        if let Some(ref fn_val) = compare_or_key_fn
            && !matches!(
                fn_val.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            )
        {
            return Err(RuntimeError::new("must be code if specified"));
        }
        if values.is_empty() {
            return Ok(build_supply(Vec::new()));
        }

        let Some(compare_or_key_fn) = compare_or_key_fn else {
            let mut emitted = Vec::new();
            let mut best = values[0].clone();
            emitted.push(best.clone());
            for item in values.iter().skip(1) {
                let cmp = compare_values(item, &best);
                let is_better = if want_max { cmp > 0 } else { cmp < 0 };
                if is_better {
                    best = item.clone();
                    emitted.push(item.clone());
                }
            }
            return Ok(build_supply(emitted));
        };

        let is_binary_comparator =
            matches!(compare_or_key_fn.view(), ValueView::Sub(data) if data.params.len() >= 2);
        let mut emitted = Vec::new();
        let mut best = values[0].clone();
        emitted.push(best.clone());

        if is_binary_comparator {
            for item in values.into_iter().skip(1) {
                let cmp_val = self.call_sub_value(
                    compare_or_key_fn.clone(),
                    vec![item.clone(), best.clone()],
                    true,
                )?;
                let cmp = match cmp_val.view() {
                    ValueView::Enum {
                        enum_type, value, ..
                    } if enum_type == "Order" => value.as_i64(),
                    _ => {
                        let n = cmp_val.to_f64();
                        if n > 0.0 {
                            1
                        } else if n < 0.0 {
                            -1
                        } else {
                            0
                        }
                    }
                };
                let is_better = if want_max { cmp > 0 } else { cmp < 0 };
                if is_better {
                    best = item.clone();
                    emitted.push(item);
                }
            }
        } else {
            let mut best_key =
                self.call_sub_value(compare_or_key_fn.clone(), vec![best.clone()], true)?;
            for item in values.into_iter().skip(1) {
                let key =
                    self.call_sub_value(compare_or_key_fn.clone(), vec![item.clone()], true)?;
                let cmp = compare_values(&key, &best_key);
                let is_better = if want_max { cmp > 0 } else { cmp < 0 };
                if is_better {
                    best_key = key;
                    emitted.push(item);
                }
            }
        }

        Ok(build_supply(emitted))
    }
}
