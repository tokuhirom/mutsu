use super::*;

impl Interpreter {
    /// Cost: O(e), e = elements of the invocant (one comparison against the
    /// running best each; ties are appended, a new best clears them).
    pub(crate) fn dispatch_minmaxpairs(
        &mut self,
        target: Value,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        // `Any.minpairs` is `self.list.minpairs`: a `Match` reads its positional
        // captures, compared as the `Match`es they are (#13195).
        if target.is_match_instance() {
            let items = self.call_method_with_values(target, "list", Vec::new())?;
            return self.dispatch_minmaxpairs(items, method);
        }
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
            Value::array(out)
        };
        Ok(match target.view() {
            ValueView::Array(items, ..) => to_pairs(&items),
            ValueView::Set(set, ..) => {
                if set.elements.is_empty() {
                    Value::array(Vec::new())
                } else {
                    // All Set weights are True, so min == max == all elements
                    let out: Vec<Value> = set
                        .elements
                        .iter()
                        .map(|k| {
                            crate::runtime::utils::quanthash_typed_pair(
                                set.typed_key(k),
                                Value::TRUE,
                            )
                        })
                        .collect();
                    Value::array(out)
                }
            }
            ValueView::Bag(bag, ..) => {
                if bag.is_empty() {
                    Value::array(Vec::new())
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
                            out.push(crate::runtime::utils::quanthash_typed_pair(
                                bag.typed_key(key),
                                Value::from_bigint(count.clone()),
                            ));
                        } else if ord == std::cmp::Ordering::Equal {
                            out.push(crate::runtime::utils::quanthash_typed_pair(
                                bag.typed_key(key),
                                Value::from_bigint(count.clone()),
                            ));
                        }
                    }
                    Value::array(out)
                }
            }
            ValueView::Mix(mix, ..) => {
                if mix.is_empty() {
                    Value::array(Vec::new())
                } else {
                    let mut best_weight: Option<crate::value::MixWeight> = None;
                    let mut out: Vec<Value> = Vec::new();
                    for (key, weight) in mix.iter() {
                        let ord = if let Some(current) = &best_weight {
                            weight
                                .partial_cmp(current)
                                .unwrap_or(std::cmp::Ordering::Equal)
                        } else {
                            std::cmp::Ordering::Equal
                        };
                        let replace = best_weight.is_none()
                            || (want_max && ord == std::cmp::Ordering::Greater)
                            || (!want_max && ord == std::cmp::Ordering::Less);
                        if replace {
                            best_weight = Some(weight.clone());
                            out.clear();
                            out.push(crate::runtime::utils::quanthash_typed_pair(
                                mix.typed_key(key),
                                crate::value::mix_weight_to_value(weight.clone()),
                            ));
                        } else if ord == std::cmp::Ordering::Equal {
                            out.push(crate::runtime::utils::quanthash_typed_pair(
                                mix.typed_key(key),
                                crate::value::mix_weight_to_value(weight.clone()),
                            ));
                        }
                    }
                    Value::array(out)
                }
            }
            ValueView::Hash(hash) => {
                if hash.is_empty() {
                    Value::array(Vec::new())
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
                    Value::array(out)
                }
            }
            _ => Value::array(vec![Value::value_pair(Value::int(0), target.clone())]),
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

        // The running extremes are the per-tap state machine's, folded over
        // the values (#18579).
        let emitted = self.step_fold(method, args.to_vec(), values)?;
        Ok(build_supply(emitted))
    }
}
