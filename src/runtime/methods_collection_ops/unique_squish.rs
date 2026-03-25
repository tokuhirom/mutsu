use super::*;

impl Interpreter {
    pub(in crate::runtime) fn dispatch_unique(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut as_func: Option<Value> = None;
        let mut with_func: Option<Value> = None;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if key == "as" && value.truthy() {
                    as_func = Some(value.as_ref().clone());
                    continue;
                }
                if key == "with" && value.truthy() {
                    with_func = Some(value.as_ref().clone());
                    continue;
                }
            }
        }

        let items: Vec<Value> = if let Some(list_items) = target.as_list_items() {
            list_items.to_vec()
        } else {
            match target {
                Value::LazyList(ll) => self.force_lazy_list_bridge(&ll)?,
                v @ (Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }) => Self::value_to_list(&v),
                other => vec![other],
            }
        };
        let mut seen_keys: Vec<Value> = Vec::new();
        let mut unique_items: Vec<Value> = Vec::new();
        for item in items {
            let key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![item.clone()], true)?
            } else {
                item.clone()
            };

            let mut duplicate = false;
            for seen in &seen_keys {
                let is_same = if let Some(func) = with_func.clone() {
                    self.call_sub_value(func, vec![seen.clone(), key.clone()], true)?
                        .truthy()
                } else if let (
                    Value::Instance {
                        class_name: seen_class,
                        id: seen_id,
                        ..
                    },
                    Value::Instance {
                        class_name: key_class,
                        id: key_id,
                        ..
                    },
                ) = (seen, &key)
                {
                    // Some instances still use placeholder id=0; treat those as
                    // distinct for unique's default identity semantics.
                    if *seen_id == 0
                        && *key_id == 0
                        && seen_class == key_class
                        && seen_class.resolve() != "Stash"
                        && seen_class.resolve() != "Supply"
                    {
                        false
                    } else {
                        values_identical(seen, &key)
                    }
                } else {
                    values_identical(seen, &key)
                };
                if is_same {
                    duplicate = true;
                    break;
                }
            }

            if !duplicate {
                seen_keys.push(key);
                unique_items.push(item);
            }
        }

        Ok(Value::array(unique_items))
    }

    pub(in crate::runtime) fn dispatch_repeated(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut as_func: Option<Value> = None;
        let mut with_func: Option<Value> = None;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if key == "as" && value.truthy() {
                    as_func = Some(value.as_ref().clone());
                    continue;
                }
                if key == "with" && value.truthy() {
                    with_func = Some(value.as_ref().clone());
                    continue;
                }
            }
        }

        let items: Vec<Value> = if let Some(list_items) = target.as_list_items() {
            list_items.to_vec()
        } else {
            match target {
                Value::LazyList(ll) => self.force_lazy_list_bridge(&ll)?,
                v @ (Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }) => Self::value_to_list(&v),
                other => vec![other],
            }
        };
        let mut seen_keys: Vec<Value> = Vec::new();
        let mut repeated_items: Vec<Value> = Vec::new();
        for item in items {
            let key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![item.clone()], true)?
            } else {
                item.clone()
            };

            let mut duplicate = false;
            for seen in &seen_keys {
                let is_same = if let Some(func) = with_func.clone() {
                    self.call_sub_value(func, vec![seen.clone(), key.clone()], true)?
                        .truthy()
                } else if let (
                    Value::Instance {
                        class_name: seen_class,
                        id: seen_id,
                        ..
                    },
                    Value::Instance {
                        class_name: key_class,
                        id: key_id,
                        ..
                    },
                ) = (seen, &key)
                {
                    if *seen_id == 0
                        && *key_id == 0
                        && seen_class == key_class
                        && seen_class.resolve() != "Stash"
                        && seen_class.resolve() != "Supply"
                    {
                        false
                    } else {
                        values_identical(seen, &key)
                    }
                } else {
                    values_identical(seen, &key)
                };
                if is_same {
                    duplicate = true;
                    break;
                }
            }

            if duplicate {
                repeated_items.push(item);
            } else {
                seen_keys.push(key);
            }
        }

        Ok(Value::array(repeated_items))
    }

    pub(crate) fn dispatch_squish(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut as_func: Option<Value> = None;
        let mut with_func: Option<Value> = None;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if key == "as" && value.truthy() {
                    as_func = Some(value.as_ref().clone());
                    continue;
                }
                if key == "with" && value.truthy() {
                    with_func = Some(value.as_ref().clone());
                    continue;
                }
            }
        }

        if as_func.is_none()
            && with_func.is_none()
            && matches!(
                target,
                Value::Range(_, _)
                    | Value::RangeExcl(_, _)
                    | Value::RangeExclStart(_, _)
                    | Value::RangeExclBoth(_, _)
                    | Value::GenericRange { .. }
            )
        {
            return Ok(target);
        }

        let items: Vec<Value> = if let Some(list_items) = target.as_list_items() {
            list_items.to_vec()
        } else {
            match target {
                Value::LazyList(ll) => self.force_lazy_list_bridge(&ll)?,
                v @ (Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::GenericRange { .. }) => Self::value_to_list(&v),
                other => vec![other],
            }
        };
        let source_items = items.clone();

        if items.is_empty() {
            return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
        }

        let env_before_callbacks = if as_func.is_some() || with_func.is_some() {
            Some(self.env.clone())
        } else {
            None
        };

        let mut squished_items: Vec<Value> = Vec::new();
        squished_items.push(items[0].clone());

        let mut prev_key = if let Some(func) = as_func.clone() {
            self.call_sub_value(func, vec![items[0].clone()], true)?
        } else {
            items[0].clone()
        };

        for item in items.iter().skip(1) {
            let key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![item.clone()], true)?
            } else {
                item.clone()
            };

            let duplicate = if let Some(func) = with_func.clone() {
                self.call_sub_value(func, vec![prev_key.clone(), key.clone()], true)?
                    .truthy()
            } else {
                values_identical(&prev_key, &key)
            };

            if !duplicate {
                squished_items.push(item.clone());
            }
            prev_key = key;
        }

        let result = Value::Seq(std::sync::Arc::new(squished_items));
        if (as_func.is_some() || with_func.is_some())
            && let Value::Seq(items) = &result
        {
            let mut revert_values = HashMap::new();
            let mut revert_remove = Vec::new();
            if let Some(before) = env_before_callbacks {
                for (k, old_v) in &before {
                    if self.env.get(k) != Some(old_v) {
                        revert_values.insert(k.clone(), old_v.clone());
                    }
                }
                for k in self.env.keys() {
                    if !before.contains_key(k) {
                        revert_remove.push(k.clone());
                    }
                }
            }
            let seq_id = std::sync::Arc::as_ptr(items) as usize;
            self.squish_iterator_meta.insert(
                seq_id,
                super::super::SquishIteratorMeta {
                    source_items,
                    as_func: as_func.clone(),
                    with_func: with_func.clone(),
                    revert_values,
                    revert_remove,
                },
            );
        }
        Ok(result)
    }
}
