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
            if let ValueView::Pair(key, value) = arg.view() {
                if key == "as" && value.truthy() {
                    as_func = Some(value.clone());
                    continue;
                }
                if key == "with" && value.truthy() {
                    with_func = Some(value.clone());
                    continue;
                }
            }
        }

        let items: Vec<Value> = if let Some(list_items) = target.as_list_items() {
            list_items.to_vec()
        } else if let ValueView::LazyList(ll) = target.view() {
            self.force_lazy_list_bridge(&ll)?
        } else if matches!(
            target.view(),
            ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
        ) {
            Self::value_to_list(&target)
        } else {
            vec![target]
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
                    ValueView::Instance {
                        class_name: seen_class,
                        id: seen_id,
                        ..
                    },
                    ValueView::Instance {
                        class_name: key_class,
                        id: key_id,
                        ..
                    },
                ) = (seen.view(), key.view())
                {
                    // Some instances still use placeholder id=0; treat those as
                    // distinct for unique's default identity semantics.
                    if seen_id == 0
                        && key_id == 0
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

        Ok(Value::seq(unique_items))
    }

    pub(in crate::runtime) fn dispatch_repeated(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut as_func: Option<Value> = None;
        let mut with_func: Option<Value> = None;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                if key == "as" && value.truthy() {
                    as_func = Some(value.clone());
                    continue;
                }
                if key == "with" && value.truthy() {
                    with_func = Some(value.clone());
                    continue;
                }
            }
        }

        let items: Vec<Value> = if let Some(list_items) = target.as_list_items() {
            list_items.to_vec()
        } else if let ValueView::LazyList(ll) = target.view() {
            self.force_lazy_list_bridge(&ll)?
        } else if matches!(
            target.view(),
            ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
        ) {
            Self::value_to_list(&target)
        } else {
            vec![target]
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
                    ValueView::Instance {
                        class_name: seen_class,
                        id: seen_id,
                        ..
                    },
                    ValueView::Instance {
                        class_name: key_class,
                        id: key_id,
                        ..
                    },
                ) = (seen.view(), key.view())
                {
                    if seen_id == 0
                        && key_id == 0
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

        Ok(Value::seq(repeated_items))
    }

    pub(crate) fn dispatch_squish(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut as_func: Option<Value> = None;
        let mut with_func: Option<Value> = None;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                if key == "as" && value.truthy() {
                    as_func = Some(value.clone());
                    continue;
                }
                if key == "with" && value.truthy() {
                    with_func = Some(value.clone());
                    continue;
                }
            }
        }

        if as_func.is_none()
            && with_func.is_none()
            && matches!(
                target.view(),
                ValueView::Range(_, _)
                    | ValueView::RangeExcl(_, _)
                    | ValueView::RangeExclStart(_, _)
                    | ValueView::RangeExclBoth(_, _)
                    | ValueView::GenericRange { .. }
            )
        {
            return Ok(target);
        }

        let items: Vec<Value> = if let Some(list_items) = target.as_list_items() {
            list_items.to_vec()
        } else if let ValueView::LazyList(ll) = target.view() {
            self.force_lazy_list_bridge(&ll)?
        } else if matches!(
            target.view(),
            ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
        ) {
            Self::value_to_list(&target)
        } else {
            vec![target]
        };
        let source_items = items.clone();

        if items.is_empty() {
            return Ok(Value::seq(Vec::new()));
        }

        // Snapshot the env BY VALUE (container contents detached): the eager
        // pass below runs the user callbacks whose side effects (`@with.push`)
        // now mutate containers IN PLACE through the shared backing node
        // (container identity §3) — a plain `env.clone()` would share those
        // nodes, making the changed-value diff below blind and the revert a
        // no-op, so the lazy iterator's re-run would double the side effects.
        let env_before_callbacks = if as_func.is_some() || with_func.is_some() {
            let mut snapshot = self.env.clone();
            let detach: Vec<(crate::symbol::Symbol, Value)> = snapshot
                .iter()
                .filter(|(_, v)| matches!(v.view(), ValueView::Array(..) | ValueView::Hash(..)))
                .map(|(k, v)| (*k, v.clone().detach_shared_container()))
                .collect();
            for (k, v) in detach {
                snapshot.insert_sym(k, v);
            }
            Some(snapshot)
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

        let result = Value::seq(squished_items);
        if (as_func.is_some() || with_func.is_some())
            && let ValueView::Seq(items) = result.view()
        {
            let mut revert_values = HashMap::new();
            let mut revert_remove = Vec::new();
            if let Some(before) = env_before_callbacks {
                for (k, old_v) in &before {
                    if self.env.get_sym(*k) != Some(old_v) {
                        revert_values.insert(k.resolve(), old_v.clone());
                    }
                }
                for k in self.env.keys() {
                    if !before.contains_key_sym(*k) {
                        revert_remove.push(k.resolve());
                    }
                }
            }
            let seq_id = std::sync::Arc::as_ptr(&items) as usize;
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
