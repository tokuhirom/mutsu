use super::*;

impl VM {
    pub(super) fn exec_set_elem_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let key = left.to_string_value();
        let result = match &right {
            Value::Set(s) => s.contains(&key),
            Value::Bag(b) => b.contains_key(&key),
            Value::Mix(m) => m.contains_key(&key),
            _ => false,
        };
        self.stack.push(Value::Bool(result));
    }

    pub(super) fn exec_set_cont_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let key = right.to_string_value();
        let result = match &left {
            Value::Set(s) => s.contains(&key),
            Value::Bag(b) => b.contains_key(&key),
            Value::Mix(m) => m.contains_key(&key),
            _ => false,
        };
        self.stack.push(Value::Bool(result));
    }

    pub(super) fn exec_set_union_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(mut a), Value::Set(b)) => {
                for elem in b {
                    a.insert(elem);
                }
                Value::Set(a)
            }
            (Value::Bag(mut a), Value::Bag(b)) => {
                for (k, v) in b {
                    let e = a.entry(k).or_insert(0);
                    *e = (*e).max(v);
                }
                Value::Bag(a)
            }
            (Value::Mix(mut a), Value::Mix(b)) => {
                for (k, v) in b {
                    let e = a.entry(k).or_insert(0.0);
                    *e = e.max(v);
                }
                Value::Mix(a)
            }
            (Value::Set(a), Value::Bag(mut b)) => {
                for elem in a {
                    b.entry(elem).or_insert(1);
                }
                Value::Bag(b)
            }
            (Value::Bag(mut a), Value::Set(b)) => {
                for elem in b {
                    a.entry(elem).or_insert(1);
                }
                Value::Bag(a)
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                let mut result = a;
                for elem in b {
                    result.insert(elem);
                }
                Value::Set(result)
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_intersect_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => Value::Set(a.intersection(&b).cloned().collect()),
            (Value::Bag(a), Value::Bag(b)) => {
                let mut result = HashMap::new();
                for (k, v) in &a {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), (*v).min(*bv));
                    }
                }
                Value::Bag(result)
            }
            (Value::Mix(a), Value::Mix(b)) => {
                let mut result = HashMap::new();
                for (k, v) in &a {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), v.min(*bv));
                    }
                }
                Value::Mix(result)
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::Set(a.intersection(&b).cloned().collect())
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => Value::Set(a.difference(&b).cloned().collect()),
            (Value::Bag(a), Value::Bag(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a {
                    let bv = b.get(&k).copied().unwrap_or(0);
                    if v > bv {
                        result.insert(k, v - bv);
                    }
                }
                Value::Bag(result)
            }
            (Value::Mix(a), Value::Mix(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a {
                    let bv = b.get(&k).copied().unwrap_or(0.0);
                    if v > bv {
                        result.insert(k, v - bv);
                    }
                }
                Value::Mix(result)
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::Set(a.difference(&b).cloned().collect())
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_sym_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => {
                Value::Set(a.symmetric_difference(&b).cloned().collect())
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::Set(a.symmetric_difference(&b).cloned().collect())
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack.push(Value::Bool(a.is_subset(&b)));
    }

    pub(super) fn exec_set_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack.push(Value::Bool(a.is_superset(&b)));
    }

    pub(super) fn exec_set_strict_subset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack
            .push(Value::Bool(a.is_subset(&b) && a.len() < b.len()));
    }

    pub(super) fn exec_set_strict_superset_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let a = runtime::coerce_to_set(&left);
        let b = runtime::coerce_to_set(&right);
        self.stack
            .push(Value::Bool(a.is_superset(&b) && a.len() > b.len()));
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
}
