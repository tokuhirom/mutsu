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
            (Value::Set(a), Value::Set(b)) => {
                let mut result = (*a).clone();
                for elem in b.iter() {
                    result.insert(elem.clone());
                }
                Value::set(result)
            }
            (Value::Bag(a), Value::Bag(b)) => {
                let mut result = (*a).clone();
                for (k, v) in b.iter() {
                    let e = result.entry(k.clone()).or_insert(0);
                    *e = (*e).max(*v);
                }
                Value::bag(result)
            }
            (Value::Mix(a), Value::Mix(b)) => {
                let mut result = (*a).clone();
                for (k, v) in b.iter() {
                    let e = result.entry(k.clone()).or_insert(0.0);
                    *e = e.max(*v);
                }
                Value::mix(result)
            }
            (Value::Set(a), Value::Bag(b)) => {
                let mut result = (*b).clone();
                for elem in a.iter() {
                    result.entry(elem.clone()).or_insert(1);
                }
                Value::bag(result)
            }
            (Value::Bag(a), Value::Set(b)) => {
                let mut result = (*a).clone();
                for elem in b.iter() {
                    result.entry(elem.clone()).or_insert(1);
                }
                Value::bag(result)
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                let mut result = a;
                for elem in b {
                    result.insert(elem);
                }
                Value::set(result)
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_intersect_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => Value::set(a.intersection(&b).cloned().collect()),
            (Value::Bag(a), Value::Bag(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), (*v).min(*bv));
                    }
                }
                Value::bag(result)
            }
            (Value::Mix(a), Value::Mix(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    if let Some(bv) = b.get(k) {
                        result.insert(k.clone(), v.min(*bv));
                    }
                }
                Value::mix(result)
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::set(a.intersection(&b).cloned().collect())
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => Value::set(a.difference(&b).cloned().collect()),
            (Value::Bag(a), Value::Bag(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    let bv = b.get(k).copied().unwrap_or(0);
                    if *v > bv {
                        result.insert(k.clone(), *v - bv);
                    }
                }
                Value::bag(result)
            }
            (Value::Mix(a), Value::Mix(b)) => {
                let mut result = HashMap::new();
                for (k, v) in a.iter() {
                    let bv = b.get(k).copied().unwrap_or(0.0);
                    if *v > bv {
                        result.insert(k.clone(), *v - bv);
                    }
                }
                Value::mix(result)
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::set(a.difference(&b).cloned().collect())
            }
        };
        self.stack.push(result);
    }

    pub(super) fn exec_set_sym_diff_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = match (left, right) {
            (Value::Set(a), Value::Set(b)) => {
                Value::set(a.symmetric_difference(&b).cloned().collect())
            }
            (l, r) => {
                let a = runtime::coerce_to_set(&l);
                let b = runtime::coerce_to_set(&r);
                Value::set(a.symmetric_difference(&b).cloned().collect())
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
