use super::*;
use crate::ast::{CallArg, ControlFlowKind};
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn dispatch_rotate(
        &self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let items = match target {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            Value::Capture { positional, .. } => positional,
            Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
            _ => return Ok(Value::Nil),
        };
        if items.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let len = items.len() as i64;
        let by = match args.first() {
            Some(Value::Int(i)) => *i,
            Some(Value::Num(n)) => *n as i64,
            Some(other) => other.to_string_value().parse::<i64>().unwrap_or(1),
            None => 1,
        };
        let shift = ((by % len) + len) % len;
        let mut out = vec![Value::Nil; items.len()];
        for (i, item) in items.into_iter().enumerate() {
            let dst = ((i as i64 + len - shift) % len) as usize;
            out[dst] = item;
        }
        Ok(Value::array(out))
    }

    pub(super) fn dispatch_minmaxpairs(
        &mut self,
        target: Value,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        if matches!(target, Value::Instance { .. })
            && let Ok(pairs) = self.call_method_with_values(target.clone(), "pairs", Vec::new())
        {
            return Ok(pairs);
        }
        let want_max = method == "maxpairs";
        let to_pairs = |items: &[Value]| -> Value {
            let mut best: Option<Value> = None;
            let mut out: Vec<Value> = Vec::new();
            for (idx, item) in items.iter().enumerate() {
                if matches!(item, Value::Nil) || matches!(item, Value::Package(n) if n == "Any") {
                    continue;
                }
                let ord = if let Some(current) = &best {
                    // Use `cmp` semantics: numeric comparison for numeric
                    // pairs, string comparison otherwise
                    match (item, current) {
                        (Value::Int(a), Value::Int(b)) => a.cmp(b),
                        (Value::Num(a), Value::Num(b)) => {
                            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                        }
                        (Value::Int(a), Value::Num(b)) => (*a as f64)
                            .partial_cmp(b)
                            .unwrap_or(std::cmp::Ordering::Equal),
                        (Value::Num(a), Value::Int(b)) => a
                            .partial_cmp(&(*b as f64))
                            .unwrap_or(std::cmp::Ordering::Equal),
                        (Value::Rat(..), _) | (_, Value::Rat(..)) => {
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
                    out.push(Value::ValuePair(
                        Box::new(Value::Int(idx as i64)),
                        Box::new(item.clone()),
                    ));
                } else if ord == std::cmp::Ordering::Equal {
                    out.push(Value::ValuePair(
                        Box::new(Value::Int(idx as i64)),
                        Box::new(item.clone()),
                    ));
                }
            }
            Value::Seq(Arc::new(out))
        };
        Ok(match target {
            Value::Array(items, ..) => to_pairs(&items),
            other => Value::Seq(Arc::new(vec![Value::ValuePair(
                Box::new(Value::Int(0)),
                Box::new(other),
            )])),
        })
    }

    pub(super) fn dispatch_supply_running_extrema(
        &mut self,
        target: Value,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Value::Instance { attributes, .. } = target else {
            return Err(RuntimeError::new("Expected Supply instance"));
        };
        let values = if let Some(Value::Array(items, ..)) = attributes.get("values") {
            items.to_vec()
        } else {
            Vec::new()
        };
        let want_max = method == "max";

        let build_supply = |vals: Vec<Value>| {
            let mut attrs = HashMap::new();
            attrs.insert("values".to_string(), Value::array(vals));
            attrs.insert("taps".to_string(), Value::array(Vec::new()));
            attrs.insert("live".to_string(), Value::Bool(false));
            Value::make_instance(Symbol::intern("Supply"), attrs)
        };

        let compare_or_key_fn = args.first().cloned();
        if let Some(ref fn_val) = compare_or_key_fn
            && !matches!(
                fn_val,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
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
            matches!(&compare_or_key_fn, Value::Sub(data) if data.params.len() >= 2);
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
                let cmp = match cmp_val {
                    Value::Enum {
                        ref enum_type,
                        value,
                        ..
                    } if enum_type == "Order" => value,
                    other => {
                        let n = other.to_f64();
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

    pub(crate) fn dispatch_sort(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match target {
            Value::Array(mut items, ..) => {
                let items_mut = Arc::make_mut(&mut items);
                // Handle Routine comparator (e.g. &[<=>], &infix:<+>)
                if let Some(comparator @ Value::Routine { .. }) = args.first().cloned() {
                    items_mut.sort_by(|a, b| {
                        let call_args = vec![a.clone(), b.clone()];
                        match self.eval_call_on_value(comparator.clone(), call_args) {
                            Ok(result) => match &result {
                                Value::Int(n) => n.cmp(&0),
                                Value::Enum {
                                    enum_type, value, ..
                                } if enum_type == "Order" => value.cmp(&0),
                                _ => std::cmp::Ordering::Equal,
                            },
                            Err(_) => std::cmp::Ordering::Equal,
                        }
                    });
                } else if let Some(Value::Sub(data)) = args.first().cloned() {
                    let is_key_extractor = data.params.len() <= 1;
                    if is_key_extractor {
                        items_mut.sort_by(|a, b| {
                            let saved = self.env.clone();
                            for (k, v) in &data.env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if let Some(p) = data.params.first() {
                                self.env.insert(p.clone(), a.clone());
                            }
                            self.env.insert("_".to_string(), a.clone());
                            let key_a = self.eval_block_value(&data.body).unwrap_or(Value::Nil);
                            self.env = saved.clone();
                            for (k, v) in &data.env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if let Some(p) = data.params.first() {
                                self.env.insert(p.clone(), b.clone());
                            }
                            self.env.insert("_".to_string(), b.clone());
                            let key_b = self.eval_block_value(&data.body).unwrap_or(Value::Nil);
                            self.env = saved;
                            compare_values(&key_a, &key_b).cmp(&0)
                        });
                    } else {
                        items_mut.sort_by(|a, b| {
                            let saved = self.env.clone();
                            for (k, v) in &data.env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if data.params.len() >= 2 {
                                self.env.insert(data.params[0].clone(), a.clone());
                                self.env.insert(data.params[1].clone(), b.clone());
                            } else if let Some(p) = data.params.first() {
                                self.env.insert(p.clone(), a.clone());
                            }
                            self.env.insert("_".to_string(), a.clone());
                            let result = self.eval_block_value(&data.body).unwrap_or(Value::Int(0));
                            self.env = saved;
                            match result {
                                Value::Int(n) => n.cmp(&0),
                                Value::Enum {
                                    enum_type, value, ..
                                } if enum_type == "Order" => value.cmp(&0),
                                _ => std::cmp::Ordering::Equal,
                            }
                        });
                    }
                } else {
                    items_mut.sort_by(|a, b| compare_values(a, b).cmp(&0));
                }
                Ok(Value::Array(items, false))
            }
            Value::Hash(map) => {
                // Convert hash to list of pairs, then sort
                let items: Vec<Value> = map
                    .iter()
                    .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                    .collect();
                self.dispatch_sort(Value::array(items), args)
            }
            other => Ok(other),
        }
    }

    pub(super) fn dispatch_unique(
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

        let items: Vec<Value> = match target {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            Value::LazyList(ll) => self.force_lazy_list_bridge(&ll)?,
            v @ (Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. }) => Self::value_to_list(&v),
            other => vec![other],
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

        let items: Vec<Value> = match target {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            Value::LazyList(ll) => self.force_lazy_list_bridge(&ll)?,
            v @ (Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. }) => Self::value_to_list(&v),
            other => vec![other],
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
                super::SquishIteratorMeta {
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

    pub(super) fn dispatch_collate(&mut self, target: Value) -> Result<Value, RuntimeError> {
        fn case_profile(s: &str) -> Vec<u8> {
            s.chars()
                .map(|ch| {
                    if ch.is_lowercase() {
                        0
                    } else if ch.is_uppercase() {
                        1
                    } else {
                        2
                    }
                })
                .collect()
        }

        fn collate_sorted(values: Vec<Value>) -> Vec<Value> {
            let mut keyed: Vec<(String, Vec<u8>, String, Value)> = values
                .into_iter()
                .map(|value| {
                    let s = value.to_string_value();
                    (s.to_lowercase(), case_profile(&s), s.clone(), value)
                })
                .collect();
            keyed.sort_by(|a, b| {
                a.0.cmp(&b.0)
                    .then_with(|| a.1.cmp(&b.1))
                    .then_with(|| a.2.cmp(&b.2))
            });
            keyed.into_iter().map(|(_, _, _, value)| value).collect()
        }

        match target {
            Value::Package(class_name) if class_name == "Supply" => {
                Ok(Value::Seq(Arc::new(vec![Value::Package(class_name)])))
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(collate_sorted(values)));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            Value::Array(items, ..) => Ok(Value::Seq(Arc::new(collate_sorted(items.to_vec())))),
            other => {
                let values = Self::value_to_list(&other);
                Ok(Value::Seq(Arc::new(collate_sorted(values))))
            }
        }
    }

    pub(super) fn civil_to_epoch_days(year: i64, month: i64, day: i64) -> i64 {
        let y = year - i64::from(month <= 2);
        let era = if y >= 0 { y } else { y - 399 } / 400;
        let yoe = y - era * 400;
        let mp = month + if month > 2 { -3 } else { 9 };
        let doy = (153 * mp + 2) / 5 + day - 1;
        let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
        era * 146_097 + doe - 719_468
    }

    pub(super) fn leap_seconds_before_day(epoch_days: i64) -> i64 {
        const LEAP_EFFECTIVE_DATES: &[(i64, i64, i64)] = &[
            (1972, 7, 1),
            (1973, 1, 1),
            (1974, 1, 1),
            (1975, 1, 1),
            (1976, 1, 1),
            (1977, 1, 1),
            (1978, 1, 1),
            (1979, 1, 1),
            (1980, 1, 1),
            (1981, 7, 1),
            (1982, 7, 1),
            (1983, 7, 1),
            (1985, 7, 1),
            (1988, 1, 1),
            (1990, 1, 1),
            (1991, 1, 1),
            (1992, 7, 1),
            (1993, 7, 1),
            (1994, 7, 1),
            (1996, 1, 1),
            (1997, 7, 1),
            (1999, 1, 1),
            (2006, 1, 1),
            (2009, 1, 1),
            (2012, 7, 1),
            (2015, 7, 1),
            (2017, 1, 1),
        ];
        LEAP_EFFECTIVE_DATES
            .iter()
            .filter(|&&(y, m, d)| Self::civil_to_epoch_days(y, m, d) <= epoch_days)
            .count() as i64
    }

    pub(super) fn date_days_to_epoch_with_leap_seconds(days: i64) -> f64 {
        (days * 86_400 + Self::leap_seconds_before_day(days)) as f64
    }

    pub(super) fn dispatch_grep(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn stmt_contains_last(stmt: &Stmt) -> bool {
            match stmt {
                Stmt::Last(_) => true,
                Stmt::Expr(expr)
                | Stmt::Return(expr)
                | Stmt::Die(expr)
                | Stmt::Fail(expr)
                | Stmt::Take(expr) => expr_contains_last(expr),
                Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => expr_contains_last(expr),
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                    ..
                } => {
                    expr_contains_last(cond)
                        || then_branch.iter().any(stmt_contains_last)
                        || else_branch.iter().any(stmt_contains_last)
                }
                Stmt::While { cond, body, .. } => {
                    expr_contains_last(cond) || body.iter().any(stmt_contains_last)
                }
                Stmt::For { iterable, body, .. } => {
                    expr_contains_last(iterable) || body.iter().any(stmt_contains_last)
                }
                Stmt::Loop {
                    init,
                    cond,
                    step,
                    body,
                    ..
                } => {
                    init.as_ref().is_some_and(|s| stmt_contains_last(s))
                        || cond.as_ref().is_some_and(expr_contains_last)
                        || step.as_ref().is_some_and(expr_contains_last)
                        || body.iter().any(stmt_contains_last)
                }
                Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::React { body }
                | Stmt::Catch(body)
                | Stmt::Control(body)
                | Stmt::Default(body) => body.iter().any(stmt_contains_last),
                Stmt::Whenever { supply, body, .. } => {
                    expr_contains_last(supply) || body.iter().any(stmt_contains_last)
                }
                Stmt::Given { topic, body } => {
                    expr_contains_last(topic) || body.iter().any(stmt_contains_last)
                }
                Stmt::When { cond, body } => {
                    expr_contains_last(cond) || body.iter().any(stmt_contains_last)
                }
                Stmt::ClassDecl { body, .. }
                | Stmt::RoleDecl { body, .. }
                | Stmt::Package { body, .. }
                | Stmt::SubDecl { body, .. }
                | Stmt::MethodDecl { body, .. } => body.iter().any(stmt_contains_last),
                Stmt::HasDecl { default, .. } => default.as_ref().is_some_and(expr_contains_last),
                Stmt::Call { args, .. } => args.iter().any(|arg| match arg {
                    CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => {
                        expr_contains_last(e)
                    }
                    CallArg::Named { value, .. } => value.as_ref().is_some_and(expr_contains_last),
                }),
                Stmt::Label { stmt, .. } => stmt_contains_last(stmt),
                Stmt::EnumDecl { variants, .. } => variants
                    .iter()
                    .any(|(_, v)| v.as_ref().is_some_and(expr_contains_last)),
                Stmt::Goto(expr) => expr_contains_last(expr),
                _ => false,
            }
        }

        fn expr_contains_last(expr: &Expr) -> bool {
            match expr {
                Expr::ControlFlow {
                    kind: ControlFlowKind::Last,
                    ..
                } => true,
                Expr::Unary { expr, .. }
                | Expr::PostfixOp { expr, .. }
                | Expr::Reduction { expr, .. }
                | Expr::PositionalPair(expr)
                | Expr::ZenSlice(expr)
                | Expr::IndirectTypeLookup(expr) => expr_contains_last(expr),
                Expr::DoStmt(stmt) => stmt_contains_last(stmt),
                Expr::Binary { left, right, .. }
                | Expr::HyperOp { left, right, .. }
                | Expr::MetaOp { left, right, .. } => {
                    expr_contains_last(left) || expr_contains_last(right)
                }
                Expr::InfixFunc { left, right, .. } => {
                    expr_contains_last(left) || right.iter().any(expr_contains_last)
                }
                Expr::Ternary {
                    cond,
                    then_expr,
                    else_expr,
                } => {
                    expr_contains_last(cond)
                        || expr_contains_last(then_expr)
                        || expr_contains_last(else_expr)
                }
                Expr::Index { target, index } => {
                    expr_contains_last(target) || expr_contains_last(index)
                }
                Expr::Exists { target, arg, .. } => {
                    expr_contains_last(target)
                        || arg
                            .as_ref()
                            .is_some_and(|index_expr| expr_contains_last(index_expr))
                }
                Expr::MethodCall { target, args, .. }
                | Expr::DynamicMethodCall { target, args, .. }
                | Expr::HyperMethodCall { target, args, .. }
                | Expr::HyperMethodCallDynamic { target, args, .. } => {
                    expr_contains_last(target) || args.iter().any(expr_contains_last)
                }
                Expr::CallOn { target, args } => {
                    expr_contains_last(target) || args.iter().any(expr_contains_last)
                }
                Expr::Call { args, .. } => args.iter().any(expr_contains_last),
                Expr::StringInterpolation(items)
                | Expr::ArrayLiteral(items)
                | Expr::BracketArray(items)
                | Expr::CaptureLiteral(items) => items.iter().any(expr_contains_last),
                Expr::Hash(items) => items
                    .iter()
                    .any(|(_, val)| val.as_ref().is_some_and(expr_contains_last)),
                Expr::Block(body)
                | Expr::AnonSub { body, .. }
                | Expr::AnonSubParams { body, .. }
                | Expr::Gather(body)
                | Expr::DoBlock { body, .. } => body.iter().any(stmt_contains_last),
                Expr::Try { body, catch } => {
                    body.iter().any(stmt_contains_last)
                        || catch
                            .as_ref()
                            .is_some_and(|stmts| stmts.iter().any(stmt_contains_last))
                }
                Expr::IndexAssign {
                    target,
                    index,
                    value,
                } => {
                    expr_contains_last(target)
                        || expr_contains_last(index)
                        || expr_contains_last(value)
                }
                Expr::AssignExpr { expr, .. } => expr_contains_last(expr),
                Expr::Lambda { body, .. } => body.iter().any(stmt_contains_last),
                Expr::Subst { .. }
                | Expr::NonDestructiveSubst { .. }
                | Expr::Transliterate { .. }
                | Expr::MatchRegex(_)
                | Expr::Literal(_)
                | Expr::Whatever
                | Expr::HyperWhatever
                | Expr::BareWord(_)
                | Expr::Var(_)
                | Expr::CaptureVar(_)
                | Expr::ArrayVar(_)
                | Expr::HashVar(_)
                | Expr::CodeVar(_)
                | Expr::EnvIndex(_)
                | Expr::RoutineMagic
                | Expr::BlockMagic
                | Expr::PseudoStash(_) => false,
                Expr::IndirectCodeLookup { package, .. } => expr_contains_last(package),
                Expr::HyperSlice { target, .. } => expr_contains_last(target),
                Expr::HyperIndex { target, keys } => {
                    expr_contains_last(target) || expr_contains_last(keys)
                }
                _ => false,
            }
        }

        if let Some(Value::Pair(key, _)) =
            args.iter().skip(1).find(|v| matches!(v, Value::Pair(_, _)))
        {
            return Err(RuntimeError::new(format!(
                "X::Adverb: Unexpected adverb '{}'",
                key
            )));
        }

        match target {
            Value::Package(class_name) if class_name == "Supply" => Err(RuntimeError::new(
                "Cannot call .grep on a Supply type object",
            )),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let source_values = if let Some(on_demand_cb) = attributes.get("on_demand_callback")
                {
                    let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    self.supply_emit_buffer.pop().unwrap_or_default()
                } else {
                    attributes
                        .get("values")
                        .and_then(|v| {
                            if let Value::Array(items, ..) = v {
                                Some(items.to_vec())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_default()
                };
                let filtered = self.eval_grep_over_items(args.first().cloned(), source_values)?;
                let filtered_values = Self::value_to_list(&filtered);
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(filtered_values));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert(
                    "live".to_string(),
                    attributes
                        .get("live")
                        .cloned()
                        .unwrap_or(Value::Bool(false)),
                );
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            Value::Array(items, is_array) => {
                let (filtered, mutated_items) =
                    self.eval_grep_over_items_with_mutated(args.first().cloned(), items.to_vec())?;
                let updated_source = std::sync::Arc::new(mutated_items);
                self.overwrite_array_bindings_by_identity(
                    &items,
                    Value::Array(updated_source.clone(), is_array),
                );
                if let Value::Array(filtered_items, ..) = &filtered {
                    let mut indices = Vec::new();
                    let mut scan_from = 0usize;
                    let source_items = updated_source.as_ref();
                    for needle in filtered_items.iter() {
                        if let Some(rel) = source_items[scan_from..].iter().position(|candidate| {
                            crate::runtime::utils::values_identical(candidate, needle)
                        }) {
                            let absolute = scan_from + rel;
                            indices.push(absolute);
                            scan_from = absolute.saturating_add(1);
                        }
                    }
                    if !indices.is_empty() {
                        crate::runtime::utils::register_grep_view_binding(
                            filtered_items,
                            &updated_source,
                            indices,
                            is_array,
                        );
                    }
                }
                Ok(filtered)
            }
            Value::Range(a, b) => {
                let items: Vec<Value> = (a..=b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::RangeExcl(a, b) => {
                let items: Vec<Value> = (a..b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::RangeExclStart(a, b) => {
                let items: Vec<Value> = (a + 1..=b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::RangeExclBoth(a, b) => {
                let items: Vec<Value> = (a + 1..b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::GenericRange { .. } => {
                if let Value::GenericRange {
                    start,
                    end,
                    excl_start,
                    ..
                } = &target
                {
                    let end_num = end.to_f64();
                    if end_num.is_infinite()
                        && end_num.is_sign_positive()
                        && let Some(Value::Sub(data)) = args.first().cloned()
                        && data.body.iter().any(stmt_contains_last)
                    {
                        let mut current = start.to_f64() as i64;
                        if *excl_start {
                            current += 1;
                        }
                        let mut result = Vec::new();
                        let limit = 1_000_000usize;
                        while result.len() < limit {
                            let item = match start.as_ref() {
                                Value::Num(_) => Value::Num(current as f64),
                                Value::Rat(_, den) => crate::value::make_rat(current * *den, *den),
                                _ => Value::Int(current),
                            };
                            'redo_item: loop {
                                match self.call_sub_value(
                                    Value::Sub(data.clone()),
                                    vec![item.clone()],
                                    false,
                                ) {
                                    Ok(pred) => {
                                        if pred.truthy() {
                                            result.push(item.clone());
                                        }
                                        break 'redo_item;
                                    }
                                    Err(e) if e.is_redo => continue 'redo_item,
                                    Err(e) if e.is_next => break 'redo_item,
                                    Err(e) if e.is_last => return Ok(Value::array(result)),
                                    Err(e) => return Err(e),
                                }
                            }
                            current += 1;
                        }
                        return Ok(Value::array(result));
                    }
                    if end_num.is_infinite() && end_num.is_sign_positive() {
                        // Preserve laziness for open-ended ranges in grep.
                        return Ok(target);
                    }
                }
                let items = crate::runtime::utils::value_to_list(&target);
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::Str(s) => {
                if let Some(Value::Sub(data)) = args.first()
                    && matches!(
                        data.body.last(),
                        Some(Stmt::Expr(Expr::Literal(Value::Regex(_))))
                    )
                {
                    return self.eval_grep_over_items(args.first().cloned(), vec![Value::Str(s)]);
                }
                Ok(Value::Str(s))
            }
            other => Ok(other),
        }
    }

    pub(super) fn dispatch_first(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Separate named args (Pairs) from positional args
        let mut positional = Vec::new();
        let mut has_neg_v = false;
        let mut has_end = false;
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "v" => {
                    if !value.truthy() {
                        has_neg_v = true;
                    }
                }
                Value::Pair(key, value) if key == "end" => {
                    if value.truthy() {
                        has_end = true;
                    }
                }
                Value::Pair(key, value) if key == "k" => {
                    has_k = value.truthy();
                }
                Value::Pair(key, value) if key == "kv" => {
                    has_kv = value.truthy();
                }
                Value::Pair(key, value) if key == "p" => {
                    has_p = value.truthy();
                }
                _ => positional.push(arg.clone()),
            }
        }
        if has_neg_v {
            return Err(RuntimeError::new(
                "Throwing `:!v` on first is not supported",
            ));
        }
        // Check for Bool matcher (X::Match::Bool)
        if matches!(positional.first(), Some(Value::Bool(_))) {
            let mut err = RuntimeError::new("Cannot use Bool as a matcher");
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Match::Bool"),
                std::collections::HashMap::new(),
            )));
            return Err(err);
        }
        let func = positional.first().cloned();
        let items = crate::runtime::utils::value_to_list(&target);
        if let Some((idx, value)) = self.find_first_match_over_items(func, &items, has_end)? {
            return Ok(super::builtins_collection::format_first_result(
                idx, value, has_k, has_kv, has_p,
            ));
        }
        Ok(Value::Nil)
    }

    /// `$n.polymod(@divisors)` — successive modular decomposition.
    pub(super) fn method_polymod(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn val_to_f64(v: &Value) -> f64 {
            match v {
                Value::Int(n) => *n as f64,
                Value::Num(n) => *n,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Bool(b) => {
                    if *b {
                        1.0
                    } else {
                        0.0
                    }
                }
                Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                _ => 0.0,
            }
        }
        fn f64_to_val(n: f64) -> Value {
            if n.is_finite() && n == n.trunc() && n.abs() < i64::MAX as f64 {
                Value::Int(n as i64)
            } else {
                Value::Num(n)
            }
        }
        let mut n = val_to_f64(target);
        // Flatten args into a list of divisors
        let mut divisors = Vec::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => divisors.extend(items.iter().cloned()),
                _ => divisors.push(arg.clone()),
            }
        }
        let mut result = Vec::new();
        for d in &divisors {
            let d_val = val_to_f64(d);
            if d_val == 0.0 {
                result.push(f64_to_val(n));
                n = f64::INFINITY;
                continue;
            }
            let rem = n % d_val;
            let quot = ((n - rem) / d_val).trunc();
            result.push(f64_to_val(rem));
            n = quot;
            // Modulo 1 always yields remainder 0 and quotient = n; stop infinite loops
            if d_val == 1.0 {
                break;
            }
        }
        result.push(f64_to_val(n));
        Ok(Value::array(result))
    }

    pub(super) fn dispatch_tree(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Non-iterable: .tree(anything) returns self
        let items = match &target {
            Value::Array(items, ..) => items.clone(),
            _ => return Ok(target),
        };

        let arg = &args[0];
        match arg {
            // .tree(0) — identity
            Value::Int(0) => Ok(target),
            // .tree(n) — tree to n levels
            Value::Int(n) if *n > 0 => Ok(Value::array(self.tree_depth(&items, *n as usize)?)),
            // .tree(*) — full depth (same as .tree()); * compiles to Inf
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                Ok(Value::array(self.tree_depth(&items, usize::MAX)?))
            }
            // .tree([&first, *@rest]) — array of closures form
            Value::Array(closure_list, ..) => {
                if closure_list.is_empty() {
                    return Ok(target);
                }
                let first = &closure_list[0];
                self.call_sub_value(first.clone(), vec![target], false)
            }
            // .tree(&closure, ...) — apply closures at depth levels
            Value::Sub(_) => {
                let closures: Vec<Value> = args.to_vec();
                self.tree_with_closures(&items, &closures, 0)
            }
            _ => Ok(target),
        }
    }

    pub(super) fn tree_depth(
        &mut self,
        items: &[Value],
        depth: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut result = Vec::new();
        for item in items {
            match item {
                Value::Array(inner, ..) if depth > 0 => {
                    result.push(Value::array(self.tree_depth(inner, depth - 1)?));
                }
                other => result.push(other.clone()),
            }
        }
        Ok(result)
    }

    pub(super) fn tree_with_closures(
        &mut self,
        items: &[Value],
        closures: &[Value],
        depth: usize,
    ) -> Result<Value, RuntimeError> {
        let mut processed = Vec::new();
        for item in items {
            match item {
                Value::Array(inner, ..) if closures.len() > depth + 1 => {
                    let sub_result = self.tree_with_closures(inner, closures, depth + 1)?;
                    processed.push(sub_result);
                }
                Value::Array(inner, ..) => {
                    // Apply the last closure to leaf arrays
                    if let Some(closure) = closures.last()
                        && closures.len() > 1
                    {
                        processed.push(self.call_sub_value(
                            closure.clone(),
                            vec![Value::Array(inner.clone(), false)],
                            false,
                        )?);
                    } else {
                        processed.push(Value::Array(inner.clone(), false)); // already Arc-wrapped
                    }
                }
                other => processed.push(other.clone()),
            }
        }
        // Apply the closure at this depth level
        if let Some(closure) = closures.get(depth) {
            self.call_sub_value(closure.clone(), vec![Value::array(processed)], false)
        } else {
            Ok(Value::array(processed))
        }
    }

    pub(super) fn dispatch_socket_connect(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let host = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let port = args
            .get(1)
            .map(|v| match v {
                Value::Int(i) => *i as u16,
                Value::Num(f) => *f as u16,
                other => other.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);
        let addr = format!("{}:{}", host, port);
        let stream = std::net::TcpStream::connect_timeout(
            &addr
                .to_socket_addrs()
                .map_err(|e| RuntimeError::new(format!("Failed to resolve '{}': {}", addr, e)))?
                .next()
                .ok_or_else(|| RuntimeError::new(format!("No addresses found for '{}'", addr)))?,
            Duration::from_secs(10),
        )
        .map_err(|e| RuntimeError::new(format!("Failed to connect to '{}': {}", addr, e)))?;
        let id = self.next_handle_id;
        self.next_handle_id += 1;
        let state = IoHandleState {
            target: IoHandleTarget::Socket,
            mode: IoHandleMode::ReadWrite,
            path: None,
            line_separators: self.default_line_separators(),
            line_chomp: true,
            encoding: "utf-8".to_string(),
            file: None,
            socket: Some(stream),
            closed: false,
            bin: false,
        };
        self.handles.insert(id, state);
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::Int(id as i64));
        attrs.insert("host".to_string(), Value::Str(host));
        attrs.insert("port".to_string(), Value::Int(port as i64));
        Ok(Value::make_instance(
            Symbol::intern("IO::Socket::INET"),
            attrs,
        ))
    }

    /// Replay deferred Proc::Async taps on the main thread.
    /// Called when a Proc result is retrieved via .result or await.
    pub(super) fn replay_proc_taps(&mut self, attributes: &Arc<HashMap<String, Value>>) {
        let stdout_taps = match attributes.get("stdout_taps") {
            Some(Value::Array(taps, ..)) => taps.to_vec(),
            _ => Vec::new(),
        };
        let stderr_taps = match attributes.get("stderr_taps") {
            Some(Value::Array(taps, ..)) => taps.to_vec(),
            _ => Vec::new(),
        };
        let collected_stdout = match attributes.get("collected_stdout") {
            Some(Value::Str(s)) => s.clone(),
            _ => String::new(),
        };
        let collected_stderr = match attributes.get("collected_stderr") {
            Some(Value::Str(s)) => s.clone(),
            _ => String::new(),
        };

        if !collected_stdout.is_empty() && !stdout_taps.is_empty() {
            for tap in &stdout_taps {
                let _ = self.call_sub_value(
                    tap.clone(),
                    vec![Value::Str(collected_stdout.clone())],
                    true,
                );
            }
        }
        if !collected_stderr.is_empty() && !stderr_taps.is_empty() {
            for tap in &stderr_taps {
                let _ = self.call_sub_value(
                    tap.clone(),
                    vec![Value::Str(collected_stderr.clone())],
                    true,
                );
            }
        }
    }

    /// Returns Some(class_name) if target is Promise or a Promise subclass package.
    pub(super) fn promise_class_name(&mut self, target: &Value) -> Option<String> {
        match target {
            Value::Package(name) => {
                if name == "Promise" {
                    Some("Promise".to_string())
                } else if self
                    .class_mro(&name.resolve())
                    .contains(&"Promise".to_string())
                {
                    Some(name.resolve())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub(super) fn dispatch_encoding_registry_find(
        &self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let name = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if let Some(entry) = self.find_encoding(&name) {
            if let Some(ref user_type) = entry.user_type {
                // User-registered encoding: return an instance of the user's type
                return Ok(user_type.clone());
            }
            // Built-in encoding: create an Encoding::Builtin instance
            let mut attrs = HashMap::new();
            attrs.insert("name".to_string(), Value::Str(entry.name.clone()));
            let alt_names: Vec<Value> = entry
                .alternative_names
                .iter()
                .map(|s| Value::Str(s.clone()))
                .collect();
            attrs.insert("alternative-names".to_string(), Value::array(alt_names));
            Ok(Value::make_instance(
                Symbol::intern("Encoding::Builtin"),
                attrs,
            ))
        } else {
            // Throw X::Encoding::Unknown
            let mut ex_attrs = HashMap::new();
            ex_attrs.insert("name".to_string(), Value::Str(name.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Encoding::Unknown"), ex_attrs);
            let mut err = RuntimeError::new(format!("Unknown encoding '{}'", name));
            err.exception = Some(Box::new(ex));
            Err(err)
        }
    }

    pub(super) fn dispatch_encoding_registry_register(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let encoding_val = args.first().cloned().unwrap_or(Value::Nil);
        // The encoding is a type object (Package) or class instance.
        // We need to call .name and .alternative-names on it to get registration info.
        let enc_name = self.call_method_with_values(encoding_val.clone(), "name", vec![])?;
        let enc_name_str = enc_name.to_string_value();

        let alt_names_val = self
            .call_method_with_values(encoding_val.clone(), "alternative-names", vec![])
            .unwrap_or(Value::array(Vec::new()));
        let alt_names: Vec<String> = match alt_names_val {
            Value::Array(items, ..) => items.iter().map(|v| v.to_string_value()).collect(),
            Value::Slip(items) => items.iter().map(|v| v.to_string_value()).collect(),
            _ => Vec::new(),
        };

        let entry = super::EncodingEntry {
            name: enc_name_str.clone(),
            alternative_names: alt_names,
            user_type: Some(encoding_val),
        };

        match self.register_encoding(entry) {
            Ok(()) => Ok(Value::Nil),
            Err(conflicting_name) => {
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("name".to_string(), Value::Str(conflicting_name.clone()));
                let ex = Value::make_instance(
                    Symbol::intern("X::Encoding::AlreadyRegistered"),
                    ex_attrs,
                );
                let mut err = RuntimeError::new(format!(
                    "Encoding '{}' is already registered",
                    conflicting_name
                ));
                err.exception = Some(Box::new(ex));
                Err(err)
            }
        }
    }

    pub(super) fn dispatch_rotor(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::utils::to_float_value;

        // Extract :partial named arg
        let mut partial = false;
        let mut positional_args: Vec<Value> = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, val) if key == "partial" => {
                    partial = val.truthy();
                }
                _ => positional_args.push(arg.clone()),
            }
        }

        // Build spec list from positional args
        // If single arg is a list/array, use its elements as specs
        let specs = if positional_args.len() == 1 {
            match &positional_args[0] {
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) => items.to_vec(),
                Value::LazyList(_) => Self::value_to_list(&positional_args[0]),
                other => vec![other.clone()],
            }
        } else {
            positional_args
        };

        // Parse each spec into (count, gap) pairs
        // count can be: Int, Whatever (*), Inf, Range
        // gap is from Pair's value
        struct RotorSpec {
            count: RotorCount,
            gap: i64,
        }
        #[derive(Clone)]
        enum RotorCount {
            Fixed(usize),
            Whatever,        // * — take everything remaining
            Inf,             // Inf — take everything remaining
            Range(Vec<i64>), // 1..* or similar — cycling counts
        }

        // Flatten any nested Seq/Array specs into a flat list
        let mut flat_specs: Vec<Value> = Vec::new();
        let mut to_process: std::collections::VecDeque<Value> = specs.into();
        while let Some(spec) = to_process.pop_front() {
            match &spec {
                Value::Seq(items) => {
                    for item in items.iter() {
                        to_process.push_back(item.clone());
                    }
                }
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        to_process.push_back(item.clone());
                    }
                }
                _ => flat_specs.push(spec),
            }
        }

        let mut rotor_specs: Vec<RotorSpec> = Vec::new();
        for spec in &flat_specs {
            match spec {
                Value::Int(n) => {
                    let count = *n;
                    if count < 0 {
                        let mut attrs = HashMap::new();
                        attrs.insert("got".to_string(), Value::Int(count));
                        attrs.insert(
                            "message".to_string(),
                            Value::Str(format!(
                                "Expected a non-negative integer for rotor count, got {}",
                                count
                            )),
                        );
                        let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                        let mut err = RuntimeError::new(format!(
                            "X::OutOfRange: Expected non-negative count, got {}",
                            count
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                Value::Whatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Inf,
                        gap: 0,
                    });
                }
                Value::Num(n) if n.is_infinite() && n.is_sign_positive() => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Inf,
                        gap: 0,
                    });
                }
                Value::Num(n) => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(*n as usize),
                        gap: 0,
                    });
                }
                Value::Rat(n, d) => {
                    let count = if *d != 0 { n / d } else { 0 };
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                Value::Pair(..) | Value::ValuePair(..) => {
                    let (count_val, gap_val) = match spec {
                        Value::Pair(k, v) => (Value::Str(k.clone()), v.as_ref().clone()),
                        Value::ValuePair(k, v) => (k.as_ref().clone(), v.as_ref().clone()),
                        _ => unreachable!(),
                    };
                    let count = match &count_val {
                        Value::Int(n) => RotorCount::Fixed(*n as usize),
                        Value::Num(n) => RotorCount::Fixed(*n as usize),
                        Value::Rat(n, d) => {
                            RotorCount::Fixed(if *d != 0 { (n / d) as usize } else { 0 })
                        }
                        Value::Str(s) => RotorCount::Fixed(s.parse::<i64>().unwrap_or(0) as usize),
                        _ => RotorCount::Fixed(0),
                    };
                    let gap = match &gap_val {
                        Value::Int(n) => *n,
                        Value::Num(n) => *n as i64,
                        Value::Rat(n, d) => {
                            if *d != 0 {
                                n / d
                            } else {
                                0
                            }
                        }
                        _ => 0,
                    };
                    rotor_specs.push(RotorSpec { count, gap });
                }
                Value::HyperWhatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Whatever,
                        gap: 0,
                    });
                }
                Value::Range(start, end) | Value::RangeExcl(start, end) => {
                    let is_excl = matches!(spec, Value::RangeExcl(..));
                    let end_val = if is_excl { *end - 1 } else { *end };
                    if end_val == i64::MAX || *end == i64::MAX {
                        // 1..* — infinite range, treated like cycling counts
                        let mut counts = Vec::new();
                        for i in *start.. {
                            counts.push(i);
                            if counts.len() > 10000 {
                                break; // safety limit
                            }
                        }
                        rotor_specs.push(RotorSpec {
                            count: RotorCount::Range(counts),
                            gap: 0,
                        });
                    } else {
                        let mut counts = Vec::new();
                        for i in *start..=end_val {
                            counts.push(i);
                        }
                        rotor_specs.push(RotorSpec {
                            count: RotorCount::Range(counts),
                            gap: 0,
                        });
                    }
                }
                _ => {
                    // Try to coerce to int
                    if let Some(n) = to_float_value(spec) {
                        if n.is_infinite() && n.is_sign_positive() {
                            rotor_specs.push(RotorSpec {
                                count: RotorCount::Inf,
                                gap: 0,
                            });
                        } else {
                            rotor_specs.push(RotorSpec {
                                count: RotorCount::Fixed(n as usize),
                                gap: 0,
                            });
                        }
                    }
                }
            }
        }

        if rotor_specs.is_empty() {
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }

        // Get the items to rotor over (force LazyList if needed)
        let target = if let Value::LazyList(ll) = &target {
            Value::array(self.force_lazy_list_bridge(ll)?)
        } else {
            target
        };
        let items = Self::value_to_list(&target);

        if items.is_empty() {
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }

        let mut result: Vec<Value> = Vec::new();
        let mut pos = 0usize;
        let mut spec_idx = 0usize;
        let mut range_sub_idx = 0usize; // for Range specs

        loop {
            if pos >= items.len() {
                break;
            }

            let spec = &rotor_specs[spec_idx % rotor_specs.len()];

            let count = match &spec.count {
                RotorCount::Fixed(n) => *n,
                RotorCount::Whatever | RotorCount::Inf => items.len() - pos,
                RotorCount::Range(counts) => {
                    let c = counts[range_sub_idx % counts.len()];
                    if c == i64::MAX {
                        items.len() - pos
                    } else {
                        c as usize
                    }
                }
            };

            let gap = spec.gap;

            // Take `count` items starting at pos
            let end = std::cmp::min(pos.saturating_add(count), items.len());
            let chunk_len = end - pos;
            let chunk: Vec<Value> = items[pos..end].to_vec();

            if (chunk_len == count || partial) && (!chunk.is_empty() || count == 0) {
                result.push(Value::array(chunk));
            }

            if chunk_len < count && !partial {
                break;
            }

            // Advance position: count + gap (gap can be negative for overlap)
            let new_pos = (pos as i64).saturating_add((count as i64).saturating_add(gap));
            if new_pos < 0 {
                // Negative gap past start of list
                let mut attrs = HashMap::new();
                attrs.insert("got".to_string(), Value::Int(new_pos));
                attrs.insert(
                    "message".to_string(),
                    Value::Str(
                        "Rotoring gap is too large and causes an index below zero".to_string(),
                    ),
                );
                let ex = Value::make_instance(Symbol::intern("X::OutOfRange"), attrs);
                let mut err =
                    RuntimeError::new("X::OutOfRange: Rotoring gap is too large".to_string());
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
            pos = new_pos as usize;

            // Advance spec index
            match &spec.count {
                RotorCount::Range(counts) => {
                    range_sub_idx += 1;
                    if range_sub_idx >= counts.len() {
                        range_sub_idx = 0;
                        spec_idx += 1;
                    }
                }
                _ => {
                    spec_idx += 1;
                }
            }
        }

        Ok(Value::Seq(Arc::new(result)))
    }
}
