use super::*;

#[derive(Debug, Clone, Copy)]
pub(super) enum OpAssoc {
    Left,
    Right,
    Chain,
}

impl Interpreter {
    pub(super) fn callable_produce_arity(&self, callable: &Value) -> usize {
        let (params, param_defs) = self.callable_signature(callable);
        if !param_defs.is_empty() {
            let mut total = 0usize;
            let mut required = 0usize;
            for pd in &param_defs {
                if pd.named
                    || pd.slurpy
                    || pd.double_slurpy
                    || pd.traits.iter().any(|t| t == "invocant")
                {
                    continue;
                }
                total += 1;
                let is_required = pd.required || (!pd.optional_marker && pd.default.is_none());
                if is_required {
                    required += 1;
                }
            }
            if required >= 2 {
                return required;
            }
            if total >= 2 {
                return total;
            }
        }
        params.len().max(2)
    }

    pub(super) fn callable_produce_assoc(&self, callable: &Value) -> OpAssoc {
        let callable_name: Option<String> = match callable.view() {
            ValueView::Sub(data) => Some(data.name.resolve()),
            ValueView::Routine { name, .. } => Some(name.resolve()),
            _ => None,
        };
        if let Some(name) = &callable_name
            && let Some(assoc) = self.infix_associativity(name)
        {
            return match assoc.as_str() {
                "right" => OpAssoc::Right,
                "chain" => OpAssoc::Chain,
                _ => OpAssoc::Left,
            };
        }
        Self::op_associativity(callable)
    }

    pub(super) fn eval_produce_over_items(
        &mut self,
        callable: Value,
        items: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        if items.is_empty() {
            return Ok(Vec::new());
        }
        if items.len() == 1 {
            return Ok(vec![items[0].clone()]);
        }

        let arity = self.callable_produce_arity(&callable);
        let step = arity.saturating_sub(1).max(1);
        let assoc = self.callable_produce_assoc(&callable);

        // `last`/`next` in the producer block follow Rakudo's one-behind emit
        // order: each accumulator value is emitted only after the NEXT reducer
        // call completes. So `last` on call N truncates the output to the
        // initial element plus calls 1..N-2 (the pending value is dropped too:
        // `(2,3,4,5).produce({ last if $^a > 7; $^a + $^b })` is `(2 5)`, and a
        // first-call `last` yields `()`), while `next` skips that step's emit
        // and keeps the accumulator unchanged.
        match assoc {
            OpAssoc::Right => {
                let mut out = Vec::new();
                let mut acc = items.last().cloned().unwrap_or(Value::NIL);
                let mut right_edge = items.len().saturating_sub(1);
                while right_edge >= step {
                    let start = right_edge - step;
                    let mut call_args = items[start..right_edge].to_vec();
                    call_args.push(acc.clone());
                    match self.call_sub_value(callable.clone(), call_args, true) {
                        Ok(new_acc) => {
                            out.push(acc);
                            acc = new_acc;
                        }
                        Err(e) if e.is_last() => return Ok(out),
                        Err(e) if e.is_next() => {}
                        Err(e) => return Err(e),
                    }
                    right_edge = start;
                }
                out.push(acc);
                Ok(out)
            }
            _ => {
                let mut out = Vec::new();
                let mut acc = items[0].clone();
                let mut idx = 1usize;
                while idx + step <= items.len() {
                    let mut call_args = vec![acc.clone()];
                    call_args.extend(items[idx..idx + step].iter().cloned());
                    match self.call_sub_value(callable.clone(), call_args, true) {
                        Ok(new_acc) => {
                            out.push(acc);
                            acc = new_acc;
                        }
                        Err(e) if e.is_last() => return Ok(out),
                        Err(e) if e.is_next() => {}
                        Err(e) => return Err(e),
                    }
                    idx += step;
                }
                out.push(acc);
                Ok(out)
            }
        }
    }

    pub(super) fn builtin_reduce(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let callable = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("reduce expects a callable as first argument"))?;

        // `reduce &f, +values` follows the one-arg rule: a SINGLE list argument
        // supplies the item list (flattened), but with several arguments each
        // one is its own item — `reduce &f, (0, 0), |@nums` keeps the seed
        // tuple `(0, 0)` as one value (the reduce-with-destructuring shape).
        let mut items = Vec::new();
        if args.len() == 2 {
            let arg = &args[1];
            if matches!(arg.view(), ValueView::Hash(_)) {
                items.push(arg.clone());
            } else {
                items.extend(crate::runtime::value_to_list(arg));
            }
        } else {
            items.extend(args.iter().skip(1).cloned());
        }
        self.reduce_items(callable, items)
    }

    /// Check if the first positional parameter of a callable has the `is raw` trait.
    fn callable_has_raw_first_param(&self, callable: &Value) -> bool {
        let (_, param_defs) = self.callable_signature(callable);
        param_defs
            .first()
            .map(|pd| pd.traits.iter().any(|t| t == "raw"))
            .unwrap_or(false)
    }

    pub(crate) fn reduce_items(
        &mut self,
        callable: Value,
        items: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if items.is_empty() {
            // An empty reduce returns the operator's identity element (`0` for
            // `+`, `""` for `~`, `1` for `*`, `True` for chaining comparisons, …),
            // matching raku (`[+] ()` is `0`; `().reduce(&infix:<+>)` is `0`;
            // `reduce &infix:<*>, ()` is `1`). An operator with no known identity
            // (a bare block) yields Nil. An *undefined* invocant is caught earlier
            // by `dispatch_reduce_method` and returns Nil regardless of operator.
            let op_name = match callable.view() {
                ValueView::Sub(data) => Some(data.name.resolve()),
                ValueView::Routine { name, .. } => Some(name.resolve()),
                _ => None,
            };
            return Ok(match op_name {
                Some(op) => {
                    let bare_op = op
                        .strip_prefix("infix:<")
                        .and_then(|s| s.strip_suffix('>'))
                        .unwrap_or(op.as_str());
                    crate::runtime::reduction_identity(bare_op)
                }
                None => Value::NIL,
            });
        }
        if items.len() == 1 {
            // A single-element reduce returns the element's *value* coerced to the
            // operator's natural type — NOT `identity op elem` (that would negate
            // for `-`, invert for `/`, etc.). Raku: `[+] "2"` is `2` (Int), `[-] 10`
            // is `10`, `[~] 5` is `"5"` (Str). Numeric operators coerce the element
            // to Numeric; `~` (and other string ops) coerce to Str; everything else
            // (e.g. `[,]`, `[max]`, chaining ops) returns the element unchanged.
            let op_name = match callable.view() {
                ValueView::Sub(data) => Some(data.name.resolve()),
                ValueView::Routine { name, .. } => Some(name.resolve()),
                _ => None,
            };
            let elem = items.into_iter().next().unwrap();
            if let Some(ref op) = op_name {
                // Strip infix:<...> wrapper if present
                let bare_op = op
                    .strip_prefix("infix:<")
                    .and_then(|s| s.strip_suffix('>'))
                    .unwrap_or(op.as_str());
                if matches!(
                    bare_op,
                    "+" | "-" | "*" | "/" | "%" | "**" | "+|" | "+&" | "+^"
                ) {
                    // Validate non-numeric strings (raku throws on `[+] "x"`).
                    if let ValueView::Str(s) = elem.view()
                        && crate::runtime::str_numeric::parse_raku_str_to_numeric(&s).is_none()
                    {
                        return Err(RuntimeError::str_numeric(
                            &s,
                            "base-10 number must begin with valid digits or '.'",
                        ));
                    }
                    return Ok(crate::runtime::utils::coerce_to_numeric(elem));
                }
                if matches!(bare_op, "~" | "x") {
                    return Ok(Value::str(elem.to_str_context()));
                }
            }
            return Ok(elem);
        }

        let arity = self.reduce_callable_arity(&callable);
        let step = arity.saturating_sub(1).max(1);
        let assoc = self.callable_reduce_assoc(&callable);
        let is_thunky = Self::is_thunky_reduce_op(&callable);
        let is_raw = self.callable_has_raw_first_param(&callable);

        // Save and set autovivify flag for `is raw` reduce callbacks.
        let saved_autovivify = self.hash_autovivify;
        if is_raw {
            self.hash_autovivify = true;
        }

        let result = match assoc {
            OpAssoc::Right => {
                let mut acc = items.last().cloned().unwrap();
                let mut right_edge = items.len().saturating_sub(1);
                let mut out = Ok(());
                while right_edge >= step {
                    let start = right_edge - step;
                    let mut call_args = items[start..right_edge].to_vec();
                    call_args.push(acc.clone());
                    right_edge = start;
                    // `last` inside the reduce block stops and keeps the current
                    // accumulator; `next` skips this step, also keeping the accumulator.
                    match self.call_sub_value(callable.clone(), call_args, true) {
                        Ok(v) => {
                            acc = if is_thunky {
                                match Self::dethunk(self, v) {
                                    Ok(d) => d,
                                    Err(e) => {
                                        out = Err(e);
                                        break;
                                    }
                                }
                            } else {
                                v
                            };
                        }
                        Err(e) if e.is_last() => break,
                        Err(e) if e.is_next() => continue,
                        Err(e) => {
                            out = Err(e);
                            break;
                        }
                    }
                }
                out.map(|()| acc)
            }
            OpAssoc::Chain => {
                let mut result = true;
                for i in 0..items.len() - 1 {
                    let v = self.call_sub_value(
                        callable.clone(),
                        vec![items[i].clone(), items[i + 1].clone()],
                        true,
                    )?;
                    if !v.truthy() {
                        result = false;
                        break;
                    }
                }
                Ok(Value::truth(result))
            }
            OpAssoc::Left => {
                let mut acc = items[0].clone();
                let mut idx = 1usize;
                let mut out = Ok(());
                while idx + step <= items.len() {
                    let mut call_args = vec![acc.clone()];
                    call_args.extend(items[idx..idx + step].iter().cloned());
                    idx += step;
                    // `last` inside the reduce block stops and keeps the current
                    // accumulator; `next` skips this step, also keeping the accumulator.
                    match self.call_sub_value(callable.clone(), call_args, true) {
                        Ok(v) => {
                            acc = if is_thunky {
                                match Self::dethunk(self, v) {
                                    Ok(d) => d,
                                    Err(e) => {
                                        out = Err(e);
                                        break;
                                    }
                                }
                            } else {
                                v
                            };
                        }
                        Err(e) if e.is_last() => break,
                        Err(e) if e.is_next() => continue,
                        Err(e) => {
                            out = Err(e);
                            break;
                        }
                    }
                }
                out.map(|()| acc)
            }
        };

        self.hash_autovivify = saved_autovivify;
        result
    }

    fn is_thunky_reduce_op(callable: &Value) -> bool {
        let name = match callable.view() {
            ValueView::Routine { name, .. } => name.resolve(),
            ValueView::Sub(data) => data.name.resolve(),
            _ => return false,
        };
        matches!(
            name.as_str(),
            "infix:<&&>" | "infix:<||>" | "infix:<and>" | "infix:<or>"
        )
    }

    fn dethunk(&mut self, val: Value) -> Result<Value, RuntimeError> {
        match val.view() {
            ValueView::Sub(_) | ValueView::WeakSub(_) => self.call_sub_value(val, vec![], false),
            _ => Ok(val),
        }
    }

    pub(super) fn reduce_callable_arity(&self, callable: &Value) -> usize {
        let (params, param_defs) = self.callable_signature(callable);
        if !param_defs.is_empty() {
            let mut total = 0usize;
            let mut required = 0usize;
            for pd in &param_defs {
                if pd.named
                    || pd.slurpy
                    || pd.double_slurpy
                    || pd.traits.iter().any(|t| t == "invocant")
                {
                    continue;
                }
                total += 1;
                let is_required = pd.required || (!pd.optional_marker && pd.default.is_none());
                if is_required {
                    required += 1;
                }
            }
            if required >= 2 {
                return required;
            }
            if total >= 2 {
                return total;
            }
        }
        params.len().max(2)
    }

    pub(super) fn callable_reduce_assoc(&self, callable: &Value) -> OpAssoc {
        // Check the name in the operator_assoc map first (handles `is assoc<...>` trait)
        let name = match callable.view() {
            ValueView::Sub(data) => Some(data.name.resolve()),
            ValueView::Routine { name, .. } => Some(name.resolve()),
            _ => None,
        };
        if let Some(ref name_str) = name {
            if let Some(assoc) = self.infix_associativity(name_str) {
                return match assoc.as_str() {
                    "right" => OpAssoc::Right,
                    "chain" => OpAssoc::Chain,
                    _ => OpAssoc::Left,
                };
            }
            // Also try the infix:<name> form
            let infix_name = format!("infix:<{}>", name_str);
            if let Some(assoc) = self.infix_associativity(&infix_name) {
                return match assoc.as_str() {
                    "right" => OpAssoc::Right,
                    "chain" => OpAssoc::Chain,
                    _ => OpAssoc::Left,
                };
            }
        }
        Self::op_associativity(callable)
    }

    pub(super) fn builtin_produce(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let callable = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("produce expects a callable as first argument"))?;
        if !matches!(
            callable.view(),
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
        ) {
            return Err(RuntimeError::new(
                "produce expects a callable as first argument",
            ));
        }

        let mut items = Vec::new();
        for arg in args.iter().skip(1) {
            if matches!(arg.view(), ValueView::Hash(_)) {
                items.push(arg.clone());
            } else {
                items.extend(crate::runtime::value_to_list(arg));
            }
        }
        let out = self.eval_produce_over_items(callable, items)?;
        Ok(Value::seq(out))
    }

    /// zip:with — zip lists using a custom combining function.
    pub(super) fn builtin_zip_with(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut raw_inputs: Vec<&Value> = Vec::new();
        let mut with_fn: Option<Value> = None;
        for arg in args {
            if let ValueView::Pair(key, val) = arg.view()
                && key == "with"
            {
                with_fn = Some((*val).clone());
                continue;
            }
            raw_inputs.push(arg);
        }
        let is_lazy_input = |v: &Value| -> bool {
            matches!(v.view(), ValueView::LazyList(_))
                || matches!(v.view(),
                    ValueView::Range(_, end)
                    | ValueView::RangeExcl(_, end)
                    | ValueView::RangeExclStart(_, end)
                    | ValueView::RangeExclBoth(_, end) if end == i64::MAX)
                || matches!(v.view(), ValueView::GenericRange { end, .. } if {
                    let f = end.to_f64();
                    f.is_infinite() && f.is_sign_positive()
                })
        };
        let all_lazy = raw_inputs.iter().all(|v| is_lazy_input(v));
        let lists: Vec<Vec<Value>> = raw_inputs
            .iter()
            .map(|v| crate::runtime::value_to_list(v))
            .collect();
        let combiner = with_fn.ok_or_else(|| RuntimeError::new("zip: missing :with argument"))?;
        if lists.is_empty() {
            return Ok(Value::array(vec![]));
        }
        // Determine associativity from the operator name
        let assoc = Self::op_associativity(&combiner);
        // Check if the combiner has special multi-arg semantics (e.g. set
        // symmetric difference) that requires passing all elements at once
        // rather than folding pairwise.
        let use_multi_arg = Self::combiner_needs_multi_arg(&combiner);
        let max_expand: usize = 1_000;
        let min_len = lists
            .iter()
            .map(|l| l.len())
            .min()
            .unwrap_or(0)
            .min(max_expand);
        let mut result = Vec::with_capacity(min_len);
        for i in 0..min_len {
            let elements: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
            let combined = if elements.len() <= 1 {
                elements.into_iter().next().unwrap_or(Value::NIL)
            } else if elements.len() > 2 && use_multi_arg {
                // Pass all elements at once for operators with special
                // multi-arg semantics (e.g. set symmetric difference).
                self.call_sub_value(combiner.clone(), elements, false)?
            } else {
                match assoc {
                    OpAssoc::Right => {
                        // Right-associative: fold from right
                        let mut acc = elements.last().unwrap().clone();
                        for elem in elements[..elements.len() - 1].iter().rev() {
                            acc = self.call_sub_value(
                                combiner.clone(),
                                vec![elem.clone(), acc],
                                false,
                            )?;
                        }
                        acc
                    }
                    OpAssoc::Chain => {
                        // Chain-associative: all pairwise comparisons must be true
                        let mut all_true = true;
                        for pair in elements.windows(2) {
                            let r = self.call_sub_value(
                                combiner.clone(),
                                vec![pair[0].clone(), pair[1].clone()],
                                false,
                            )?;
                            if !r.truthy() {
                                all_true = false;
                                break;
                            }
                        }
                        Value::truth(all_true)
                    }
                    OpAssoc::Left => {
                        // Left-associative (default): fold from left
                        let mut acc = elements[0].clone();
                        for elem in &elements[1..] {
                            acc = self.call_sub_value(
                                combiner.clone(),
                                vec![acc, elem.clone()],
                                false,
                            )?;
                        }
                        acc
                    }
                }
            };
            result.push(combined);
        }
        if all_lazy {
            Ok(Value::lazy_list(crate::gc::Gc::new(
                crate::value::LazyList::new_cached(result),
            )))
        } else {
            Ok(Value::array(result))
        }
    }

    /// Check if a combiner operator needs all elements passed at once
    /// (multi-arg semantics) rather than pairwise folding. This is true
    /// for set operators like (^)/⊖ where multi-arg behavior differs
    /// from left-fold.
    fn combiner_needs_multi_arg(func: &Value) -> bool {
        let name_str = match func.view() {
            ValueView::Routine { name, .. } => name.resolve(),
            _ => return false,
        };
        let op = name_str
            .strip_prefix("infix:<")
            .and_then(|s: &str| s.strip_suffix('>'))
            .unwrap_or(&name_str);
        matches!(
            op,
            "(^)" | "⊖" | "(-)" | "∖" | "(|)" | "∪" | "(&)" | "∩" | "(.)" | "⊍" | "(+)" | "⊎"
        )
    }

    /// Determine the associativity of an operator from its name.
    pub(super) fn op_associativity(func: &Value) -> OpAssoc {
        let name_str = match func.view() {
            ValueView::Routine { name, .. } => name.resolve(),
            _ => return OpAssoc::Left,
        };
        // Extract the operator from "infix:<op>"
        let op = name_str
            .strip_prefix("infix:<")
            .and_then(|s: &str| s.strip_suffix('>'))
            .map(|s| s.to_string())
            .unwrap_or_else(|| name_str.clone());
        match op.as_str() {
            "**" => OpAssoc::Right,
            "=" | ":=" | "=>" | "x" | "xx" => OpAssoc::Right,
            "eqv" | "===" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "eq" | "ne" | "lt" | "gt"
            | "le" | "ge" | "~~" | "=~=" | "=:=" | "!=:=" => OpAssoc::Chain,
            _ => OpAssoc::Left,
        }
    }
}
