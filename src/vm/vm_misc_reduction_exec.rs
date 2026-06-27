use super::vm_misc_ops::*;
use super::*;

impl Interpreter {
    pub(super) fn exec_reduction_op(
        &mut self,
        code: &CompiledCode,
        op_idx: u32,
    ) -> Result<(), RuntimeError> {
        let op = Self::const_str(code, op_idx).to_string();
        // Support scan/meta reduction [\op] and negated forms like [!after].
        let (scan, op_no_scan) = if let Some(stripped) = op.strip_prefix('\\') {
            (true, stripped.to_string())
        } else {
            (false, op.clone())
        };
        // Only treat '!' as negation prefix when the remaining part is a known
        // operator (e.g. [!after], [!==], [!eqv]).  Operators like '!=' are their
        // own base operators and must not be split.
        const KNOWN_BASE_OPS: &[&str] = &[
            "+", "-", "*", "/", "%", "~", "||", "&&", "//", "%%", "**", "^^", "+&", "+|", "+^",
            "+<", "+>", "~&", "~|", "~^", "~<", "~>", "?&", "?|", "?^", "==", "!=", "<", ">", "<=",
            ">=", "<=>", "===", "=:=", "!=:=", "=>", "eqv", "eq", "ne", "lt", "gt", "le", "ge",
            "leg", "cmp", "~~", "min", "max", "gcd", "lcm", "and", "or", "not", "andthen",
            "orelse", "xor", "minmax", ",", "after", "before", "X", "Z", "x", "xx", "&", "|", "^",
            "o", "∘", "(-)", "∖", "(|)", "∪", "(&)", "∩", "(^)", "⊖", "(.)", "⊍", "(==)", "≡", "≢",
        ];
        let (negate, base_op) = if let Some(stripped) = op_no_scan.strip_prefix('!')
            && KNOWN_BASE_OPS.contains(&stripped)
        {
            (true, stripped.to_string())
        } else {
            (false, op_no_scan)
        };
        let mut base_op = if base_op == "∘" {
            "o".to_string()
        } else {
            base_op
        };
        // Handle lazy-scan short-circuit reduction compiled from ArrayLiteral.
        // The operator has prefix "_sc_" and the operand is an array of thunks.
        if let Some(sc_op) = base_op.strip_prefix("_sc_") {
            let list_value = self.stack.pop().unwrap_or(Value::Nil);
            let thunks: Vec<Value> = runtime::value_to_list(&list_value);
            return self.exec_scan_shortcircuit_reduction(sc_op, negate, scan, thunks);
        }
        let list_value = self.stack.pop().unwrap_or(Value::Nil);
        let input_is_lazy = crate::builtins::methods_0arg::is_value_lazy(&list_value);
        // For scan (triangle reduce) on infinite/lazy inputs, handle lazily
        // to avoid materializing the entire infinite range.
        if scan && input_is_lazy {
            return self.exec_lazy_scan_reduction(&base_op, negate, &list_value);
        }
        let mut list = if let Value::LazyList(ref ll) = list_value {
            self.force_lazy_list_vm(ll)?
        } else {
            runtime::value_to_list(&list_value)
        };
        if list.iter().any(|v| matches!(v, Value::Slip(_))) {
            let mut flattened = Vec::new();
            for item in list {
                if let Value::Slip(items) = item {
                    flattened.extend(items.iter().cloned());
                } else {
                    flattened.push(item);
                }
            }
            list = flattened;
        }
        // Reduction is list-contextual; when the operand itself is a single list-like
        // value, flatten that one value into reduction elements.
        if list.len() == 1 {
            let only = list.remove(0);
            list = match only {
                Value::Array(items, kind) if !kind.is_itemized() => items.iter().cloned().collect(),
                Value::Seq(items) => items.iter().cloned().collect(),
                Value::LazyList(ll) => {
                    if let Ok(items) = self.force_lazy_list_vm(&ll) {
                        items
                    } else {
                        vec![Value::LazyList(ll)]
                    }
                }
                other => vec![other],
            };
        }
        // The `R` (reverse) metaop on a reduction reverses the entire fold:
        // `[R op] @list` == `[op] @list.reverse` (and likewise for the scan form
        // `[\R op]`). Reversing the operand list and stripping the `R` yields the
        // correct result for non-commutative / non-associative ops (`-`, `/`),
        // where the per-step operand swap done by `eval_reduction_operator_values`
        // would instead compute `[op]` right-folded (e.g. `[R/] 100,10,2` is
        // `2/10/100` = 0.002, not `100/(10/2)` = 20).
        while let Some(inner) = base_op.strip_prefix('R') {
            if !inner.is_empty() && Self::is_builtin_reduction_op(inner) {
                list.reverse();
                base_op = inner.to_string();
            } else {
                break;
            }
        }
        if base_op == "," {
            if scan {
                let mut out = Vec::with_capacity(list.len());
                let mut prefix = Vec::new();
                for item in list {
                    prefix.push(item);
                    out.push(Value::array(prefix.clone()));
                }
                self.stack.push(Value::Seq(std::sync::Arc::new(out)));
            } else {
                self.stack.push(Value::array(list));
            }
            return Ok(());
        }
        let callable = self.reduction_callable_for_op(&base_op);
        let arity = callable
            .as_ref()
            .map(|c| self.reduction_callable_arity(c))
            .unwrap_or(2);
        let step = arity.saturating_sub(1).max(1);
        let assoc = if base_op == "=>" {
            ReductionAssoc::Right
        } else if runtime::is_chain_comparison_op(&base_op) {
            ReductionAssoc::Chain
        } else {
            self.reduction_op_associativity(&base_op)
        };

        if scan {
            if list.is_empty() {
                self.stack.push(Value::Seq(std::sync::Arc::new(Vec::new())));
                return Ok(());
            }
            if list.len() == 1 {
                let is_chain = runtime::is_chain_comparison_op(&base_op);
                let val = if is_chain {
                    Value::Bool(true)
                } else {
                    list[0].clone()
                };
                self.stack.push(Value::Seq(std::sync::Arc::new(vec![val])));
                return Ok(());
            }
            let out = match assoc {
                ReductionAssoc::Right => {
                    let mut out = Vec::new();
                    let mut acc = list.last().cloned().unwrap_or(Value::Nil);
                    out.push(acc.clone());
                    let mut right_edge = list.len().saturating_sub(1);
                    while right_edge >= step {
                        let start = right_edge - step;
                        let mut call_args = list[start..right_edge].to_vec();
                        call_args.push(acc);
                        let v =
                            self.reduction_step_with_args(&base_op, callable.as_ref(), call_args)?;
                        acc = if negate { Value::Bool(!v.truthy()) } else { v };
                        out.push(acc.clone());
                        right_edge = start;
                    }
                    out
                }
                _ => {
                    let is_chain = runtime::is_chain_comparison_op(&base_op);
                    let is_xor = matches!(base_op.as_str(), "^^" | "xor");
                    let mut out = Vec::new();
                    if is_chain {
                        // Chain comparison scan: first element is always True
                        // (vacuously true), then each subsequent element is the
                        // AND of all pairwise comparisons so far.
                        out.push(Value::Bool(true));
                        let mut all_true = true;
                        for i in 0..list.len() - 1 {
                            if all_true {
                                let v = self.eval_reduction_operator_values(
                                    &base_op,
                                    &list[i],
                                    &list[i + 1],
                                )?;
                                let truthy = if negate { !v.truthy() } else { v.truthy() };
                                all_true = truthy;
                            }
                            out.push(Value::Bool(all_true));
                        }
                    } else if is_xor {
                        // [\^^] and [\xor] scan: each element is the xor-reduce
                        // of the prefix up to that point.
                        let mut found: Option<Value> = None;
                        let mut multiple = false;
                        for item in &list {
                            if item.truthy() {
                                if found.is_some() {
                                    multiple = true;
                                }
                                if !multiple {
                                    found = Some(item.clone());
                                }
                            }
                            let result = if multiple {
                                Value::Nil
                            } else if let Some(ref v) = found {
                                v.clone()
                            } else {
                                item.clone()
                            };
                            out.push(result);
                        }
                    } else {
                        // Special case for Z/X meta-operators (e.g. [\Z~], [\X~])
                        // when scan elements are themselves lists. In this case a
                        // simple left-fold loses structure, so we re-apply the
                        // operator to the entire prefix at each step.
                        // E.g. [\Z~](<a b c>, <1 2 3>):
                        //   step 1 = [Z~](<a b c>)       = ("abc",)
                        //   step 2 = [Z~](<a b c>,<1 2 3>) = ("a1","b2","c3")
                        let is_multi_list_zx = callable.is_none()
                            && list
                                .iter()
                                .any(|v| matches!(v, Value::Array(_, _) | Value::Seq(_)))
                            && ((base_op.starts_with('Z') && base_op.len() > 1)
                                || (base_op.starts_with('X') && base_op.len() > 1));
                        if is_multi_list_zx {
                            // Compute prefix reductions from scratch at each step
                            for i in 0..list.len() {
                                let v = if i == 0 {
                                    // Single-element: apply inner_op as a left-fold
                                    // over the first list's elements, wrapped in Seq.
                                    let inner_op = &base_op[1..];
                                    let items = runtime::value_to_list(&list[0]);
                                    if items.is_empty() {
                                        Value::Seq(std::sync::Arc::new(vec![]))
                                    } else {
                                        let mut acc0 = items[0].clone();
                                        for item in items.iter().skip(1) {
                                            acc0 = self.eval_reduction_operator_values(
                                                inner_op, &acc0, item,
                                            )?;
                                        }
                                        Value::Seq(std::sync::Arc::new(vec![acc0]))
                                    }
                                } else {
                                    // Apply the Z/X op to all prefix elements
                                    let mut acc0 = list[0].clone();
                                    for item in list.iter().take(i + 1).skip(1) {
                                        acc0 = self.eval_reduction_operator_values(
                                            &base_op, &acc0, item,
                                        )?;
                                    }
                                    acc0
                                };
                                let result = if negate { Value::Bool(!v.truthy()) } else { v };
                                out.push(result);
                            }
                        } else {
                            let mut acc = list[0].clone();
                            // For certain operators, the first scan element should be
                            // the result of applying [op] to a single element, not the
                            // element itself:
                            //   Z~/X~ meta-operators: [Z~]("a") = ("a",) not "a"
                            //   minmax: [minmax](x) = x..x not x
                            if callable.is_none() {
                                let zx_prefix = (base_op.starts_with('Z') && base_op.len() > 1)
                                    || (base_op.starts_with('X') && base_op.len() > 1);
                                if zx_prefix {
                                    acc = Value::Seq(std::sync::Arc::new(vec![acc]));
                                } else if base_op == "minmax" {
                                    // [minmax](x) = x..x for scalars,
                                    // or min(x)..max(x) for array/list x.
                                    let (lo, hi) = minmax_bounds_of_value(&acc);
                                    acc = match (&lo, &hi) {
                                        (Value::Int(l), Value::Int(h)) => Value::Range(*l, *h),
                                        _ => Value::GenericRange {
                                            start: std::sync::Arc::new(lo),
                                            end: std::sync::Arc::new(hi),
                                            excl_start: false,
                                            excl_end: false,
                                        },
                                    };
                                }
                            }
                            out.push(acc.clone());
                            let mut idx = 1usize;
                            while idx + step <= list.len() {
                                let mut call_args = vec![acc];
                                call_args.extend(list[idx..idx + step].iter().cloned());
                                let v = self.reduction_step_with_args(
                                    &base_op,
                                    callable.as_ref(),
                                    call_args,
                                )?;
                                acc = if negate { Value::Bool(!v.truthy()) } else { v };
                                out.push(acc.clone());
                                idx += step;
                            }
                        }
                    }
                    out
                }
            };
            self.stack.push(Value::Seq(std::sync::Arc::new(out)));
            return Ok(());
        }
        // [^^] and [xor] are list-associative: they check that exactly one element
        // is truthy.  Returns:
        //   - the truthy value if exactly one is truthy
        //   - Nil if more than one is truthy (short-circuits)
        //   - the last element if all are falsy
        if matches!(base_op.as_str(), "^^" | "xor") {
            if list.is_empty() {
                self.stack.push(runtime::reduction_identity(&base_op));
                return Ok(());
            }
            let mut found: Option<Value> = None;
            let mut multiple = false;
            let mut last = Value::Nil;
            for item in &list {
                last = item.clone();
                if item.truthy() {
                    if found.is_some() {
                        multiple = true;
                        break;
                    }
                    found = Some(item.clone());
                }
            }
            let result = if multiple {
                Value::Nil
            } else if let Some(v) = found {
                v
            } else {
                last
            };
            self.stack.push(result);
            return Ok(());
        }
        // For set operators, promote all elements to the highest set type before reducing.
        // In Raku, [(-)] [Set, Set, Mix] first promotes all to Mix, then reduces.
        if matches!(
            base_op.as_str(),
            "(-)" | "∖" | "(|)" | "∪" | "(&)" | "∩" | "(^)" | "⊖" | "(.)" | "⊍" | "(+)" | "⊎"
        ) && list.len() > 2
        {
            let set_level = |v: &Value| -> u8 {
                match v {
                    Value::Mix(_, _) => 2,
                    Value::Bag(_, _) => 1,
                    _ => 0,
                }
            };
            let max_level = list.iter().map(&set_level).max().unwrap_or(0);
            if max_level > 0 {
                for item in &mut list {
                    let level = set_level(item);
                    if level < max_level {
                        let promoted = match max_level {
                            2 => self
                                .try_compiled_method_or_interpret(item.clone(), "Mix", vec![])
                                .unwrap_or_else(|_| item.clone()),
                            1 => self
                                .try_compiled_method_or_interpret(item.clone(), "Bag", vec![])
                                .unwrap_or_else(|_| item.clone()),
                            _ => item.clone(),
                        };
                        *item = promoted;
                    }
                }
            }
        }
        // Multi-arg symmetric difference is NOT a left-fold.
        // For each key, the result weight = max_weight - second_max_weight.
        if matches!(base_op.as_str(), "(^)" | "⊖") && list.len() > 2 {
            self.stack.push(runtime::set_sym_diff_multi(&list));
            return Ok(());
        }
        if list.is_empty() {
            self.stack.push(runtime::reduction_identity(&base_op));
        } else {
            // Chain-associative operators (built-in comparisons and user-defined
            // `is assoc<chain>` infixes) reduce as a conjunction of pairwise
            // applications, not a left-fold.
            let is_comparison =
                runtime::is_chain_comparison_op(&base_op) || matches!(assoc, ReductionAssoc::Chain);
            if is_comparison {
                let mut result = true;
                for i in 0..list.len() - 1 {
                    let v =
                        self.eval_reduction_operator_values(&base_op, &list[i], &list[i + 1])?;
                    let truthy = if negate { !v.truthy() } else { v.truthy() };
                    if !truthy {
                        result = false;
                        break;
                    }
                }
                self.stack.push(Value::Bool(result));
            } else {
                if base_op == "o" {
                    let mut acc = list[0].clone();
                    for item in &list[1..] {
                        acc = self.compose_callables(acc, item.clone());
                    }
                    self.stack.push(acc);
                    return Ok(());
                }
                // Single-element reduction with a numeric/coercing operator:
                // apply op(identity, element) so that coercions (e.g. numification
                // for `+`) happen and type errors (e.g. X::Str::Numeric for
                // `[+] "hello"`) are raised.  This matches Raku semantics where
                // `[+] "2"` returns Int 2 (not Str "2").
                if list.len() == 1
                    && callable.is_none()
                    && matches!(
                        base_op.as_str(),
                        "+" | "-" | "*" | "/" | "%" | "**" | "+|" | "+&" | "+^"
                    )
                {
                    // Validate: non-numeric strings must throw X::Str::Numeric
                    if let Value::Str(ref s) = list[0]
                        && crate::runtime::str_numeric::parse_raku_str_to_numeric(s).is_none()
                    {
                        return Err(RuntimeError::str_numeric(
                            s,
                            "base-10 number must begin with valid digits or '.'",
                        ));
                    }
                    // Coerce Instance values via Numeric()/Bridge() so that
                    // user-defined numeric types work (e.g. `[*] CustomNumify.new`).
                    let elem = self.coerce_numeric_bridge_value(list[0].clone())?;
                    // A single-element reduction returns the element *numified*,
                    // NOT `op(identity, element)`. The latter is only correct for
                    // the commutative `+`/`*` (where `0 + x == x`); for `-`/`/`/
                    // `**`/`%` it would wrongly compute `0 - 5`, `1 / 5`, etc.
                    // Numify via the additive identity so `[+] "2"` is Int 2,
                    // `[-] 5` is 5, and `[/] 5` is 5 (matching Rakudo).
                    let v = self.reduction_step_with_args("+", None, vec![Value::Int(0), elem])?;
                    let result = if negate { Value::Bool(!v.truthy()) } else { v };
                    self.stack.push(result);
                    return Ok(());
                }
                let acc = match assoc {
                    ReductionAssoc::Right => {
                        let mut acc = list.last().cloned().unwrap_or(Value::Nil);
                        let mut right_edge = list.len().saturating_sub(1);
                        while right_edge >= step {
                            let start = right_edge - step;
                            let mut call_args = list[start..right_edge].to_vec();
                            call_args.push(acc);
                            let v = self.reduction_step_with_args(
                                &base_op,
                                callable.as_ref(),
                                call_args,
                            )?;
                            acc = if negate { Value::Bool(!v.truthy()) } else { v };
                            right_edge = start;
                        }
                        acc
                    }
                    _ => {
                        let mut acc = list[0].clone();
                        let mut idx = 1usize;
                        while idx + step <= list.len() {
                            let mut call_args = vec![acc];
                            call_args.extend(list[idx..idx + step].iter().cloned());
                            let v = self.reduction_step_with_args(
                                &base_op,
                                callable.as_ref(),
                                call_args,
                            )?;
                            acc = if negate { Value::Bool(!v.truthy()) } else { v };
                            idx += step;
                        }
                        acc
                    }
                };
                self.stack.push(acc);
            }
        }
        Ok(())
    }
}
