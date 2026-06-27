// Note: call_infix_routine is 476 lines on its own; adding required helpers
// (infix_uses_numeric_bridge, infix_is_strictly_numeric) and the two free
// functions it calls (parse_hyper_infix, assignment_metaop_base) brings this
// file to ~565 lines — marginally over the 500-line guideline, but unavoidable
// without splitting the function body itself.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn call_infix_routine(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Hyper meta-operators called via their subroutine name, e.g.
        // `&infix:<<<+>>>(@a, @b)` / `&infix:<»+«>(@a, @b)`. Apply the inner
        // operator elementwise according to the dwim sides.
        if args.len() == 2
            && let Some((inner, dwim_left, dwim_right)) = parse_hyper_infix(op)
        {
            return self.apply_hyper_infix(inner, dwim_left, dwim_right, &args[0], &args[1]);
        }
        // Assignment meta-operator called as a routine, e.g. `&[+=]` / `&[~=]`.
        // When the left operand is a writable reference (passed by a hyper
        // function-op or any rw caller), compute the base op and write the
        // result back into the referenced variable, mirroring `$a op= $b`.
        if args.len() == 2
            && let Some(base) = assignment_metaop_base(op)
            && let Some((source_name, inner, _)) =
                crate::runtime::types::indexed_varref_from_value(&args[0])
        {
            let result = self.call_infix_routine(base, &[inner, args[1].clone()])?;
            self.env.insert(source_name, result.clone());
            return Ok(result);
        }
        let is_set_op = matches!(
            op,
            "(-)"
                | "∖"
                | "(|)"
                | "∪"
                | "(&)"
                | "∩"
                | "(.)"
                | "⊍"
                | "(+)"
                | "⊎"
                | "(^)"
                | "⊖"
                | "(elem)"
                | "∈"
                | "(cont)"
                | "∋"
                | "(<=)"
                | "⊆"
                | "(>=)"
                | "⊇"
                | "⊈"
                | "⊉"
                | "⊄"
                | "⊅"
                | "(<)"
                | "⊂"
                | "(>)"
                | "⊃"
                | "(==)"
                | "≡"
                | "≢"
        );
        // 1-arg Iterable gets flattened (like +@foo slurpy), but not for set operators
        // which coerce their single argument to a QuantHash instead
        let args: Vec<Value> = if args.len() == 1 && !is_set_op {
            match &args[0] {
                Value::Array(items, ..) => items.to_vec(),
                Value::Hash(map) if matches!(op, "andthen" | "notandthen" | "orelse") => map
                    .iter()
                    .map(|(k, v)| {
                        Value::ValuePair(Box::new(Value::str(k.clone())), Box::new(v.clone()))
                    })
                    .collect(),
                _ => args.to_vec(),
            }
        } else {
            args.to_vec()
        };
        if op == "x" || op == "xx" {
            return self.call_repeat_infix(op, &args);
        }
        // Parser normalization fallback: `method foo { ... }` can appear as
        // InfixFunc(name="foo", left=BareWord("method"), right=[ArrayLiteral(...)]).
        // Treat this as a Method object value.
        if !args.is_empty() && args[0].to_string_value() == "method" {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(op.to_string()));
            attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
            let mut sig_attrs = std::collections::HashMap::new();
            sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
            attrs.insert(
                "signature".to_string(),
                Value::make_instance(Symbol::intern("Signature"), sig_attrs),
            );
            attrs.insert("returns".to_string(), Value::Package(Symbol::intern("Mu")));
            attrs.insert("of".to_string(), Value::Package(Symbol::intern("Mu")));
            return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
        }
        if args.is_empty() {
            return Ok(reduction_identity(op));
        }
        if args.len() == 1 {
            if op == "(|)" || op == "∪" {
                let arg0 = match &args[0] {
                    Value::Scalar(inner) => inner.as_ref(),
                    other => other,
                };
                let is_lazy_union_input = |value: &Value| match value {
                    Value::LazyList(_) => true,
                    Value::GenericRange { start, end, .. } => {
                        let is_infinite = |bound: &Value| match bound {
                            Value::Num(n) => n.is_infinite(),
                            Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
                            Value::Mixin(inner, _) => match inner.as_ref() {
                                Value::Num(n) => n.is_infinite(),
                                Value::Rat(_, d) | Value::FatRat(_, d) => *d == 0,
                                _ => false,
                            },
                            _ => false,
                        };
                        is_infinite(start) || is_infinite(end)
                    }
                    _ => false,
                };
                if matches!(arg0, Value::Instance { class_name, .. } if class_name == "Failure") {
                    return Err(RuntimeError::new("Exception"));
                }
                if is_lazy_union_input(arg0) {
                    return Err(RuntimeError::cannot_lazy("coerce"));
                }
                return Ok(coerce_value_to_quanthash(arg0));
            }
            if is_chain_comparison_op(op) {
                return Ok(Value::Bool(true));
            }
            if op == "~" {
                // Buf/Blob: arity-1 infix:<~> is identity (returns the same Buf/Blob)
                if let Value::Instance { class_name, .. } = &args[0]
                    && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
                {
                    return Ok(args[0].clone());
                }
                return Ok(Value::str(crate::runtime::utils::coerce_to_str(&args[0])));
            }
            // Set operators with single arg: coerce to appropriate set type
            if matches!(op, "(.)" | "⊍" | "(+)" | "⊎") {
                let mut arg0 = match &args[0] {
                    Value::Scalar(inner) => inner.as_ref().clone(),
                    other => (*other).clone(),
                };
                // De-itemize arrays for set operator coercion (e.g. [(+)] @a
                // passes .item-wrapped list, which should be coerced as elements)
                if let Value::Array(items, kind) = &arg0
                    && kind.is_itemized()
                {
                    arg0 = Value::Array(items.clone(), crate::value::ArrayKind::List);
                }
                // Single-arg multiply preserves the operand's mutability
                // (`[(.)] SetHash` -> BagHash); single-arg addition does not
                // (`[(+)] SetHash` -> Bag).
                let mutable = matches!(op, "(.)" | "⊍") && set_result_mutability(&arg0);
                let coerced = if matches!(arg0, Value::Mix(_, _)) {
                    self.dispatch_to_mix(arg0)?
                } else {
                    self.dispatch_to_bag_with_what(arg0, "Bag")?
                };
                return Ok(with_set_mutability(coerced, mutable));
            }
            if matches!(op, "(-)" | "∖") {
                // Single-arg difference always yields the immutable variant
                // (`[(-)] SetHash` -> Set), unlike union/intersection which
                // preserve the operand's mutability.
                return Ok(with_set_mutability(
                    coerce_value_to_quanthash(&args[0]),
                    false,
                ));
            }
            if matches!(op, "(|)" | "∪" | "(&)" | "∩" | "(^)" | "⊖") {
                return Ok(coerce_value_to_quanthash(&args[0]));
            }
            return Ok(args[0].clone());
        }
        // Sequence operators: dispatch directly to eval_sequence
        match op {
            "..." | "...^" | "^..." | "^...^" => {
                let exclude_end = op == "...^" || op == "^...^";
                let exclude_start = op.starts_with('^');

                let mut result = if args.len() > 2 {
                    // Chained sequence: multiple waypoints
                    // args = [seed, waypoint1, waypoint2, ..., endpoint]
                    // Each waypoint can be a list: first element is endpoint for
                    // previous segment, whole list is seed for next segment.
                    self.eval_chained_sequence(&args, exclude_end)?
                } else {
                    let left = args[0].clone();
                    let right = args.last().cloned().unwrap_or(Value::Nil);
                    self.eval_sequence(left, right, exclude_end)?
                };

                if exclude_start {
                    // Remove the first element
                    match &result {
                        Value::Array(items, kind) if !items.is_empty() => {
                            let new_items = items[1..].to_vec();
                            result = if kind.is_real_array() {
                                Value::real_array(new_items)
                            } else {
                                Value::array(new_items)
                            };
                        }
                        Value::LazyList(ll) => {
                            let mut items = ll.cache.lock().unwrap().clone().unwrap_or_default();
                            if !items.is_empty() {
                                items.remove(0);
                            }
                            result = Value::LazyList(std::sync::Arc::new(
                                crate::value::LazyList::new_cached(items),
                            ));
                        }
                        _ => {}
                    }
                }
                return Ok(result);
            }
            _ => {}
        }
        // Short-circuit operators need special handling
        match op {
            "andthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if !crate::runtime::types::value_is_defined(&acc) {
                        // Return Empty (empty Slip) when LHS is not defined
                        return Ok(Value::Slip(std::sync::Arc::new(vec![])));
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            "notandthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Value::Nil);
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            "orelse" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(acc);
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            "//" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(acc);
                    }
                    // When RHS is a Sub, invoke it (thunking behavior)
                    acc = if let Value::Sub(_) = rhs {
                        self.call_sub_value(rhs.clone(), vec![], false)?
                    } else {
                        rhs.clone()
                    };
                }
                return Ok(acc);
            }
            "^^" | "xor" => {
                // XOR: exactly one operand must be truthy
                let mut truthy_val: Option<Value> = None;
                for arg in &args {
                    // Invoke Sub operands (thunking behavior)
                    let val = if let Value::Sub(_) = arg {
                        self.call_sub_value(arg.clone(), vec![], false)?
                    } else {
                        arg.clone()
                    };
                    if val.truthy() {
                        if truthy_val.is_some() {
                            return Ok(Value::Nil);
                        }
                        truthy_val = Some(val);
                    }
                }
                return Ok(truthy_val.unwrap_or(Value::Nil));
            }
            _ => {}
        }
        // Set operators: check for Failure and lazy list values
        if is_set_op {
            let is_lazy_value = |v: &Value| -> bool {
                match v {
                    Value::LazyList(_) => true,
                    Value::Range(_, end)
                    | Value::RangeExcl(_, end)
                    | Value::RangeExclStart(_, end)
                    | Value::RangeExclBoth(_, end) => *end == i64::MAX,
                    Value::GenericRange { end, .. } => match end.as_ref() {
                        Value::HyperWhatever => true,
                        Value::Num(n) => n.is_infinite() && n.is_sign_positive(),
                        _ => {
                            let n = end.to_f64();
                            n.is_infinite() && n.is_sign_positive()
                        }
                    },
                    _ => false,
                }
            };
            for arg in &args {
                if let Value::Instance { class_name, .. } = arg
                    && class_name == "Failure"
                {
                    return Err(RuntimeError::new("Exception"));
                }
                if is_lazy_value(arg) {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::str_from("Cannot coerce a lazy list onto a Set"),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), attrs);
                    let mut err = RuntimeError::new("Cannot coerce a lazy list onto a Set");
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
        }
        // A 3+-arg set reduction takes its result mutability from the first
        // operand (captured before promotion below may strip it).
        let multi_set_mutable = is_set_op && args.len() > 2 && set_result_mutability(&args[0]);
        // For set operators with 3+ args, promote all to the highest set type first.
        // In Raku, infix:<(-)>(Set, Set, Mix) promotes all to Mix before reducing.
        let args = if is_set_op && args.len() > 2 {
            let set_level = |v: &Value| -> u8 {
                match v {
                    Value::Mix(_, _) => 2,
                    Value::Bag(_, _) => 1,
                    _ => 0,
                }
            };
            let max_level = args.iter().map(&set_level).max().unwrap_or(0);
            if max_level > 0 {
                args.iter()
                    .map(|item| {
                        let level = set_level(item);
                        if level < max_level {
                            match max_level {
                                2 => self
                                    .call_method_with_values(item.clone(), "Mix", vec![])
                                    .unwrap_or_else(|_| item.clone()),
                                1 => self
                                    .call_method_with_values(item.clone(), "Bag", vec![])
                                    .unwrap_or_else(|_| item.clone()),
                                _ => item.clone(),
                            }
                        } else {
                            item.clone()
                        }
                    })
                    .collect()
            } else {
                args
            }
        } else {
            args
        };
        // Multi-arg symmetric difference is NOT a left-fold.
        // For each key, the result weight = max_weight - second_max_weight.
        if matches!(op, "(^)" | "⊖") && args.len() > 2 {
            return Ok(with_set_mutability(
                crate::runtime::utils::set_sym_diff_multi(&args),
                multi_set_mutable,
            ));
        }
        let mut acc = args[0].clone();
        for rhs in &args[1..] {
            let pair_args = vec![acc.clone(), rhs.clone()];
            let infix_name = format!("infix:<{}>", op);
            if let Some(def) = self.resolve_function_with_types(&infix_name, &pair_args) {
                crate::trace::trace_log!("call", "call_infix_routine dispatch def: {}", infix_name);
                acc = self.call_function_def(&def, &pair_args)?;
                continue;
            }
            if let Some(callable) = self.env.get(&format!("&{}", infix_name)).cloned()
                && matches!(
                    callable,
                    Value::Sub(_) | Value::WeakSub(_) | Value::Instance { .. } | Value::Mixin(..)
                )
            {
                crate::trace::trace_log!(
                    "call",
                    "call_infix_routine dispatch callable env: {}",
                    infix_name
                );
                acc = self.eval_call_on_value(callable, pair_args)?;
                continue;
            }
            crate::trace::trace_log!(
                "call",
                "call_infix_routine fallback reduce/eval: {}",
                infix_name
            );
            // Smart match needs the full interpreter for regex dispatch
            if op == "~~" {
                acc = Value::Bool(self.smart_match(&acc, rhs));
                continue;
            }
            if op == "!~~" {
                acc = Value::Bool(!self.smart_match(&acc, rhs));
                continue;
            }
            let mut lhs = acc.clone();
            let mut rhs = rhs.clone();
            if self.infix_uses_numeric_bridge(op) {
                // Genuinely-numeric ops reject non-numeric strings; the generic
                // comparators (cmp/before/after/min/max) compare them as strings.
                if Self::infix_is_strictly_numeric(op) {
                    crate::runtime::utils::check_str_numeric(&lhs)?;
                    crate::runtime::utils::check_str_numeric(&rhs)?;
                }
                lhs = self.coerce_infix_operand_numeric(lhs)?;
                rhs = self.coerce_infix_operand_numeric(rhs)?;
            }
            if let Ok(value) = Self::apply_reduction_op(op, &lhs, &rhs) {
                acc = value;
            } else {
                let (expr_op, expr_left, expr_right) =
                    if let Some(inner) = op.strip_prefix('R').filter(|inner| !inner.is_empty()) {
                        // Runtime autogen for callable reverse meta-ops (&infix:<Rop>):
                        // evaluate the inner operator with swapped operands.
                        (inner, rhs.clone(), acc)
                    } else {
                        (op, acc, rhs.clone())
                    };
                let expr_args = vec![expr_left.clone(), expr_right.clone()];
                let expr_infix_name = format!("infix:<{}>", expr_op);
                if let Some(def) = self.resolve_function_with_types(&expr_infix_name, &expr_args) {
                    crate::trace::trace_log!(
                        "call",
                        "call_infix_routine fallback dispatch def: {}",
                        expr_infix_name
                    );
                    acc = self.call_function_def(&def, &expr_args)?;
                    continue;
                }
                if let Some(callable) = self.env.get(&format!("&{}", expr_infix_name)).cloned()
                    && matches!(
                        callable,
                        Value::Sub(_)
                            | Value::WeakSub(_)
                            | Value::Instance { .. }
                            | Value::Mixin(..)
                    )
                {
                    crate::trace::trace_log!(
                        "call",
                        "call_infix_routine fallback dispatch callable env: {}",
                        expr_infix_name
                    );
                    acc = self.eval_call_on_value(callable, expr_args)?;
                    continue;
                }
                acc = self.eval_block_value(&[Stmt::Expr(Self::build_infix_expr(
                    expr_op, expr_left, expr_right,
                ))])?;
            }
        }
        Ok(acc)
    }

    pub(super) fn infix_uses_numeric_bridge(&self, op: &str) -> bool {
        matches!(
            op,
            "+" | "-"
                | "*"
                | "/"
                | "%"
                | "**"
                | "=="
                | "!="
                | "<"
                | ">"
                | "<="
                | ">="
                | "<=>"
                | "cmp"
                | "before"
                | "after"
                | "min"
                | "max"
        )
    }

    /// Subset of [`infix_uses_numeric_bridge`] for which a non-numeric string
    /// operand is an X::Str::Numeric error rather than a silent 0-coercion.
    /// Restricted to the arithmetic operators: numeric *comparison* (`==`/`<`/
    /// `<=>` …) is intentionally NOT strict because mutsu still models some
    /// enums (e.g. PromiseStatus `Broken`/`Kept`) as bare strings, so
    /// `$status == Broken` must keep comparing them leniently. Arithmetic on a
    /// non-numeric string is far rarer and is the case the spec exercises
    /// (`"5 foo" + 8`).
    fn infix_is_strictly_numeric(op: &str) -> bool {
        matches!(op, "+" | "-" | "*" | "/" | "%" | "**")
    }
}

/// Parse a hyper meta-operator name into `(inner_op, dwim_left, dwim_right)`.
/// Recognizes both ASCII (`<<`/`>>`) and Unicode (`«`/`»`) delimiters.
///
/// The dwimmy side (the one that may be recycled) is the one whose delimiter
/// points "outward" away from the operator. Verified against the reference
/// implementation:
///   `[1,2,3] »+» [4]`  => `5 6 7`   (right dwims; result length = left)
///   `[1] «+« [4,5,6]`  => `5 6 7`   (left dwims;  result length = right)
///   `[1] «+» [4,5,6]`  => dwim both (result length = max)
///   `[1] »+« [4,5,6]`  => non-dwimmy (length mismatch is an error)
/// So: left `«`/`<<` => dwim left; right `»`/`>>` => dwim right.
fn parse_hyper_infix(op: &str) -> Option<(&str, bool, bool)> {
    let (after_left, dwim_left) = if let Some(rest) = op.strip_prefix("<<") {
        (rest, true)
    } else if let Some(rest) = op.strip_prefix('\u{00AB}') {
        (rest, true)
    } else if let Some(rest) = op.strip_prefix(">>") {
        (rest, false)
    } else if let Some(rest) = op.strip_prefix('\u{00BB}') {
        (rest, false)
    } else {
        return None;
    };
    let (inner, dwim_right) = if let Some(rest) = after_left.strip_suffix(">>") {
        (rest, true)
    } else if let Some(rest) = after_left.strip_suffix('\u{00BB}') {
        (rest, true)
    } else if let Some(rest) = after_left.strip_suffix("<<") {
        (rest, false)
    } else if let Some(rest) = after_left.strip_suffix('\u{00AB}') {
        (rest, false)
    } else {
        return None;
    };
    if inner.is_empty() {
        return None;
    }
    Some((inner, dwim_left, dwim_right))
}

/// If `op` is an assignment meta-operator (`+=`, `~=`, `min=`, ...), return the
/// base operator. Comparison/identity operators that merely *end* in `=` are
/// excluded.
fn assignment_metaop_base(op: &str) -> Option<&str> {
    let base = op.strip_suffix('=')?;
    if base.is_empty() {
        return None;
    }
    // Exclude operators where the trailing `=` is part of the operator itself.
    if matches!(
        op,
        "==" | "!=" | "<=" | ">=" | "===" | "!==" | "=:=" | "!=:=" | "<=>"
    ) {
        return None;
    }
    // A base that itself ends in `=`/`<`/`>`/`!` is a comparison-like operator
    // (`==`, `<=`, `>=`, `!=`), not an assignment meta-op.
    if base.ends_with(['=', '<', '>', '!']) {
        return None;
    }
    Some(base)
}
