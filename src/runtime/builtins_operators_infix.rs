// Note: call_infix_routine is 476 lines on its own; adding required helpers
// (infix_uses_numeric_bridge, infix_is_strictly_numeric) and the two free
// functions it calls (parse_hyper_infix, assignment_metaop_base) brings this
// file to ~565 lines — marginally over the 500-line guideline, but unavoidable
// without splitting the function body itself.
use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// Whether `callable` is the captured multi family bound as `&{infix_name}`
    /// whose candidates `try_user_infix` has just declined: dispatching the
    /// family again would reach this built-in operator once more, forever
    /// (the same `multi infix:` imported by an `EXPORT` hook and a direct
    /// `use`).
    // Cost: O(1).
    pub(super) fn declined_captured_family(&self, infix_name: &str, callable: &Value) -> bool {
        self.user_infix_override(infix_name)
            && matches!(callable.view(), ValueView::Sub(data)
                if data.env.get_sym(crate::symbol::wk::multi_dispatch_candidates()).is_some())
    }

    pub(crate) fn call_infix_routine(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.call_infix_routine_in(op, args, false)
    }

    /// The core operator `op` applied to `args`, ignoring every candidate of
    /// the same name the program or an import declared (`&CORE::infix:<op>`).
    // Cost: as `call_infix_routine`.
    pub(crate) fn call_core_infix_routine(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.call_infix_routine_in(op, args, true)
    }

    /// The routine `name` of the CORE setting called with `args`: the built-in
    /// operator or routine, never a same-named declaration of the program.
    // Cost: O(1) dispatch beyond the built-in's own cost.
    pub(crate) fn call_core_routine(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(op) = name
            .strip_prefix("infix:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            return self.call_core_infix_routine(Self::normalize_unicode_infix(op), &args);
        }
        let (args, _) = self.sanitize_call_args_owned(args);
        match self.call_function_arms(name, args.clone(), false) {
            // A `Cool` routine (`abs`, `uc`, `sqrt`, ...) is its invocant's
            // method of the same name.
            Err(err) if err.is_multi_no_match() && !args.is_empty() => {
                let rest = args[1..].to_vec();
                self.call_method_with_values(args[0].clone(), name, rest)
            }
            result => result,
        }
    }

    // `core_only` skips the user candidates, the `&infix:<op>` env callable
    // and the expression-evaluation fallback (which would resolve the
    // operator by name again).
    fn call_infix_routine_in(
        &mut self,
        op: &str,
        args: &[Value],
        core_only: bool,
    ) -> Result<Value, RuntimeError> {
        // A plain binary operator on two operands needs none of the meta, set,
        // sequence or short-circuit handling below (#15847). The one operand
        // shape that still does is the parser's `method` marker.
        if args.len() == 2
            && let Some(plain) = super::builtins_operators_infix_pair::plain_infix(op)
            && !matches!(args[0].view(), ValueView::Str(s) if s.as_str() == "method")
        {
            return self.infix_pair(op, Some(plain), args[0].clone(), &args[1], core_only);
        }
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
        // Numeric operators whose only one-arg CORE candidate is `(\a)`: the lone
        // operand is numified as is (`infix:<+>($[1,2])` is 2), never spread.
        let is_one_arg_numify_op = matches!(op, "+" | "-" | "*" | "/" | "**" | "%" | "gcd" | "lcm");
        // 1-arg Iterable gets flattened (like +@foo slurpy), but not for set operators
        // which coerce their single argument to a QuantHash instead, nor for the
        // numeric operators above
        let args: Vec<Value> = if args.len() == 1 && !is_set_op && !is_one_arg_numify_op {
            match args[0].view() {
                ValueView::Array(items, ..) => items.to_vec(),
                ValueView::Hash(map) if matches!(op, "andthen" | "notandthen" | "orelse") => map
                    .iter()
                    .map(|(k, v)| Value::value_pair(Value::str(k.clone()), v.clone()))
                    .collect(),
                _ => args.to_vec(),
            }
        } else {
            args.to_vec()
        };
        if args.len() == 2 {
            match op {
                "(elem)" | "∈" => {
                    return self.eval_binary_with_junctions(
                        args[0].clone(),
                        args[1].clone(),
                        Self::eval_set_elem_values,
                    );
                }
                "(cont)" | "∋" => {
                    return self.eval_binary_with_junctions(
                        args[0].clone(),
                        args[1].clone(),
                        Self::eval_set_cont_values,
                    );
                }
                _ => {}
            }
        }
        if op == "x" || op == "xx" {
            return self.call_repeat_infix(op, &args);
        }
        // Parser normalization fallback: `method foo { ... }` can appear as
        // InfixFunc(name="foo", left=BareWord("method"), right=[ArrayLiteral(...)]).
        // Treat this as a Method object value.
        // (`BareWord("method")` evaluates to a plain `Str`, so only a `Str`
        // can be the marker; stringifying every operand to test for it
        // allocated on each operator call.)
        if matches!(args.first().map(Value::view), Some(ValueView::Str(s)) if s.as_str() == "method")
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("name".to_string(), Value::str(op.to_string()));
            attrs.insert("is_dispatcher".to_string(), Value::FALSE);
            let mut sig_attrs = std::collections::HashMap::new();
            sig_attrs.insert("params".to_string(), Value::array(Vec::new()));
            attrs.insert(
                "signature".to_string(),
                Value::make_instance(Symbol::intern("Signature"), sig_attrs),
            );
            attrs.insert("returns".to_string(), Value::package(Symbol::intern("Mu")));
            attrs.insert("of".to_string(), Value::package(Symbol::intern("Mu")));
            return Ok(Value::make_instance(Symbol::intern("Method"), attrs));
        }
        if args.is_empty() {
            return Ok(reduction_identity(op));
        }
        if args.len() == 1 {
            if op == "(|)" || op == "∪" {
                let arg0 = match args[0].view() {
                    ValueView::Scalar(inner) => inner,
                    _ => &args[0],
                };
                let is_lazy_union_input = |value: &Value| match value.view() {
                    ValueView::LazyList(_) => true,
                    ValueView::GenericRange { start, end, .. } => {
                        let is_infinite = |bound: &Value| match bound.view() {
                            ValueView::Num(n) => n.is_infinite(),
                            ValueView::Rat(_, d) | ValueView::FatRat(_, d) => d == 0,
                            ValueView::Mixin(inner, _) => match inner.view() {
                                ValueView::Num(n) => n.is_infinite(),
                                ValueView::Rat(_, d) | ValueView::FatRat(_, d) => d == 0,
                                _ => false,
                            },
                            _ => false,
                        };
                        is_infinite(start) || is_infinite(end)
                    }
                    _ => false,
                };
                if matches!(arg0.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
                {
                    return Err(RuntimeError::new("Exception"));
                }
                if is_lazy_union_input(arg0) {
                    return Err(RuntimeError::cannot_lazy("coerce"));
                }
                return Ok(coerce_value_to_quanthash(arg0));
            }
            if is_chain_comparison_op(op) {
                return Ok(Value::TRUE);
            }
            if op == "~" {
                // Buf/Blob: arity-1 infix:<~> is identity (returns the same Buf/Blob)
                if let ValueView::Instance { class_name, .. } = args[0].view()
                    && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
                {
                    return Ok(args[0].clone());
                }
                return Ok(Value::str(crate::runtime::utils::coerce_to_str(&args[0])));
            }
            // Set operators with single arg: coerce to appropriate set type
            if matches!(op, "(.)" | "⊍" | "(+)" | "⊎") {
                let mut arg0 = match args[0].view() {
                    ValueView::Scalar(inner) => inner.clone(),
                    _ => args[0].clone(),
                };
                // De-itemize arrays for set operator coercion (e.g. [(+)] @a
                // passes .item-wrapped list, which should be coerced as elements)
                let relist = if let ValueView::Array(items, kind) = arg0.view() {
                    kind.is_itemized().then(|| items.clone())
                } else {
                    None
                };
                if let Some(items) = relist {
                    arg0 = Value::array_with_kind(items, crate::value::ArrayKind::List);
                }
                // Single-arg multiply preserves the operand's mutability
                // (`[(.)] SetHash` -> BagHash); single-arg addition does not
                // (`[(+)] SetHash` -> Bag).
                let mutable = matches!(op, "(.)" | "⊍") && set_result_mutability(&arg0);
                let coerced = if matches!(arg0.view(), ValueView::Mix(..)) {
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
            if is_one_arg_numify_op && matches!(args[0].view(), ValueView::Array(..)) {
                return Ok(crate::value::radix_numeric::coerce_to_numeric(
                    args[0].clone(),
                ));
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
                    let right = args.last().cloned().unwrap_or(Value::NIL);
                    self.eval_sequence(left, right, exclude_end)?
                };

                if exclude_start {
                    // Remove the first element
                    let new_result = match result.view() {
                        ValueView::Array(items, kind) if !items.is_empty() => {
                            let new_items = items[1..].to_vec();
                            Some(if kind.is_real_array() {
                                Value::real_array(new_items)
                            } else {
                                Value::array(new_items)
                            })
                        }
                        ValueView::LazyList(ll) => Some(Value::lazy_list(crate::gc::Gc::new(
                            crate::value::LazyList::new_skip_first_pipe(Value::lazy_list(
                                ll.clone(),
                            )),
                        ))),
                        _ => None,
                    };
                    if let Some(new_result) = new_result {
                        result = new_result;
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
                        return Ok(Value::slip_arc(std::sync::Arc::new(vec![])));
                    }
                    acc = rhs.clone();
                }
                return Ok(acc);
            }
            "notandthen" => {
                let mut acc = args[0].clone();
                for rhs in &args[1..] {
                    if crate::runtime::types::value_is_defined(&acc) {
                        return Ok(Value::NIL);
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
                    acc = if let ValueView::Sub(_) = rhs.view() {
                        self.call_sub_value(rhs.clone(), vec![])?
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
                    let val = if let ValueView::Sub(_) = arg.view() {
                        self.call_sub_value(arg.clone(), vec![])?
                    } else {
                        arg.clone()
                    };
                    if val.truthy() {
                        if truthy_val.is_some() {
                            return Ok(Value::NIL);
                        }
                        truthy_val = Some(val);
                    }
                }
                return Ok(truthy_val.unwrap_or(Value::NIL));
            }
            _ => {}
        }
        // Set operators: check for Failure and lazy list values
        if is_set_op {
            for arg in &args {
                if let ValueView::Instance { class_name, .. } = arg.view()
                    && class_name == "Failure"
                {
                    return Err(RuntimeError::new("Exception"));
                }
                if crate::runtime::is_lazy_set_operand(arg) {
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
                match v.view() {
                    ValueView::Mix(..) => 2,
                    ValueView::Bag(..) => 1,
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
            acc = self.infix_pair(op, None, acc, rhs, core_only)?;
        }
        Ok(acc)
    }

    // `==`, `!=`, `<`, `>`, `<=`, `>=` and `<=>` are deliberately absent here:
    // they are intercepted earlier in `call_infix_routine` and routed through
    // the real operator body (`num_eq_values` etc.), so they never reach this
    // bridge.
    pub(super) fn infix_uses_numeric_bridge(&self, op: &str) -> bool {
        matches!(
            op,
            "+" | "-" | "*" | "/" | "%" | "**" | "cmp" | "before" | "after" | "min" | "max"
        )
    }

    /// Subset of [`Self::infix_uses_numeric_bridge`] for which a non-numeric string
    /// operand is an X::Str::Numeric Failure rather than a silent 0-coercion:
    /// the arithmetic operators. The generic comparators left in the bridge
    /// (`cmp`, `before`/`after`, `min`/`max`) compare strings as strings; the
    /// numeric comparisons (`==`, `<`, ...) never reach this bridge and carry
    /// the same check in their operator bodies (`num_eq_values` etc.).
    pub(super) fn infix_is_strictly_numeric(op: &str) -> bool {
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
    } else {
        let rest = op.strip_prefix('\u{00BB}')?;
        (rest, false)
    };
    let (inner, dwim_right) = if let Some(rest) = after_left.strip_suffix(">>") {
        (rest, true)
    } else if let Some(rest) = after_left.strip_suffix('\u{00BB}') {
        (rest, true)
    } else if let Some(rest) = after_left.strip_suffix("<<") {
        (rest, false)
    } else {
        let rest = after_left.strip_suffix('\u{00AB}')?;
        (rest, false)
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
