use super::vm_misc_ops::*;
use super::*;

impl Interpreter {
    /// Whether `v` is a `Buf`/`Blob` instance. A `Blob` does NOT do `Iterable`
    /// in rakudo, so a reduction keeps it whole (`[~] $blob` IS the blob) even
    /// though list coercion (`for`, `.rotor`, `.list`) yields its bytes.
    fn value_is_buf(v: &Value) -> bool {
        matches!(
            v.view(),
            ValueView::Instance { attributes, .. }
                if crate::value::value_buf::has_buf_elems(&attributes)
        )
    }

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
        // Unicode operator aliases fold identically to their ASCII base op
        // (matching `eval_reduction_operator_values`): ∘→o, ×→*, ÷→/,
        // −(U+2212)→-, ≤→<=, ≥→>=, ≠→!=. Without this a `[×]`/`[÷]`/… fold
        // reaches an `infix:<×>` lookup that does not exist.
        let mut base_op = match base_op.as_str() {
            "\u{2218}" => "o".to_string(),
            "\u{00D7}" => "*".to_string(),
            "\u{00F7}" => "/".to_string(),
            "\u{2212}" => "-".to_string(),
            "\u{2264}" => "<=".to_string(),
            "\u{2265}" => ">=".to_string(),
            "\u{2260}" => "!=".to_string(),
            _ => base_op,
        };
        // Handle lazy-scan short-circuit reduction compiled from ArrayLiteral.
        // The operator has prefix "_sc_" and the operand is an array of thunks.
        if let Some(sc_op) = base_op.strip_prefix("_sc_") {
            let list_value = self.stack.pop().unwrap_or(Value::NIL);
            let thunks: Vec<Value> = runtime::value_to_list(&list_value);
            return self.exec_scan_shortcircuit_reduction(sc_op, negate, scan, thunks);
        }
        let list_value = self.stack.pop().unwrap_or(Value::NIL);
        // ADR-0058: `[+] (2..N).map({...})` folds over the mapped elements, so
        // a still-deferred `.map` operand has to run its callback first.
        self.reify_map_grep_seq(&list_value)?;
        let input_is_lazy = crate::builtins::methods_0arg::is_value_lazy(&list_value);
        // For scan (triangle reduce) on infinite/lazy inputs, handle lazily
        // to avoid materializing the entire infinite range.
        if scan && input_is_lazy {
            return self.exec_lazy_scan_reduction(&base_op, negate, &list_value);
        }
        let operand_is_buf = Self::value_is_buf(&list_value);
        // Nor does a Set/SetHash/Bag/BagHash/Mix/MixHash — `Set ~~ Iterable`
        // is False in rakudo — so a QuantHash operand is likewise ONE operand
        // that the one-arg rule then coerces, not a list of its pairs.
        // `[~] Set.new("a","b")` is `~Set.new("a","b")` ("a b"), and
        // `[+] bag(1,1,2)` is `bag(1,1,2).Numeric` (3); mutsu used to fold over
        // the decomposed `:a`/`:b` pairs and answer `"a\tTrueb\tTrue"` / `0`.
        // The deref matters: a `$`-lexical read yields a `ContainerRef`, which
        // `is_quanthash_instance` (a view match) would not see through.
        let derefed_operand = list_value.deref_container();
        let operand_is_quanthash = runtime::is_quanthash_instance(&derefed_operand);
        let mut list = if operand_is_buf {
            vec![list_value.clone()]
        } else if operand_is_quanthash {
            vec![derefed_operand]
        } else if let ValueView::LazyList(ll) = list_value.view() {
            self.force_lazy_list_vm(&ll)?
        } else {
            runtime::value_to_list(&list_value)
        };
        if list.iter().any(|v| matches!(v.view(), ValueView::Slip(_))) {
            let mut flattened = Vec::new();
            for item in list {
                if let ValueView::Slip(items) = item.view() {
                    flattened.extend(items.iter().cloned());
                } else {
                    flattened.push(item);
                }
            }
            list = flattened;
        }
        // ADR-0040 slices 1-2: a reduction's operands are the element VALUES of
        // the source list, so an element that is itemized *because it is an
        // element* is handed to the operator decontainerized. Measured on raku:
        // `my @m = [1,2],[3,4]; [Z] @m` is `((1, 3), (2, 4))` while the explicit
        // `@m[0] Z @m[1]` is `(($[1, 2], $[3, 4]),)` — the reduction reads the
        // values, the explicit infix receives the elements themselves.
        //
        // Only when the operand list was actually DECOMPOSED out of a container:
        // if `list_value` IS the single operand (`[+] @m[0]`, where
        // `value_to_list` keeps the itemized array whole), its own itemization
        // is not an element property and must survive — the same
        // receiver-vs-element distinction `value_to_list_for_receiver` draws.
        // The `len() > 1` guard keeps the one-arg rule below (which deliberately
        // does NOT flatten an itemized single operand) working unchanged.
        let decomposed = matches!(
            list_value.view(),
            ValueView::Array(_, kind) if !kind.is_itemized()
        ) || matches!(
            list_value.view(),
            ValueView::Seq(_) | ValueView::Slip(_) | ValueView::LazyList(_)
        );
        if decomposed && list.len() > 1 {
            list = list.into_iter().map(Value::deitemize_element).collect();
        }
        // Reduction is list-contextual; when the operand itself is a single list-like
        // value, flatten that one value into reduction elements.
        if list.len() == 1 {
            let only = list.remove(0);
            list = match only.view() {
                ValueView::Array(items, kind) if !kind.is_itemized() => {
                    items.iter().cloned().collect()
                }
                ValueView::Seq(items) => items.iter().cloned().collect(),
                ValueView::LazyList(ll) => {
                    if let Ok(items) = self.force_lazy_list_vm(&ll) {
                        items
                    } else {
                        vec![only.clone()]
                    }
                }
                _ => vec![only],
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
            // An explicit `&callable` inner (`[R[&f]]`) reverses too. It is
            // matched separately from the builtin table because a bare
            // identifier after `R` is ambiguous — a user may have declared
            // `infix:<Rfoo>` as well as `infix:<foo>` — while the `&` sigil
            // cannot be part of an operator name, so there is nothing to
            // confuse it with.
            let inner_is_callable =
                inner.starts_with('&') && self.reduction_callable_for_op(inner).is_some();
            if !inner.is_empty() && (Self::is_builtin_reduction_op(inner) || inner_is_callable) {
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
                self.stack.push(Value::seq(out));
            } else {
                self.stack.push(Value::array(list));
            }
            return Ok(());
        }
        let mut callable = self.reduction_callable_for_op(&base_op);
        // `my &op = &[+]; [[&op]] 5` is still a reduction with `infix:<+>`'s
        // identity, so unwrap a callable that merely names a builtin operator
        // back into that operator before any of the arity/associativity/
        // identity decisions below are taken.
        if let Some(op) = callable
            .as_ref()
            .and_then(|c| self.reduction_builtin_op_for_callable(c))
        {
            base_op = op;
            callable = None;
        }
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

        // Computed before the scan branch: `[\(|)] <a>, <a>`'s FIRST element is
        // `[(|)]("a")`, i.e. the same one-arg coercion the fold form applies.
        let is_set_op = Self::is_set_reduction_op(&base_op);
        if scan {
            if list.is_empty() {
                self.stack.push(Value::seq(Vec::new()));
                return Ok(());
            }
            if list.len() == 1 {
                let is_chain = runtime::is_chain_comparison_op(&base_op);
                let val = if is_chain {
                    Value::TRUE
                } else if is_set_op && callable.is_none() {
                    self.set_reduction_one_arg(&base_op, list[0].clone())?
                } else {
                    list[0].clone()
                };
                self.stack.push(Value::seq(vec![val]));
                return Ok(());
            }
            let out = match assoc {
                ReductionAssoc::Right => {
                    let mut out = Vec::new();
                    let mut acc = list.last().cloned().unwrap_or(Value::NIL);
                    out.push(acc.clone());
                    let mut right_edge = list.len().saturating_sub(1);
                    while right_edge >= step {
                        let start = right_edge - step;
                        let mut call_args = list[start..right_edge].to_vec();
                        call_args.push(acc);
                        let v =
                            self.reduction_step_with_args(&base_op, callable.as_ref(), call_args)?;
                        acc = if negate { Value::truth(!v.truthy()) } else { v };
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
                        out.push(Value::TRUE);
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
                            out.push(Value::truth(all_true));
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
                                Value::NIL
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
                            && list.iter().any(|v| {
                                matches!(v.view(), ValueView::Array(_, _) | ValueView::Seq(_))
                            })
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
                                        Value::seq(vec![])
                                    } else {
                                        let mut acc0 = items[0].clone();
                                        for item in items.iter().skip(1) {
                                            acc0 = self.eval_reduction_operator_values(
                                                inner_op, &acc0, item,
                                            )?;
                                        }
                                        Value::seq(vec![acc0])
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
                                let result = if negate { Value::truth(!v.truthy()) } else { v };
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
                                    acc = Value::seq(vec![acc]);
                                } else if is_set_op {
                                    acc = self.set_reduction_one_arg(&base_op, acc)?;
                                } else if base_op == "minmax" {
                                    // [minmax](x) = x..x for scalars,
                                    // or min(x)..max(x) for array/list x.
                                    let (lo, hi) = minmax_bounds_of_value(&acc);
                                    acc = match (lo.view(), hi.view()) {
                                        (ValueView::Int(l), ValueView::Int(h)) => {
                                            Value::range(l, h)
                                        }
                                        _ => Value::generic_range(lo, hi, false, false),
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
                                acc = if negate { Value::truth(!v.truthy()) } else { v };
                                out.push(acc.clone());
                                idx += step;
                            }
                        }
                    }
                    out
                }
            };
            self.stack.push(Value::seq(out));
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
            let mut last = Value::NIL;
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
                Value::NIL
            } else if let Some(v) = found {
                v
            } else {
                last
            };
            self.stack.push(result);
            return Ok(());
        }
        // A set operator classifies its operands by their Set/Bag/Mix type, so it
        // must see the VALUE a `$`-lexical holds, not the container. The
        // `deitemize_element` pass above strips a `Scalar`, but a plain lexical
        // read yields a `ContainerRef`, which it leaves alone -- so
        // `[(^)] $b1, $b2` classified both Bags as level 0 and returned
        // `Set.new(Bag, Bag)` (each Bag treated as one opaque element) where the
        // very same `$b1 (^) $b2` was correct, because the infix opcode receives
        // an already-dereferenced stack value.
        if is_set_op {
            for item in &mut list {
                *item = item.deref_container();
            }
            // The one-arg rule: `[(|)] $x` is `infix:<(|)>($x)`, which for
            // every set operator is a coercion (`[(|)] 3` is `Set.new(3)`,
            // `[(+)] Set.new("a")` is `("a"=>1).Bag`) -- not the bare operand
            // mutsu used to hand back.
            if list.len() == 1 && callable.is_none() {
                let coerced = self.set_reduction_one_arg(&base_op, list[0].clone())?;
                self.stack.push(coerced);
                return Ok(());
            }
        }
        // For set operators, promote all elements to the highest set type before reducing.
        // In Raku, [(-)] [Set, Set, Mix] first promotes all to Mix, then reduces.
        if is_set_op && list.len() > 2 {
            let set_level = |v: &Value| -> u8 {
                match v.view() {
                    ValueView::Mix(_, _) => 2,
                    ValueView::Bag(_, _) => 1,
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
            // A user-supplied operator has no identity element, so rakudo does
            // not answer an empty reduction from a table: it CALLS the routine
            // with no arguments and lets the binder complain ("Too few
            // positionals passed; expected 2 arguments but got 0"). Only
            // identity-bearing BUILTINS short-circuit -- and a callable that is
            // really a builtin was already unwrapped above.
            if let Some(c) = callable.clone() {
                let v = self.reduction_step_with_args(&base_op, Some(&c), Vec::new())?;
                let result = if negate { Value::truth(!v.truthy()) } else { v };
                self.stack.push(result);
                return Ok(());
            }
            self.stack.push(
                runtime::reduction_identity_opt(&base_op)
                    .unwrap_or_else(|| runtime::no_zero_arg_meaning_failure(&base_op)),
            );
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
                self.stack.push(Value::truth(result));
            } else {
                // The same rule for one element: `[myop] 5` is
                // `infix:<myop>(5)` (which dies on arity), and an arity-1
                // routine legitimately succeeds -- `[[&one]] 5` is `one(5)`.
                // mutsu used to hand back the lone element for EVERY operator,
                // which is only correct for the identity-bearing builtins
                // handled further down.
                if list.len() == 1
                    && let Some(c) = callable.clone()
                {
                    let v =
                        self.reduction_step_with_args(&base_op, Some(&c), vec![list[0].clone()])?;
                    let result = if negate { Value::truth(!v.truthy()) } else { v };
                    self.stack.push(result);
                    return Ok(());
                }
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
                    if let ValueView::Str(s) = list[0].view()
                        && crate::runtime::str_numeric::parse_raku_str_to_numeric(&s).is_none()
                    {
                        return Err(RuntimeError::str_numeric(
                            &s,
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
                    let v = self.reduction_step_with_args("+", None, vec![Value::int(0), elem])?;
                    let result = if negate { Value::truth(!v.truthy()) } else { v };
                    self.stack.push(result);
                    return Ok(());
                }
                // `~` is the same rule on the string side --
                // `multi sub infix:<~>(Any \a) { a.Str }` -- so `[~] 5` is the
                // Str "5", not the Int 5, and `[~] Set.new("a","b")` is the
                // set's `.Str` ("a b"). The one exception is `infix:<~>`'s own
                // `Blob:D` candidate, which returns the operand unchanged. That
                // is tested on the ELEMENT, not on the whole operand:
                // `my @chunks = Blob.new; [~] @chunks` arrives here with an
                // Array operand holding one Blob, so `operand_is_buf` is false
                // while the single element still must not be stringified.
                if list.len() == 1
                    && callable.is_none()
                    && base_op == "~"
                    && !Self::value_is_buf(&list[0])
                {
                    let v = self.reduction_step_with_args(
                        "~",
                        None,
                        vec![Value::str(String::new()), list[0].clone()],
                    )?;
                    let result = if negate { Value::truth(!v.truthy()) } else { v };
                    self.stack.push(result);
                    return Ok(());
                }
                let acc = match assoc {
                    ReductionAssoc::Right => {
                        let mut acc = list.last().cloned().unwrap_or(Value::NIL);
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
                            acc = if negate { Value::truth(!v.truthy()) } else { v };
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
                            acc = if negate { Value::truth(!v.truthy()) } else { v };
                            // `notandthen` short-circuits to Empty once a defined LHS
                            // produced it: the empty Slip is absorbing, so stop folding
                            // (a plain pairwise fold would wrongly resume, because an
                            // empty Slip is undefined and the next `notandthen` would
                            // pass its RHS through).
                            if base_op == "notandthen"
                                && matches!(acc.view(), ValueView::Slip(s) if s.is_empty())
                            {
                                break;
                            }
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
