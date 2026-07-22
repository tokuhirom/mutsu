use super::*;
use crate::value::AttrMap;

impl Interpreter {
    pub(in crate::runtime) fn dispatch_first(
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
            match arg.view() {
                ValueView::Pair(key, value) if key == "v" => {
                    if !value.truthy() {
                        has_neg_v = true;
                    }
                }
                ValueView::Pair(key, value) if key == "end" => {
                    if value.truthy() {
                        has_end = true;
                    }
                }
                ValueView::Pair(key, value) if key == "k" => {
                    has_k = value.truthy();
                }
                ValueView::Pair(key, value) if key == "kv" => {
                    has_kv = value.truthy();
                }
                ValueView::Pair(key, value) if key == "p" => {
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
        // Check for conflicting adverbs (X::Adverb)
        {
            let adverb_count = has_k as u8 + has_kv as u8 + has_p as u8;
            if adverb_count > 1 {
                let mut names = Vec::new();
                if has_k {
                    names.push("k");
                }
                if has_kv {
                    names.push("kv");
                }
                if has_p {
                    names.push("p");
                }
                let adverb_list = names
                    .iter()
                    .map(|n| format!("'{}'", n))
                    .collect::<Vec<_>>()
                    .join(", ");
                let mut err = RuntimeError::new(format!(
                    "Unsupported combination of adverbs ({}) passed to first on\n'List'.",
                    adverb_list
                ));
                err.exception = Some(Box::new(Value::make_instance(
                    Symbol::intern("X::Adverb"),
                    std::collections::HashMap::new(),
                )));
                return Err(err);
            }
        }
        // Check for Bool matcher (X::Match::Bool)
        if matches!(
            positional.first().map(Value::view),
            Some(ValueView::Bool(_))
        ) {
            let mut err = RuntimeError::new("Cannot use Bool as a matcher");
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Match::Bool"),
                std::collections::HashMap::new(),
            )));
            return Err(err);
        }
        let func = positional.first().cloned();

        // Supply.first returns a new Supply containing the matched value
        if let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
            && class_name == "Supply"
        {
            return self.dispatch_supply_first(&(attributes).as_map(), func, has_end);
        }

        // A Blob/Buf iterates as its element bytes (unlike list assignment, which
        // keeps it as a single element), so `.first` scans the bytes.
        let items = Self::buf_as_byte_items(&target)
            .unwrap_or_else(|| crate::runtime::utils::value_to_list(&target));
        if let Some((idx, value)) = self.find_first_match_over_items(func, &items, has_end)? {
            return Ok(super::super::builtins_collection::format_first_result(
                idx, value, has_k, has_kv, has_p,
            ));
        }
        Ok(Value::NIL)
    }

    fn dispatch_supply_first(
        &mut self,
        attributes: &AttrMap,
        func: Option<Value>,
        has_end: bool,
    ) -> Result<Value, RuntimeError> {
        let source_values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
            let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                let mut a = HashMap::new();
                a.insert("emitted".to_string(), Value::array(Vec::new()));
                a.insert("done".to_string(), Value::FALSE);
                a
            });
            self.supply_emit_buffer.push(Vec::new());
            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
            self.supply_emit_buffer.pop().unwrap_or_default()
        } else {
            attributes
                .get("values")
                .and_then(|v| {
                    if let ValueView::Array(items, ..) = v.view() {
                        Some(items.to_vec())
                    } else {
                        None
                    }
                })
                .unwrap_or_default()
        };

        let result_values = if let Some((_, value)) =
            self.find_first_match_over_items(func, &source_values, has_end)?
        {
            vec![value]
        } else {
            Vec::new()
        };

        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(result_values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert(
            "live".to_string(),
            attributes.get("live").cloned().unwrap_or(Value::FALSE),
        );
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// `$n.polymod(@divisors)` — successive modular decomposition.
    pub(in crate::runtime) fn method_polymod(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn val_to_f64(v: &Value) -> f64 {
            match v.view() {
                ValueView::Int(n) => n as f64,
                ValueView::Num(n) => n,
                ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                ValueView::Bool(b) => {
                    if b {
                        1.0
                    } else {
                        0.0
                    }
                }
                ValueView::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                _ => 0.0,
            }
        }
        fn f64_to_val(n: f64) -> Value {
            if n.is_finite() && n == n.trunc() && n.abs() < i64::MAX as f64 {
                Value::int(n as i64)
            } else {
                Value::num(n)
            }
        }
        fn flatten_to_list(v: &Value) -> Vec<Value> {
            match v.view() {
                ValueView::Array(items, ..) => items.as_ref().clone().items,
                ValueView::Seq(items) | ValueView::Slip(items) => items.as_ref().clone(),
                ValueView::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
                _ => vec![v.clone()],
            }
        }
        let mut n = val_to_f64(target);
        // Flatten args into a list of divisors, tracking if any source is infinite
        let mut divisors = Vec::new();
        let mut has_infinite = false;
        for arg in args {
            match arg.view() {
                ValueView::LazyList(ll) => {
                    // A lazy divisor source (`gather {...}`, `.map`, a `…∞` sequence)
                    // must be reified. An infinite sequence keeps a non-empty cache
                    // of its produced prefix; polymod pulls from it and stops once n
                    // reaches 0 (the has_infinite path below). A FINITE unforced
                    // source (a plain `gather` block) has an empty cache, so force it
                    // fully — otherwise its elements never become divisors and the
                    // number falls through unchanged (`600.polymod(gather {...})`
                    // wrongly returned `(600)`).
                    let cached = ll.cache.lock().unwrap().clone().unwrap_or_default();
                    if cached.is_empty() {
                        let items = self.force_lazy_list_bridge(&ll)?;
                        divisors.extend(items);
                    } else {
                        has_infinite = true;
                        divisors.extend(cached);
                    }
                }
                ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_) => {
                    divisors.extend(flatten_to_list(arg));
                }
                _ => divisors.push(arg.clone()),
            }
        }
        // Exact path: when the invocant and every divisor are non-negative
        // Int/Rat and the list is finite, do the decomposition in exact rational
        // arithmetic so results like `5.Rat.polymod(.3, .2)` stay `(0.2 0 80)`
        // instead of accreting float noise. Any operand outside that domain (Num,
        // BigRat, negatives, a zero divisor, an infinite source, or an i128
        // overflow) falls through to the float loop below unchanged.
        if !has_infinite && let Some(exact) = polymod_exact(target, &divisors) {
            return Ok(Value::seq(exact));
        }
        let mut result = Vec::new();
        let mut stopped = false;
        for d in &divisors {
            let d_val = val_to_f64(d);
            if d_val == 0.0 {
                result.push(f64_to_val(n));
                n = f64::INFINITY;
                continue;
            }
            // For infinite lists, stop when n reaches 0
            if has_infinite && n == 0.0 {
                stopped = true;
                break;
            }
            // Modulo 1 always yields remainder 0 and quotient = n; for infinite lists
            // this would loop forever, so push n directly and stop
            if has_infinite && d_val == 1.0 {
                result.push(f64_to_val(n));
                stopped = true;
                break;
            }
            let rem = n % d_val;
            let quot = ((n - rem) / d_val).trunc();
            result.push(f64_to_val(rem));
            n = quot;
        }
        if !stopped {
            result.push(f64_to_val(n));
        }
        Ok(Value::seq(result))
    }

    pub(in crate::runtime) fn dispatch_tree(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // Non-iterable: .tree(anything) returns self
        let items = match target.view() {
            ValueView::Array(items, ..) => items.clone(),
            _ => return Ok(target),
        };

        let arg = &args[0];
        match arg.view() {
            // .tree(0) — identity
            ValueView::Int(0) => Ok(target),
            // .tree(n) — tree to n levels
            ValueView::Int(n) if n > 0 => Ok(
                crate::builtins::methods_0arg::dispatch_core_math::tree_to_depth(
                    &target, n as usize,
                ),
            ),
            // .tree(*) or .tree(Inf) — full depth (same as .tree())
            ValueView::Whatever => Ok(
                crate::builtins::methods_0arg::dispatch_core_math::tree_to_depth(
                    &target,
                    usize::MAX,
                ),
            ),
            ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => Ok(
                crate::builtins::methods_0arg::dispatch_core_math::tree_to_depth(
                    &target,
                    usize::MAX,
                ),
            ),
            // .tree([&first, *@rest]) — array of closures form
            ValueView::Array(closure_list, ..) => {
                if closure_list.is_empty() {
                    return Ok(target);
                }
                let first = &closure_list[0];
                self.call_sub_value(first.clone(), vec![target], false)
            }
            // .tree(&closure, ...) — apply closures at depth levels
            ValueView::Sub(_) => {
                let closures: Vec<Value> = args.to_vec();
                self.tree_with_closures(&items, &closures, 0)
            }
            _ => Ok(target),
        }
    }

    pub(in crate::runtime) fn tree_with_closures(
        &mut self,
        items: &[Value],
        closures: &[Value],
        depth: usize,
    ) -> Result<Value, RuntimeError> {
        let mut processed = Vec::new();
        for item in items {
            match item.view() {
                ValueView::Array(inner, ..) if closures.len() > depth + 1 => {
                    let sub_result = self.tree_with_closures(&inner, closures, depth + 1)?;
                    processed.push(sub_result);
                }
                ValueView::Array(inner, ..) => {
                    // Apply the last closure to leaf arrays
                    if let Some(closure) = closures.last()
                        && closures.len() > 1
                    {
                        processed.push(self.call_sub_value(
                            closure.clone(),
                            vec![Value::array_with_kind(inner.clone(), ArrayKind::List)],
                            false,
                        )?);
                    } else {
                        processed.push(Value::array_with_kind(inner.clone(), ArrayKind::List)); // already Arc-wrapped
                    }
                }
                _ => processed.push(item.clone()),
            }
        }
        // Apply the closure at this depth level
        if let Some(closure) = closures.get(depth) {
            self.call_sub_value(closure.clone(), vec![Value::array(processed)], false)
        } else {
            Ok(Value::array(processed))
        }
    }
}

/// A non-negative Int/Rat as an exact rational `(num, den)` with `den > 0`.
/// Returns `None` for any other value (Num, BigInt/BigRat, negatives), which
/// signals `polymod_exact` to bail to the float path.
fn polymod_rat(v: &Value) -> Option<(i128, i128)> {
    match v.view() {
        ValueView::Int(n) if n >= 0 => Some((n as i128, 1)),
        ValueView::Rat(n, d) if n >= 0 && d > 0 => Some((n as i128, d as i128)),
        ValueView::Bool(b) => Some((if b { 1 } else { 0 }, 1)),
        _ => None,
    }
}

/// Build an exact `Value` from a rational, reducing to lowest terms and
/// preferring `Int` when the denominator is 1. Returns `None` if the reduced
/// numerator/denominator do not fit `i64` (mutsu's `Rat` payload width), so the
/// caller can fall back to the float path rather than truncating.
fn polymod_rat_value(num: i128, den: i128) -> Option<Value> {
    debug_assert!(den > 0);
    let g = gcd_i128(num.abs(), den);
    let g = if g == 0 { 1 } else { g };
    let n = num / g;
    let d = den / g;
    if d == 1 {
        i64::try_from(n).ok().map(Value::int)
    } else {
        let n = i64::try_from(n).ok()?;
        let d = i64::try_from(d).ok()?;
        Some(crate::value::make_rat(n, d))
    }
}

fn gcd_i128(mut a: i128, mut b: i128) -> i128 {
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a.abs()
}

/// Exact rational `polymod`: `n.polymod(d1, d2, ...)` yields
/// `(n mod d1, (n div d1) mod d2, ..., final quotient)`, all computed without
/// floating point. Returns `None` (bail to float) when any operand is out of
/// the non-negative Int/Rat domain, a divisor is zero, or an intermediate
/// value overflows `i128` / does not fit back into a `Rat`.
fn polymod_exact(target: &Value, divisors: &[Value]) -> Option<Vec<Value>> {
    let (mut nn, mut nd) = polymod_rat(target)?;
    let mut result = Vec::with_capacity(divisors.len() + 1);
    for d in divisors {
        let (dn, dd) = polymod_rat(d)?;
        if dn == 0 {
            return None; // zero divisor: leave to the float path's own handling
        }
        // q = floor( (nn/nd) / (dn/dd) ) = floor( (nn*dd) / (nd*dn) ); all terms
        // are non-negative here, so truncating division already floors.
        let p = nn.checked_mul(dd)?;
        let qden = nd.checked_mul(dn)?;
        let q = p / qden;
        // r = n - q*d = (nn*dd - q*dn*nd) / (nd*dd)
        let r_num = nn
            .checked_mul(dd)?
            .checked_sub(q.checked_mul(dn)?.checked_mul(nd)?)?;
        let r_den = nd.checked_mul(dd)?;
        result.push(polymod_rat_value(r_num, r_den)?);
        // The carried quotient is always an integer.
        nn = q;
        nd = 1;
    }
    result.push(polymod_rat_value(nn, nd)?);
    Some(result)
}
