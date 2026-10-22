use super::*;
use crate::value::AttrMap;

/// How many divisors an infinite `polymod` divisor list can consume for the
/// invocant `target`: one per bit of its integer magnitude (every divisor
/// >= 2 at least halves it), plus slack.
fn polymod_digit_bound(target: &Value) -> usize {
    let bits = match target.view() {
        ValueView::Int(i) => 64 - i.unsigned_abs().leading_zeros() as usize,
        ValueView::BigInt(b) => b.bits() as usize,
        _ => {
            let f = target.to_f64().abs();
            if f.is_finite() && f >= 1.0 {
                f.log2() as usize + 1
            } else {
                0
            }
        }
    };
    bits + 64
}

impl Interpreter {
    /// Cost: O(i), i = index of the first match (distance from the end for
    /// `:end`), on an Array, a List or a reified Seq: the receiver is scanned in
    /// doubling chunks, so only O(i) elements are decomposed and i+1 matcher
    /// calls run. O(e + i), e = elements, on any other list-like (decomposed
    /// whole first). O(1) on a live Supply: it only registers a grep and a
    /// head stage on the supplier.
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
                // Rakudo's `first` reads its adverbs out of `%_` and *validates*
                // them rather than letting the implicit slurpy swallow an
                // unknown one, so an undeclared named is an `X::Adverb` -- a
                // soft `fail`, unlike `grep`'s throw. Without this the Pair fell
                // into the positional vector and became the matcher
                // (`(1,2,3).first(:zzz)` answered `Nil`) or was silently ignored
                // (`(1,2,3).first(* > 1, :zzz)` answered `2`).
                ValueView::Pair(key, _) => {
                    return Ok(RuntimeError::unexpected_adverb_failure(
                        std::slice::from_ref(key),
                        "first",
                        crate::runtime::utils::value_type_name(&target),
                    ));
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
            return Err(RuntimeError::match_bool(".first"));
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
        //
        // A mutable array is scanned through its element CONTAINERS, so the
        // matcher's topic aliases the element rather than a copy of its value:
        // `@a.first({ $_ = 5 })` writes `@a`, exactly as `.grep`/`.map` and
        // `@a.values.first(...)` already do. Every other receiver (a `List`, a
        // `Seq`, a native or multi-dimensional array) keeps the bare-item scan.
        let found = if let Some(bytes) = Self::buf_as_byte_items(&target) {
            self.find_first_match_over_items(func, &bytes, has_end)?
        } else if Self::promotable_array_len(&target).is_some() || Self::first_borrows(&target) {
            self.find_first_match_chunked(&target, func, has_end)?
        } else if !has_end
            && let Some(mut steps) = crate::runtime::unbounded_range::Steps::new(&target)
        {
            // An unbounded range of any element type, stepped by `.succ` in
            // doubling chunks; `:k`/`:kv`/`:p` keep the absolute index.
            let mut offset = 0usize;
            let mut chunk_len = 64usize;
            loop {
                let chunk = steps.take(chunk_len);
                if chunk.is_empty() {
                    break None;
                }
                if let Some((idx, value)) =
                    self.find_first_match_over_items(func.clone(), &chunk, false)?
                {
                    break Some((offset + idx, value));
                }
                offset += chunk.len();
                chunk_len = (chunk_len * 2).min(1 << 16);
            }
        } else {
            let items = crate::runtime::utils::value_to_list_for_receiver(&target);
            self.find_first_match_over_items(func, &items, has_end)?
        };
        if let Some((idx, value)) = found {
            return Ok(super::super::builtins_collection::format_first_result(
                idx,
                // `.first` answers the element's VALUE; the container above is
                // the matcher's binding, not the result.
                value.deref_container(),
                has_k,
                has_kv,
                has_p,
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
        // A live (Supplier-backed) source has emitted nothing yet when
        // `.first` is called, so its `values` snapshot is empty. Build the
        // pipeline rakudo does instead (`self.grep(|c).head`, or
        // `self.grep(|c).tail` for `:end`): a live grep stage, then a head(1)
        // stage that emits the first match and is done -- or a tail(1) stage
        // that emits the last one when the source is done (#11839).
        if !attributes.contains_key("shared_on_demand")
            && crate::runtime::native_methods::supplier_id_from_attrs(attributes).is_some()
        {
            let stage = if has_end { "tail" } else { "head" };
            let grepped = match func {
                Some(matcher) => self.make_live_transform_supply(
                    attributes,
                    matcher,
                    crate::runtime::native_methods::TransformMode::Grep,
                ),
                None => None,
            };
            return match grepped.as_ref().map(Value::view) {
                Some(ValueView::Instance { attributes, .. }) => {
                    self.native_supply(&attributes.as_map(), stage, Vec::new())
                }
                _ => self.native_supply(attributes, stage, Vec::new()),
            };
        }
        let source_values = if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
            let emitter = Value::make_instance(Symbol::intern("Supplier"), {
                let mut a = HashMap::new();
                a.insert("emitted".to_string(), Value::array(Vec::new()));
                a.insert("done".to_string(), Value::FALSE);
                a
            });
            self.async_state
                .supply_emit_buffer
                .push(EmitFrame::default());
            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter]);
            self.async_state
                .supply_emit_buffer
                .pop()
                .unwrap_or_default()
                .values
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
        fn flatten_to_list(v: &Value) -> Vec<Value> {
            match v.view() {
                ValueView::Array(items, ..) => items.as_ref().clone().into_items(),
                ValueView::Seq(items) => items.to_vec(),
                ValueView::Slip(items) => items.as_ref().clone(),
                ValueView::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
                _ => vec![v.clone()],
            }
        }
        // A mixed-in number (`$n but Role`) divides the value it wraps;
        // otherwise it read as 0 and an infinite divisor list stopped at once.
        let unwrapped;
        let target = match target.view() {
            ValueView::Mixin(inner, _) => {
                unwrapped = inner.as_ref().clone();
                &unwrapped
            }
            _ => target,
        };
        // Flatten args into a list of divisors, tracking if any source is infinite
        let mut divisors = Vec::new();
        let mut has_infinite = false;
        for arg in args {
            match arg.view() {
                ValueView::LazyList(ll) => {
                    // A lazy divisor source (`gather {...}`, `.map`, `xx *`, a
                    // `...` sequence). A genuinely infinite one is pulled only as
                    // far as the decomposition can reach: with every divisor
                    // >= 2 each step at least halves the number, so its bit
                    // length (plus slack) bounds the digits (a divisor of 1 or 0
                    // stops the loop below on its own). A finite source (a
                    // plain `gather` block) is forced whole -- otherwise its
                    // elements never become divisors and the number falls
                    // through unchanged (`600.polymod(gather {...})` wrongly
                    // returned `(600)`).
                    if ll.is_genuinely_lazy() || ll.is_infinite_spec() {
                        has_infinite = true;
                        let needed = polymod_digit_bound(target);
                        // Pulled element by element: a cache-only lazy list
                        // (`lazy 2, 3`) has no generator to extend it, and
                        // runs out before `needed` like a finite list.
                        for i in 0..needed {
                            match self.pull_source_element(arg, i)? {
                                Some(d) => divisors.push(d),
                                None => break,
                            }
                        }
                    } else {
                        divisors.extend(self.force_lazy_list_bridge(&ll)?);
                    }
                }
                // A plain gather (ADR-11678) may be infinite: it is pulled
                // through its cached body only as far as the decomposition
                // can reach (the bound above), as Rakudo reads it lazily. One
                // that ends first is a finite divisor list.
                _ if Self::is_gather_seq(arg) => {
                    let needed = polymod_digit_bound(target);
                    let mut pulled = 0;
                    while pulled < needed {
                        match self.pull_source_element(arg, pulled)? {
                            Some(d) => divisors.push(d),
                            None => break,
                        }
                        pulled += 1;
                    }
                    has_infinite |= pulled == needed;
                }
                ValueView::Array(..) | ValueView::Seq(_) => {
                    divisors.extend(flatten_to_list(arg));
                }
                ValueView::Slip(_) => {
                    divisors.extend(flatten_to_list(arg));
                }
                _ => divisors.push(arg.clone()),
            }
        }
        // A Str divisor is the number it numifies to (`3.5.polymod("2")`
        // divides by the Int 2); one that is no number raises
        // `X::Str::Numeric` for a fractional receiver, whose `mod` numifies
        // it. `Int.polymod` leaves such a divisor to the loop below.
        let receiver_is_int = matches!(
            target.view(),
            ValueView::Int(_) | ValueView::BigInt(_) | ValueView::Bool(_)
        );
        for d in &mut divisors {
            let text = match d.view() {
                ValueView::Str(text) => text.to_string(),
                _ => continue,
            };
            match crate::value::str_numeric::parse_raku_str_to_numeric(&text) {
                Some(number) => *d = number,
                None if !receiver_is_int => {
                    return Err(crate::builtins::methods_0arg::str_numeric_error(&text));
                }
                None => {}
            }
        }
        // Exact path: when the invocant and every divisor are non-negative
        // Int/Rat and the list is finite, do the decomposition in exact rational
        // arithmetic so results like `5.Rat.polymod(.3, .2)` stay `(0.2 0 80)`
        // instead of accreting float noise. Any operand outside that domain (Num,
        // BigRat, negatives, a zero divisor, an infinite source, or an i128
        // overflow) falls through to the `mod`/`div` loop below.
        if let Some(exact) = polymod_exact_int(target, &divisors, has_infinite) {
            return Ok(Value::seq(exact));
        }
        if let Some(exact) = polymod_exact(target, &divisors, has_infinite) {
            return Ok(Value::seq(exact));
        }
        // Rakudo's `Real.polymod`: a negative invocant is a `Failure`, a zero
        // divisor throws, and each step is the shared `mod`/`div` (ADR-0118), so
        // an `Int` over a fractional divisor takes `mod`'s `Real, Real`
        // candidate and `div`'s `Int` coercion (`10.polymod(2.5)` is
        // `(-2.5, 5)`). A `Num` invocant decomposes in floating point
        // (`(n - rem) / d`), and a zero element is always an `Int`.
        let is_zero = |v: &Value| match v.view() {
            ValueView::Int(i) => i == 0,
            ValueView::Num(f) => f == 0.0,
            ValueView::Rat(n, _) | ValueView::FatRat(n, _) => n == 0,
            _ => false,
        };
        let is_negative = |v: &Value| match v.view() {
            ValueView::Int(i) => i < 0,
            ValueView::BigInt(b) => b.sign() == num_bigint::Sign::Minus,
            ValueView::Num(f) => f < 0.0,
            ValueView::Rat(n, _) | ValueView::FatRat(n, _) => n < 0,
            _ => false,
        };
        if is_negative(target) {
            return Ok(RuntimeError::out_of_range_failure(
                "invocant to polymod",
                target.clone(),
                "0..^Inf",
            ));
        }
        let mut n = target.clone();
        let mut result = Vec::new();
        let mut stopped = false;
        for d in &divisors {
            if is_zero(d) {
                return Err(RuntimeError::numeric_divide_by_zero_full(
                    Some(n.clone()),
                    Some("polymod"),
                ));
            }
            // For infinite lists, stop when n reaches 0
            if has_infinite && is_zero(&n) {
                stopped = true;
                break;
            }
            // Modulo 1 always yields remainder 0 and quotient = n; for infinite lists
            // this would loop forever, so push n directly and stop
            if has_infinite && matches!(d.view(), ValueView::Int(1)) {
                result.push(n.clone());
                stopped = true;
                break;
            }
            let (rem, quot) = if matches!(n.view(), ValueView::Num(_)) {
                let rem = crate::builtins::arith::arith_mod(n.clone(), d.clone())?;
                let diff = crate::builtins::arith::arith_sub(n.clone(), rem.clone());
                let quot = crate::builtins::arith::arith_div(diff, d.clone())?;
                (rem, quot)
            } else {
                let rem = crate::builtins::arith::arith_mod_op(n.clone(), d.clone())?;
                let quot = crate::builtins::arith::int_div(&n, d);
                (rem, quot)
            };
            // A `Failure` step (a fractional divisor that coerces to the `Int` 0,
            // `7.polymod(0.5)`) ends the decomposition, leaving the number as it
            // stood, as in Rakudo.
            let failed = |v: &Value| {
                matches!(v.view(), ValueView::Instance { class_name, .. } if class_name == "Failure")
            };
            if failed(&rem) || failed(&quot) {
                break;
            }
            let zero_to_int = |v: Value| {
                if is_zero(&v) { Value::int(0) } else { v }
            };
            result.push(zero_to_int(rem));
            n = zero_to_int(quot);
        }
        if !stopped {
            result.push(n);
        }
        Ok(Value::seq(result))
    }

    pub(crate) fn dispatch_tree(
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
            ValueView::Int(n) if n > 0 => {
                Ok(crate::value::flat::tree_to_depth(&target, n as usize))
            }
            // .tree(*) or .tree(Inf) — full depth (same as .tree())
            ValueView::Whatever => Ok(crate::value::flat::tree_to_depth(&target, usize::MAX)),
            ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                Ok(crate::value::flat::tree_to_depth(&target, usize::MAX))
            }
            // .tree([&first, *@rest]) — array of closures form
            ValueView::Array(closure_list, ..) => {
                if closure_list.is_empty() {
                    return Ok(target);
                }
                let first = &closure_list[0];
                self.call_sub_value(first.clone(), vec![target])
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
                            vec![Value::array_with_kind(inner.clone(), ArrayKind::List)])?);
                    } else {
                        processed.push(Value::array_with_kind(inner.clone(), ArrayKind::List)); // already Arc-wrapped
                    }
                }
                _ => processed.push(item.clone()),
            }
        }
        // Apply the closure at this depth level
        if let Some(closure) = closures.get(depth) {
            self.call_sub_value(closure.clone(), vec![Value::array(processed)])
        } else {
            Ok(Value::array(processed))
        }
    }
}

/// A non-negative integer operand as a `BigInt`. `None` for anything else — a
/// `Num`, a `Rat`, or a negative — which sends `polymod` on to the rational and
/// float paths below.
fn polymod_int(v: &Value) -> Option<num_bigint::BigInt> {
    use num_bigint::{BigInt, Sign};
    match v.view() {
        ValueView::Int(n) if n >= 0 => Some(BigInt::from(n)),
        ValueView::BigInt(n) if n.as_ref().sign() != Sign::Minus => Some(n.as_ref().clone()),
        ValueView::Bool(b) => Some(BigInt::from(u8::from(b))),
        _ => None,
    }
}

/// Exact integer `polymod`, in arbitrary precision.
///
/// The rational path below is capped at `i128`, so an invocant wider than that
/// — `parse-base($md5_hex, 16).polymod(256 xx *)`, a 128-bit digest — overflowed
/// it and fell through to the `f64` loop, which cannot represent the number at
/// all and produced zeros (or, for an infinite divisor list, nothing). Integer
/// operands are the overwhelmingly common case, so they get their own `BigInt`
/// loop ahead of the rational one.
///
/// `has_infinite` marks a divisor list that continues past the elements reified
/// so far (`256 xx *`): the decomposition then stops as soon as the invocant is
/// exhausted, and no final quotient is appended.
fn polymod_exact_int(target: &Value, divisors: &[Value], has_infinite: bool) -> Option<Vec<Value>> {
    use num_traits::{One, Zero};
    let mut n = polymod_int(target)?;
    let mut result = Vec::with_capacity(divisors.len() + 1);
    for d in divisors {
        let d = polymod_int(d)?;
        if d.is_zero() {
            // Zero divisor: leave the float path's own INFINITY handling in place.
            return None;
        }
        if has_infinite {
            if n.is_zero() {
                return Some(result);
            }
            // Every further step would divide by one and never terminate.
            if d.is_one() {
                result.push(Value::from_bigint(n));
                return Some(result);
            }
        }
        let (q, r) = (&n / &d, &n % &d);
        result.push(Value::from_bigint(r));
        n = q;
    }
    result.push(Value::from_bigint(n));
    Some(result)
}

/// A non-negative Int/Rat as an exact rational `(num, den)` with `den > 0`.
/// Returns `None` for any other value (Num, BigInt/BigRat, negatives), which
/// signals `polymod_exact` to bail to the float path.
fn polymod_rat(v: &Value) -> Option<(i128, i128)> {
    match v.view() {
        ValueView::Int(n) if n >= 0 => Some((n as i128, 1)),
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) if n >= 0 && d > 0 => {
            Some((n as i128, d as i128))
        }
        ValueView::Bool(b) => Some((if b { 1 } else { 0 }, 1)),
        _ => None,
    }
}

/// Build an exact `Value` from a rational, reducing to lowest terms and
/// preferring `Int` when the denominator is 1. Returns `None` if the reduced
/// numerator/denominator do not fit `i64` (mutsu's `Rat` payload width), so the
/// caller can fall back to the float path rather than truncating.
///
/// `kind` is the invocant's type: Rakudo keeps a `Rat`/`FatRat` invocant's
/// non-zero elements rational even when whole (`5.Rat.polymod(.3, .2)` ends in
/// `80.0`), while a zero element is always an `Int`.
fn polymod_rat_value(num: i128, den: i128, kind: PolymodKind) -> Option<Value> {
    debug_assert!(den > 0);
    let g = gcd_i128(num.abs(), den);
    let g = if g == 0 { 1 } else { g };
    let n = num / g;
    let d = den / g;
    let n = i64::try_from(n).ok()?;
    let d = i64::try_from(d).ok()?;
    if n == 0 {
        return Some(Value::int(0));
    }
    match kind {
        PolymodKind::FatRat => Some(Value::fat_rat_raw(n, d)),
        PolymodKind::Rat => Some(Value::rat_raw(n, d)),
        PolymodKind::Int if d == 1 => Some(Value::int(n)),
        PolymodKind::Int => Some(crate::value::make_rat(n, d)),
    }
}

/// The numeric type of a `polymod` invocant, which decides the type of the
/// whole-number elements of the exact rational path.
#[derive(Clone, Copy)]
enum PolymodKind {
    Int,
    Rat,
    FatRat,
}

fn gcd_i128(mut a: i128, mut b: i128) -> i128 {
    while b != 0 {
        (a, b) = (b, a % b);
    }
    a.abs()
}

/// `has_infinite` marks a divisor list continuing past the reified elements.
///
/// Exact rational `polymod`: `n.polymod(d1, d2, ...)` yields
/// `(n mod d1, (n div d1) mod d2, ..., final quotient)`, all computed without
/// floating point. Returns `None` (bail to float) when any operand is out of
/// the non-negative Int/Rat domain, a divisor is zero, or an intermediate
/// value overflows `i128` / does not fit back into a `Rat`.
fn polymod_exact(target: &Value, divisors: &[Value], has_infinite: bool) -> Option<Vec<Value>> {
    let (mut nn, mut nd) = polymod_rat(target)?;
    let kind = match target.view() {
        ValueView::Rat(..) => PolymodKind::Rat,
        ValueView::FatRat(..) => PolymodKind::FatRat,
        _ => PolymodKind::Int,
    };
    let mut result = Vec::with_capacity(divisors.len() + 1);
    for d in divisors {
        let (dn, dd) = polymod_rat(d)?;
        // An `Int` over a fractional divisor takes `mod`'s `Real, Real`
        // candidate and `div`'s `Int` coercion: the generic loop answers it.
        if matches!(kind, PolymodKind::Int) && dd != 1 {
            return None;
        }
        if dn == 0 {
            return None; // zero divisor: leave to the float path's own handling
        }
        if has_infinite {
            // As in `polymod_exact_int`: an exhausted invocant ends a lazy
            // divisor list, and a divisor of one would never terminate.
            if nn == 0 {
                return Some(result);
            }
            if dn == dd {
                result.push(polymod_rat_value(nn, nd, kind)?);
                return Some(result);
            }
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
        result.push(polymod_rat_value(r_num, r_den, kind)?);
        // The carried quotient is always an integer.
        nn = q;
        nd = 1;
    }
    result.push(polymod_rat_value(nn, nd, kind)?);
    Some(result)
}

/// Rakudo's element types for `Instant.polymod` / `Duration.polymod`: a zero
/// stays an `Int`, any other element is a `Num`, except that a `Duration`
/// invocant's first remainder stays a `Duration`.
// Cost: O(k), k = number of elements in the result.
pub(in crate::runtime) fn polymod_temporal_types(res: Value, is_duration: bool) -> Value {
    let ValueView::Seq(items) = res.view() else {
        return res;
    };
    let typed = items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let f = match v.view() {
                ValueView::Int(n) => n as f64,
                ValueView::Num(n) => n,
                ValueView::Rat(n, d) if d != 0 => crate::value::rat_to_f64(n, d),
                _ => return v.clone(),
            };
            if f == 0.0 {
                Value::int(0)
            } else if i == 0 && is_duration {
                crate::builtins::arith::make_duration_value(f)
            } else {
                Value::num(f)
            }
        })
        .collect();
    Value::seq(typed)
}
