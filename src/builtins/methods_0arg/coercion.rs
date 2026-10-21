use crate::builtins::primality::{is_prime_bigint, is_prime_i64};
use crate::symbol::Symbol;
use crate::value::ValueMap;
use crate::value::{RuntimeError, Value, ValueView};
use std::collections::HashMap;

/// Implement is-prime for any Value, with type coercion.
/// Negative numbers are not prime.
/// Complex with non-zero imaginary throws X::Numeric::Real.
/// Str, Num, Rat, FatRat coerce to Int first.
pub(crate) fn value_is_prime(target: &Value) -> Result<Value, RuntimeError> {
    // Unwrap allomorphic types (Mixin) to the inner numeric value
    if let ValueView::Mixin(inner, _) = target.view() {
        return value_is_prime(inner);
    }
    match target.view() {
        ValueView::Int(n) => {
            if n < 0 {
                return Ok(Value::FALSE);
            }
            Ok(Value::truth(is_prime_i64(n)))
        }
        ValueView::BigInt(n) => {
            if n.sign() == num_bigint::Sign::Minus {
                return Ok(Value::FALSE);
            }
            Ok(Value::truth(is_prime_bigint(&n)))
        }
        ValueView::Num(f) => {
            if f < 0.0 || f.fract() != 0.0 {
                return Ok(Value::FALSE);
            }
            let n = f as i64;
            Ok(Value::truth(is_prime_i64(n)))
        }
        ValueView::Rat(n, d) | ValueView::FatRat(n, d) => {
            if d == 0 {
                return Ok(Value::FALSE);
            }
            if n < 0 {
                return Ok(Value::FALSE);
            }
            if n % d != 0 {
                return Ok(Value::FALSE);
            }
            let int_val = n / d;
            Ok(Value::truth(is_prime_i64(int_val)))
        }
        ValueView::BigRat(n, d) => {
            use num_traits::Zero;
            if d.is_zero() {
                return Ok(Value::FALSE);
            }
            if n.sign() == num_bigint::Sign::Minus {
                return Ok(Value::FALSE);
            }
            let (quot, rem) = num_integer::Integer::div_rem(n, d);
            if !rem.is_zero() {
                return Ok(Value::FALSE);
            }
            Ok(Value::truth(is_prime_bigint(&quot)))
        }
        ValueView::Complex(_, i) if i != 0.0 => {
            let mut attrs = HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Cannot convert {} to Real: imaginary part not zero",
                    match target.view() {
                        ValueView::Complex(r, i) => {
                            if i >= 0.0 {
                                format!("{}+{}i", r, i)
                            } else {
                                format!("{}{}i", r, i)
                            }
                        }
                        _ => format!("{:?}", target),
                    }
                )),
            );
            attrs.insert("target".to_string(), Value::package(Symbol::intern("Real")));
            attrs.insert("source".to_string(), target.clone());
            let ex = Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs);
            let mut err =
                RuntimeError::new("Cannot convert Complex to Real: imaginary part not zero");
            err.exception = Some(Box::new(ex));
            Err(err)
        }
        ValueView::Complex(r, _) => {
            // imaginary is 0, treat as real
            if r < 0.0 || r.fract() != 0.0 {
                return Ok(Value::FALSE);
            }
            let n = r as i64;
            Ok(Value::truth(is_prime_i64(n)))
        }
        ValueView::Str(s) => {
            // Try to parse as a number
            // Handle Unicode minus sign
            let s = s.replace('\u{2212}', "-");
            if let Ok(n) = s.parse::<i64>() {
                if n < 0 {
                    return Ok(Value::FALSE);
                }
                return Ok(Value::truth(is_prime_i64(n)));
            }
            if let Ok(f) = s.parse::<f64>() {
                if f < 0.0 || f.fract() != 0.0 {
                    return Ok(Value::FALSE);
                }
                let n = f as i64;
                return Ok(Value::truth(is_prime_i64(n)));
            }
            Ok(Value::FALSE)
        }
        _ => Ok(Value::FALSE),
    }
}

/// Build a Capture from a Seq/Slip element slice: `Pair`/`ValuePair` elements
/// become named args, everything else positional. Shared by the `ValueView::Seq`
/// and `ValueView::Slip` arms of `value_to_capture` (their payload types
/// diverged under ADR-0034: `Seq`'s is `SeqBody`, `Slip`'s stays `Vec<Value>`).
fn items_to_capture_value(items: &[Value]) -> Value {
    let mut positional = vec![];
    let mut named = ValueMap::default();
    for item in items.iter() {
        match item.view() {
            ValueView::Pair(k, v) => {
                named.insert(k.clone(), v.clone());
            }
            ValueView::ValuePair(k, v) => {
                named.insert(k.to_string_value(), v.clone());
            }
            _ => positional.push(item.clone()),
        }
    }
    Value::capture(positional, named)
}

/// Convert a value to a Capture.
/// Whether `.Capture` on this List/Array/Seq/Slip must name a `Pair` whose key
/// is not a `Str` through that key's own `.Str` (a custom class's `method
/// Str`): the pure [`value_to_capture`] cannot, so the interpreter answers.
// Cost: O(e), e = elements of the receiver.
pub(crate) fn capture_needs_str_key(target: &Value) -> bool {
    matches!(
        target.view(),
        ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_)
    ) && crate::runtime::utils::value_to_list(target).iter().any(
        |item| matches!(item.view(), ValueView::ValuePair(k, _) if !matches!(k.view(), ValueView::Str(_))),
    )
}

pub(crate) fn value_to_capture(target: &Value) -> Result<Value, RuntimeError> {
    match target.view() {
        // A Capture is already a Capture
        ValueView::Capture { .. } => Ok(target.clone()),
        // Match.Capture returns self
        ValueView::Instance { .. } if target.is_match_instance() => Ok(target.clone()),
        // Blob/Buf/utf8/utf16 .Capture behaves like List.Capture: each byte
        // becomes a positional argument (no nameds).
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => {
            let positional = crate::value::value_buf::buf_elems_or_empty(&attributes);
            Ok(Value::capture(positional, ValueMap::default()))
        }
        // Numeric types follow Mu.Capture: public attributes become nameds.
        // Complex → \(:re, :im)
        ValueView::Complex(r, i) => {
            let mut named = ValueMap::default();
            named.insert("re".to_string(), Value::num(r));
            named.insert("im".to_string(), Value::num(i));
            Ok(Value::capture(vec![], named))
        }
        // Rat/FatRat/BigRat → \(:numerator, :denominator)
        ValueView::Rat(n, d) => {
            let mut named = ValueMap::default();
            named.insert("numerator".to_string(), Value::int(n));
            named.insert("denominator".to_string(), Value::int(d));
            Ok(Value::capture(vec![], named))
        }
        ValueView::FatRat(n, d) => {
            let mut named = ValueMap::default();
            named.insert("numerator".to_string(), Value::int(n));
            named.insert("denominator".to_string(), Value::int(d));
            Ok(Value::capture(vec![], named))
        }
        ValueView::BigRat(n, d) => {
            let mut named = ValueMap::default();
            named.insert("numerator".to_string(), Value::bigint(n.clone()));
            named.insert("denominator".to_string(), Value::bigint(d.clone()));
            Ok(Value::capture(vec![], named))
        }
        // Version.Capture throws X::Cannot::Capture
        ValueView::Version { .. } => Err(cannot_capture_value(target)),
        // Signature/Failure .Capture throw X::Cannot::Capture (both the type
        // object and an instance).
        ValueView::Instance { class_name, .. }
            if matches!(class_name.resolve().as_str(), "Signature" | "Failure") =>
        {
            Err(cannot_capture(&class_name.resolve()))
        }
        ValueView::Package(name) if matches!(name.resolve().as_str(), "Signature" | "Failure") => {
            Err(cannot_capture(&name.resolve()))
        }
        // Pair.Capture → \(:key($pair.key), :value($pair.value))
        ValueView::Pair(k, v) => {
            let mut named = ValueMap::default();
            named.insert("key".to_string(), Value::str(k.clone()));
            named.insert("value".to_string(), v.clone());
            Ok(Value::capture(vec![], named))
        }
        ValueView::ValuePair(k, v) => {
            let mut named = ValueMap::default();
            named.insert("key".to_string(), k.clone());
            named.insert("value".to_string(), v.clone());
            Ok(Value::capture(vec![], named))
        }
        // Set.Capture → named args where each key maps to True (the named-arg
        // name is the element's display string; store keys are `.WHICH`).
        ValueView::Set(s, _) => {
            let mut named = ValueMap::default();
            for k in s.iter() {
                named.insert(s.typed_key(k).to_string_value(), Value::TRUE);
            }
            Ok(Value::capture(vec![], named))
        }
        // Bag.Capture → named args where each key maps to its count
        ValueView::Bag(b, _) => {
            let mut named = ValueMap::default();
            for (k, v) in b.iter() {
                named.insert(
                    b.typed_key(k).to_string_value(),
                    Value::from_bigint(v.clone()),
                );
            }
            Ok(Value::capture(vec![], named))
        }
        // Mix.Capture → named args where each key maps to its weight
        ValueView::Mix(m, _) => {
            let mut named = ValueMap::default();
            for (k, v) in m.iter() {
                let val = v.to_value();
                named.insert(m.typed_key(k).to_string_value(), val);
            }
            Ok(Value::capture(vec![], named))
        }
        // Hash.Capture → named args from hash entries. An object hash stores
        // `.WHICH` keys — the named-arg name is the original key's
        // stringification (raku: `("42" => 70, :42a).Capture` on an object
        // hash is `\(:42(70), :a(42))`).
        ValueView::Hash(map) => {
            let typed = map.has_typed_keys();
            let mut named = ValueMap::default();
            for (k, v) in map.iter() {
                let name = if typed {
                    map.typed_key(k).to_string_value()
                } else {
                    k.clone()
                };
                named.insert(name, v.clone());
            }
            Ok(Value::capture(vec![], named))
        }
        // Lazy arrays must throw X::Cannot::Lazy
        ValueView::Array(_, kind) if kind.is_lazy() => Err(RuntimeError::cannot_lazy_with_action(
            "create a Capture from",
            "List",
        )),
        // Array/List.Capture → positional args, with Pair values becoming named
        ValueView::Array(items, ..) => {
            let mut positional = vec![];
            let mut named = ValueMap::default();
            for item in items.iter() {
                match item.view() {
                    ValueView::Pair(k, v) => {
                        named.insert(k.clone(), v.clone());
                    }
                    ValueView::ValuePair(k, v) => {
                        named.insert(k.to_string_value(), v.clone());
                    }
                    _ => positional.push(item.clone()),
                }
            }
            Ok(Value::capture(positional, named))
        }
        ValueView::Seq(items) => Ok(items_to_capture_value(&items)),
        ValueView::Slip(items) => Ok(items_to_capture_value(&items)),
        ValueView::LazyList(ll) => {
            // A LazyList is lazy if it is a gather/take or similar generator
            // (a body or compiled code) or a genuinely lazy sequence/pipe
            // (an unbounded Range's `.list`, `1, 2 ... *`).
            let is_lazy =
                ll.is_genuinely_lazy() || !ll.body.is_empty() || ll.compiled_code.is_some();
            if is_lazy {
                Err(RuntimeError::cannot_lazy_with_action(
                    "create a Capture from",
                    &crate::value::types::what_type_name(target),
                ))
            } else {
                let items = ll.cache.lock().unwrap().clone().unwrap_or_default();
                let mut positional = vec![];
                let mut named = ValueMap::default();
                for item in items.iter() {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            named.insert(k.clone(), v.clone());
                        }
                        ValueView::ValuePair(k, v) => {
                            named.insert(k.to_string_value(), v.clone());
                        }
                        _ => positional.push(item.clone()),
                    }
                }
                Ok(Value::capture(positional, named))
            }
        }
        // Range.Capture → Mu.Capture semantics (named args from attributes)
        ValueView::Range(start, end)
        | ValueView::RangeExcl(start, end)
        | ValueView::RangeExclStart(start, end)
        | ValueView::RangeExclBoth(start, end) => {
            let mut named = ValueMap::default();
            named.insert("min".to_string(), Value::int(start));
            named.insert("max".to_string(), Value::int(end));
            named.insert(
                "excludes-min".to_string(),
                Value::truth(matches!(
                    target.view(),
                    ValueView::RangeExclStart(..) | ValueView::RangeExclBoth(..)
                )),
            );
            named.insert(
                "excludes-max".to_string(),
                Value::truth(matches!(
                    target.view(),
                    ValueView::RangeExcl(..) | ValueView::RangeExclBoth(..)
                )),
            );
            named.insert("is-int".to_string(), Value::TRUE);
            Ok(Value::capture(vec![], named))
        }
        ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } => {
            let mut named = ValueMap::default();
            named.insert("min".to_string(), (**start).clone());
            named.insert("max".to_string(), (**end).clone());
            named.insert("excludes-min".to_string(), Value::truth(excl_start));
            named.insert("excludes-max".to_string(), Value::truth(excl_end));
            let is_int = matches!(start.view(), ValueView::Int(_) | ValueView::BigInt(_))
                && matches!(end.view(), ValueView::Int(_) | ValueView::BigInt(_));
            named.insert("is-int".to_string(), Value::truth(is_int));
            Ok(Value::capture(vec![], named))
        }
        // Duration / Instant expose their seconds as a `tai` named (a Rat),
        // following Mu.Capture (public attribute → named). Both store the value
        // internally under the `value` attribute (Duration as a Rat, Instant
        // sometimes as an Int/Num); coerce it to a Rat to match the spec.
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if matches!(class_name.resolve().as_str(), "Duration" | "Instant") => {
            let tai = attributes
                .as_map()
                .get("value")
                .map(crate::builtins::arith::real_to_rat)
                .unwrap_or_else(|| crate::value::make_rat(0, 1));
            let mut named = ValueMap::default();
            named.insert("tai".to_string(), tai);
            Ok(Value::capture(vec![], named))
        }
        // IO::Path follows Mu.Capture, but its public accessor is `.CWD` while the
        // attribute is stored under the lowercase `cwd`; rename it so the named
        // argument matches the accessor (the spec checks `:CWD`).
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if matches!(class_name.resolve().as_str(), "IO::Path" | "Path") => {
            let mut named = ValueMap::default();
            for (k, v) in attributes.as_map().iter() {
                let key = if k == "cwd" { "CWD" } else { k.as_str() };
                named.insert(key.to_string(), v.clone());
            }
            Ok(Value::capture(vec![], named))
        }
        // Generic object: Mu.Capture returns a Capture whose named arguments are
        // the object's public attributes. We expose every stored attribute as a
        // named (the spec tests only assert the contents they know about, so
        // extra bookkeeping attributes are harmless).
        ValueView::Instance { attributes, .. } => {
            let mut named = ValueMap::default();
            for (k, v) in attributes.as_map().iter() {
                named.insert(k.resolve(), v.clone());
            }
            Ok(Value::capture(vec![], named))
        }
        // Promise follows Mu.Capture: its public `.status` accessor becomes a
        // named argument. The value is the `PromiseStatus` enum constant, so
        // it `eqv`s the literal `PromiseStatus::<status>`.
        ValueView::Promise(shared) => {
            let mut named = ValueMap::default();
            named.insert(
                "status".to_string(),
                crate::runtime::Interpreter::promise_status_value(&shared.status()),
            );
            Ok(Value::capture(vec![], named))
        }
        // Nil.Capture → empty capture
        ValueView::Nil => Ok(Value::capture(vec![], ValueMap::default())),
        // Types whose .Capture throws X::Cannot::Capture
        ValueView::Bool(_)
        | ValueView::Str(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Whatever
        | ValueView::HyperWhatever => Err(cannot_capture_value(target)),
        // Sub → X::Cannot::Capture (Callable)
        ValueView::Sub(..) => Err(cannot_capture(&crate::value::types::what_type_name(target))),
        // Regex → X::Cannot::Capture. RegexWithAdverbs is the metadata-carrying
        // representation used for source-preserving regex literals, but it has
        // the same nominal Capture behavior as a plain Regex.
        ValueView::Regex(..) | ValueView::RegexWithAdverbs(..) => Err(cannot_capture("Regex")),
        // Mixin types that should throw X::Cannot::Capture
        // (e.g., IntStr, NumStr allomorphs, WhateverCode, Signature, Version)
        ValueView::Mixin(..) => {
            let type_name = crate::value::types::what_type_name(target);
            match type_name.as_str() {
                "IntStr" | "NumStr" | "RatStr" | "ComplexStr" | "WhateverCode" | "Signature"
                | "Version" => Err(cannot_capture(&type_name)),
                _ => Ok(Value::capture(vec![target.clone()], ValueMap::default())),
            }
        }
        // Default: wrap in a single-positional capture
        _ => Ok(Value::capture(vec![target.clone()], ValueMap::default())),
    }
}

pub(crate) fn cannot_capture(type_name: &str) -> RuntimeError {
    let mut attrs = ValueMap::default();
    attrs.insert("what".to_string(), Value::str(type_name.to_string()));
    attrs.insert(
        "message".to_string(),
        Value::str(format!("Cannot unpack or Capture {}", type_name)),
    );
    RuntimeError::typed("X::Cannot::Capture", attrs)
}

/// `X::Cannot::Capture` for a value that has no `Capture` (an `Int`, `Num`, `Str`,
/// `Bool`, `Version`, `*`): Rakudo names the value by its `.gist` in backticks
/// and explains how to write a `Capture`, and keeps the value as `.what`.
// Cost: O(n), n = chars of the value's gist.
pub(crate) fn cannot_capture_value(target: &Value) -> RuntimeError {
    let mut attrs = ValueMap::default();
    attrs.insert("what".to_string(), target.clone());
    attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Cannot unpack or Capture `{}`.\nTo create a Capture, add parentheses: \\(...)\n\
             If unpacking in a signature, perhaps you needlessly used parentheses? -> ($x) {{}} vs. -> $x {{}}\n\
             or missed `:` in signature unpacking? -> &c:(Int) {{}}",
            crate::value::gist::gist_value(target)
        )),
    );
    RuntimeError::typed("X::Cannot::Capture", attrs)
}

/// The genuinely-lazy `List` an unbounded `Range.List` coerces to
/// (`(1..Inf).List`, `("a"..*).List`): its `unbounded_range::lazy_list`,
/// tagged as list context so `.^name` reads `List` and `.gist` renders `(...)`
/// rather than the Array's `[...]`. `None` for any other value.
/// Cost: O(1) (allocates a one-element seed vector and the LazyList shell).
pub(crate) fn unbounded_range_list(target: &Value) -> Option<Value> {
    let ll = crate::runtime::unbounded_range::lazy_list(target)?;
    Some(Value::lazy_list(crate::gc::Gc::new(ll.with_list_context())))
}
