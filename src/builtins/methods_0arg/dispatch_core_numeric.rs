use super::{int_lsb_value, int_msb_value, range_elems_lazy_failure};
use crate::builtins::rng::builtin_rand;
/// Numeric and element methods: elems, default, abs, lsb, msb, rand,
/// uc, lc, fc, tc, sign
use crate::symbol::Symbol;
use crate::value::types::is_stash_class_name;
use crate::value::{RuntimeError, Value, ValueView};

/// Check if a Package type object is calling a :D-requiring numeric method.
/// Returns X::Parameter::InvalidConcreteness error if so.
fn check_numeric_type_object_method(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    if let ValueView::Package(pkg_name) = target.view() {
        let name = pkg_name.resolve();
        // Numeric types whose instances have these methods
        let is_numeric_type = matches!(
            name.as_ref(),
            "Int" | "UInt" | "Num" | "Rat" | "FatRat" | "Complex" | "Cool" | "Numeric"
        );
        if !is_numeric_type {
            return None;
        }
        // Methods that require :D (a concrete instance)
        let is_d_method = matches!(
            method,
            "abs"
                | "sign"
                | "sqrt"
                | "exp"
                | "log"
                | "log2"
                | "log10"
                | "ceiling"
                | "floor"
                | "round"
                | "truncate"
                | "narrow"
                | "lsb"
                | "msb"
                | "base"
                | "polymod"
        );
        if !is_d_method {
            return None;
        }
        // Determine the parent numeric type for the error
        let expected_type = match name.as_ref() {
            "UInt" => "Int",
            _ => &name,
        };
        return Some(Some(Err(RuntimeError::parameter_invalid_concreteness(
            expected_type,
            &name,
            method,
            "self",
            true, // should_be_concrete
            true, // param_is_invocant
        ))));
    }
    None
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    // Type objects calling :D-requiring numeric methods
    if let result @ Some(_) = check_numeric_type_object_method(target, method) {
        return result;
    }
    match method {
        // Cost: O(1) on a reified list/array, hash or integer Range; a finite
        // LazyList is forced first (O(e), deferred to the runtime), as in Rakudo.
        "elems" => {
            if crate::builtins::method_table::any_collection::scalar_like(target) {
                return Some(Some(crate::builtins::method_table::any_collection::elems(
                    target,
                    &[],
                )));
            }
            if let ValueView::LazyList(list) = target.view() {
                // Only a GENUINELY lazy list refuses `.elems`; everything else
                // reifies and counts. A plain `gather {...}` Seq is not lazy
                // (raku: `.is-lazy` is False), nor is a `.map`/`.grep` pipe
                // whose source chain bottoms out finite (`gather {...}.map(*+1)`),
                // nor an `IO::CatHandle` pull. A `lazy gather` (or `gather.lazy`)
                // carries the preserve-lazy marker and must still throw
                // X::Cannot::Lazy. `is_genuinely_lazy` is the single authority
                // for that question -- this arm used to re-derive a narrower
                // "from a gather env marker" version of it, which threw on a
                // pipe over a gather.
                if !list.is_genuinely_lazy() {
                    return Some(None);
                }
                let mut ex_attrs = std::collections::HashMap::new();
                ex_attrs.insert(
                    "message".to_string(),
                    Value::str("Cannot .elems a lazy list".to_string()),
                );
                let exception = Value::make_instance(Symbol::intern("X::Cannot::Lazy"), ex_attrs);
                let mut failure_attrs = std::collections::HashMap::new();
                failure_attrs.insert("exception".to_string(), exception);
                failure_attrs.insert("handled".to_string(), Value::FALSE);
                return Some(Some(Ok(Value::make_instance(
                    Symbol::intern("Failure"),
                    failure_attrs,
                ))));
            }
            // A lazy (infinite-backed) array cannot report its element count;
            // raku throws X::Cannot::Lazy. Check before `as_list_items`, which
            // would otherwise return the capped backing length.
            if let ValueView::Array(_, kind) = target.view()
                && kind.is_lazy()
            {
                return Some(range_elems_lazy_failure("elems"));
            }
            if let Some(items) = target.as_list_items() {
                return Some(Some(Ok(Value::int(items.len() as i64))));
            }
            let result = match target.view() {
                ValueView::Hash(items) => Value::int(items.len() as i64),
                // The quant hashes' rows' implementation (`method_table::quanthash`).
                ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => {
                    return Some(crate::builtins::method_table::quanthash::elems(target, &[]));
                }
                ValueView::Junction { values, .. } => Value::int(values.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => {
                    // An **unmanaged** `CArray` — a `nativecast`ed handle, which
                    // carries a bare C address and no storage of its own — has no
                    // length to report, and Rakudo throws rather than answering 0.
                    // `NativeHelpers::Blob`'s `01-basic.t` asserts exactly that
                    // (`dies-ok { $au.elems }`) before going on to use `:size`.
                    if !crate::value::value_buf::has_buf_elems(&attributes)
                        && attributes.contains_key("address")
                    {
                        return Some(Some(Err(crate::value::RuntimeError::new(
                            "Don't know how many elements a C array returned from a library",
                        ))));
                    }
                    Value::int(crate::value::value_buf::buf_len_or_zero(&attributes) as i64)
                }
                ValueView::Channel(_) => {
                    return Some(Some(Err(RuntimeError::new(
                        "Cannot call '.elems' on a Channel instance".to_string(),
                    ))));
                }
                // The `Range.elems` row's implementation (`method_table::range`).
                _ if target.is_range() => {
                    return Some(crate::builtins::method_table::range::elems(target, &[]));
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if is_stash_class_name(class_name.as_str()) => {
                    match attributes.as_map().get("symbols").map(Value::view) {
                        Some(ValueView::Hash(map)) => Value::int(map.len() as i64),
                        _ => Value::int(0),
                    }
                }
                _ => Value::int(1),
            };
            Some(Some(Ok(result)))
        }
        "default" => {
            let result = match target.view() {
                // A value-carried `is default(...)` (embedded in HashData/
                // ArrayData) takes priority over the type default, so it survives
                // raw-parameter binding and list construction.
                ValueView::Array(a, _) if a.default.is_some() => {
                    a.default.as_deref().cloned().unwrap()
                }
                ValueView::Hash(h) if h.default.is_some() => h.default.as_deref().cloned().unwrap(),
                ValueView::Array(..) | ValueView::Hash(..) => {
                    Value::package(crate::symbol::wk::any())
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        // Numeric receivers: the numeric types' rows' implementation
        // (ADR-11276, `method_table::real`).
        // Cost: O(1) for word-sized values; O(b) for big ones, b = size in bits.
        "abs" => {
            if let Some(result) = crate::builtins::method_table::real::abs_of(target) {
                return Some(Some(result));
            }
            let result = match target.view() {
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                // `Instant`/`Duration` `does Real`, so `Real.abs` applies — and
                // it keeps the type (`Instant.abs` is an `Instant`,
                // `Duration.abs` a `Duration`), because rakudo's `Real.abs`
                // is `self < 0 ?? -self !! self` on the value itself. They store
                // their seconds as a Real `value` attribute, the same shape the
                // `.Rat`/`.Int` arms coerce.
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if matches!(class_name.resolve().as_str(), "Duration" | "Instant") => {
                    let inner = attributes.as_map().get("value")?.clone();
                    let abs_inner = match dispatch(&inner, "abs") {
                        Some(Some(Ok(v))) => v,
                        Some(Some(Err(e))) => return Some(Some(Err(e))),
                        _ => return Some(None),
                    };
                    let mut attrs = attributes.as_map().clone();
                    attrs.insert("value".to_string(), abs_inner);
                    Value::make_instance(class_name, attrs)
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "lsb" => Some(int_lsb_value(target).map(Ok)),
        "msb" => Some(int_msb_value(target).map(Ok)),
        "rand" => {
            let max = match target.view() {
                ValueView::Int(n) => n as f64,
                ValueView::Num(n) => n,
                ValueView::Rat(n, d) => crate::value::rat_to_f64(n, d),
                // `Duration`/`Instant` `does Real`, whose `rand` is
                // `self.Bridge.rand`: a `Num` below the stored seconds.
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if matches!(class_name.resolve().as_str(), "Duration" | "Instant") => {
                    let inner = attributes.as_map().get("value")?.clone();
                    return dispatch(&inner, "rand");
                }
                // `Range.rand` is the `Range` row's (`method_table::range`).
                // Cool types: numify first (e.g., List.rand returns rand in 0..^elems)
                ValueView::Array(items, ..) => items.len() as f64,
                ValueView::Seq(items) => items.len() as f64,
                ValueView::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                ValueView::Bool(b) => {
                    if b {
                        1.0
                    } else {
                        0.0
                    }
                }
                _ => return Some(None),
            };
            Some(Some(Ok(Value::num(builtin_rand() * max))))
        }
        // `Cool`'s case maps: the `Str` rows' handlers (ADR-11276), on the
        // receiver's string form.
        // Cost: O(n), n = chars of the invocant's string form.
        "uc" => Some(Some(crate::builtins::method_table::str::uc(target, &[]))),
        // Cost: O(n), n = chars of the invocant's string form.
        "lc" => Some(Some(crate::builtins::method_table::str::lc(target, &[]))),
        // Cost: O(n), n = chars of the invocant's string form.
        "fc" => Some(Some(crate::builtins::method_table::str::fc(target, &[]))),
        // Cost: O(n), n = chars of the invocant's string form.
        "tc" => Some(Some(crate::builtins::method_table::str::tc(target, &[]))),
        // Numeric receivers: the numeric types' rows' implementation
        // (ADR-11276, `method_table::real`).
        // Cost: O(1).
        "sign" => {
            if let Some(result) = crate::builtins::method_table::real::sign_of(target) {
                return Some(Some(result));
            }
            let result = match target.view() {
                ValueView::Enum { value, .. } => Value::int(value.as_i64().signum()),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        _ => None,
    }
}
