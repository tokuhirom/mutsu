use super::range_elems_lazy_failure;
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
                | "roots"
                | "expmod"
                | "is-prime"
                | "chr"
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
        // `Array.default` and `Hash.default`: the rows' implementations
        // (`method_table::list`, `method_table::map`).
        "default" => Some(match target.view() {
            ValueView::Array(..) => crate::builtins::method_table::list::default(target, &[]),
            ValueView::Hash(_) => crate::builtins::method_table::map::default(target, &[]),
            _ => None,
        }),
        // `abs` of a number is a row (`method_table::real`, `cool_real`); this arm
        // keeps `Instant` and `Duration`, which have no table shape.
        // `Real.abs` keeps the type (`Instant.abs` is an `Instant`,
        // `Duration.abs` a `Duration`), because rakudo's is `self < 0 ?? -self !!
        // self` on the value itself. They store their seconds as a Real `value`
        // attribute, the same shape the `.Rat`/`.Int` arms coerce.
        // Cost: O(1).
        "abs" => match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if matches!(class_name.resolve().as_str(), "Duration" | "Instant") => {
                let inner = attributes.as_map().get("value")?.clone();
                let abs_inner = match crate::builtins::method_table::real::abs_of(&inner) {
                    Some(Ok(v)) => v,
                    Some(Err(e)) => return Some(Some(Err(e))),
                    None => return Some(None),
                };
                let mut attrs = attributes.as_map().clone();
                attrs.insert("value".to_string(), abs_inner);
                Some(Some(Ok(Value::make_instance(class_name, attrs))))
            }
            _ => Some(None),
        },
        // `rand` of a numeric receiver is a row (`method_table::real_misc`);
        // this arm keeps the receivers with no table shape.
        // Cost: O(1).
        "rand" => match target.view() {
            // `Duration`/`Instant` `does Real`, whose `rand` is
            // `self.Bridge.rand`: a `Num` below the stored seconds.
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if matches!(class_name.resolve().as_str(), "Duration" | "Instant") => {
                let inner = attributes.as_map().get("value")?.clone();
                Some(Some(crate::builtins::method_table::real_misc::rand(
                    &inner,
                    &[],
                )))
            }
            // `Range.rand` is the `Range` row's (`method_table::range`). A
            // `Seq` is `Cool` and numifies to its element count.
            ValueView::Seq(_) => Some(Some(crate::builtins::method_table::real_misc::rand(
                target,
                &[],
            ))),
            _ => Some(None),
        },
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
        // `sign` of a number is a row; an enum value (no table shape) is the sign
        // of its integer value.
        // Cost: O(1).
        "sign" => match target.view() {
            ValueView::Enum { value, .. } => Some(Some(Ok(Value::int(value.as_i64().signum())))),
            _ => Some(None),
        },
        _ => None,
    }
}
