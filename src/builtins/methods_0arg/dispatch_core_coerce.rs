/// Type coercion methods: Str, Int, UInt, Version, Num, Real, Numeric. `self`,
/// `clone`, `defined`, `DEFINITE`, `Bool`, `WHICH` and `WHERE` are `Mu`'s rows
/// (`method_table::ctors_mop::mu_identity`, ADR-11276 §9.91).
use crate::symbol::Symbol;
use crate::value::str_numeric::str_numifies_to_complex;
use crate::value::value_buf::buf_len_or_zero;
use crate::value::{RuntimeError, Value, ValueView};

use crate::value::ValueMap;
use crate::value::types::is_stash_class_name;

/// Build the `X::Str::Numeric` attribute map for a string that cannot be
/// numified, deriving `pos`/`reason` from the same analyzer the numeric
/// operators use (so `"5 foo"` reports `trailing characters after number` at
/// pos 1, not a blanket "must begin with valid digits" at pos 0), and the
/// matching `source-indicator`.
fn str_numeric_exception_attrs(s: &str) -> ValueMap {
    let (pos, reason) = crate::runtime::str_numeric::str_numeric_failure(s).unwrap_or((
        0,
        "base-10 number must begin with valid digits or '.'".to_string(),
    ));
    let mut ex_attrs = ValueMap::default();
    ex_attrs.insert("source".to_string(), Value::str(s.to_string()));
    ex_attrs.insert("reason".to_string(), Value::str(reason.clone()));
    ex_attrs.insert("pos".to_string(), Value::int(pos as i64));
    let source_indicator = crate::runtime::str_numeric::build_source_indicator(s, pos);
    ex_attrs.insert(
        "source-indicator".to_string(),
        Value::str(source_indicator.clone()),
    );
    // Include the `⏏` position marker, matching Rakudo:
    // `Cannot convert string to number: trailing characters after number
    //  in '5⏏ foo' (indicated by ⏏)`.
    ex_attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Cannot convert string to number: {reason} {source_indicator}"
        )),
    );
    ex_attrs
}

/// Build a lazy `Failure` wrapping an `X::Str::Numeric` exception for a string
/// that cannot be numified — the shape raku's `.Int`/`.Num` produce on a bad
/// string (the exception only fires when the Failure is sunk/used).
pub(crate) fn str_numeric_failure(s: &str) -> Value {
    let ex = Value::make_instance(
        Symbol::intern("X::Str::Numeric"),
        str_numeric_exception_attrs(s),
    );
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// The thrown `X::Str::Numeric` for a string that cannot be numified, for a
/// caller that coerces its argument itself (`(1..3).EXISTS-POS("a")`).
// Cost: O(d), d = chars of `s`.
pub(crate) fn str_numeric_error(s: &str) -> RuntimeError {
    let attrs = str_numeric_exception_attrs(s);
    let message = attrs
        .get("message")
        .map(|m| m.to_string_value())
        .unwrap_or_default();
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::Str::Numeric"),
        attrs,
    )));
    err
}

/// Render a Complex's literal form (`1+2i`, `1-2i`, `3.7+1e-20i`) for an
/// `X::Numeric::Real` message: its `.Str`, as raku prints the source value.
fn render_complex_literal(re: f64, im: f64) -> String {
    crate::value::format_complex(re, im)
}

/// Build the `X::Numeric::Real` exception raised when a Complex with a
/// non-zero imaginary part is coerced to a type that requires a Real value
/// (`.Int`, `.UInt`, `.Num`, `.Rat`, `.FatRat`, `.Real`, `.sign`, ...).
/// `target_type` is the type reported by `.target` — the type the coercion
/// actually attempted, e.g. `"Int"` for `.Int` — matching raku's per-method
/// message (`Cannot convert 1+2i to Int: imaginary part not zero`).
pub(crate) fn complex_not_real_exception(
    re: f64,
    im: f64,
    target_type: &str,
    source: &Value,
) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert(
        "message".to_string(),
        Value::str(format!(
            "Cannot convert {} to {target_type}: imaginary part not zero",
            render_complex_literal(re, im)
        )),
    );
    attrs.insert(
        "target".to_string(),
        Value::package(Symbol::intern(target_type)),
    );
    attrs.insert("source".to_string(), source.clone());
    Value::make_instance(Symbol::intern("X::Numeric::Real"), attrs)
}

/// The eager-throw form of [`complex_not_real_exception`], for coercions that
/// fail immediately rather than returning a lazy `Failure`.
pub(crate) fn complex_not_real_error(
    re: f64,
    im: f64,
    target_type: &str,
    source: &Value,
) -> RuntimeError {
    let msg = format!(
        "Cannot convert {} to {target_type}: imaginary part not zero",
        render_complex_literal(re, im)
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(complex_not_real_exception(
        re,
        im,
        target_type,
        source,
    )));
    err
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    // A Range numerifies to its element count (`.elems`): `.Int`/`.Numeric`/
    // `.Real` yield that count as an `Int`, `.Num` as a `Num`. An infinite
    // range yields `Inf` for the real-valued coercions and fails for `.Int`
    // ("Cannot convert Inf to Int"). Handle this before the per-method arms,
    // which only know how to numerify scalar types.
    if target.is_range() && matches!(method, "Int" | "Numeric" | "Real" | "Num") {
        return Some(Some(
            crate::builtins::method_table::range::numeric_coercion(target, method),
        ));
    }
    // A lazy (infinite-backed) array numerifies to its element count, which it
    // cannot report: raku throws `X::Cannot::Lazy` (`Cannot .elems a lazy list`)
    // for every numeric coercion (`.Int`/`.Numeric`/`.Real`/`.Num`/prefix `+`).
    // Guard before the per-method arms, which would return the capped backing
    // length. (`.elems` itself is handled in `dispatch_core_numeric`.)
    if matches!(method, "Int" | "Numeric" | "Real" | "Num") && super::is_lazy_count_source(target) {
        return Some(super::range_elems_lazy_failure("elems"));
    }
    // `.Buf` / `.Blob` on any byte-string (Buf/Blob/utf8/blob8/...) returns a
    // value of the requested role carrying the same bytes. `utf8` (what
    // `.encode` produces) does Blob, so `.Buf`/`.Blob` reinterpret those bytes
    // as a plain `Buf[uint8]` / `Blob[uint8]`. Humming-Bird's HTTPServer uses
    // `"\r\n".encode.Buf` to build its constant delimiters.
    if matches!(method, "Buf" | "Blob")
        && let ValueView::Instance { class_name, .. } = target.view()
        && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
    {
        // The `Blob`/`Buf` rows' implementation (`method_table::blob`).
        return Some(crate::builtins::method_table::blob::reinterpret(
            target, method,
        ));
    }
    // A numeric coercion method invoked on a *type object* of the same type
    // (`Int.Int`, `Num.Num`, `Complex.Complex`) is the identity coercion: raku
    // defines e.g. `method Int() { self }`, so for an undefined invocant (a
    // type object) the result is that same type object rather than a "No such
    // method" error. Zef's URI parser relies on `($auth<port> // Int).Int`
    // yielding the `Int` type object when the port is absent.
    //
    // Only `Int`/`Num`/`Complex` are handled here: raku returns the type object
    // cleanly for these, but `Rat.Rat`/`FatRat.FatRat` throw "must be an object
    // instance" (no `method Rat() { self }`), so those are left to fall through.
    if let ValueView::Package(name) = target.view()
        && matches!(method, "Int" | "Num" | "Complex")
        && (name.resolve() == method
            // `UInt.Int` also returns the invocant unchanged: UInt is a subset
            // of Int, so it inherits Int's `method Int() { self }`.
            || (method == "Int" && name.resolve() == "UInt"))
    {
        return Some(Some(Ok(target.clone())));
    }
    match method {
        // The `Any.serial` row's implementation (`method_table::collections::any_misc`).
        "serial" => Some(Some(
            crate::builtins::method_table::collection_any_misc::serial(target, &[]),
        )),
        "Str" | "Stringy" => Some(match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Failure" => {
                // Using a Failure in string context throws the wrapped exception.
                if let Some(ex) = attributes.as_map().get("exception") {
                    Some(Err(RuntimeError::from_exception_value(ex.clone())))
                } else {
                    Some(Err(RuntimeError::new("Failed")))
                }
            }
            ValueView::Package(_) | ValueView::Instance { .. } => None,
            ValueView::LazyList(_) => None, // fall through to runtime to force the list
            // A lazy (infinite-backed) array stringifies to a bounded `...`
            // placeholder rather than materializing its capped backing.
            ValueView::Array(_, crate::value::ArrayKind::Lazy) => Some(Ok(Value::str_from("..."))),
            ValueView::Enum { .. } => None, // fall through to enum dispatch for string enum support
            ValueView::Str(s) if s.as_str() == "IO::Special" => Some(Ok(Value::str_from(""))),
            ValueView::Rat(_, 0) | ValueView::FatRat(_, 0) => {
                // Zero-denominator Rat/FatRat .Str throws X::Numeric::DivideByZero
                None // fall through to runtime for exception with proper context
            }
            // A collection stringifies every element, so a zero-denominator
            // Rational inside it dies like its own `.Str` (GH #9608). The walk
            // runs twice only on the error path.
            ValueView::Array(..)
            | ValueView::Seq(..)
            | ValueView::Slip(..)
            | ValueView::Hash(..)
            | ValueView::Pair(..)
            | ValueView::ValuePair(..)
                if crate::runtime::utils::zero_denominator_rational_error(target).is_some() =>
            {
                crate::runtime::utils::zero_denominator_rational_error(target).map(Err)
            }
            // A list holding an `Instance` element needs the interpreter: the
            // element's class may define its own `Str`, which this pure
            // renderer cannot call (it would print the `ClassName()` fallback
            // -- `~@a` then disagreed with `@a.join("")` for the same array).
            // `dispatch_list_str_method` resolves the elements and re-renders.
            _ if crate::Interpreter::list_str_needs_interpreter(target) => None,
            // Likewise a quant hash with an object element
            // (`quanthash_element_stringify.rs`).
            _ if crate::Interpreter::quanthash_needs_element_dispatch(target) => None,
            // A `Regex` warns and yields the EMPTY string rather than its
            // source text, and the warning needs the interpreter -- see
            // `Interpreter::regex_str_coercion`.
            ValueView::Regex(_)
            | ValueView::RegexWithAdverbs(..)
            | ValueView::Routine { is_regex: true, .. } => None,
            // Every other code object answers its name with the same kind of
            // warning: the `Code.Str` row (`method_table::code`).
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                if method == "Str" =>
            {
                None
            }
            // Cost: O(1) for a plain Str invocant (the value is shared, not
            // copied); O(n) otherwise, n = chars of the rendered string.
            _ if target.is_str_value() => Some(Ok(target.clone())),
            _ => Some(Ok(Value::str(target.to_string_value()))),
        }),
        "Int" => {
            // `.Int` on a type object: `Int.Int`/`UInt.Int` identity is handled
            // by the same-type check above; the concrete Cool types only define
            // `Int` multis with a `:D` invocant, so their type objects throw
            // X::Parameter::InvalidConcreteness. Every other type object (Any,
            // Mu, Cool, IntStr, user classes, roles) inherits Mu's coercion —
            // warn "uninitialized ... in numeric context" and return 0 — which
            // lives on the slow path so a user-defined `.Int` dispatches first.
            if let ValueView::Package(name) = target.view() {
                let n = name.resolve();
                return match n.as_str() {
                    "Num" | "Str" | "Rat" | "FatRat" | "Complex" => {
                        Some(Some(Err(RuntimeError::parameter_invalid_concreteness(
                            &n, &n, "Int", "self", true, true,
                        ))))
                    }
                    _ => Some(None),
                };
            }
            let result = match target.view() {
                // A real number: the numeric types' `Int` rows' implementation
                // (ADR-11276, `method_table::coerce`).
                ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..) => crate::builtins::method_table::coerce::int_of(target)?,
                // The `Str.Int` row's implementation
                // (`method_table::str_numeric`); a string that numifies to a
                // `Complex` is the interpreter's.
                ValueView::Str(s) => {
                    return Some(
                        crate::builtins::method_table::str_numeric::int_of_str(&s).map(Ok),
                    );
                }
                ValueView::Bool(b) => Value::int(if b { 1 } else { 0 }),
                // A Complex coerces to Int via its real part when its imaginary
                // part is `≅ 0` (`$*TOLERANCE`); otherwise it is not Real and
                // throws X::Numeric::Real. Reading the dynamic variable needs the
                // interpreter, so this falls through to
                // `Interpreter::dispatch_complex_to_real`.
                ValueView::Complex(..) => return Some(None),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    Value::int(buf_len_or_zero(&attributes) as i64)
                }
                // A StrDistance's `.Int` is the edit distance between its
                // before/after strings, matching `+$str-dist` / `.Numeric`.
                ValueView::Instance { class_name, .. } if class_name == "StrDistance" => {
                    Value::int(
                        super::dispatch_core_math::cool_instance_numeric(target).unwrap_or(0.0)
                            as i64,
                    )
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        // `.Int`, then the range check (the `Cool.UInt` row's implementation,
        // `method_table::str_numeric`).
        "UInt" => {
            // A Complex coerces to UInt like `.Int` (`(5+0i).UInt` is 5): the
            // runtime checks its imaginary part against `$*TOLERANCE`. A type
            // object is the runtime's.
            if matches!(
                target.view(),
                ValueView::Complex(..) | ValueView::Package(_)
            ) {
                return Some(None);
            }
            let Some(int) = dispatch(target, "Int") else {
                return Some(None);
            };
            Some(int.map(|r| r.map(crate::builtins::method_table::str_numeric::uint_of_int)))
        }
        // The `Cool.Version` row's implementation (`method_table::version`).
        "Version" => match target.view() {
            ValueView::Version { .. }
            | ValueView::Str(_)
            | ValueView::Int(_)
            | ValueView::BigInt(_)
            | ValueView::Num(_)
            | ValueView::Rat(_, _)
            | ValueView::BigRat(_, _)
            | ValueView::FatRat(_, _)
            | ValueView::Bool(_) => Some(Some(Ok(
                crate::builtins::method_table::version::version_of(target),
            ))),
            _ => None,
        },
        "Num" => {
            // `.Num` on a concrete-only Cool type's type object dies with
            // X::Parameter::InvalidConcreteness; `Num.Num` is identity (below)
            // and the other type objects inherit Mu/Cool's warn-and-0.
            if let ValueView::Package(name) = target.view() {
                let n = name.resolve();
                let expected = match n.as_str() {
                    "Int" | "Str" | "Complex" => Some(n.as_str()),
                    "UInt" => Some("Int"),
                    "Rat" | "FatRat" => Some("Rational"),
                    _ => None,
                };
                if let Some(expected) = expected {
                    return Some(Some(Err(RuntimeError::parameter_invalid_concreteness(
                        expected, &n, "Num", "self", true, true,
                    ))));
                }
            }
            // A real number: the numeric types' `Num` rows' implementation
            // (ADR-11276, `method_table::coerce`).
            if let Some(result) = crate::builtins::method_table::coerce::num_of(target) {
                return Some(Some(Ok(result)));
            }
            let result = match target.view() {
                // The `Str.Num` row's implementation (`method_table::str_numeric`); a
                // string that numifies to a `Complex` is the interpreter's.
                ValueView::Str(s) => {
                    return Some(
                        crate::builtins::method_table::str_numeric::num_of_str(&s).map(Ok),
                    );
                }
                ValueView::Bool(b) => Value::num(if b { 1.0 } else { 0.0 }),
                // The runtime checks the imaginary part against `$*TOLERANCE`
                // (`Interpreter::dispatch_complex_to_real`).
                ValueView::Complex(..) => return Some(None),
                ValueView::Array(items, ..) => Value::num(items.len() as f64),
                ValueView::Seq(items) => Value::num(items.len() as f64),
                ValueView::Slip(items) => Value::num(items.len() as f64),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Real" => {
            // Calling .Real on a type object (Package) should warn and return zero
            if let ValueView::Package(name) = target.view() {
                let type_name = name.resolve();
                let zero = match type_name.as_str() {
                    "Int" | "IntStr" => Value::int(0),
                    "Rat" | "FatRat" | "RatStr" => Value::rat_raw(0, 1),
                    "Num" | "NumStr" | "Complex" | "ComplexStr" => Value::num(0.0),
                    _ => return Some(None),
                };
                let msg = format!(
                    "Use of uninitialized value of type {} in numeric context",
                    type_name
                );
                return Some(Some(Err(RuntimeError::warn_signal_with_resume(msg, zero))));
            }
            // A real number: the numeric types' `Real` rows' implementation
            // (`method_table::numeric_coerce`).
            if let Some(result) = crate::builtins::method_table::numeric_coerce::real_of(target) {
                return Some(Some(Ok(result)));
            }
            let result = match target.view() {
                // A Complex is Real when its imaginary part is `≅ 0`; the runtime
                // reads `$*TOLERANCE` (`Interpreter::dispatch_complex_to_real`).
                ValueView::Complex(..) => return Some(None),
                // Cost: O(d^2) for a d-digit integer string, O(n) otherwise (as `.Numeric`).
                ValueView::Str(s) => {
                    // `.Real` yields the natural numeric type (Int/Rat/Num); use the
                    // canonical parser so radix prefixes and underscores work and the
                    // result agrees with `.Numeric`.
                    if str_numifies_to_complex(&s).is_some() {
                        // As `.Int`: coerced as the `Complex` it numifies to.
                        return Some(None);
                    }
                    if let Some(v) =
                        crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim())
                    {
                        v
                    } else {
                        // Same X::Str::Numeric Failure (typed, with the `⏏` marker)
                        // as `.Int`/`.Numeric`.
                        return Some(Some(Ok(str_numeric_failure(&s))));
                    }
                }
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Seq(items) => Value::int(items.len() as i64),
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        "Numeric" => {
            // Calling .Numeric on a type object (Package) should warn and return zero
            if let ValueView::Package(name) = target.view() {
                let type_name = name.resolve();
                let zero = match type_name.as_str() {
                    "Int" | "IntStr" => Value::int(0),
                    "Rat" | "RatStr" => Value::rat_raw(0, 1),
                    "FatRat" => Value::fat_rat_raw(0, 1),
                    "Num" | "NumStr" => Value::num(0.0),
                    "Complex" | "ComplexStr" => Value::complex(0.0, 0.0),
                    _ => return Some(None),
                };
                let msg = format!(
                    "Use of uninitialized value of type {} in numeric context",
                    type_name
                );
                return Some(Some(Err(RuntimeError::warn_signal_with_resume(msg, zero))));
            }
            // A number: the numeric types' `Numeric` rows' implementation
            // (`method_table::numeric_coerce`). Rationals stay exact; converting
            // a `Rat` through f64 loses large denominators (and turns
            // `1/100000` into the nearby `1/99999`), and `Numeric.Rat` is the
            // identity in Rakudo. A `Complex` is preserved too: converting it to
            // a real type belongs to `.Num`/`.Real`, where a non-zero imaginary
            // component is rejected.
            if let Some(result) = crate::builtins::method_table::numeric_coerce::numeric_of(target)
            {
                return Some(Some(Ok(result)));
            }
            let result = match target.view() {
                // The `Str.Numeric` row's implementation (`method_table::str_numeric`).
                ValueView::Str(s) => crate::builtins::method_table::str_numeric::numeric_of_str(&s),
                // Cost: O(1) (`.Numeric` / `+@a` of an array is its element count).
                ValueView::Array(items, ..) => Value::int(items.len() as i64),
                ValueView::Hash(h) => Value::int(h.len() as i64),
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    Value::int(buf_len_or_zero(&attributes) as i64)
                }
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if is_stash_class_name(class_name.as_str()) => {
                    let count = match attributes.as_map().get("symbols").map(Value::view) {
                        Some(ValueView::Hash(map)) => map.len() as i64,
                        _ => 0,
                    };
                    Value::int(count)
                }
                // A StrDistance (`$str ~~ tr/a/b/`) numifies to the edit distance
                // between its before/after strings, so `+($str ~~ tr/old/new/)`
                // is the number of changes, not 0.
                ValueView::Instance { class_name, .. } if class_name == "StrDistance" => {
                    match super::dispatch_core_math::cool_instance_numeric(target) {
                        Some(n) if n.fract() == 0.0 && n.is_finite() => Value::int(n as i64),
                        Some(n) => Value::num(n),
                        None => Value::int(0),
                    }
                }
                _ => return Some(None),
            };
            Some(Some(Ok(result)))
        }
        _ => None,
    }
}
