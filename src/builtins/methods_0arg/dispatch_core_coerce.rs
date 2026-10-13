/// Type coercion methods: Str, Int, UInt, Version, Num, Real, Numeric. `self`,
/// `clone`, `defined`, `DEFINITE`, `Bool`, `WHICH` and `WHERE` are `Mu`'s rows
/// (`method_table::ctors_mop::mu_identity`, ADR-11276 §9.91).
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

use crate::value::ValueMap;

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
    // `Complex.Complex` is the identity coercion (`method Complex() { self }`);
    // `Int.Int` is `Mu.Int`'s row (`method_table::ctors_mop::mu_numeric`).
    if let ValueView::Package(name) = target.view()
        && method == "Complex"
        && name == "Complex"
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
        _ => None,
    }
}
