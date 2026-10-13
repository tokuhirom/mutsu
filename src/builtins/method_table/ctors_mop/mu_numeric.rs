//! `Mu.Int` and `Mu.Numeric` (ADR-11276 slice 5, §9.92): the coercions a
//! receiver reaches when its own type declares none.
//!
//! Rakudo declares both on `Mu`. The numeric types, `Str`, `List`, `Map`,
//! `Match` and the other shapes with their own `Int`/`Numeric` answer first;
//! these rows answer what is left: a type object (`Int.Int` is `Int`, the
//! concrete-only types throw `X::Parameter::InvalidConcreteness`, a numeric
//! type's `.Numeric` warns and is its zero), a `Range` (its element count), a
//! lazy list (`X::Cannot::Lazy`), a `FatRat`, a `Blob`/`Buf` (its length), a
//! `StrDistance` (its edit distance), a `Stash` (its symbol count) and a lazy
//! or shaped array. Every other receiver declines to the interpreter's
//! `Interpreter::mu_numeric_coercion` (a warning for a type object,
//! `X::Multi::NoMatch` for an instance) or `dispatch_complex_to_real` (a
//! `Complex`, judged against `$*TOLERANCE`).

use crate::builtins::method_table::{Handler, MethodRow, RowFlags};
use crate::value::types::is_stash_class_name;
use crate::value::value_buf::buf_len_or_zero;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Mu",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::TYPE_OBJECT_OK,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("Int", int), row!("Numeric", numeric)];

/// The count a `Range` or a lazy list numifies to, shared by both rows: the
/// element count of a `Range` (`Inf` fails `.Int`), `X::Cannot::Lazy` for a
/// list that cannot count itself.
// Cost: O(1) for an Int range, O(e) otherwise.
fn count_of(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    if target.is_range() {
        return Some(crate::builtins::method_table::range::numeric_coercion(
            target, method,
        ));
    }
    if crate::builtins::methods_0arg::is_lazy_count_source(target) {
        return Some(Ok(crate::runtime::utils::cannot_lazy_failure("elems")));
    }
    None
}

/// The length of a `Blob`/`Buf` instance, if `target` is one.
// Cost: O(1).
fn buf_len(target: &Value) -> Option<Value> {
    match target.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
            Some(Value::int(buf_len_or_zero(&attributes) as i64))
        }
        _ => None,
    }
}

/// `Mu.Int`.
// Cost: O(1), except a `Range`'s count and a `Str`'s parse (O(d^2), d digits).
fn int(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Package(name) = target.view() {
        let name = name.resolve();
        return match name.as_str() {
            // `method Int() { self }`; `UInt` is a subset of `Int`.
            "Int" | "UInt" => Some(Ok(target.clone())),
            // The concrete `Cool` types declare `Int` for a `:D` invocant only.
            "Num" | "Str" | "Rat" | "FatRat" | "Complex" => {
                Some(Err(RuntimeError::parameter_invalid_concreteness(
                    &name, &name, "Int", "self", true, true,
                )))
            }
            _ => None,
        };
    }
    if let Some(answer) = count_of(target, "Int") {
        return Some(answer);
    }
    if let Some(len) = buf_len(target) {
        return Some(Ok(len));
    }
    Some(Ok(match target.view() {
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..) => crate::builtins::method_table::coerce::int_of(target)?,
        ValueView::Str(s) => {
            return crate::builtins::method_table::str_numeric::int_of_str(&s).map(Ok);
        }
        ValueView::Bool(b) => Value::int(i64::from(b)),
        ValueView::Hash(h) => Value::int(h.len() as i64),
        ValueView::Array(items, ..) => Value::int(items.len() as i64),
        // A StrDistance's `.Int` is the edit distance between its before and
        // after strings, as `+$str-dist` / `.Numeric`.
        ValueView::Instance { class_name, .. } if class_name == "StrDistance" => Value::int(
            crate::builtins::methods_0arg::dispatch_core_math::cool_instance_numeric(target)
                .unwrap_or(0.0) as i64,
        ),
        // A built-in class with a shape and no `Int` of its own (`DateTime`):
        // Rakudo's `Mu.Int` has no candidate for it.
        ValueView::Instance { class_name, .. } if target.dispatch_shape().is_some() => {
            return Some(Err(crate::runtime::mu_coercion_no_match_error(
                "Int",
                &class_name.resolve(),
            )));
        }
        _ => return None,
    }))
}

/// `Mu.Numeric`.
// Cost: O(1), except a `Range`'s count and a `Str`'s parse.
fn numeric(target: &Value, _args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    if let ValueView::Package(name) = target.view() {
        let name = name.resolve();
        let zero = match name.as_str() {
            "Int" | "IntStr" => Value::int(0),
            "Rat" | "RatStr" => Value::rat_raw(0, 1),
            "FatRat" => Value::fat_rat_raw(0, 1),
            "Num" | "NumStr" => Value::num(0.0),
            "Complex" | "ComplexStr" => Value::complex(0.0, 0.0),
            _ => return None,
        };
        let msg = format!("Use of uninitialized value of type {name} in numeric context");
        return Some(Err(RuntimeError::warn_signal_with_resume(msg, zero)));
    }
    if let Some(answer) = count_of(target, "Numeric") {
        return Some(answer);
    }
    // A number keeps its type: a `Rat` stays exact, a `Complex` stays complex.
    if let Some(number) = crate::builtins::method_table::numeric_coerce::numeric_of(target) {
        return Some(Ok(number));
    }
    if let Some(len) = buf_len(target) {
        return Some(Ok(len));
    }
    Some(Ok(match target.view() {
        ValueView::Str(s) => crate::builtins::method_table::str_numeric::numeric_of_str(&s),
        ValueView::Array(items, ..) => Value::int(items.len() as i64),
        ValueView::Hash(h) => Value::int(h.len() as i64),
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
        // A StrDistance (`$str ~~ tr/a/b/`) numifies to the edit distance, so
        // `+($str ~~ tr/old/new/)` is the number of changes.
        ValueView::Instance { class_name, .. } if class_name == "StrDistance" => {
            match crate::builtins::methods_0arg::dispatch_core_math::cool_instance_numeric(target) {
                Some(n) if n.fract() == 0.0 && n.is_finite() => Value::int(n as i64),
                Some(n) => Value::num(n),
                None => Value::int(0),
            }
        }
        // A built-in class with a shape and no `Numeric` of its own
        // (`IO::Handle`): Rakudo's `Mu.Numeric` has no candidate for it.
        ValueView::Instance { class_name, .. } if target.dispatch_shape().is_some() => {
            return Some(Err(crate::runtime::mu_coercion_no_match_error(
                "Numeric",
                &class_name.resolve(),
            )));
        }
        _ => return None,
    }))
}
