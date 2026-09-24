//! `.succ` / `.pred`, the one implementation behind the methods, `++` / `--`
//! (prefix and postfix) and `.=succ` / `.=pred` (ADR-0118).
//!
//! These used to be four separate routines -- the two methods, the VM's
//! `increment_value` / `decrement_value` and the mutating-method path's
//! `increment_mut_target_value` / `decrement_mut_target_value` -- and they
//! disagreed: `9223372036854775807.succ` wrapped to a negative while `++`
//! promoted to a BigInt, `(2**70).succ` returned its invocant unchanged,
//! `"²".succ` ignored the superscript digits `++` understood, and `.=pred` on
//! `"a"` silently kept the string where `--` produced a Failure.
//!
//! A number's successor is `$n + 1`, so the numeric arms are literally
//! `infix:<+>` / `infix:<->` (`arith_add` / `arith_sub`): overflow, rational
//! precision and FatRat-ness follow from that one routine.

use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The value of a superscript digit (`²` is 2), for the `"x²"++` idiom.
fn superscript_digit_value(c: char) -> Option<u32> {
    Some(match c {
        '\u{2070}' => 0,
        '\u{00B9}' => 1,
        '\u{00B2}' => 2,
        '\u{00B3}' => 3,
        '\u{2074}'..='\u{2079}' => c as u32 - 0x2070,
        _ => return None,
    })
}

fn superscript_digit_char(d: u32) -> char {
    match d {
        1 => '\u{00B9}',
        2 => '\u{00B2}',
        3 => '\u{00B3}',
        _ => char::from_u32(0x2070 + d).unwrap_or('\u{2070}'),
    }
}

/// `s` read as a number in superscript digits, stepped by `delta` (+1 / -1).
/// `None` when `s` is not all superscript digits, or would go below zero.
fn superscript_step(s: &str, delta: i32) -> Option<String> {
    let mut digits = s
        .chars()
        .map(superscript_digit_value)
        .collect::<Option<Vec<u32>>>()?;
    if digits.is_empty() {
        return None;
    }
    let mut carry = true;
    for d in digits.iter_mut().rev() {
        if !carry {
            break;
        }
        if delta > 0 {
            *d = (*d + 1) % 10;
            carry = *d == 0;
        } else if *d == 0 {
            *d = 9;
        } else {
            *d -= 1;
            carry = false;
        }
    }
    if carry {
        if delta < 0 {
            return None;
        }
        digits.insert(0, 1);
    }
    Some(digits.into_iter().map(superscript_digit_char).collect())
}

/// "Decrement out of range", the Failure `"a".pred` answers.
fn decrement_failure() -> Value {
    let mut ex_attrs = std::collections::HashMap::new();
    ex_attrs.insert(
        "message".to_string(),
        Value::str("Decrement out of range".to_string()),
    );
    let exception = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);
    let mut failure_attrs = std::collections::HashMap::new();
    failure_attrs.insert("exception".to_string(), exception);
    failure_attrs.insert("handled".to_string(), Value::FALSE);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

/// The successor of `v`, or `None` for a value `.succ` does not handle
/// natively (an enum, an instance with its own `succ`, a type object...).
///
/// Cost: O(1) for a numeric invocant; O(n) for a Str, n = chars.
pub(crate) fn value_succ(v: &Value) -> Option<Value> {
    Some(match v.view() {
        ValueView::Int(i) => i.checked_add(1).map_or_else(
            || Value::from_bigint(num_bigint::BigInt::from(i) + 1),
            Value::int,
        ),
        ValueView::BigInt(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Num(_)
        | ValueView::Complex(..) => super::arith_add(v.clone(), Value::int(1)).ok()?,
        ValueView::Bool(_) => Value::TRUE,
        ValueView::Str(s) => Value::str(
            superscript_step(&s, 1)
                .unwrap_or_else(|| crate::builtins::str_increment::string_succ(&s)),
        ),
        ValueView::Mixin(inner, _) => return value_succ(inner),
        _ => return None,
    })
}

/// The predecessor of `v` (see [`value_succ`]). A string that cannot be
/// decremented (`"a".pred`) answers a "Decrement out of range" Failure.
///
/// Cost: O(1) for a numeric invocant; O(n) for a Str, n = chars.
pub(crate) fn value_pred(v: &Value) -> Option<Value> {
    Some(match v.view() {
        ValueView::Int(i) => i.checked_sub(1).map_or_else(
            || Value::from_bigint(num_bigint::BigInt::from(i) - 1),
            Value::int,
        ),
        ValueView::BigInt(_)
        | ValueView::Rat(..)
        | ValueView::FatRat(..)
        | ValueView::BigRat(..)
        | ValueView::Num(_)
        | ValueView::Complex(..) => super::arith_sub(v.clone(), Value::int(1)),
        ValueView::Bool(_) => Value::FALSE,
        ValueView::Str(s) => match superscript_step(&s, -1)
            .or_else(|| crate::builtins::str_increment::string_pred_checked(&s))
        {
            Some(prev) => Value::str(prev),
            None => decrement_failure(),
        },
        ValueView::Mixin(inner, _) => return value_pred(inner),
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn succ(v: Value) -> String {
        value_succ(&v)
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    }

    fn pred(v: Value) -> String {
        value_pred(&v)
            .map(|v| v.to_string_value())
            .unwrap_or_default()
    }

    #[test]
    fn integers_promote() {
        assert_eq!(succ(Value::int(i64::MAX)), "9223372036854775808");
        assert_eq!(pred(Value::int(i64::MIN)), "-9223372036854775809");
    }

    #[test]
    fn superscripts_count() {
        assert_eq!(succ(Value::str_from("\u{B2}")), "\u{B3}");
        assert_eq!(succ(Value::str_from("\u{2079}")), "\u{B9}\u{2070}");
        assert_eq!(pred(Value::str_from("\u{B9}\u{2070}")), "\u{2070}\u{2079}");
    }

    #[test]
    fn strings_step() {
        assert_eq!(succ(Value::str_from("az")), "ba");
        assert_eq!(pred(Value::str_from("x")), "w");
    }
}
