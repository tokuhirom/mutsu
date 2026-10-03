//! The native coercion, big-integer conversion and value-test `nqp::` ops
//! (#11492): `coerce_ni` and kin, `intify` / `numify`, `tostr_I` /
//! `fromstr_I` and kin, `bool_I`, `isbig_I`, `isprime_I`, `box_n` / `box_u`,
//! `decont_i` / `decont_n` / `decont_s`, `isinvokable`, `isttyfh`.
//!
//! A link of the chained `nqp::` tables (`... -> nqp_ops_native -> here ->
//! nativecall_nqp`). The conversions share their routines with the Raku
//! spellings: a Num renders through the same `Str` form `Num.Str` gives, a
//! big integer parses and prints through `num_bigint` as `Int.Str` /
//! `Str.Int` do, `isprime_I` is `Int.is-prime`'s `builtins::primality`,
//! `isttyfh` is `IO::Handle.t`, and the unsigned reads use
//! `nqp_ops_native::uint64_value`, the `unbox_u` body.
//!
//! The ops whose answer depends on a representation mutsu does not have yet
//! — `isstr` / `isint` / `isnum` / `ishash` / `iscoderef` and the `boot*`
//! types (MoarVM's BOOT* REPRs), `iscont_i` / `_n` / `_s` (native lexical
//! references) and `isrwcont` (a container descriptor's rw-ness) — are not
//! here and stay loudly unsupported until #11553 settles that representation.

use super::*;
use crate::runtime::nqp_ops_native::uint64_value;
use crate::value::ValueView;
use num_bigint::BigInt as NumBigInt;

/// An operand with any argument wrapper and container stripped.
fn operand(args: &[Value], i: usize) -> Value {
    crate::runtime::types::unwrap_varref_value(args.get(i).cloned().unwrap_or(Value::NIL))
        .deref_container()
}

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn narg(args: &[Value], i: usize) -> f64 {
    args.get(i).map(|v| v.to_f64()).unwrap_or(0.0)
}

fn flag(b: bool) -> Value {
    Value::int(i64::from(b))
}

fn int_value(n: NumBigInt) -> Value {
    match i64::try_from(&n) {
        Ok(small) => Value::int(small),
        Err(_) => Value::bigint(n),
    }
}

/// MoarVM's "This type cannot unbox to a native <kind>" error.
fn cannot_unbox(kind: &str, v: &Value) -> RuntimeError {
    RuntimeError::new(format!(
        "This type cannot unbox to a native {kind}: P6opaque, {}",
        crate::value::type_name::value_type_name(v)
    ))
}

/// The native `int` a Num truncates to, with x86's answer for a value that
/// does not fit (NaN, ±Inf, beyond ±2**63): `i64::MIN`, as MoarVM gives.
fn num_to_native_int(n: f64) -> i64 {
    const LIMIT: f64 = 9_223_372_036_854_775_808.0; // 2**63
    if n.is_finite() && (-LIMIT..LIMIT).contains(&n) {
        n.trunc() as i64
    } else {
        i64::MIN
    }
}

/// The `Str` form of a Num, the one `Num.Str` gives (`1e+16`, `-0`, `0.1`).
fn num_str(n: f64) -> Value {
    Value::str(Value::num(n).to_string_value())
}

/// `nqp::fromstr_I`'s grammar: an optional `-` and ASCII digits, or the empty
/// string (0); anything else is MoarVM's "Value out of range" error.
fn parse_big_decimal(s: &str) -> Result<NumBigInt, RuntimeError> {
    if s.is_empty() {
        return Ok(NumBigInt::from(0));
    }
    let digits = s.strip_prefix('-').unwrap_or(s);
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return Err(RuntimeError::new(
            "Error reading a big integer from a string: Value out of range",
        ));
    }
    s.parse::<NumBigInt>().map_err(|_| {
        RuntimeError::new("Error reading a big integer from a string: Value out of range")
    })
}

/// Whether a value can be invoked, as MoarVM's invocation spec answers: a
/// code object (or a code type object) does, an object with only a
/// `CALL-ME` method does not.
fn is_invokable(v: &Value) -> bool {
    match v.view() {
        ValueView::Sub(_)
        | ValueView::WeakSub(_)
        | ValueView::Routine { .. }
        | ValueView::Regex(_)
        | ValueView::RegexWithAdverbs(_) => true,
        ValueView::Mixin(inner, _) => is_invokable(inner),
        ValueView::Package(name) => matches!(
            name.resolve().as_str(),
            "Code"
                | "Block"
                | "Routine"
                | "Sub"
                | "Method"
                | "Submethod"
                | "Macro"
                | "Regex"
                | "WhateverCode"
        ),
        _ => false,
    }
}

impl Interpreter {
    /// Try a coercion / conversion / value-test `nqp::` op. `None` means "not
    /// an op this table knows".
    pub(crate) fn call_nqp_op_coerce(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(Ok(match op {
            // -- native coercions --
            // Cost: O(1).
            "coerce_in" => Value::num(iarg(args, 0) as f64),
            // Truncates; NaN, ±Inf and out-of-range values give i64::MIN.
            // Cost: O(1).
            "coerce_ni" => Value::int(num_to_native_int(narg(args, 0))),
            // Cost: O(n), n = chars of the rendered Num.
            "coerce_ns" => num_str(narg(args, 0)),
            // A native int read as unsigned (`-1` is 2**64 - 1).
            // Cost: O(1).
            "coerce_iu" => uint64_value(&operand(args, 0)),
            // A native uint's 64 bits read as signed (2**64 - 1 is -1).
            // Cost: O(1) for a machine-word Int; O(d), d = digits of a BigInt.
            "coerce_ui" => Value::int(uint_bits(&operand(args, 0)) as i64),
            // MoarVM renders the 64 bits as SIGNED here (`coerce_us(2**64-1)`
            // is "-1"); matched as measured.
            // Cost: O(1).
            "coerce_us" => Value::str((uint_bits(&operand(args, 0)) as i64).to_string()),

            // -- smart coercions --
            // nqp::intify($v): an Int as is, a Str by its leading integer
            // (`"3.9"` is 3, `"abc"` is 0); a Num does not unbox to an int.
            // Cost: O(n), n = chars of a Str operand; O(1) otherwise.
            "intify" => {
                let v = operand(args, 0);
                match v.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) => v.clone(),
                    ValueView::Bool(b) => Value::int(i64::from(b)),
                    ValueView::Str(s) => Value::int(super::nqp_ops::parse_leading_int(s.trim())),
                    _ => return Some(Err(cannot_unbox("integer", &v))),
                }
            }
            // nqp::numify($v): a number as a native num; a Str numifies like
            // `+$str` (whitespace and `_` allowed, the empty string is 0) and
            // a non-numeric one is an error.
            // Cost: O(n), n = chars of a Str operand; O(1) otherwise.
            "numify" => {
                let v = operand(args, 0);
                if let ValueView::Str(s) = v.view() {
                    let text: &str = &s;
                    if text.trim().is_empty() {
                        return Some(Ok(Value::num(0.0)));
                    }
                    match crate::value::str_numeric::parse_raku_str_to_numeric(text) {
                        Some(n) => Value::num(n.to_f64()),
                        None => {
                            return Some(Err(RuntimeError::new(format!(
                                "Can't convert '{text}' to num: expecting a number"
                            ))));
                        }
                    }
                } else {
                    Value::num(v.to_f64())
                }
            }

            // -- big integers --
            // Cost: O(d), d = digits of the operand.
            "bool_I" => flag(!num_traits::Zero::is_zero(&operand(args, 0).to_bigint())),
            // True outside the range MoarVM stores inline, `-2**31 < $n <
            // 2**31` (as measured, `-2**31` itself already counts as big).
            // Cost: O(1) for a machine-word Int; O(d), d = digits of a BigInt.
            "isbig_I" => flag(match operand(args, 0).view() {
                ValueView::Int(n) => !(i64::from(i32::MIN) < n && n <= i64::from(i32::MAX)),
                _ => true,
            }),
            // Cost: O(sqrt n) for a machine-word Int (see builtins::primality); O(d^3) per Miller-Rabin round for a BigInt.
            "isprime_I" => flag(match operand(args, 0).view() {
                ValueView::Int(n) => crate::builtins::primality::is_prime_i64(n),
                ValueView::BigInt(n) => crate::builtins::primality::is_prime_bigint(&n),
                _ => false,
            }),
            // Cost: O(d^2), d = digits of the operand.
            "tostr_I" => Value::str(operand(args, 0).to_bigint().to_string()),
            // Cost: O(d), d = digits of the operand.
            "tonum_I" => {
                let n = operand(args, 0).to_bigint();
                Value::num(crate::builtins::arith::bigint_ratio_to_f64(
                    &n,
                    &NumBigInt::from(1),
                ))
            }
            // Cost: O(d^2), d = digits of $str.
            "fromstr_I" => match parse_big_decimal(&operand(args, 0).to_string_value()) {
                Ok(n) => int_value(n),
                Err(e) => return Some(Err(e)),
            },
            // Truncates toward zero; NaN and ±Inf are errors.
            // Cost: O(d), d = digits of the result.
            "fromnum_I" => {
                let n = narg(args, 0);
                if !n.is_finite() {
                    return Some(Err(RuntimeError::new(format!(
                        "Error storing an MVMnum64 ({}) in a big integer: Value out of range",
                        if n.is_nan() {
                            "nan".to_string()
                        } else {
                            n.to_string()
                        }
                    ))));
                }
                match num_traits::FromPrimitive::from_f64(n.trunc()) {
                    Some(b) => int_value(b),
                    None => Value::int(0),
                }
            }
            // Cost: O(1).
            "fromI_I" => operand(args, 0),

            // -- boxing / native reads --
            // Cost: O(1).
            "box_n" => Value::num(narg(args, 0)),
            // Cost: O(1) for a machine-word Int; O(d), d = digits of a BigInt.
            "box_u" => uint64_value(&operand(args, 0)),
            // Only the matching boxed type unboxes, as in MoarVM.
            // Cost: O(1).
            "decont_i" => {
                let v = operand(args, 0);
                match v.view() {
                    ValueView::Int(_) | ValueView::BigInt(_) => v.clone(),
                    ValueView::Bool(b) => Value::int(i64::from(b)),
                    _ => return Some(Err(cannot_unbox("integer", &v))),
                }
            }
            // Cost: O(1).
            "decont_n" => {
                let v = operand(args, 0);
                match v.view() {
                    ValueView::Num(n) => Value::num(n),
                    _ => return Some(Err(cannot_unbox("number", &v))),
                }
            }
            // Cost: O(1).
            "decont_s" => {
                let v = operand(args, 0);
                match v.view() {
                    ValueView::Str(_) => v.clone(),
                    _ => return Some(Err(cannot_unbox("string", &v))),
                }
            }

            // -- tests --
            // Cost: O(1).
            "isinvokable" => flag(is_invokable(&operand(args, 0))),
            // nqp::isttyfh($fh): `IO::Handle.t` on the VM handle.
            // Cost: O(1) + one isatty(3).
            "isttyfh" => {
                let fh = operand(args, 0);
                match self.with_handle_mut(&fh, |state| Ok(state.is_tty())) {
                    Ok(t) => flag(t),
                    Err(e) => return Some(Err(e)),
                }
            }
            // The FFI ops (`nativecall_nqp.rs`).
            _ => return self.call_nqp_op_ffi(op, args),
        }))
    }
}

/// The 64 bits of a native uint operand (`2**64 - 1` and `-1` are the same).
fn uint_bits(v: &Value) -> u64 {
    match uint64_value(v).view() {
        ValueView::Int(n) => n as u64,
        ValueView::BigInt(b) => num_traits::ToPrimitive::to_u64(b.as_ref()).unwrap_or(0),
        _ => 0,
    }
}
