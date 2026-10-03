//! The ordered-string, unsigned and ignore-mark comparison `nqp::` ops, and
//! the string bitwise ops (#11491): `islt_s` .. `isge_s`, `cmp_u` and
//! `iseq_u` .. `isge_u`, `eqatim` / `eqaticim`, `bitand_s` / `bitor_s` /
//! `bitxor_s`.
//!
//! Each is the routine its Raku spelling already uses (ADR-0117): the `_s`
//! orderings are `str_prim::str_order`, the order of `lt`/`leg` and
//! `nqp::cmp_s`; `eqatim`/`eqaticim` are `str_prim::nqp_eqat` with the mark
//! folds `indexim`/`indexicim` use; and the bitwise ops are
//! `Interpreter::str_bitwise_op`, the body of `infix:<~&>`/`~|`/`~^` (in
//! Rakudo those operators *are* these ops). Comparisons answer a native int
//! 0/1 (or -1/0/1 for `cmp_u`), like their `_i`/`_n` siblings.

use crate::builtins::str_prim::{self, Fold};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};
use std::cmp::Ordering;

fn flag(b: bool) -> Value {
    Value::int(i64::from(b))
}

fn str_form(args: &[Value], i: usize) -> std::borrow::Cow<'_, str> {
    args.get(i)
        .map(Value::string_value_cow)
        .unwrap_or(std::borrow::Cow::Borrowed(""))
}

/// A native `uint` operand: the 64 bits of an Int reinterpreted as unsigned
/// (`-1` is `2**64 - 1`), and the low 64 bits of a BigInt, so
/// `iseq_u(-1, 2**64 - 1)` holds as in MoarVM.
fn uarg(args: &[Value], i: usize) -> u64 {
    match args.get(i).map(Value::view) {
        Some(ValueView::Int(n)) => n as u64,
        Some(ValueView::BigInt(n)) => {
            let low = n.as_ref() & num_bigint::BigInt::from(u64::MAX);
            num_traits::ToPrimitive::to_u64(&low).unwrap_or(0)
        }
        Some(_) => crate::runtime::to_int(&args[i]) as u64,
        None => 0,
    }
}

/// Dispatch one comparison / string-bitwise op, or `None` when `op` is not
/// one of them.
pub(super) fn call_nqp_compare_op(op: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(Ok(match op {
        // -- ordered string comparisons (by codepoint, as `lt` / `leg`) --
        // Cost: O(p), p = common prefix.
        "islt_s" => flag(str_prim::str_order(&str_form(args, 0), &str_form(args, 1)).is_lt()),
        // Cost: O(p), p = common prefix.
        "isle_s" => flag(str_prim::str_order(&str_form(args, 0), &str_form(args, 1)).is_le()),
        // Cost: O(p), p = common prefix.
        "isgt_s" => flag(str_prim::str_order(&str_form(args, 0), &str_form(args, 1)).is_gt()),
        // Cost: O(p), p = common prefix.
        "isge_s" => flag(str_prim::str_order(&str_form(args, 0), &str_form(args, 1)).is_ge()),

        // -- unsigned native int comparisons --
        // Cost: O(1).
        "cmp_u" => Value::int(match uarg(args, 0).cmp(&uarg(args, 1)) {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        }),
        // Cost: O(1).
        "iseq_u" => flag(uarg(args, 0) == uarg(args, 1)),
        // Cost: O(1).
        "isne_u" => flag(uarg(args, 0) != uarg(args, 1)),
        // Cost: O(1).
        "islt_u" => flag(uarg(args, 0) < uarg(args, 1)),
        // Cost: O(1).
        "isle_u" => flag(uarg(args, 0) <= uarg(args, 1)),
        // Cost: O(1).
        "isgt_u" => flag(uarg(args, 0) > uarg(args, 1)),
        // Cost: O(1).
        "isge_u" => flag(uarg(args, 0) >= uarg(args, 1)),

        // -- `eqat` ignoring marks (and case) --
        // nqp::eqatim / eqaticim($haystack, $needle, $pos).
        // Cost: O(m) amortized, m = chars of $needle.
        "eqatim" | "eqaticim" => {
            let fold = if op == "eqaticim" {
                Fold::CaseMark
            } else {
                Fold::Mark
            };
            flag(str_prim::nqp_eqat(
                args.first().unwrap_or(&Value::NIL),
                &str_form(args, 1),
                args.get(2).map(crate::runtime::to_int).unwrap_or(0),
                fold,
            ))
        }

        // -- codepoint-wise string bit ops: `~&` (shorter length), `~|` and
        // `~^` (longer length, the shorter padded with 0) --
        // Cost: O(n1 + n2), n = codepoints of each operand.
        "bitand_s" | "bitor_s" | "bitxor_s" => {
            let nil = Value::NIL;
            let (a, b) = (args.first().unwrap_or(&nil), args.get(1).unwrap_or(&nil));
            let result = match op {
                "bitand_s" => Interpreter::str_bitwise_op(a, b, |x, y| x & y, false),
                "bitor_s" => Interpreter::str_bitwise_op(a, b, |x, y| x | y, true),
                _ => Interpreter::str_bitwise_op(a, b, |x, y| x ^ y, true),
            };
            return Some(result);
        }
        _ => return None,
    }))
}
