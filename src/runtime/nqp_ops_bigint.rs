//! The big-integer `nqp::*_I` op family (#9344).
//!
//! In MoarVM these are the P6bigint ops behind Rakudo's own `Int` operators
//! (`infix:<div>` on two Ints *is* `nqp::div_I`), so per ADR-0118 each one
//! here is a thin call into the shared Int home in `src/builtins/arith/` —
//! never a private copy that could drift from the operator it implements.
//!
//! Operands are read at full precision (an i64 Int or a BigInt), and the
//! trailing type argument(s) (`div_I($a, $b, Int)`, `pow_I($a, $b, Num, Int)`)
//! name the boxing type(s), which mutsu's single `Int` value does not need.
//! The comparison ops answer a native int 0/1, like their `_i` siblings.

use crate::builtins::{self, BitOp};
use crate::value::{RuntimeError, Value};

fn arg(args: &[Value], i: usize) -> Value {
    args.get(i).cloned().unwrap_or_else(|| Value::int(0))
}

fn flag(b: bool) -> Value {
    Value::int(i64::from(b))
}

fn nonzero_divisor(op: &str, args: &[Value]) -> Result<(), RuntimeError> {
    if builtins::int_cmp(&arg(args, 1), &Value::int(0)).is_eq() {
        return Err(RuntimeError::new(format!("nqp::{op}: division by zero")));
    }
    Ok(())
}

/// Dispatch one `*_I` op, or `None` when `op` is not one of them.
pub(super) fn call_nqp_bigint_op(op: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let (a, b) = (arg(args, 0), arg(args, 1));
    Some(Ok(match op {
        // Cost: O(d), d = digits of the larger operand.
        "add_I" => {
            return Some(builtins::arith_add(
                builtins::int_operand(&a),
                builtins::int_operand(&b),
            ));
        }
        // Cost: O(d), d = digits of the larger operand.
        "sub_I" => builtins::arith_sub(builtins::int_operand(&a), builtins::int_operand(&b)),
        // Cost: O(d^2), d = digits of the larger operand.
        "mul_I" => builtins::arith_mul(builtins::int_operand(&a), builtins::int_operand(&b)),
        // Floored, like `infix:<div>`.
        // Cost: O(d^2), d = digits of the larger operand.
        "div_I" => {
            if let Err(e) = nonzero_divisor(op, args) {
                return Some(Err(e));
            }
            builtins::int_div(&a, &b)
        }
        // Floored (the result takes the divisor's sign), like `infix:<%>`.
        // Cost: O(d^2), d = digits of the larger operand.
        "mod_I" => {
            if let Err(e) = nonzero_divisor(op, args) {
                return Some(Err(e));
            }
            return Some(builtins::arith_mod(
                builtins::int_operand(&a),
                builtins::int_operand(&b),
            ));
        }
        // A negative exponent answers a Num, as in MoarVM (`pow_I(2, -1)` is
        // 0.5, not the Rat `2 ** -1` gives).
        // Cost: O(d^2 * log e), d = digits of the result, e = the exponent.
        "pow_I" => {
            let (base, exp) = (builtins::int_operand(&a), builtins::int_operand(&b));
            if builtins::int_cmp(&exp, &Value::int(0)).is_lt() {
                Value::num(base.to_f64().powf(exp.to_f64()))
            } else {
                builtins::arith_pow(base, exp)
            }
        }
        // Cost: O(d), d = digits of the operand.
        "neg_I" => return Some(builtins::arith_negate(builtins::int_operand(&a))),
        // Cost: O(d), d = digits of the operand.
        "abs_I" => builtins::int_abs_value(&a),
        // Cost: O(d^2), d = digits of the larger operand.
        "gcd_I" => builtins::int_gcd(&a, &b),
        // Cost: O(d^2), d = digits of the larger operand.
        "lcm_I" => builtins::int_lcm(&a, &b),
        // Cost: O(d), d = digits of the larger operand.
        "bitand_I" => builtins::int_bitop(&a, &b, BitOp::And),
        // Cost: O(d), d = digits of the larger operand.
        "bitor_I" => builtins::int_bitop(&a, &b, BitOp::Or),
        // Cost: O(d), d = digits of the larger operand.
        "bitxor_I" => builtins::int_bitop(&a, &b, BitOp::Xor),
        // Cost: O(d), d = digits of the operand.
        "bitneg_I" => builtins::int_bitneg(&a),
        // Cost: O(d + s), d = digits of the operand, s = the shift count.
        "bitshiftl_I" => builtins::int_shift_left(&a, &b),
        // Cost: O(d + s), d = digits of the operand, s = the shift count.
        "bitshiftr_I" => builtins::int_shift_right(&a, &b),
        // Cost: O(d), d = digits of the larger operand.
        "cmp_I" => Value::int(match builtins::int_cmp(&a, &b) {
            std::cmp::Ordering::Less => -1,
            std::cmp::Ordering::Equal => 0,
            std::cmp::Ordering::Greater => 1,
        }),
        // Cost: O(d), d = digits of the larger operand.
        "iseq_I" => flag(builtins::int_cmp(&a, &b).is_eq()),
        // Cost: O(d), d = digits of the larger operand.
        "isne_I" => flag(builtins::int_cmp(&a, &b).is_ne()),
        // Cost: O(d), d = digits of the larger operand.
        "islt_I" => flag(builtins::int_cmp(&a, &b).is_lt()),
        // Cost: O(d), d = digits of the larger operand.
        "isle_I" => flag(builtins::int_cmp(&a, &b).is_le()),
        // Cost: O(d), d = digits of the larger operand.
        "isgt_I" => flag(builtins::int_cmp(&a, &b).is_gt()),
        // Cost: O(d), d = digits of the larger operand.
        "isge_I" => flag(builtins::int_cmp(&a, &b).is_ge()),
        _ => return None,
    }))
}
