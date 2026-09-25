//! The scalar body of each native-int / native-num `nqp::` op (ADR-0118).
//!
//! `nqp::add_i` and friends have three executors: the interpreter's `nqp::`
//! op tables (through `nqp_pure::eval`), TRIR's typed ops (`trir/exec.rs`),
//! and the JIT's inline fast paths (which only claim operands whose answer is
//! the same as these by construction, and otherwise call back into the
//! interpreter). The first two used to carry their own bodies and they
//! drifted: `nqp::bitshiftl_i(1, 64)` was `i64::MIN` in the interpreter
//! (count clamped to 63) and 1 under TRIR (count masked to 6 bits, which is
//! MoarVM's answer). Every executor now calls the function here.
//!
//! These are *native* semantics: they wrap on overflow and never promote to a
//! BigInt. Raku's own `Int` operators are a different contract and live in
//! `builtins::arith` (`int_div`, `int_bitop`, ...).

/// `nqp::add_i`. Cost: O(1).
#[inline]
pub(crate) fn add_i(a: i64, b: i64) -> i64 {
    a.wrapping_add(b)
}

/// `nqp::sub_i`. Cost: O(1).
#[inline]
pub(crate) fn sub_i(a: i64, b: i64) -> i64 {
    a.wrapping_sub(b)
}

/// `nqp::mul_i`. Cost: O(1).
#[inline]
pub(crate) fn mul_i(a: i64, b: i64) -> i64 {
    a.wrapping_mul(b)
}

/// Native unsigned `+` (a `uint` register): wraps modulo 2**64. The result
/// is the register's bits boxed as a signed Int, as Rakudo does; the
/// destination native store reinterprets a negative value back into the
/// unsigned range (`uint $x = 0; $x - 1` is -1, `$x -= 1` stores
/// `uint64.max`). Cost: O(1).
#[inline]
pub(crate) fn add_u(a: u64, b: u64) -> i64 {
    a.wrapping_add(b) as i64
}

/// Native unsigned `-`; see [`add_u`] for the result's representation.
/// Cost: O(1).
#[inline]
pub(crate) fn sub_u(a: u64, b: u64) -> i64 {
    a.wrapping_sub(b) as i64
}

/// Native unsigned `*`; see [`add_u`] for the result's representation.
/// Cost: O(1).
#[inline]
pub(crate) fn mul_u(a: u64, b: u64) -> i64 {
    a.wrapping_mul(b) as i64
}

/// `nqp::neg_i`. Cost: O(1).
#[inline]
pub(crate) fn neg_i(a: i64) -> i64 {
    a.wrapping_neg()
}

/// `nqp::abs_i` (`abs_i(i64::MIN)` is `i64::MIN`, as in MoarVM). Cost: O(1).
#[inline]
pub(crate) fn abs_i(a: i64) -> i64 {
    a.wrapping_abs()
}

/// `nqp::bitshiftl_i`: the count is taken modulo 64 (its low six bits, as the
/// machine shift MoarVM emits does), so `bitshiftl_i(1, 64)` is 1 and a
/// negative count is a large positive one. Cost: O(1).
#[inline]
pub(crate) fn shl_i(a: i64, count: i64) -> i64 {
    a.wrapping_shl(count as u32)
}

/// `nqp::bitshiftr_i`: an arithmetic shift, count modulo 64. Cost: O(1).
#[inline]
pub(crate) fn shr_i(a: i64, count: i64) -> i64 {
    a.wrapping_shr(count as u32)
}

/// `nqp::div_i`: *floored* division (`div_i(-7, 2)` is -4). `None` on a zero
/// divisor, which the caller turns into its own error. `i64::MIN div -1`
/// wraps to `i64::MIN` rather than trapping. Cost: O(1).
#[inline]
pub(crate) fn div_i(a: i64, b: i64) -> Option<i64> {
    if b == 0 {
        return None;
    }
    if a == i64::MIN && b == -1 {
        return Some(i64::MIN);
    }
    let q = a / b;
    Some(if a % b != 0 && (a < 0) != (b < 0) {
        q - 1
    } else {
        q
    })
}

/// `nqp::mod_i`: MoarVM's *truncated* remainder, taking the dividend's sign
/// (`mod_i(-7, 3)` is -1), unlike Raku's floored `%`. `None` on a zero
/// divisor. Cost: O(1).
#[inline]
pub(crate) fn mod_i(a: i64, b: i64) -> Option<i64> {
    (b != 0).then(|| a.wrapping_rem(b))
}

/// `nqp::cmp_i`. Cost: O(1).
#[inline]
pub(crate) fn cmp_i(a: i64, b: i64) -> i64 {
    ordering(a.cmp(&b))
}

/// `nqp::cmp_n`: 0 when either side is NaN, as in MoarVM. Cost: O(1).
#[inline]
pub(crate) fn cmp_n(a: f64, b: f64) -> i64 {
    a.partial_cmp(&b).map_or(0, ordering)
}

#[inline]
fn ordering(o: std::cmp::Ordering) -> i64 {
    match o {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Measured against rakudo's `use nqp`.
    #[test]
    fn matches_moarvm() {
        assert_eq!(shl_i(1, 64), 1);
        assert_eq!(shl_i(1, 65), 2);
        assert_eq!(shl_i(1, -1), i64::MIN);
        assert_eq!(shl_i(3, -62), 12);
        assert_eq!(shr_i(8, -1), 0);
        assert_eq!(shr_i(-8, 64), -8);
        assert_eq!(shr_i(-8, 65), -4);
        assert_eq!(div_i(-7, 2), Some(-4));
        assert_eq!(div_i(1, 0), None);
        assert_eq!(div_i(i64::MIN, -1), Some(i64::MIN));
        assert_eq!(mod_i(-7, 2), Some(-1));
        assert_eq!(abs_i(i64::MIN), i64::MIN);
        assert_eq!(cmp_n(f64::NAN, f64::NAN), 0);
        assert_eq!(add_i(i64::MAX, 1), i64::MIN);
    }
}
