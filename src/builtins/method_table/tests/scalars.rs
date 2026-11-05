//! Tests of the scalars group (ADR-11276 §10, slice 3B).

use super::*;

/// Pin the architectural handoff, not just the observable coercion: the
/// native integer coercers are rows of `Cool`, so a closed receiver (a `Range`,
/// an `IO::Path`) never reaches an arity cascade. The `Cool` rows are
/// interpreter-aware (an undefined `Cool` raises the resumable numeric-context
/// warning, #15892), so a pure `answer` does not run them; their values are
/// pinned by `t/nativecall/cool-native-int-coerce-warns.t`.
#[test]
fn native_integer_coercers_are_cool_rows() {
    for &name in crate::native_types::NATIVE_INT_COERCE_METHODS {
        let method = Symbol::intern(name);
        assert!(
            owner_row(Symbol::intern("Cool"), method, 0).is_some(),
            "Cool.{name}"
        );
    }
}

/// A big-component rational has the shape of the type its flag names.
#[test]
fn big_rationals_take_their_type_s_shape() {
    let big = num_bigint::BigInt::from(u64::MAX) * num_bigint::BigInt::from(3);
    let three = num_bigint::BigInt::from(3);
    assert_eq!(
        Value::bigrat(big.clone(), three.clone() + 1).dispatch_shape(),
        Some(DispatchShape::Rat)
    );
    assert_eq!(
        Value::bigfatrat(big.clone(), three + 1).dispatch_shape(),
        Some(DispatchShape::FatRat)
    );
    assert_eq!(
        Value::bigint(big).dispatch_shape(),
        Some(DispatchShape::Int)
    );
}
