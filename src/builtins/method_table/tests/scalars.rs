//! Tests of the scalars group (ADR-11276 §10, slice 3B).

use super::*;

/// Pin the architectural handoff, not just the observable coercion: closed
/// receivers must be answered by rows before any arity cascade is consulted.
#[test]
fn native_integer_coercers_reach_rows_for_closed_receivers() {
    for &name in crate::native_types::NATIVE_INT_COERCE_METHODS {
        let method = Symbol::intern(name);
        let range = Value::range(1, 3);
        assert_eq!(
            answer(&range, method, &[])
                .expect("Range coercion must have a row")
                .unwrap(),
            Value::int(3),
            "Range.{name}"
        );
        let path = sample(DispatchShape::IoPath);
        assert!(answer(&path, method, &[]).is_some(), "IO::Path.{name}");
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
