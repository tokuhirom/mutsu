//! Tests of the scalars group (ADR-11276 §10, slice 3B).

use super::*;

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
