use Test;

plan 11;

# `9223372036854775808` (2**63, one past i64::MAX) parses as a BigInt.
# Negating it lands exactly on i64::MIN (-9223372036854775808), which DOES
# fit in a plain Int -- but `arith_negate`'s BigInt arm used the raw
# `Value::bigint` constructor instead of the normalizing `Value::from_bigint`
# (which downcasts back to Int when the result fits), leaving a BigInt-typed
# value that a downstream numeric fast path (`Value::as_int`, an enum's
# `EnumValue::as_i64`) silently read as 0 instead of the real value. Found
# investigating CBOR::Simple's own bounds-checking constants
# (`enum CBORMinMax (... CBOR_Min_NInt_63Bit => -9223372036854775808)`),
# which triggered exactly this shape. See
# news/2026-08/bigint-negate-i64-min-downcast.md.

is -9223372036854775808 - 9223372036854775808 + 9223372036854775808, -9223372036854775808,
    'sanity: i64::MIN arithmetic without an enum still works (regression guard)';

my $big = 9223372036854775808;
isa-ok $big, Int, 'the boundary literal parses as Int (allomorph)';
is -$big, -9223372036854775808, 'negating one-past-i64::MAX lands on i64::MIN correctly';
is (-$big).WHAT, Int, 'the negated value is a plain Int type object, not a BigInt-only shape';

# The exact shape that surfaced the bug: an enum constant computed from a
# negated BigInt boundary, then read numerically via +, binary +, and a
# chained comparison.
enum CBORMinMax (
    CBOR_Max_UInt_63Bit => 9223372036854775807,
    CBOR_Min_NInt_63Bit => -9223372036854775808,
);
is +CBOR_Min_NInt_63Bit, -9223372036854775808, 'unary + on the enum constant reads its real value';
is CBOR_Min_NInt_63Bit + 0, -9223372036854775808, 'binary + on the enum constant reads its real value';
ok (CBOR_Min_NInt_63Bit <= -1 <= CBOR_Max_UInt_63Bit),
    'a chained comparison against the enum constant is correct for a negative operand';
ok (CBOR_Min_NInt_63Bit <= -9223372036854775808 <= CBOR_Max_UInt_63Bit),
    'a chained comparison against the enum constant is correct at its own boundary';

# A second, separate bug surfaced by the fix above: making `-2**63` a plain
# Int (correctly, matching Rakudo) exposed that Int+Rat addition for a
# large-magnitude Int degraded to a lossy Num whenever the sum's NUMERATOR
# (not its denominator) overflowed i64, instead of promoting to BigRat --
# `rat_from_i128_or_num` (src/builtins/arith/rat.rs) duplicated (and got
# wrong) the numerator/denominator distinction `make_big_rat_arith` already
# implements correctly; now delegates to it instead. Real Rakudo's `Rat`
# keeps an arbitrary-precision numerator with a small denominator exactly.
is (9223372036854775807 + 0.1).FatRat, "9223372036854775807.1",
    'Int + Rat keeps exact precision when only the numerator overflows i64';
is ((-2**63) + 0.1).FatRat, "-9223372036854775807.9",
    'the exact CBOR::Simple-adjacent shape (Int::MIN + 0.1) keeps precision';
is (9223372036854775807 + 0.1).WHAT, Rat,
    'the result stays a Rat (BigRat internally), not a lossy Num';
