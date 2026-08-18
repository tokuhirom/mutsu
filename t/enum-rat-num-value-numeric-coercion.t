use Test;

plan 6;

# `coerce_to_numeric`'s `ValueView::Enum` arm unconditionally forced the
# result to a plain Int via `EnumValue::as_i64()`, which is correct for an
# Int-valued enum but silently truncates a Rat/Num-valued enum's fractional
# part, and always returns 0 for a Str-valued enum instead of parsing it
# numerically (as real raku does). Now numifies the enum's own underlying
# value recursively instead. See
# todo/tickets/rat-valued-enum-numeric-coercion-loses-fraction.md.

enum RatE (Half => 1/2);
is +Half, 0.5, 'unary + on a Rat-valued enum keeps its fractional value';
is Half + 1, 1.5, 'binary + on a Rat-valued enum keeps its fractional value';

enum NumE (F => 2.5e0);
is +NumE::F, 2.5, 'unary + on a Num-valued enum keeps its value';

enum IntE (Five => 5);
is +IntE::Five, 5, 'unary + on an Int-valued enum is unaffected (regression guard)';

enum StrNumE (A => "5");
is +StrNumE::A, 5, 'unary + on a Str-valued enum parses it numerically';

enum BoundaryE (
    Max => 9223372036854775807,
    Min => -9223372036854775808,
);
is +BoundaryE::Min, -9223372036854775808,
    'unary + on a BigInt-boundary-valued enum stays exact (regression guard)';
