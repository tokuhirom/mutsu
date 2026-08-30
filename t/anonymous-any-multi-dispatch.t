use Test;

# Anonymous type-only parameters use the internal `__type_only__` name.  Real
# type constraints must still use nominal type matching even when a type name
# also has a value in the environment (as `Any` does).

plan 8;

multi any-fallback(Any) { 'any' }

is any-fallback(42), 'any', 'anonymous Any accepts an Int';
is any-fallback('text'), 'any', 'anonymous Any accepts a Str';

multi redispatch(Int) { 'int:' ~ callsame() }
multi redispatch(Any) { 'any' }

is redispatch(1), 'int:any', 'callsame reaches an anonymous Any fallback';
is redispatch('text'), 'any', 'the Any fallback is directly selectable';

# Other anonymous type constraints are controls for the ordinary nominal path.
multi nominal(Cool) { 'cool' }
multi nominal(Mu) { 'mu' }

is nominal(42), 'cool', 'a narrower anonymous type still wins';
is nominal(Mu), 'mu', 'anonymous Mu accepts its type object';

# Bare value terms deliberately share the parser representation with anonymous
# types.  They must keep comparing against their environment value rather than
# turning into nominal type checks.
multi order-value(Less) { 'less' }
multi order-value(Any) { 'other' }

is order-value(Less), 'less', 'anonymous enum-value parameter still matches';
is order-value(More), 'other', 'anonymous enum-value parameter still rejects peers';
