use Test;

# `eqv` on Sets is type-strict for allomorphs: a Set holding the IntStr
# allomorph <42> is NOT eqv to a Set holding the plain Int 42, even though both
# share the string key "42". (See roast/S03-operators/eqv.t, "Setty eqv Setty".)

plan 8;

is-deeply set(<42>) eqv set( 42 ), False, 'IntStr does not eqv Int';
is-deeply set(<42>) eqv set('42'), False, 'IntStr does not eqv Str';
is-deeply set( 42 ) eqv set(<42>), False, 'Int does not eqv IntStr';
is-deeply set('42') eqv set(<42>), False, 'Str does not eqv IntStr';
is-deeply set(<42>) eqv set(<42>), True,  'IntStr does eqv IntStr';

# Non-allomorph elements that share a string key remain eqv regardless of how
# the type was tracked internally.
is-deeply set(1, 2)   eqv set(1, 2),   True, 'identical Int sets eqv';
is-deeply set(1.1, 2.1) eqv set(1.1, 2.1), True, 'identical Rat sets eqv';

# A RatStr allomorph (string key "1.5") is distinct from the plain Rat 1.5.
is-deeply set(<1.5>) eqv set(1.5), False, 'RatStr does not eqv Rat';
