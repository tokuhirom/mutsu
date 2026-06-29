use Test;

plan 12;

# The sound multi-function resolution cache (func_multi_resolve_cache) memoizes
# the winning candidate keyed by (package, name, positional-arg-types) for a
# type+arity-deterministic multi. These tests lock in that the cache dispatches
# correctly across arg types, arities, runtime registry changes, and value-
# dependent multis (which must NOT be cached).

# Type-based dispatch: different arg types hit different cache keys.
multi g(Int $x) { "int" }
multi g(Str $x) { "str" }
multi g($x)     { "any" }
is g(1),    'int', 'Int arg -> Int candidate';
is g("x"),  'str', 'Str arg -> Str candidate';
is g(1.5),  'any', 'Rat arg -> Any candidate';
# Repeat to exercise the cache-hit path.
is g(2),    'int', 'second Int call (cache hit) still correct';
is g("yy"), 'str', 'second Str call (cache hit) still correct';

# Arity is part of the key.
multi h($a)     { "one" }
multi h($a, $b) { "two" }
is h(1),    'one', 'arity-1 candidate';
is h(1, 2), 'two', 'arity-2 candidate';

# A value-dependent multi (where-constraint) must NOT be cached by type alone:
# two Int args with different values take different candidates.
multi w(Int $x where * > 10) { "big" }
multi w(Int $x)              { "small" }
is w(5),  'small', 'where-constrained multi: small value';
is w(50), 'big',   'where-constrained multi: big value (not mis-cached as small)';

# Value-refining numeric pseudo-types (Inf/NaN) match WITHIN one value_type_name
# ("Num"), so they must NOT be type-cached, or a plain Num would mis-route to the
# Inf/NaN candidate (or vice versa).
multi v(Inf)         { 'inf' }
multi v(NaN)         { 'nan' }
multi v(Numeric $)   { 'num' }
is v(Inf), 'inf', 'Inf value-pseudo-type not mis-cached as Numeric';
is v(NaN), 'nan', 'NaN value-pseudo-type not mis-cached as Numeric';
is v(3.5), 'num', 'plain Num reaches Numeric candidate (not Inf/NaN)';
