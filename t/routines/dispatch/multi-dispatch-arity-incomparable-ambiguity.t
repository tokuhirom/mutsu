use Test;

# Two multi-sub candidates that differ in both positional count and required
# positional count, with no slurpy, are incomparable in rakudo's
# `is_narrower`. When both land in one tier and both bind, the call is
# ambiguous unless a bind-time check (a `where`, a named parameter) is
# involved, in which case the first declared that binds wins (#12795).
# Expected values from rakudo.

plan 6;

multi sub a(Int $g, Int $e, $d = '')         { "A" }
multi sub a(Any $g, Any $e, Any $t, $d = '') { "B" }
throws-like { a(5, 6, 1) }, X::Multi::Ambiguous,
    'two binding arity-incomparable candidates are ambiguous';
is a(5, 6), 'A', 'a call only one of them binds is not';

multi sub b(Int $g where * > 0, Int $e, $d = '') { "A" }
multi sub b(Any $g, Any $e, Any $t, $d = '')     { "B" }
is b(5, 6, 1), 'A', 'a where clause makes the first declared win';

multi sub c(Any $g, Any $e, Any $t, $d = '')     { "B" }
multi sub c(Int $g where * > 0, Int $e, $d = '') { "A" }
is c(5, 6, 1), 'B', 'whichever is declared first';

multi sub d(Int $g, Int $e, $d = '', :$x)    { "A" }
multi sub d(Any $g, Any $e, Any $t, $d = '') { "B" }
is d(5, 6, 1), 'A', 'so does a named parameter';

multi sub e(Int $g, $d?)          { "A" }
multi sub e(Any $g, $e, $f, $h?)  { "B" }
is e(5, 6), 'A', 'a candidate that cannot bind is no rival';
