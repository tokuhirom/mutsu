use Test;

# Multi-sub candidates are ranked in rakudo's topological tiers over the
# whole candidate set, including candidates that cannot bind the call
# (#12795). Two candidates whose arities both differ are incomparable on
# their own; a third candidate narrower than one of them pushes that one into
# a later tier. Expected values from rakudo.

plan 7;

# `C` (a required named) is narrower than `A` and incomparable with `B`, so
# `B` shares the first tier with `C` and `A` sits below them: `B` wins
# although `A` has the more specific types.
multi sub a(Int $g, Int $e, $d = '')            { "A" }
multi sub a(Any $g, Any $e, Any $t, $d = '')    { "B" }
multi sub a(Int $g, Int $e, $d = '', :$rel!)    { "C" }
is a(5, 6, 1), 'B', 'a later-tier candidate loses to an incomparable first-tier one';
is a(5, 6), 'A', 'it still wins a call only it binds';
is a(5, 6, :rel(1)), 'C', 'the required-named candidate wins when its named is passed';

multi sub b(Any $g, Any $e, Any $t, $d = '')    { "B" }
multi sub b(Int $g, Int $e, $d = '')            { "A" }
multi sub b(Int $g, Int $e, $d = '', :$rel!)    { "C" }
is b(5, 6, 1), 'B', 'whatever the declaration order';

# Test's own is-approx candidates resolve through the same tiers.
multi sub approx(Numeric $got, Numeric $expected, $desc = '') { "rel" }
multi sub approx(Numeric $got, Numeric $expected, Numeric $abs-tol, $desc = '') { "abs" }
multi sub approx(Numeric $got, Numeric $expected, $desc = '', Numeric :$rel-tol!) { "rel-tol" }
multi sub approx(Numeric $got, Numeric $expected, $desc = '', Numeric :$abs-tol!) { "abs-tol" }
is approx(5, 6, 1), 'abs', 'a third positional is the absolute tolerance';
is approx(5, 6, 'desc'), 'rel', 'a string third positional is the description';
is approx(5, 6, :abs-tol(1)), 'abs-tol', 'the named tolerance candidate';
