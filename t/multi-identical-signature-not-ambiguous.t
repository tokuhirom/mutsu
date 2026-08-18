use Test;
plan 4;

# Two `multi` candidates whose declared signatures are byte-identical are
# not a meaningful overload in Rakudo -- it silently runs whichever was
# declared/imported first, rather than raising X::Multi::Ambiguous. This
# matters for real-world code: `Test.rakumod` exports
# `multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) is export`, and
# a test file that declares its own identical `trait_mod:<is>` candidate
# (a documented, deliberate pattern -- see
# t/user-trait-mod-does-not-consume-every-trait.t) must not turn every
# `is test-assertion` application into an ambiguous-dispatch error.

multi sub foo(Int $x, :$bar!) { "first" }
multi sub foo(Int $x, :$bar!) { "second" }
is foo(1, :bar), "first", "identical multi signatures: first-declared wins, not ambiguous";

# Parameter *names* don't affect narrowness or identity for this purpose.
multi sub baz(Int $x, :$bar!) { "first" }
multi sub baz(Int $y, :$bar!) { "second" }
is baz(1, :bar), "first", "identical signatures with differently-named params still resolve, not ambiguous";

# Regression guard: candidates that are equal on every *dispatch-visible*
# parameter but differ after a `;;` long-name separator are NOT the same
# declaration and must still be reported ambiguous.
multi sub sep-diff(;; Any $v) { "any" }
multi sub sep-diff(;; Int $v) { "int" }
dies-ok { sep-diff(1) },
    "candidates differing only after ';;' are still genuinely ambiguous";

# Regression guard: the "identical signature is not ambiguous" leniency does
# NOT extend to two purely *positional* duplicate declarations (no named
# parameter at all on either side) -- Rakudo's dispatcher only takes the
# lenient trial-bind path when a named parameter is present.
# roast/integration/advent2011-day24.t pins exactly this shape
# (`multi sub Slurp($filename) {...}` declared twice).
multi sub Slurp2($filename) { "first" }
multi sub Slurp2($filename) { "second" }
dies-ok { Slurp2("x") },
    "two purely positional duplicate declarations are still genuinely ambiguous";
