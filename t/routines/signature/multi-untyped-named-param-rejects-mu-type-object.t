use v6;
use Test;

plan 6;

# An untyped `$` named parameter is `Any`, so a `Mu` type object is not a
# candidate for it and dispatch moves on to the `Mu :$type!` sibling.
multi f($v, :$type = $v.WHAT) { "any" }
multi f($v, Mu :$type!) { "mu" }

is f(1, :type(Mu)), "mu", "a Mu type object picks the Mu-typed candidate";
is f(1, :type(Int)), "any", "an Int type object stays on the untyped candidate";
is f(1), "any", "no :type stays on the untyped candidate";
is f(1, :type(Any)), "any", "Any is accepted by the untyped parameter";

sub g($v, :$type) { "g" }
throws-like { g(1, :type(Mu)) }, X::TypeCheck::Binding::Parameter,
    "a lone untyped named parameter still refuses a Mu type object";

multi h(:$x) { "untyped" }
multi h(Mu :$x!) { "mu" }
is h(:x(Mu)), "mu", "same with an optional untyped sibling";
