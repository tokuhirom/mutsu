use v6;
use Test;

# A required parameter is narrower than an optional one in multi dispatch.
# Regression: mutsu's specificity comparator ignored required-vs-optional, so
# `multi f(Int:D)` tied with `multi f(Int $z?)` and the tie broke by declaration
# order — and a *non-matching* `Int:U` sibling shifted the result. raku ranks a
# required param as narrower than an optional/defaulted/slurpy one, with type
# narrowness still dominating.

plan 9;

# The finding [6] repro: three candidates, defined arg picks the :D (required),
# even though a non-matching :U candidate is present.
{
    my multi c(Int:U)  { "typeobj" }
    my multi c(Int:D)  { "defined" }
    my multi c(Int $z?) { "optional" }
    is c(42),  "defined", "Int:D (required) beats Int \$z? (optional) with a :U sibling";
    is c(Int), "typeobj", "Int:U still matches the type object";
}

# Two candidates only: plain required beats optional.
{
    my multi d(Int)     { "plain" }
    my multi d(Int $z?) { "opt" }
    is d(42), "plain", "plain required Int beats optional Int";
}

# Declaration order must not decide it (optional declared first).
{
    my multi g(Int $z?) { "opt" }
    my multi g(Int:D)   { "defined" }
    is g(42), "defined", "required wins regardless of declaration order";
}

# Type narrowness still dominates required-vs-optional.
{
    my multi p(Cool $x)  { "req-Cool" }
    my multi p(Int $y?)  { "opt-Int" }
    is p(42), "opt-Int", "narrower type wins even when it is the optional candidate";
}

# Fewer optional/defaulted positionals is narrower.
{
    my multi q1($a)      { "one" }
    my multi q1($a, $b?) { "two-opt" }
    is q1(1), "one", "candidate with no optional param is narrower";
}
{
    my multi q2(Int $a)              { "one" }
    my multi q2(Int $a, Int $b = 5)  { "two-default" }
    is q2(1), "one", "candidate with no defaulted param is narrower";
}

# Required beats slurpy.
{
    my multi r(Int $a) { "req" }
    my multi r(*@a)    { "slurpy" }
    is r(1), "req", "required positional beats a slurpy candidate";
}

# Regression guard: :D vs plain (both required) is still a genuine specificity
# tie — required-vs-optional does not spuriously separate them.
{
    my multi s(Int:D $x) { "D" }
    my multi s(Int $z?)  { "opt" }
    is s(42), "D", "Int:D (required) beats Int \$z? (optional), 2-candidate case";
}
