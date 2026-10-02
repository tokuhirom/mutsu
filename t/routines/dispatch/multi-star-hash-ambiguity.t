use Test;

# A slurpy hash (`*%m`) never takes a positional, so it does not change a
# candidate's dispatch shape: `multi f(*%m)` and `multi f()` tie for `f()` and
# the call is ambiguous, whichever was declared first. Every expectation below
# was checked against rakudo (#10663).

plan 12;

{
    multi f(*%m) { "m" }
    multi f() { "e" }
    throws-like { f() }, X::Multi::Ambiguous, 'f(*%m) and f() are ambiguous for f()';
    is f(:a), 'm', 'a named argument still selects the slurpy-hash candidate';
}

{
    multi g() { "e" }
    multi g(*%m) { "m" }
    throws-like { g() }, X::Multi::Ambiguous, '... in either declaration order';
}

{
    multi k($x, *%m) { "mx" }
    multi k($x) { "x" }
    throws-like { k(1) }, X::Multi::Ambiguous, 'a trailing *%m does not split ($x, *%m) from ($x)';
}

{
    multi p($x?, *%m) { "mx" }
    multi p($x?) { "x" }
    throws-like { p(1) }, X::Multi::Ambiguous, '($x?, *%m) vs ($x?) with an argument';
    throws-like { p() }, X::Multi::Ambiguous, '($x?, *%m) vs ($x?) without one';
}

{
    multi r(*@a, *%m) { "am" }
    multi r(*@a) { "a" }
    throws-like { r(1) }, X::Multi::Ambiguous, '(*@a, *%m) vs (*@a) with an argument';
    throws-like { r() }, X::Multi::Ambiguous, '(*@a, *%m) vs (*@a) without one';
}

{
    multi w(*%m where .elems == 0) { "w" }
    multi w() { "e" }
    is w(), 'w', 'a where on the slurpy hash is a named bind check, not a tie';
}

{
    multi z($x) { "x" }
    multi z(*%m) { "m" }
    is z(1), 'x', 'a positional candidate is unaffected';
    is z(), 'm', 'the slurpy-hash candidate is the only one for z()';
}

{
    my class C {
        multi method n(*%h) { "h" }
        multi method n() { "e" }
    }
    throws-like { C.n() }, X::Multi::Ambiguous, 'methods agree';
}
