use v6;
use Test;

plan 9;

# A multi candidate with a `where` constraint on an OPTIONAL (defaulted) param
# must still match a call that omits that argument: the omitted param defaults,
# and the `where` is checked at bind time against the default — not against the
# bare type object during dispatch. mutsu used to reject the candidate with
# "No matching candidates for proto sub" (regression driver: Math::Root's
# `multi iroot(Int() $i where * < 1e12, Int $n where * >= 2 = 2)`).

{
    multi f(Int $a, Int $b where * >= 2 = 2) { "$a,$b" }
    is f(1),    '1,2', 'defaulted where-param: candidate matches with arg omitted';
    is f(1, 5), '1,5', 'defaulted where-param: supplied value that satisfies where';
}

# When the argument IS supplied and fails the `where`, the candidate is still
# rejected (dispatch falls through to a fallback candidate).
{
    multi g(Int $a, Int $b where * >= 2 = 2) { "constrained:$a,$b" }
    multi g(Int $a, Int $b)                  { "fallback:$a,$b" }
    is g(1),    'constrained:1,2', 'omitted arg → constrained candidate (default 2 satisfies where)';
    is g(1, 5), 'constrained:1,5', 'supplied 5 satisfies where → constrained';
    is g(1, 1), 'fallback:1,1',    'supplied 1 fails where → fallback candidate';
}

# The same, with a coercion type on the leading param (the exact Math::Root shape).
{
    multi h(Int() $a where * < 100, Int $n where * >= 2 = 2) { "small:$a,$n" }
    multi h(Int() $a where * >= 100, Int $n where * >= 2 = 2) { "big:$a,$n" }
    is h(5),   'small:5,2',  'coercion + where-optional: small branch, arg omitted';
    is h(500), 'big:500,2',  'coercion + where-optional: big branch, arg omitted';
    is h(5, 3), 'small:5,3', 'coercion + where-optional: small branch, arg supplied';
}

# A non-multi sub already worked; keep it covered.
{
    sub s(Int $a, Int $b where * >= 2 = 2) { "$a,$b" }
    is s(1), '1,2', 'non-multi sub with where-optional param';
}
