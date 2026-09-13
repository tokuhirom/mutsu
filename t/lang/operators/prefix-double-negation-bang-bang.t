use v6;
use Test;

# #8206: `!!$x` (double negation, raku's idiomatic "boolify" spelling) did
# not parse -- `parse_prefix_unary_op` excluded `!!` wholesale because it is
# also the ternary's else marker (`$c ?? $a !! $b`). raku itself resolves
# the ambiguity by requiring `!!` to be glued directly onto its term with no
# whitespace to mean double negation; with a space it is a compile error
# ("Two terms in a row"), and a ternary's `!!` marker always has a space on
# both sides in valid Raku, so the two can never collide.

plan 14;

my $x = 1;
my $zero = 0;

is !!$x, True, '!!$x is double negation (boolify)';
is !!$zero, False, '!!$zero is False';
is !!(1 < 2), True, '!!(...) parses the same way';
is !!(1 > 2), False, '!!(...) with a false inner expression';

sub f($point) { return !!($point < 1) }
is f(0.5), True, 'return !!(...) works inside a routine body';

# The ternary's `!!` marker is unaffected -- it always has surrounding
# whitespace, which the glued-only prefix check can never match.
is (True ?? 1 !! 2), 1, 'ternary !! marker still works';
is (False ?? 1 !! 2), 2, 'ternary !! marker still works (else branch)';

# A bareword/type-object then-branch (the ambiguity the ticket calls out --
# a bareword listop head could otherwise try to gobble the !! as an
# argument) must still resolve the ternary correctly.
enum Color <Red Green Blue>;
is (True ?? Red !! Blue), Red, 'ternary with an enum-value then-branch';
is (True ?? Int !! Str), Int, 'ternary with a type-object then-branch';

# The `!!!` fatal-stub operator (a distinct, atomic 3-bang marker) must not
# be mistaken for `!!` applied to a `!`-prefixed term.
{
    my $died = False;
    try { !!!; CATCH { default { $died = True } } }
    ok $died, '!!! (fatal stub) still throws, unaffected by the !! fix';
}

# Plain single negation is unaffected.
is !$x, False, 'plain single ! negation is unaffected';

# A word infix operator's own name can never be a term, so `!!eq`/`!!and`
# must stay illegal rather than parse as double negation of a bareword call
# (roast/S03-metaops/not.t: "Doubled prefix:<!> is illegal").
throws-like '"a" !!eq "a"', X::Syntax::Confused, '!!eq is illegal, not double negation';
throws-like 'True !!and False', X::Syntax::Confused, '!!and is illegal too';

# A bareword that is NOT a word infix operator's name is still a legitimate
# glued term, so double negation of it still works.
sub truthy { True }
is !!truthy, True, '!!bareword-call is still double negation when the word is not an infix op';
