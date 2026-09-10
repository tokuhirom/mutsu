use v6;
use Test;

plan 10;

# `[ident]` is a one-element array literal unless `ident` is an infix
# operator: raku only reduces with infixes. mutsu accepted ANY identifier
# as a reduction op, so `[red]` became a reduction over the unknown op
# "red" and evaluated to Nil.

enum Color <red green>;
is [red].elems, 1, '[enum-value] is a one-element array';
ok [red][0] === red, 'the element is the enum value itself';
is [RaceSeq].elems, 1, '[Type] with a meta-letter initial is an array, not R+"aceSeq"';
is [green, red].elems, 2, 'enum list literal unaffected';

# Real reductions keep working.
is ([+] 1, 2, 3), 6, 'symbolic reduction';
is ([min] 3, 1, 2), 1, 'word-infix reduction (min)';
is ([lcm] 4, 6), 12, 'word-infix reduction (lcm)';
is ([Z] (1, 2), (3, 4)).gist, '((1 3) (2 4))', 'meta Z reduction';

# A user-declared infix reduces — including via the whole-op match that must
# win before meta-prefix stripping.
sub infix:<foo>($a, $b) { $a + $b }
is ([foo] 1, 2, 3), 6, 'user-declared word infix reduces';
sub infix:<Xyz>($a, $b) { $a * $b }
is ([Xyz] 2, 3, 4), 24, 'declared infix starting with a meta letter reduces whole';
