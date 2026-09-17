use v6;
use Test;

# A Range endpoint that is a `Rat`/`FatRat` (not `Int`, `Whatever`, or a
# WhateverCode) reaches a positional subscript as a `GenericRange`. Dividing
# two `Int`s in Raku always produces a `Rat` back, even on an even split
# (`2/2` is `Rat` `1.0`, not `Int` `1`), so `^($x.codes / 2)` -- ordinary,
# not an edge case -- built a Range whose endpoint the subscript's endpoint
# resolver did not recognize. It fell through to a `_ => 0` catch-all and
# silently emptied the whole slice instead of resolving the Rat to its
# integer value.
#
# Found via Email::MIME's dependency Email::Simple (from the vendored
# distribution), whose header parser computes the CRLF/LF newline style with
# exactly this pattern: `$newlines.NFC[^($newlines.codes / 2)]>>.chr.join`.

plan 8;

my $n = 2 / 2;
is $n.^name, 'Rat', 'sanity: dividing two Ints evenly still yields a Rat, not an Int';

my @a = (10, 20, 30);
is-deeply @a[^$n], (10,), 'Array slice with a Rat-endpoint Range resolves like the equivalent Int one';

my $s = "hello";
is-deeply $s.comb[^$n], ("h",), 'a Str-derived Positional slices the same way';

# The exact shape from Email::Simple: slicing a `Uni` (`.NFC`/`.NFD`/...)
# with a Rat-endpoint Range built from dividing two Int `.codes`.
my $lit = "\n\n";
my $chr = $lit.NFC[^($lit.codes / 2)]>>.chr.join;
is $chr, "\n", 'Uni (.NFC) slice with a Rat-endpoint Range resolves to the right codepoint(s)';

# A Rat endpoint that is genuinely fractional behaves like Raku's own Range
# iteration: every integer strictly LESS than the endpoint is included, so
# `^2.5` reaches index 2 as well as 0 and 1 (not just the two below its floor).
my @b = (1, 2, 3, 4, 5);
my $frac = 5 / 2; # 2.5
is-deeply @b[^$frac], (1, 2, 3), 'a fractional Rat endpoint includes every index strictly below it';

# Exclusive-both GenericRange shapes resolve the same way.
is-deeply @b[1 ^..^ $frac], (3,), 'an exclusive-both Rat-endpoint Range resolves correctly too';

# An unbounded (`Inf`) end alongside another GenericRange endpoint must clamp
# to the container's length like any other unbounded slice -- NOT overflow.
# `Inf` is a plain `Num`, unlike `Whatever`/WhateverCode, so it takes the same
# "numeric endpoint" path a Rat does; the endpoint resolver's `floor()`
# saturates a non-finite value to `i64::MAX`, and a later un-saturating `+ 1`
# on that silently wrapped to `i64::MIN` in a release build, turning the
# unbounded end into an EMPTY slice (roast S09-subscript/slice.t regression).
my @c = (0, 1, 2);
is-deeply @c[0 ..^ Inf], (0, 1, 2), 'an exclusive Inf end clamps to the array length rather than overflowing';
is-deeply @c[0 ..  Inf], (0, 1, 2), 'an inclusive Inf end clamps to the array length too';
