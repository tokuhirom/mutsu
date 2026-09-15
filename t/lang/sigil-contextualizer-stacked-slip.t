use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): the
# contextualizers STACK, so a second `|` takes the first as its operand and
# `||(5)` is `|(|(5))`. #8298 did that for `&` (`&&(0, 1)` is `&(&(0, 1))`);
# the `|` half was never done, in either the bare or the parenthesized form, so
# a term-position `||` was not a term in any reading. `Data::Translators` opens
# an assignment with one:
#
#     my $isHTML =
#             || $_.trim.starts-with('<math') && $_.trim.ends-with('</math>')
#             || $_.trim.starts-with('<table') && ...;
#
# which LOOKS like a leading-`||` boolean chain and is not one: rakudo reads it
# as `(||(A && B)) || (C && D)` -- a one-element Slip on the left, which the
# distribution only gets the answer it wants from because a one-element list is
# truthy. Every assertion below is rakudo's own answer.

plan 13;

# The bare and parenthesized forms both make a Slip, exactly as the single `|`
# already did.
is (|| 5).raku, 'slip(5,)', '|| 5 is a one-element Slip';
is (||(5)).raku, 'slip(5,)', '||(5) is a one-element Slip';
is (|||(5)).raku, 'slip(5,)', 'a third | stacks too';
is (| 5).raku, 'slip(5,)', 'the single | form is unchanged';

# The operand is a whole parenthesized group, so a comma list is in scope.
is (||(1, 2)).raku, 'slip(1, 2)', '||(1, 2) keeps both elements';

# Prefix `|` binds tighter than `&&`, so `|| False && 1` is `(||False) && 1`,
# and a one-element Slip is truthy.
is (|| False && 1).raku, '1', '|| False && 1 is (||False) && 1';
is (|| 0 && 5).raku, '5', '|| 0 && 5 is (||0) && 5';

# The `Data::Translators` shape itself: the leading `||` is the Slip, and the
# SECOND `||` is the ordinary infix.
is (|| False || True).raku, 'slip(Bool::False,)',
        'a leading || is a Slip, not a boolean chain';

# `||` after a term is still the infix logical-or operator.
is (3 || 4), 3, '|| after a term is still infix or';
my $a = 0;
my $b = 7;
is ($a || $b), 7, 'infix || picks the true operand';
my $c = 0;
$c ||= 9;
is $c, 9, '||= is still the compound assignment';

# A Slip really slips when it reaches a list.
my @flat = 1, (||(5, 6)), 2;
is @flat.raku, '[1, 5, 6, 2]', 'a stacked-contextualizer Slip flattens in a list';

# `&&` is the other half, landed in #8298 -- pinned here so the two stay
# symmetric.
is (&&(5)).raku, '5', '&&(5) is still the callable contextualizer stacked';
