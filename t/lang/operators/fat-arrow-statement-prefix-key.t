use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a
# bareword followed by `=>` is a pair KEY, whatever else the word means. The
# declarator keywords already got that treatment, but the `lazy` / `eager` /
# `hyper` / `race` statement prefixes were matched by an earlier parser that
# never looked for the `=>`, so those four words were the only ones in the
# language that could not name a pair outside a `{ ... }` hash composer
# (Qwiratry: `%(lazy => %(enabled => $enabled, type => $type))`).

plan 14;

# The construct the index reduced to.
my Bool $enabled = True;
my Str $type = 'none';
my %nested = lazy => %(enabled => $enabled, type => $type);
is %nested<lazy><enabled>, True, 'lazy names the outer key of a nested %( ) hash';
is %nested<lazy><type>, 'none', 'and the nested pairs survive';

# Each of the four words, in every construct that takes a pair.
is %(lazy => 1)<lazy>, 1, 'lazy is a key in %( )';
is %(eager => 2)<eager>, 2, 'eager is a key in %( )';
is %(hyper => 3)<hyper>, 3, 'hyper is a key in %( )';
is %(race => 4)<race>, 4, 'race is a key in %( )';

is (lazy => 1).key, 'lazy', 'lazy is a key in a parenthesized pair';
is (lazy => 1).value, 1, 'and carries its value';
is [eager => 5][0].key, 'eager', 'eager is a key in an array composer';
is { race => 6 }<race>, 6, 'race is a key in a hash composer';

sub named-only(*%n) { %n }
is named-only(race => 7, hyper => 8)<race>, 7, 'race is a named argument';
is named-only(race => 7, hyper => 8)<hyper>, 8, 'hyper is a named argument';

# The statement-prefix meanings are untouched: only a following `=>` diverts
# the word to the pair-key reading.
my @lazily = lazy 1 .. 3;
ok @lazily.is-lazy, 'lazy is still the statement prefix';
is (eager gather { take 9 }), (9,), 'eager is still the statement prefix';
