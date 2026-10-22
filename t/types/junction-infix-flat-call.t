use Test;

# A user-declared junction operator (`&`, `|`, `^`) is list-associative, as the
# core operator is: a chain is one call over every operand, and a trailing
# adverb reaches that one call. A parenthesized operand stays one operand.
# Measured on rakudo 2026.09.

plan 8;

my @calls;
sub infix:<&>(*@a, :$adverb) { @calls.push: (@a.raku ~ ' ' ~ $adverb.raku); 42 }
sub infix:<|>(*@a, :$adverb) { @calls.push: (@a.raku ~ ' ' ~ $adverb.raku); 42 }
sub infix:<^>(*@a, :$adverb) { @calls.push: (@a.raku ~ ' ' ~ $adverb.raku); 42 }

1 & 2 & 3 :adverb;
is-deeply @calls, ['[1, 2, 3] Bool::True'], 'a three-operand chain with an adverb is one call';

@calls = ();
1 & 2 & 3;
is-deeply @calls, ['[1, 2, 3] Any'], 'without an adverb it is still one call';

@calls = ();
1 | 2 | 3 | 4 :adverb;
is-deeply @calls, ['[1, 2, 3, 4] Bool::True'], 'the same for `|`';

@calls = ();
1 ^ 2 ^ 3 :adverb;
is-deeply @calls, ['[1, 2, 3] Bool::True'], 'and for `^`';

@calls = ();
(1 & 2) & 3 :adverb;
is-deeply @calls, ['[1, 2] Any', '[42, 3] Bool::True'], 'a parenthesized operand is a nested call';

@calls = ();
1 & (2 & 3) :adverb;
is-deeply @calls, ['[2, 3] Any', '[1, 42] Bool::True'], 'also on the right';

@calls = ();
1 & 2 :adverb;
is-deeply @calls, ['[1, 2] Bool::True'], 'two operands with an adverb';

@calls = ();
my $r = 1 & 2 & 3;
is $r, 42, 'the call result is the expression value';
