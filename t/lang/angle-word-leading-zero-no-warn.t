use v6;
use Test;

plan 4;

# `<021>` is a quote-word (q:w) that produces an allomorphic IntStr whose
# numeric value ignores the leading zero (decimal 21, not octal). Since the
# word is string-literal syntax, not numeric-literal syntax, the "Leading 0
# does not indicate octal" compile-time warning must not fire for it.
my @ans = <0 10 021 1320 02431>;
is @ans.join(' '), '0 10 021 1320 02431', 'angle word list keeps original spelling';

my $x = <021>;
isa-ok $x, IntStr, '<021> is an IntStr allomorph';
is $x.Str, '021', '<021>.Str keeps the leading zero';
is $x.Int, 21, '<021>.Int parses as decimal 21, not octal';
