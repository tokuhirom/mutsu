use v6;
use Test;

# SQL::Abstract: `$o ?? ('ONLY', $n) !! $n,;` as the last statement of a sub
# declared `--> List` is a one-element List (the trailing comma), exactly like
# the `{ 42, }` form.

plan 7;

sub one { 1,; }
is-deeply one(), (1,), 'a bare `1,;`';

sub var { my $n = 3; $n,; }
is-deeply var(), (3,), 'a variable then `,;`';

sub ternary($o --> List) { my $n = "nm"; $o ?? ('ONLY', $n) !! $n,; }
is-deeply ternary(False), ("nm",), 'ternary else branch with a trailing comma';
is-deeply ternary(True), (('ONLY', "nm"),), 'ternary then branch is wrapped by the comma';

sub spaced { 1, ; }
is-deeply spaced(), (1,), 'a space before the semicolon';

sub plain { 1; }
is plain(), 1, 'no comma stays a scalar';

sub two { 1, 2,; }
is-deeply two(), (1, 2), 'two elements and a trailing comma';

done-testing;
