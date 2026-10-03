use v6.e.PREVIEW;
use Test;

# v6.e's `last VALUE` / `next VALUE` (#11073): the loop ends (or the
# iteration does) and VALUE is that iteration's contribution to the loop's
# result list. Expected values are rakudo's.

plan 17;

is-deeply (do for ^5 { last $_ * 10 if $_ == 2; $_ }).List, (0, 1, 20),
    'last VALUE ends a for loop with VALUE as its final element';
is-deeply (do for ^5 { next $_ * 10 if $_ %% 2; $_ }).List, (0, 1, 20, 3, 40),
    'next VALUE replaces the iteration value';
is-deeply (for ^5 { next 42 if $_ == 1; $_ }).List, (0, 42, 2, 3, 4),
    'next VALUE in a parenthesized for';
is-deeply (for ^5 { last($_) if $_ == 3; $_ }).List, (0, 1, 2, 3),
    'the call form last(VALUE) takes a value too';

my $i = 0;
my @w = do while $i < 5 { $i++; last 99 if $i == 3; $i };
is-deeply @w, [1, 2, 99], 'last VALUE in a while loop';

my @l = do loop (my $k = 0; $k < 5; $k++) { next -1 if $k == 1; $k };
is-deeply @l, [0, -1, 2, 3, 4], 'next VALUE in a C-style loop';

my $j = 0;
is-deeply (while $j < 5 { $j++; next 0 if $j == 2; last(Nil) if $j == 4; $j }).List,
    (1, 0, 3, Nil), 'next/last VALUE in a parenthesized while';

is-deeply (^5).map({ last 7 if $_ == 2; $_ }).List, (0, 1, 7),
    'last VALUE in a map block';
is-deeply (^5).map({ next 7 if $_ == 2; $_ }).List, (0, 1, 7, 3, 4),
    'next VALUE in a map block';

is-deeply (for ^3 { last Nil }).List, (Nil,), 'last Nil contributes a Nil';
is-deeply (for ^3 { next Empty }).List, (), 'next Empty contributes nothing';
is-deeply (for ^3 { last (1, 2) }).List, ((1, 2),),
    'a spaced parenthesized list is one value';

my $x = 5;
is-deeply (for ^3 { last $x }).List, (5,), 'last with a variable';

my @g = gather for ^4 { take $_; last 9 if $_ == 1 };
is-deeply @g, [0, 1], 'the value is not taken by an enclosing gather';

my $seen = 0;
LBL: for ^3 { for ^3 { $seen++; last(LBL) } }
is $seen, 1, 'last(LABEL) is still the labelled last under v6.e';

for ^3 { last 5 };
pass 'last VALUE in sink context just ends the loop';

throws-like { last 5 }, X::ControlFlow,
    'last VALUE with no loop is X::ControlFlow';
