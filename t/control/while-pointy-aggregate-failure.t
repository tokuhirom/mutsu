use Test;

# From Concurrent::Queue: `while COND -> @t` tests COND's own truthiness,
# so a Failure ends the loop instead of being bound into @t.
plan 4;

sub f($n) { $n > 0 ?? ($n, 1) !! fail "empty" }

my @got;
my $i = 2;
while f($i--) -> @t { @got.push: @t.List }
is-deeply @got, [(2, 1), (1, 1)], 'Failure ends a while with an @-parameter';

my $j = 0;
while f($j--) -> @t { $j = 99 }
is $j, -1, 'body never runs for a falsy condition';

my %h;
my $k = 1;
while ($k-- ?? {a => 1} !! Nil) -> %p { %h = %p }
is-deeply %h, {a => 1}, '%-parameter binds the condition value';

my $r = start { my $n = 1; my $c = 0; while f($n--) -> @t { $c++ }; $c }.result;
is $r, 1, 'works inside start';

done-testing;
