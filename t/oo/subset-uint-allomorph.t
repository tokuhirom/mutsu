use Test;

plan 7;

# An IntStr allomorph is an Int, so UInt's non-negative predicate applies.
ok <3> ~~ UInt, 'IntStr literal matches UInt';
nok <-3> ~~ UInt, 'negative IntStr does not match UInt';
my $x = <3 4>[0];
ok $x ~~ UInt, 'IntStr held in a variable matches UInt';
sub takes-uint(UInt $n) { $n }
is takes-uint($x), 3, 'IntStr variable binds to a UInt parameter';
my @seen;
for <3 4> -> $l { @seen.push: takes-uint($l) }
is @seen, [3, 4], 'IntStr loop variable binds to a UInt parameter';
ok (5 but True) ~~ UInt, 'an Int with a mixin matches UInt';
nok "3" ~~ UInt, 'a plain Str does not match UInt';
