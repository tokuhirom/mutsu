use Test;
# #17376 (Monad): a routine with a sigilless parameter called with a plain
# lexical must bind the caller's container, not take a statically linked
# TRIR call that always declines.
plan 6;
sub twice(\x) { x * 2 }
sub bump(\x) { x = x + 1 }
sub kind(Mu \t) { t.^name }
my $n = 21;
is twice($n), 42, 'read through a sigilless param bound to a variable';
my $m = 5;
bump($m);
is $m, 6, 'a sigilless param writes through to the caller variable';
my $T = Any;
is kind($T), 'Any', 'type object through Mu \t';
my $s = 'str';
is kind($s), 'Str', 'Str through Mu \t';
my $total = 0;
$total += twice($_) for 1..50;
is $total, 2550, 'repeated calls give the same answer';
sub two(\a, \b) { a + b }
my ($p, $q) = 3, 4;
is two($p, $q), 7, 'two sigilless params from two variables';
