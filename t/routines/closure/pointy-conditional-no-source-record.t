use Test;
plan 7;
my $r;
given (1, 2) -> ($a, $b) { $r = $a + $b }
is $r, 3, 'given destructure';
with (3, 4) -> ($a, $b) { $r = $a * $b }
is $r, 12, 'with destructure';
if (5, 6) -> ($a, $b) { $r = $a - $b }
is $r, -1, 'if destructure';
if 0 { } else -> $e { $r = "e$e" }
is $r, 'e0', 'else pointy';
given 5 -> \x { $r = x + 1 }
is $r, 6, 'sigilless given';
is EVAL('given (7, 8) -> ($p, $q) { $p + $q }'), 15, 'EVAL destructure';
is EVAL('my $v = 3; if $v -> $w { $w * 2 }'), 6, 'EVAL if pointy';
