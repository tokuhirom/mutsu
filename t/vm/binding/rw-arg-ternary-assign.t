use Test;

# From JSON::Fast (t/14-comments.t): `f($c ?? $p !! $p = $p + 1)` assigns to
# the conditional's chosen native `is rw` parameter and passes that container on.

plan 4;

my sub inner(int $p is rw --> Nil) { $p = $p + 100 }
my sub outer(int $pos is rw, int $ord --> Nil) {
    inner($ord == -1 ?? $pos !! $pos = $pos + 1);
}

my int $a = 1;
outer($a, 5);
is $a, 102, 'else arm assigned, then bound through';

my int $b = 1;
outer($b, -1);
is $b, 102, 'then arm assigned, then bound through';

my sub two(int $x is rw, int $y is rw, int $sel --> Nil) {
    inner($sel == 0 ?? $x !! $y = 10);
}
my int ($p, $q) = 1, 2;
two($p, $q, 0);
is "$p $q", "110 2", 'only the chosen parameter is touched (then)';
($p, $q) = 1, 2;
two($p, $q, 1);
is "$p $q", "1 110", 'only the chosen parameter is touched (else)';
