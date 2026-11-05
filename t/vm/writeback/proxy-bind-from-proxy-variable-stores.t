use Test;

# `my $p := $q` binds `$p` to the same Proxy container `$q` is bound to, so an
# assignment to either variable runs the Proxy's STORE (#16306).

plan 6;

my $stores = 0;
my $q := Proxy.new(
    FETCH => method ()   { 1 },
    STORE => method ($v) { $stores++ },
);

my $p := $q;
$p = 7;
is $stores, 1, 'assigning to the second-hand binding runs STORE';

$q = 8;
is $stores, 2, 'the original variable is still the Proxy afterwards';

is $p.VAR.^name, 'Proxy', 'the alias is the Proxy container';
is $p, 1, 'reading the alias FETCHes';

# A bound alias of a plain scalar still writes through to it.
my $x = 1;
my $y := $x;
$y = 5;
is $x, 5, 'a bound alias of a scalar writes through';

# The Proxy object .VAR hands back stays readonly when bound.
my $b := $q.VAR;
throws-like { $b = 9 }, X::AdHoc, message => /'readonly variable'/,
    'a variable bound to .VAR still refuses assignment';
