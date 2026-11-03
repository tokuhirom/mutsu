use Test;

# `.VAR` of a Proxy is the Proxy object itself. Binding it, passing it as an
# argument and sinking it neither FETCH nor replace it by its value (#15893).

plan 8;

my $fetches = 0;
my $q := Proxy.new(
    FETCH => method ()   { $fetches++; 1 },
    STORE => method ($v) { },
);

my $b := $q.VAR;
is $b.^name, 'Proxy', 'a bound .VAR is the Proxy';
is $fetches, 0, '... and binding it did not FETCH';

sub take($x) { $x.^name }
is take($q.VAR), 'Proxy', 'a positional argument is the Proxy';
is $fetches, 0, '... without a FETCH';

sub take-raw(\x) { x.^name }
is take-raw($q.VAR), 'Proxy', 'a raw parameter sees the Proxy';

$q.VAR;
is $fetches, 0, 'sinking .VAR does not FETCH';

# The plain Proxy still FETCHes when something reads it.
my $w = $q;
is $w, 1, 'reading the Proxy itself FETCHes';
is $fetches, 1, 'exactly once';
