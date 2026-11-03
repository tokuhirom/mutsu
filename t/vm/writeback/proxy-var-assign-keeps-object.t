use Test;

# `.VAR` of a Proxy is the Proxy object itself, so assigning it to a scalar
# lands the object as a value: no FETCH runs, `.^name` is `Proxy`, and a later
# `$v = 5` replaces it instead of calling STORE (#15893).

plan 8;

my $fetches = 0;
my $stores  = 0;
my $q := Proxy.new(
    FETCH => method ()   { $fetches++; 1 },
    STORE => method ($v) { $stores++ },
);

my $v = $q.VAR;
is $fetches, 0, 'assigning .VAR does not FETCH';
is $v.^name, 'Proxy', 'the variable holds the Proxy object';
is $fetches, 0, 'reading its type does not FETCH either';

$v = 5;
is $stores, 0, 'assigning to that variable does not call STORE';
is $v, 5, 'it simply replaces the held Proxy';

# The container handoff paths keep FETCHing, as before.
is $q.VAR.^name, 'Proxy', '.VAR without assignment is still the Proxy';
my $w = $q;
is $w, 1, 'a plain Proxy assignment FETCHes';
is $fetches, 1, 'exactly once';
