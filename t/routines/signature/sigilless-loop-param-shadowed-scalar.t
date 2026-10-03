use Test;

plan 2;

# A sigilless `-> \x` loop parameter must not be confused with a same-named
# `$x` declared later in the enclosing scope (#11361).
my @b = 1, 2;
for (@b,) -> \x { x = Empty }
my $x = 1;
is-deeply @b, [], 'sigilless loop param assigns into the bound array';
is $x, 1, 'the enclosing scalar is untouched';
