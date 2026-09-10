use Test;

plan 4;

use experimental :cached;

my $calls = 0;
sub cached-value(Int:D $x) is cached {
    $calls++;
    $x * 2;
}

is cached-value(21), 42, 'cached routine returns its result';
is cached-value(21), 42, 'cached routine returns the memoized result';
is cached-value(22), 44, 'a different argument gets its own cache entry';
is $calls, 2, 'cached routine body runs once per argument value';
