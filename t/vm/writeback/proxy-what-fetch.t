use Test;

plan 2;

my $fetches = 0;
my $proxy = Proxy.new(
    FETCH => method () { $fetches++; 'value from FETCH' },
    STORE => method ($value) { },
);

is $proxy.WHAT.^name, 'Str', '.WHAT reports the type returned by FETCH';
is $fetches, 1, '.WHAT invokes FETCH exactly once';
