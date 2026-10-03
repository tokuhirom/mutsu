use Test;

# From the Cache::Async distribution (t/05-monitoring.rakutest):
# atomic-fetch-sub / atomic-sub-fetch on an atomicint.
plan 7;

my atomicint $a = 10;
is atomic-fetch-sub($a, 3), 10, 'atomic-fetch-sub returns the old value';
is $a, 7, 'atomic-fetch-sub subtracts';
is atomic-sub-fetch($a, 2), 5, 'atomic-sub-fetch returns the new value';
is $a, 5, 'atomic-sub-fetch subtracts';

my int $b = 5;
is atomic-fetch-sub($b, 2), 5, 'works on a plain native int';
is $b, 3, 'native int updated';

class C {
    has atomicint $!n = 9;
    method drain { my $c = atomic-fetch($!n); atomic-fetch-sub($!n, $c); atomic-fetch($!n) }
}
is C.new.drain, 0, 'works on an attribute';
