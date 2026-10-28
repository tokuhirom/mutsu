use Test;

# `($proxy = v)` is the Proxy container: reading it FETCHes, so the expression
# is FETCH's answer, not the assigned value and not STORE's return value.

plan 4;

my $store;
my $p := Proxy.new(FETCH => { "fetched" }, STORE => -> $, $v { $store = $v });
is ($p = 7), "fetched", 'the assignment expression reads back through FETCH';
is $store, 7, 'STORE saw the value';
my $r = ($p = 9);
is $r, "fetched", 'a copy of the expression is the FETCHed value';
is $store, 9, 'STORE ran once more';
