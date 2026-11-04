use v6;
use Test;

# Universal::Errno: a Proxy's FETCH answers the class `errno` (a type object)
# whose `Numeric` method reads C errno. Infix numeric operators call a user
# `Numeric` on a bare type object, as prefix `+` already did.

plan 8;

my class Seven { method Numeric() { 7 } }

is +Seven, 7, 'prefix +';
is Seven + 1, 8, 'infix + on the left';
is 1 + Seven, 8, 'infix + on the right';
is Seven * 2, 14, 'infix *';
ok Seven == 7, 'numeric ==';
nok Seven == 8, 'numeric == (false)';

my $p := Proxy.new(FETCH => -> $ { Seven }, STORE => -> $, $v { });
is $p + 1, 8, 'through a Proxy FETCH';
is +$p, 7, 'prefix + through a Proxy FETCH';

done-testing;
