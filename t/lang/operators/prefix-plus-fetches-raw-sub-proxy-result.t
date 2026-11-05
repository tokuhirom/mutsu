use v6;
use Test;

# Universal::Errno: `errno` is `sub errno() is raw { $proxy }`; `+errno` has to
# FETCH the Proxy the call returns (it arrives wrapped in the call's container).

plan 4;

my class Seven { method Numeric() { 7 } }
my $proxy := Proxy.new(FETCH => -> $ { Seven }, STORE => -> $, $v { });
sub get() is raw { $proxy }
module M {
    our sub get() is raw { $proxy }
}

is +get(), 7, 'raw sub call';
is +M::get(), 7, 'package-qualified raw sub call';
is +(M::get()), 7, 'parenthesised';
is 1 + M::get(), 8, 'infix';

done-testing;
