use v6;
use Test;

plan 7;

# A closure a role method hands out runs after the method returned, so it has
# to carry the role's `&` parameters like it carries the scalar ones.
role R[&rel1, $v, Str :$model] {
    method scalar { sub () { $v } }
    method named  { sub () { $model } }
    method as-value { sub () { &rel1.defined } }
    method called { sub () { rel1(7) } }
    method called-in-block { -> { rel1(8) } }
    method proxied {
        Proxy.new(FETCH => method () { do if &rel1.count <= 1 { rel1(9) } else { 0 } },
                  STORE => method ($) { })
    }
}

my $o = (class { }).new but R[-> $x { $x * 2 }, 5, :model<m>];
is $o.scalar()(), 5, 'a scalar role parameter is captured';
is $o.named()(), 'm', 'a named role parameter is captured';
ok $o.as-value()(), '&param read as a value inside the escaping closure';
is $o.called()(), 14, '&param called by name inside the escaping closure';
is $o.called-in-block()(), 16, '... and inside a pointy block';
is $o.proxied, 18, '... and inside a Proxy FETCH run by the runtime';

class C does R[-> $x { $x + 1 }, 0] {}
is C.new.called()(), 8, 'a class that does the role gets its own parameter';
