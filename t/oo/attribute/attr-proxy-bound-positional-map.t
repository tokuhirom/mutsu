use Test;
use nqp;

plan 4;

# `@!attr.map(...)` where the attribute was bound (nqp::bindattr) to a Proxy
# whose FETCH returns a user Positional with its own `map` (Red's relationship
# ResultSeq): the FETCHed object's `map` answers, not a one-element array map.
class RS does Iterable does Positional {
    method iterator { (1, 2, 3).iterator }
    method map(&f) { "RS.map" }
    method foo { "RS.foo" }
}
class A {
    has @.xs;
    has $.s;
    method viaat  { @!xs.map({ $_ }) }
    method viaparen { (@!xs).map({ $_ }) }
    method viafoo { @!xs.foo }
}
my $a = A.new;
nqp::bindattr(nqp::decont($a), A, '@!xs', Proxy.new(FETCH => method () { RS.new }, STORE => method (|) { }));

is $a.viafoo, 'RS.foo', 'baseline: another method reaches the FETCHed object';
is $a.viaat, 'RS.map', '@!attr.map calls the FETCHed object\'s own map';
is $a.viaparen, 'RS.map', '(@!attr).map too';

class B { has @.ys; method m { @!ys.map({ $_ * 2 }).join(',') } }
is B.new(ys => (1, 2, 3)).m, '2,4,6', 'a plain array attribute still maps natively';
