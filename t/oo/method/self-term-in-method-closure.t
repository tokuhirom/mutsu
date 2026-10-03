use Test;

# `self` read from a block nested in a method is the invocant the block
# closed over — the innermost lexical `self` — even when a file-scope
# `constant self` exists (#9494 moved this read ahead of the term probes).

plan 6;

constant self = 5;

class A {
    has $.x = 3;
    method in-map { (1, 2).map({ self.x + $_ }).List }
    method in-sub { my sub s { self.x }; s() }
    method in-pointy { my &f = -> $y { self.x * $y }; f(2) }
    method nested { (1,).map({ (10,).map({ self.x + $_ }).List }).flat.List }
}

is-deeply A.new.in-map, (4, 5), 'self in a map block';
is A.new.in-sub, 3, 'self in a nested my sub';
is A.new.in-pointy, 6, 'self in a pointy block';
is-deeply A.new.nested, (13,), 'self two blocks deep';
is self, 5, 'outside any method, the constant';

class B { has $.v; method closure { -> { self.v } } }
my &c = B.new(v => 'kept').closure;
is c(), 'kept', 'an escaping closure keeps its invocant';
