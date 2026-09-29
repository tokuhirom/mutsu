use Test;

# From the FatRatStr distribution: a parent multi method constrained
# `(A:D:)` must stay a deferral candidate for an attribute-less instance
# (the dispatch frame used to carry the type object, failing the `:D` check).

plan 5;

class A { multi method g(A:D:) { "Ag" } multi method i(A:D: $x) { "Ai" } }
class B is A {
    multi method g() { nextsame }
    multi method i($x) { callsame }
}
is B.new.g, "Ag", 'nextsame reaches a parent (A:D:) candidate';
is B.new.i(1), "Ai", 'callsame reaches a parent (A:D: $x) candidate';

class C { has $.v = 1; }
class D is C { }
is D.new.v, 1, 'attribute-bearing instance still dispatches';

# `my Int()` must keep integers beyond i64.
my Int() $big = "10938370151111111111";
is $big, 10938370151111111111, 'my Int() keeps a big integer string exact';
sub f(Int() $x) { $x }
is f("10938370151111111111"), 10938370151111111111, 'Int() parameter keeps a big integer';
