use Test;

# #15721: the deferral chain of a multi method family visits the remaining
# candidates narrowest first, `Any` before `Mu`, whatever the declaration order.
# The expected values are rakudo's.

plan 8;

class W {
    multi method go(Mu $x)  { 'Mu' }
    multi method go(Any $x) { 'Any>' ~ (callsame() // '') }
    multi method go(Int $x) { 'Int>' ~ (callsame() // '') }
    multi method go(Str $x) { 'Str' }
}
is W.new.go(5), 'Int>Any>Mu', 'Int defers to Any, then Mu';
is W.new.go(1.5), 'Any>Mu', 'a Rat takes Any, then Mu';
is W.new.go("s"), 'Str', 'Str is the narrowest and stops';

class F {
    multi method go(Int $x) { 'Int>' ~ (callsame() // '') }
    multi method go(Mu $x)  { 'Mu' }
    multi method go(Any $x) { 'Any>' ~ (callsame() // '') }
}
is F.new.go(5), 'Int>Any>Mu', 'declared Int, Mu, Any';

class U {
    multi method go($x)     { 'untyped>' ~ (callsame() // '') }
    multi method go(Mu $x)  { 'Mu' }
    multi method go(Int $x) { 'Int>' ~ (callsame() // '') }
}
is U.new.go(5), 'Int>untyped>Mu', 'an untyped parameter counts as Any';

class N {
    multi method go(Mu $x)  { 'Mu' }
    multi method go(Any $x) { 'Any>' ~ (callsame() // '') }
    multi method go(Int $x) { 'Int>' ~ (nextsame() // '') }
}
is N.new.go(5), 'Any>Mu', 'nextsame walks the same chain';

class A {
    multi method go(Any $x) { 'Any>' ~ (callsame() // '') }
    multi method go(Int $x) { 'Int>' ~ (callsame() // '') }
}
is A.new.go(5), 'Int>Any>', 'two candidates still work';

multi sub f(Mu $x) { 'Mu' }
multi sub f(Any $x) { 'Any>' ~ (callsame() // '') }
multi sub f(Int $x) { 'Int>' ~ (callsame() // '') }
is f(5), 'Int>Any>Mu', 'the sub form agrees';
