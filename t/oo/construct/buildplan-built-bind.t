use v6.d;
use Test;

plan 8;

# `is built(:bind)` is recorded, and the BUILDPLAN encodes it as Rakudo does
# (1300 binds the named argument, 1400 binds the default).
sub codes(\type) { type.^BUILDPLAN.map(*.[0]).join(",") }

class A { has $.e is built(:bind); has $!f is built(:bind) }
is codes(A), "1300,1300,1000,1000", 'a bound attribute has a 1300 init task';

class B { has $.g is built(:bind) = 4 }
is codes(B), "1300,1400", 'a bound attribute with a default has a 1400 default task';

class C { has $.h is built(:bind) is required }
is codes(C), "1300,800", 'a required bound attribute keeps its 800 check';

class D { has int $.n is built(:bind) = 3 }
is codes(D), "1,401", 'a native attribute ignores :bind in its tasks';

class E { has $.p is built(:!bind); has $.q is built }
is codes(E), "0,0,1000,1000", '`:!bind` and a plain `is built` are ordinary';

# Construction still initializes a bound attribute from its argument.
is B.new.g, 4, 'a bound default is used';
is B.new(g => 2).g, 2, 'a bound argument is used';
is A.new(e => 5).e, 5, 'a bound attribute is initialized from its named argument';
