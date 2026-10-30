use Test;

plan 16;

# A family of 16+ multi method candidates ranks only the tiers of the
# candidates that bound the call (the walk behind `assign_tiers_wanted`); the
# winner and the callsame/nextsame order must stay what the whole-relation
# ranking gave.

class N0 {}
class N1 is N0 {}
class N2 is N1 {}
class N3 is N2 {}
class P0 {}
class P1 is P0 {}
class Q0 {}
class Q1 is Q0 {}
class R0 {}
class S0 {}
class T0 {}
class U0 {}
class V0 {}
class W0 {}

class Walker {
    multi method go(N0 $x) { 'N0>' }
    multi method go(N1 $x) { 'N1>' ~ (callsame() // '') }
    multi method go(N2 $x) { 'N2>' ~ (callsame() // '') }
    multi method go(N3 $x) { 'N3' }
    multi method go(P0 $x) { 'P0' }
    multi method go(P1 $x) { 'P1>' ~ (callsame() // '') }
    multi method go(Q0 $x) { 'Q0' }
    multi method go(Q1 $x) { 'Q1>' ~ (callsame() // '') }
    multi method go(R0 $x) { 'R0' }
    multi method go(S0 $x) { 'S0' }
    multi method go(T0 $x) { 'T0' }
    multi method go(U0 $x) { 'U0' }
    multi method go(V0 $x) { 'V0' }
    multi method go(W0 $x) { 'W0' }
    multi method go(Mu $x) { 'Mu' }
    multi method go(Any $x) { 'Any' }
    multi method go(Int $x) { 'Int' }
    multi method go(Str $x) { 'Str' }
}

my $w = Walker.new;
is $w.go(N3.new), 'N3', 'the most derived candidate wins';
is $w.go(N2.new), 'N2>N1>N0>', 'a chain of callsame follows the tiers';
is $w.go(N1.new), 'N1>N0>', 'one level up';
is $w.go(N0.new), 'N0>', 'the root of the chain';
is $w.go(P1.new), 'P1>P0', 'a second chain';
is $w.go(Q1.new), 'Q1>Q0', 'a third chain';
is $w.go(R0.new), 'R0', 'an unrelated leaf candidate';
is $w.go(W0.new), 'W0', 'the last candidate of the family';
is $w.go(5), 'Int', 'Int beats Any and Mu';
is $w.go('s'), 'Str', 'Str candidate';
is $w.go(1.5), 'Any', 'a Rat falls to Any';
is $w.go(Nil), 'Any', 'Nil is an Any';

# The answers are the same on a second round (the per-family index is reused).
is $w.go(N2.new), 'N2>N1>N0>', 'chain, second round';
is $w.go(Q1.new), 'Q1>Q0', 'chain, second round (Q)';
is $w.go(5), 'Int', 'Int, second round';

# A candidate added later changes the family and is found.
my $extra = Walker.^add_method('late', method (Int $x) { 'late' });
is $w.late(1), 'late', 'a method added after the family was ranked';
