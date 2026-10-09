use v6;
use Test;

# Found via the SBOM::CycloneDX distribution: two methods each declare a
# `sub mapify` inside a `.map` callback. The routine is lexical to one call
# of the callback, so a same-named one elsewhere is not a redeclaration.

plan 6;

my @a = (1, 2).map(-> $n { sub h($x) { "m$x" }; h($n) });
my @b = (1, 2).map(-> $n { sub h($x) { "n$x" }; h($n) });
is @a.join(','), 'm1,m2', 'sub in the first map callback';
is @b.join(','), 'n1,n2', 'same-named sub in a second map callback';

sub c { (1, 2).map: -> $n { sub h($x) { "c$x" }; h($n) } }
sub d { (1, 2).map: -> $n { sub h($x) { "d$x" }; h($n) } }
is c().join(','), 'c1,c2', 'sub in a routine\'s map callback';
is d().join(','), 'd1,d2', 'same-named sub in another routine\'s map callback';

class K {
    multi method p(::?CLASS:U:) { 0 }
    multi method p(::?CLASS:D:) { (1, 2).map: -> $n { sub h($x) { "p$x" }; h($n) } }
    multi method q(::?CLASS:U:) { 0 }
    multi method q(::?CLASS:D:) { (1, 2).map: -> $n { sub h($x) { "q$x" }; h($n) } }
}
is (K.new.p.join(','), K.new.q.join(',')).join(';'), 'p1,p2;q1,q2',
    'multi methods each declaring the same local sub in a map callback';

# A local sub that closes over a bound lexical sees this call's binding.
my @c = (1, 2).map(-> $n { my $type := $n == 1 ?? "X" !! "Y"; sub s($x) { $type }; s(1) });
is @c.join(','), 'X,Y', 'sub in a map callback closes over the current iteration';
