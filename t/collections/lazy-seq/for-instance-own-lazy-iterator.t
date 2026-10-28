use Test;

# `for` over a bare instance whose class has its own `iterator` pulls it per
# iteration, also for an `is Range` subclass (whose `list` used to read the range
# it carries) and inside `gather`. Found with Math::Sequences
# (`lazy gather for 𝕀 -> $n { ... }`). #15353

plan 6;

class Count does Iterator {
    has $.n is rw = 0;
    method is-lazy { True }
    method pull-one { $!n++ }
}
class Nat is Range { method iterator { Count.new } }
constant N = Nat.new(0, Inf);

my @seen;
for N -> $n { @seen.push($n); last if $n >= 3 }
is-deeply @seen, [0, 1, 2, 3], 'for over an infinite own iterator stops at last';

my @x = gather for N -> $n { take $n; last if $n >= 3 };
is-deeply @x, [0, 1, 2, 3], 'gather for over an own lazy iterator';

my @y = lazy gather for N -> $n { take $n * 2 };
is-deeply @y[^3].List, (0, 2, 4), 'lazy gather for pulls on demand';

my $g = gather for N -> $n { take $n * 2 };
is-deeply $g.head(3).List, (0, 2, 4), '.head(n) of a gather over an own lazy iterator';

class Three does Iterator {
    has $.n is rw = 0;
    method pull-one { $!n < 3 ?? $!n++ !! IterationEnd }
}
class Small is Range { method iterator { Three.new } }
my @s;
for Small.new(0, 5) -> $n { @s.push($n) }
is-deeply @s, [0, 1, 2], 'a Range subclass with a finite own iterator iterates it, not the range';

my @t;
for Nat.new(0, 5).map(* * 2) -> $n { @t.push($n); last if $n >= 4 }
is-deeply @t, [0, 2, 4], 'map over the own iterator still works';
