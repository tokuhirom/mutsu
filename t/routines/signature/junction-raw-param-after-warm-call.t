use Test;

# A Junction argument autothreads over a routine's `Any` parameter even after
# earlier calls (a `.map` warm-up) put the routine in the call caches: the
# cached route used to bind the Junction itself and refuse it (#18198).

plan 4;

sub d($x is raw) { $x }
my @w = (1, 2).map({ d($_) });
is d(any(1, 2)).raku, any(1, 2).raku, 'an is raw parameter autothreads after a warm-up';

sub e($x) { $x * 2 }
my @v = (1, 2).map({ e($_) });
is e(any(1, 2)).raku, any(2, 4).raku, 'a plain parameter autothreads after a warm-up';

sub f(Mu $x is raw) { $x.WHAT }
my @u = (1, 2).map({ f($_) });
is f(any(1, 2)).^name, 'Junction', 'a Mu parameter still takes the Junction itself';

sub g($a, $b is raw) { $a + $b }
my @t = (1, 2).map({ g($_, 1) });
is g(1, one(10, 20)).raku, one(11, 21).raku, 'a Junction in a later position';
