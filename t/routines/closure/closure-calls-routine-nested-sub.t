use Test;

# A named sub declared inside a routine reads its free variables from the
# env live at call time. A closure in the same body that calls it must
# therefore capture those variables itself, or a call made after the
# routine returned finds none of them (mutsu#9106).

plan 7;

sub escaping($p) {
    my sub twice() { $p * 2 }
    -> $x { twice() + $x }
}
is escaping(7)(1), 15, 'closure calling an inner sub sees the parameter it reads';

sub via-my($p) { my $q = $p; my sub t() { $q * 2 }; -> $x { t() + $x } }
is via-my(7)(1), 15, 'same with a captured `my` variable';

my ($a, $b) = escaping(7), escaping(100);
is "{$a(1)} {$b(1)}", '15 201', 'each activation keeps its own binding';

sub chained($p) { my sub t() { $p * 2 }; my sub u() { t() + 1 }; -> $x { u() + $x } }
is chained(7)(1), 16, 'capture is transitive through a sibling inner sub';

sub counter() { my $n = 0; my sub bump() { ++$n }; -> { bump(); bump() } }
my &c = counter();
is "{c()} {c()}", '2 4', 'a variable the inner sub mutates stays shared';

sub rec($p) { my sub t($k) { $k <= 0 ?? $p !! t($k - 1) }; -> { t(3) } }
is rec(42)(), 42, 'recursive inner sub';

sub setter($p) { my $v = 0; my sub set($x) { $v = $x + $p }; -> $x { set($x); $v } }
is setter(10)(5), 15, 'write-only free variable of the inner sub';
