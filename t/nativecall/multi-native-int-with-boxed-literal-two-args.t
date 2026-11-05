use v6;
use Test;

# rakudo 2026.09: a two-argument call that holds a native argument binds a
# boxed Int to an `int` parameter of a multi, and the native candidate wins
# over a boxed one. Calls with no native argument, or three arguments, do not
# (#16483).

plan 9;

multi f(int, int) { "ii" }
my int $y;
is (f 42, my int $x), "ii", 'literal and an anonymous native declaration';
is $x, 0, 'the declaration is the native zero';
is f(42, $y), "ii", 'literal first, native second';
is f($y, 42), "ii", 'native first, literal second';
is f($y, $y), "ii", 'two natives';
throws-like { EVAL 'f(42, 7)' }, X::Multi::NoMatch, 'two literals do not bind';

multi g(int, int) { "ii" }
multi g(Int, Int) { "II" }
is g(42, $y), "ii", 'the native candidate wins beside a boxed one';
is g(42, 7), "II", 'two literals pick the boxed candidate';

multi h(int, int, int) { "iii" }
throws-like { EVAL 'h(1, 2, $y)' }, X::Multi::NoMatch, 'a three-argument call does not relax';
