# A `&name` bound to an object that does Callable (has a CALL-ME) is invoked
# through it by a bare `name(...)` call. Found via Test::Describe
# (`for @!its -> &it { it |%pars }` over `Test::Describe::It` objects).
use Test;

plan 3;

class Runner does Callable {
    has $.n;
    method CALL-ME(*%p) { "run $!n {%p<x> // ''}" }
}

my @runners = Runner.new(:n(1)), Runner.new(:n(2));
my @got;
for @runners -> &it { @got.push: it :x(7) }
is-deeply @got, ['run 1 7', 'run 2 7'], 'a loop parameter holding a Callable object';

my &f = Runner.new(:n(3));
is f(:x(8)), 'run 3 8', 'a my &f holding a Callable object';

class Sub-Runner is Runner { }
my &g = Sub-Runner.new(:n(4));
is g(), 'run 4 ', 'an inherited CALL-ME';
