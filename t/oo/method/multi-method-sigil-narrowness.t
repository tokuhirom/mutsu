use v6;
use Test;

# Regression (TODO_dist T-057, Statistics::LinearRegression's `my LR $m .= new: @x, @y`):
# a `multi method` whose candidates differ only by parameter sigil must resolve
# by sigil narrowness, NOT report X::Multi::Ambiguous. An `@`/`%`/`&` sigil
# imposes an implicit Positional/Associative/Callable constraint, so `(@x, @y)`
# is narrower than `($a, $b)` for two array arguments. (multi *sub* dispatch
# already did this; the *method* path did not.)

plan 4;

class C {
    multi method new(@x, @y) { self.bless }
    multi method new($a, $b) { self.bless }
    multi method pick2(@x, @y) { "arrays" }
    multi method pick2($a, $b) { "scalars" }
    multi method pick1(%h)     { "hash" }
    multi method pick1($x)     { "scalar" }
}

my @a = 1, 2, 3;
my @b = 4, 5, 6;
my %m = a => 1;

lives-ok { C.new(@a, @b) }, 'multi method new(@x,@y) vs new($a,$b) is not ambiguous for arrays';
is C.pick2(@a, @b), 'arrays', '(@x, @y) wins over ($a, $b) for two array args';
is C.pick2(1, 2),   'scalars', '($a, $b) still wins for two scalar args';
is C.pick1(%m),     'hash', '(%h) wins over ($x) for a hash arg';
