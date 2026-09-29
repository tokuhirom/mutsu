use Test;

# From the Commands ecosystem distribution: a role instantiated directly
# (punned) must not fill an `is built(False)` attribute from a constructor
# argument of the same name, just like a class or a composing class.

plan 6;

my role R {
    has %.h is built(False);
    has $.s is built(False) = 5;
    has @.a is built(False);
    has $.b = 1;
}
my $e = R.new(h => (1,), s => 9, a => [1, 2], b => 2);
is $e.h.WHAT, Hash, 'is built(False) % attribute stays a Hash';
is-deeply $e.h, {}, '... and is empty';
is $e.s, 5, 'is built(False) scalar keeps its default';
is-deeply $e.a, [], 'is built(False) @ attribute stays empty';
is $e.b, 2, 'an ordinary attribute is still built';

my role Q {
    has %.commands is built(False);
    method TWEAK(:$commands!) { my %c := %!commands; %c<x> = $commands.elems }
}
is Q.new(commands => (sub {1},)).commands<x>, 1, 'TWEAK binds %!attr of an odd-length List argument';
