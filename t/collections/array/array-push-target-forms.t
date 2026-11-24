# `.push` onto arrays reached by name: plain, typed, self-aliased, a nested
# sub's routine array, lazy and List-bound targets (#18274).
use Test;
my @plain;
@plain.push(1) for ^3;
is-deeply @plain, [1, 1, 1], 'push to a plain array';
my Int @typed;
@typed.push(1, 2);
throws-like { @typed.push('x') }, X::TypeCheck, 'a typed array checks every push';
is @typed.elems, 2, 'and keeps its elements';
my @aliased = 1, 2;
@aliased.push(@aliased);
is @aliased.elems, 3, 'pushing an array onto itself';
sub outer() {
    my @acc;
    my sub add($x) { @acc.push($x) }
    add($_) for 1..3;
    @acc
}
is-deeply outer(), [1, 2, 3], "a nested sub pushes onto its routine's array";
my @lazy = 1 .. *;
throws-like { @lazy.push(1) }, X::Cannot::Lazy, 'a lazy array refuses a push';
my @list := (1, 2);
throws-like { @list.push(3) }, X::Immutable, 'a List bound to an array refuses a push';
done-testing;
