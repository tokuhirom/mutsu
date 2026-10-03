use Test;

plan 14;

# `pick($n, +values)` / `roll($n, +values)`: past the count, each argument
# is one element.
is pick(*, 1, 2, 3).sort.List, (1, 2, 3), 'pick(*, list of args) picks every arg';
is pick(1, 4, 5, 6).elems, 1, 'pick(1, args) picks one';
is roll(3, 7, 8).elems, 3, 'roll(3, args) rolls three';
ok roll(5, 7, 8).all ~~ 7 | 8, 'roll draws from the args';
is pick(*, [1, 2], [3]).elems, 2, 'array args are single elements';
is pick(*, [1, 2, 3]).elems, 3, 'one array arg still follows the one-arg rule';

# `&pick` / `&roll` and other list-shaped core subs are first-class Routines.
ok &roll.defined, '&roll is defined';
ok &pick ~~ Callable, '&pick is Callable';
my &m = &roll;
is m(2, [5]), (5, 5), 'calling a stored &roll';
&m = &pick;
is m(*, [1, 2, 3]).elems, 3, 'assigning &pick to a & variable';
sub draw($n, :&method is copy = WhateverCode) {
    &method = $n.isa(Whatever) ?? &pick !! &roll if &method.isa(WhateverCode);
    method($n, [9])
}
is draw(2), (9, 9), '&roll chosen in a ternary and called through a parameter';
is draw(*).elems, 1, '&pick chosen in a ternary';
is (&head)(2, [1, 2, 3]), (1, 2), '&head';
is (&first)(* > 1, [1, 2, 3]), 2, '&first';
