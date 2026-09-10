use v6;
use lib 't/lib';
use Test;
use ListopShadow;

# The compiler rewrites the container listops (`pop(@a)` -> `@a.pop()`,
# `push(@a, v)` -> `@a.push(v)`) to get array mutation right. That rewrite bakes
# the builtin in at compile time, so it must be suppressed when a user routine of
# the same name is visible — otherwise the imported routine can never be reached
# no matter what the runtime's builtin-vs-user preference does. `ListopShadow`
# exports Perl 5 style `push`/`pop` (P5push's shape).

plan 6;

my @a = 1;
is push(@a, 42), 2, 'an imported push returns the module value, not the array';
is-deeply @a, [1, 42], 'and the array was still mutated by the module body';
is push(@a, 666, 667), 4, 'the slurpy form returns the new element count too';

my @b = 1, 2;
is pop(@b), 2, 'an imported pop returns the element';
is pop(@b), 1, 'and the next one';
# The builtin would surface a `Cannot pop from an empty Array` Failure here; the
# module's `@array.elems ?? @array.pop !! Nil` must win.
is pop(@b), Nil, 'popping empty yields the module Nil, not the builtin Failure';
