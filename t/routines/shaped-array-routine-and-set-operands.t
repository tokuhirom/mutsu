use Test;

# A multi-dimensional shaped Array passed to an iterating builtin routine or
# used as a set operator operand contributes its leaves, as its iteration
# does (#14248). The routine forms and set operators read its rows (#14308).
# Expected values are the `raku` (v2026.09) output.

plan 13;

my @c[2;2] = [1,2],[3,4];

is sum(@c), 10, 'sum()';
is max(@c), 4, 'max()';
is min(@c), 1, 'min()';
is first(* > 2, @c), 3, 'first()';
is grep(* > 1, @c), (2, 3, 4), 'grep()';
is set(@c).elems, 4, 'set()';
ok 3 ∈ @c, '∈';
nok 5 ∈ @c, '... not a member';
ok @c ∋ 4, '∋';
ok @c ⊆ (1..4), '⊆';
is (@c (|) (5,)).elems, 5, '(|)';
is ([∪] @c, (9,)).elems, 5, 'a reduced ∪';
is elems(@c), 2, 'elems() still counts the first dimension';
