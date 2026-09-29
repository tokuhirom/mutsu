use Test;

# A WhateverCode subscript (`@a[*-1]`, `@a[*-3..*-1]`) runs the closure's own
# compiled bytecode (issue #10118). These pin that every subscript form that
# resolves a Callable index still sees the element count, its captured
# lexicals, and a fresh value of a changing lexical on every iteration.

plan 16;

my @a = 1..5;
is @a[*-1], 5, 'read @a[*-1]';
is-deeply @a[*-3..*-1], (3, 4, 5), 'read @a[*-3..*-1]';
is-deeply @a[*-2, *-1], (4, 5), 'list of WhateverCodes';

my $k = 2;
is @a[*-$k], 4, 'captured lexical in the WhateverCode';

my @seen;
for 1..3 -> $i {
    @seen.push: @a[*-$i];
}
is-deeply @seen, [5, 4, 3], 'loop variable captured afresh per iteration';

my $sum = 0;
$sum += @a[*-3..*-1].sum for ^50;
is $sum, 600, 'repeated range subscript in a loop';

my @b = 1, 2, 3;
@b[*-1] = 30;
is-deeply @b, [1, 2, 30], 'assign @b[*-1]';
@b[*-3..*-2] = 10, 20;
is-deeply @b, [10, 20, 30], 'assign @b[*-3..*-2]';

my Int @typed = 1, 2, 3;
@typed[*-1] = 9;
is-deeply @typed.List, (1, 2, 9), 'assign into a typed array';

my @c = 1, 2, 3;
@c[*-1]:delete;
is @c.elems, 2, ':delete @c[*-1]';

is (1, 2, 3).Seq[*-1], 3, 'Seq[*-1]';
is (1..10)[*-2], 9, 'Range[*-2]';

my @m = [1, 2], [3, 4];
is @m[*-1; *-1], 4, 'multi-dimensional [*-1; *-1]';

my $buf = Buf.new(1, 2, 3, 4, 5);
is-deeply $buf.subbuf(*-2).list, (4, 5), 'Buf.subbuf(*-2)';
is-deeply $buf.subbuf(1, *-1).list, (2, 3, 4, 5), 'Buf.subbuf($from, *-1) takes an end index';

my @e;
isa-ok @e[*-1], Failure, 'empty array [*-1] is an out-of-range Failure';
