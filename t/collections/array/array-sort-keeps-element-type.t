use Test;

# On a native array, `sort`/`squish`/`reverse`/`unique`/`head`/`tail`/`rotate`
# answer a native array of the same element type (so Code::Coverage's
# `$lines.sort.squish` can `push` again), `values` answers a `List`, and the
# same methods on a boxed `Int @` still answer a `Seq` (measured on rakudo
# 2026.09).

plan 17;

my int @a = 3, 1, 2, 1;
for <sort squish reverse unique head(2) tail(2) rotate(1)> -> $m {
    my $r = EVAL "@a.$m";
    is $r.WHAT.raku, 'array[int]', ".$m of an int array is an array[int]";
}
is @a.sort.raku, 'array[int].new(1, 1, 2, 3)', 'with the sorted elements';
is @a.sort.squish.raku, 'array[int].new(1, 2, 3)', 'and the methods chain';
my $s = @a.sort.squish;
$s.push: 7;
is $s.elems, 4, 'so the result can be pushed to';
is @a.values.WHAT.raku, 'List', 'values is a List';

my num @n = 2.5e0, 1e0;
is @n.sort.WHAT.raku, 'array[num]', 'a num array';
my str @t = <b a>;
is @t.sort.WHAT.raku, 'array[str]', 'a str array';
my int @e;
is @e.sort.WHAT.raku, 'array[int]', 'an empty int array';

my Int @b = 3, 1;
is @b.sort.WHAT.raku, 'Seq', 'a boxed Int array still answers a Seq';
is (3, 1).sort.WHAT.raku, 'Seq', 'so does a List';
is <b a>.sort.WHAT.raku, 'Seq', 'so does a literal word list';
