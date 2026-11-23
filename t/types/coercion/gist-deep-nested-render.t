use Test;

# #12714: `.gist` and `.raku` of a deeply nested array render each level into
# one shared buffer instead of copying the child's text into the parent's.
# The text is unchanged; this pins it for nested shapes of every list kind,
# including the cycle back-references that are spliced in afterwards.

plan 12;

my $a = [];
for ^2000 { $a = [$a] }
my $g = $a.gist;
is $g.chars, 4002, '.gist of 2000 nested arrays has every bracket';
ok $g.starts-with('[[[') && $g.ends-with(']]]'), '... open and closed in order';
my $r = $a.raku;
is $r.chars, 6003, '.raku of 2000 nested arrays';
ok $r.starts-with('$[[[') && $r.ends-with('],],],]'), '... only the outermost itemized, each with its trailing comma';
is "{$a}".chars, 0, '.Str of nested empty arrays is empty';

is [[1, [2, (3, 4)]], $(5, 6)].gist, '[[1 [2 (3 4)]] (5 6)]', 'gist of mixed list kinds';
is ((1, 2).Seq, slip(3, 4)).gist, '((1 2) 3 4)', 'gist of a Seq and a Slip inside a List';
is (1, (2, (3, 4)), [5, [6]]).raku, '(1, (2, (3, 4)), [5, [6]])', 'raku of mixed list kinds';
is [(1, 2),].raku, '[(1, 2),]', 'raku keeps the trailing comma of a single listy element';
is ((1, 2),).raku, '((1, 2),)', '... and of a one-element List';

my @c; @c = 42, @c;
like @c.gist, /^ '(\\Array_' \d+ ' = [42 Array_' \d+ '])' $/, 'gist of a self-referencing array';
like @c.raku, /^ '((my @Array_' \d+ ') = [42, @Array_' \d+ '])' $/, 'raku of a self-referencing array';
