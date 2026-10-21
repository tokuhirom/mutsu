use Test;

# A `.map` block with a slurpy (`*@`) parameter takes its required
# positionals per call, at least one element, so a prefix read of such a map
# pulls only what it needs. A map over an infinite source used to hang: the
# slurpy block was read whole (#14300). Expected values are the `raku`
# (v2026.09) output.

plan 6;

is (1..5).map(-> *@a { @a.elems }), (1, 1, 1, 1, 1), 'one element per call';
is (1..*).map(-> *@a { @a.elems }).head(1), (1,), 'a prefix of an infinite source';
is (1..*).map(-> *@a { @a[0] * 2 }).head(3), (2, 4, 6), '... reads the elements in order';
is (1..*).map(-> $x, *@r { $x }).head(3), (1, 2, 3),
    'a required positional before the slurpy one is one element per call';

my $n = 0;
my $s = (1..10).map(-> *@a { $n++; @a[0] });
is $s[2], 3, 'an index read of a deferred map';
is $n, 3, '... runs the block only up to that element';
