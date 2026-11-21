use Test;

# #18389: a native array held in a `$` scalar answers its order-preserving
# methods (`head`, `tail`, `sort`, `reverse`, `unique`, `squish`, `rotate`)
# with its own native array type, exactly as the bare `@` variable does.
# The method-table lane and the full call path must agree on that.

plan 12;

my num @n = 1e0, 2e0, 3e0;
my $c = @n;
for ^2 {
    # A loop, so the call site runs once to fill its memo and once from it.
    is $c.head(2).raku, 'array[num].new(1e0, 2e0)', 'itemized num array .head(n)';
}
is $c.tail(1).raku, 'array[num].new(3e0)', 'itemized num array .tail(n)';
is $c.sort.raku, 'array[num].new(1e0, 2e0, 3e0)', 'itemized num array .sort';

my int @i = 1, 2, 2;
my $d = @i;
is $d.reverse.raku, 'array[int].new(2, 2, 1)', 'itemized int array .reverse';
is $d.unique.raku, 'array[int].new(1, 2)', 'itemized int array .unique';
is $d.squish.raku, 'array[int].new(1, 2)', 'itemized int array .squish';
is $d.rotate.raku, 'array[int].new(2, 2, 1)', 'itemized int array .rotate';
is $d.values.raku, '(1, 2, 2)', 'itemized int array .values is a List';
is $d.head.raku, '1', 'argless .head is the element';

# Returned from a routine as part of a List, then destructured.
sub pair-of(--> List) { my num @a = 4e0, 5e0; my num @b = 6e0; (@a, @b) }
my ($x, $y) = pair-of();
is $x.head(1).raku, 'array[num].new(4e0)', 'destructured native array .head(n)';

# An untyped array stays a Seq.
my @u = 1, 2;
my $e = @u;
is $e.head(1).raku, '(1,).Seq', 'untyped itemized array .head(n) is a Seq';
