use Test;

plan 6;

sub l () { 1, 2 }

# A parenthesized single scalar is a LIST-assignment target: `($a) = 1, 2, 3`
# makes `$a` slurp the whole list (unlike a bare `$a = 1, 2, 3` item assignment).
{
    my $a;
    my @z = (($a) = l, l, l);
    is $a.elems, 3, '($a) = l, l, l makes $a slurp the whole list';
    is @z.elems, 1, 'the list-assignment rvalue is the 1-element target list';
}

# The `Grouped` wrapper stays transparent everywhere else:
{
    my $x = 5;
    is ($x), 5, '($x) reads the scalar';
    is ($x) + 1, 6, '($x) + 1 works';
}

# `for ($scalar)` still iterates once and writes back through `is rw`.
{
    my $x = 42;
    for ($x) -> $v is rw { $v++ }
    is $x, 43, 'for ($scalar) -> $v is rw writes back';

    my $ref = [1, 2, 3];
    my $count = 0;
    for ($ref) { $count++ }
    is $count, 1, 'for ($scalar-holding-arrayref) iterates once';
}
