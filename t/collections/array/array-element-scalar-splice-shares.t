use Test;

plan 6;

# A scalar holding an array taken from an element shares that array, so a
# structural mutation through the scalar reaches the element (Terminal::Print
# edits grid rows as `my $row = @!grid[$y]; $row.splice(...)`).
{
    my @h = [1, 2, 3], [4, 5, 6];
    my $row = @h[0];
    $row.splice(0, 2, <a b>);
    is-deeply @h[0], ['a', 'b', 3], 'splice with replacement reaches the element';
}
{
    my @h = [1, 2, 3], [4, 5, 6];
    my $row = @h[0];
    $row.splice(1, 1);
    is-deeply @h[0], [1, 3], 'splice without replacement reaches the element';
}
{
    my @h = [1, 2, 3], [4, 5, 6];
    my $row = @h[0];
    is $row.pop, 3, 'pop returns the last element';
    is-deeply @h[0], [1, 2], '... and removes it from the element';
}
{
    my @h = [1, 2, 3], [4, 5, 6];
    my $row = @h[1];
    $row.shift;
    is-deeply @h[1], [5, 6], 'shift reaches the element';
}

# A copy is still independent.
{
    my @a = 1, 2, 3;
    my $t = [@a];
    $t.shift;
    is-deeply @a, [1, 2, 3], 'mutating a fresh [@a] copy leaves @a alone';
}
