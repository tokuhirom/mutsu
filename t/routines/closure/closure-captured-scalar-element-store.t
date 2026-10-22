use Test;

# An element store on a captured, still-undefined scalar writes that scalar,
# so the closure's capture must be a shared cell (#14637, #14647). Expected
# values are the `raku` output.

plan 5;

{
    my $a;
    my $c = -> { $a{"k"} = 1 };
    $c();
    is $a.keys.join(','), 'k', 'hash element store through a lambda';
    is $a<k>, 1, '... and the value';
}
{
    my $a;
    my $c = -> { $a[1] = 5 };
    $c();
    is $a.elems, 2, 'array element store through a lambda';
}
{
    my $a;
    my $c = -> { $a{"k"}.push(1) };
    $c();
    is $a<k>.join(','), '1', 'push on an element through a lambda';
}
{
    my $a;
    for 1..2 -> $i { my $f = -> { $a{$i} = $i }; $f() }
    is $a.keys.sort.join(','), '1,2', 'a lambda created per iteration';
}
