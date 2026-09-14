use Test;

plan 8;

# An inline declaration inside a list literal denotes the freshly-declared
# variable's own container, so a subscript store through the list writes
# that variable, not a fresh copy (#7556 section F).

{
    my $a = 1;
    (my $x = $a, 6)[0] = 10;
    is $x, 10, 'the declared element is written through';
    is $a, 1, 'and the source it was initialized from is untouched';
}

{
    my $a = 1;
    (6, my $x = $a)[1] = 20;
    is $x, 20, 'works at a non-zero index too';
}

{
    my $b = 2;
    my $c = 3;
    (my $y = $b, my $z = $c)[1] = 99;
    is $y, 2, 'a non-targeted inline declaration keeps its initial value';
    is $z, 99, 'and the targeted one is overwritten';
}

{
    (my $w)[0] = 42;
    is $w, 42, 'an uninitialized inline declaration is still writable';
}

# What must keep failing: a genuine literal element still refuses the store.
{
    my $lit = (1, 2);
    throws-like { $lit[0] = 5 }, X::Assignment::RO,
        message => 'Cannot modify an immutable List ((1 2))',
        'a plain literal list element is still refused';
}

{
    my $a = 1;
    my $b = 2;
    (my $x = $a, $b)[0] = 10;
    is $b, 2, 'a plain variable sibling element is unaffected by the fix';
}
