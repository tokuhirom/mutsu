use Test;

plan 8;

# A Junction passed to a *slurpy* parameter is captured as-is (one element),
# NOT auto-threaded — the slurpy binds its elements as raw Mu values.

{
    my $count = 0;
    sub f(*@a) { $count++ }
    f(3|4|5);
    is $count, 1, '*@a slurpy does not autothread a Junction arg';
}

{
    my $count = 0;
    multi sub g(*@a) { $count++ }
    g(3|4|5);
    is $count, 1, 'multi *@a slurpy does not autothread a Junction arg';
}

{
    my $count = 0;
    sub h(+@a) { $count++ }
    h(3|4|5);
    is $count, 1, '+@a single-argument-rule slurpy does not autothread';
}

{
    my $count = 0;
    sub i($x, *@a) { $count++ }
    i(6, 3|4|5);
    is $count, 1, 'Junction landing in a trailing slurpy does not autothread';
}

# But a Junction bound to an ordinary (Any-typed) positional param DOES autothread.
{
    my $count = 0;
    sub j($x) { $count++ }
    j(3|4|5);
    is $count, 3, 'Junction in an untyped scalar param autothreads';
}

{
    my $count = 0;
    sub k($x, *@a) { $count++ }
    k(3|4|5, 6);
    is $count, 3, 'Junction in a leading scalar param autothreads even with a slurpy';
}

# The slurpy actually holds the Junction value.
{
    sub l(*@a) { @a[0] }
    my $j = l(3|4|5);
    ok $j ~~ Junction, 'the slurpy element is the Junction itself';
    is l(3|4|5).elems, 1, 'one element captured';
}
