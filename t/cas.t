use Test;

plan 8;

# 3-arg form: cas($var, $expected, $new)
{
    my atomicint $x = 0;
    my $old = cas($x, 0, 42);
    is $old, 0, 'cas 3-arg returns old value';
    is $x, 42, 'cas 3-arg swaps when current == expected';
}

{
    my atomicint $x = 10;
    my $old = cas($x, 0, 42);
    is $old, 10, 'cas 3-arg returns old value when no match';
    is $x, 10, 'cas 3-arg does not swap when current != expected';
}

# 2-arg form: cas($var, &code)
{
    my atomicint $y = 10;
    my $result = cas($y, -> $old { $old * 2 });
    is $result, 20, 'cas 2-arg returns new value';
    is $y, 20, 'cas 2-arg updates the variable';
}

{
    my atomicint $z = 5;
    cas($z, -> $old { $old + 3 });
    is $z, 8, 'cas 2-arg with addition';
}

# Multiple cas operations
{
    my atomicint $a = 0;
    cas($a, 0, 10);
    cas($a, 10, 20);
    cas($a, 5, 99);  # should not change since $a is 20
    is $a, 20, 'sequential cas operations work correctly';
}
