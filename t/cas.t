use Test;

plan 10;

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

{
    my atomicint $value = 0;
    await start {
        cas($value, 0, 1);
        cas($value, 1, 2);
    };
    is $value, 2, 'cas updates from start thread are visible to main thread';
}

# Regression pin: the `cas($var, -> $v { $v + delta })` compile-time
# rewrite to `__mutsu_atomic_add_var` must fire regardless of whether the
# lambda body is a bare `Stmt::Expr` or, as pointy-block parsing normally
# produces, prefixed with a `Stmt::SetLine`. Two call sites on the SAME
# variable use different lambda shapes here (one hits the delta fast path,
# one falls through to the general `cas` path) to pin that both are kept
# at counts_as_write=false, so neither gets a different cell-promotion
# classification than the other for the same variable.
{
    my atomicint $mixed = 0;
    await Promise.allof(
        start { for 1..25 { cas $mixed, -> $v { $v + 1 } } },
        start { for 1..25 { my $x = 1; cas $mixed, -> $v { $v + $x } } },
    );
    is $mixed, 50, 'delta-shape and general-shape cas calls on the same var interleave correctly';
}
