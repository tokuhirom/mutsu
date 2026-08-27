use Test;

plan 7;

# rakudo's `exit` is overridable through the dynamic `&*EXIT` hook: when one is
# in scope it is CALLED with the status and `exit` then returns normally,
# leaving the process running. (The genuine upstream `Test.rakumod`'s
# `exits-ok` is built entirely on this, but it is a plain language feature.)

{
    my $seen;
    my &*EXIT = -> $c { $seen = $c };
    exit 4;
    is $seen, 4, 'exit called the dynamic &*EXIT hook with its status';
    pass 'and execution continued past the exit';
}

{
    my $seen = -1;
    my &*EXIT = -> $c { $seen = $c };
    exit;
    is $seen, 0, 'a bare exit hands the hook the default status 0';
}

# The hook is DYNAMIC, so an `exit` inside a callee finds the caller's.
{
    my @seen;
    sub deep() { exit 9 }
    sub mid() { deep() }
    my &*EXIT = -> $c { @seen.push: $c };
    mid();
    is-deeply @seen.List, (9,), 'the hook is found from a nested routine';
}

# Its dynamic scope ends with the block that declared it.
{
    my $outer;
    {
        my &*EXIT = -> $c { $outer = "inner-$c" };
        exit 1;
    }
    is $outer, 'inner-1', 'the innermost hook in scope wins';
}

# Sibling blocks each see only their own hook.
{
    my $first = 0;
    my $second = 0;
    { my &*EXIT = -> $c { $first = $c }; exit 5 }
    { my &*EXIT = -> $c { $second = $c }; exit 6 }
    is-deeply ($first, $second), (5, 6), 'sibling blocks each get their own hook';
}

# `exit` inside a `try` with a hook installed still just calls the hook.
{
    my $seen;
    my &*EXIT = -> $c { $seen = $c };
    my $r = do { exit 3; 'after' };
    is $seen, 3, 'exit under a hook does not unwind';
}
