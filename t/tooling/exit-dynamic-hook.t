use Test;

plan 8;

# rakudo's `exit` is overridable through the dynamic `&*EXIT` hook: when one is
# in scope it is CALLED with the status instead of ending the process. In
# statement position the call then LEAVES the enclosing block, which returns
# Nil (the rest of the block is skipped). The genuine upstream `Test.rakumod`'s
# `exits-ok` is built entirely on this, but it is a plain language feature.

{
    my $seen;
    my $after = 'not run';
    sub run-it {
        my &*EXIT = -> $c { $seen = $c };
        exit 4;
        $after = 'ran';
    }
    run-it();
    is $seen, 4, 'exit called the dynamic &*EXIT hook with its status';
    is $after, 'not run', 'and the rest of the routine was skipped';
}

{
    my $seen = -1;
    sub bare { my &*EXIT = -> $c { $seen = $c }; exit; }
    bare();
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
    sub inner-scope {
        my &*EXIT = -> $c { $outer = "inner-$c" };
        exit 1;
    }
    inner-scope();
    is $outer, 'inner-1', 'the innermost hook in scope wins';
}

# Sibling routines each see only their own hook.
{
    my $first = 0;
    my $second = 0;
    sub one { my &*EXIT = -> $c { $first = $c }; exit 5 }
    sub two { my &*EXIT = -> $c { $second = $c }; exit 6 }
    one();
    two();
    is-deeply ($first, $second), (5, 6), 'sibling routines each get their own hook';
}

# A routine whose `exit` is trapped returns Nil, not what follows the exit.
{
    sub f { my &*EXIT = -> $c { }; exit 7; 42 }
    is-deeply f(), Nil, 'a trapped exit makes the routine return Nil';
}

# `exit` under a hook does not unwind past the routine that called it.
{
    my $seen;
    my &*EXIT = -> $c { $seen = $c };
    sub g { exit 3; 'after' }
    my $r = g();
    is $seen, 3, 'exit under a hook does not unwind the caller';
}
