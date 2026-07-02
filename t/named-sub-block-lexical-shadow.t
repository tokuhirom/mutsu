use Test;

# A block's own `my $x` shadows a same-named outer-scope variable for the
# *entire* block, even before the `my` statement executes -- because `sub`
# declarations are hoisted, a nested named `sub` called via forward
# reference must see the block's own (still undefined) `$x`, not the outer
# one. mutsu's per-routine local-slot storage reuses the outer slot for a
# same-named `my`, so the shadow only used to take effect once the `my`
# statement itself ran; a forward-referenced call before that point leaked
# the outer value. roast: S04-declarations/my-6e.t.
# https://github.com/Raku/old-issue-tracker/issues/2540

plan 4;

{
    my $x = 1;
    {
        nok declare_later().defined,
            'nested sub sees block-own $x as undefined before its my runs, not outer $x';
        my $x;
        sub declare_later {
            $x;
        }
    }
    is $x, 1, 'outer $x is unaffected by the inner shadow';
}

# No outer name collision: the previous fix must not regress this case.
{
    nok declare_later_fresh().defined,
        'nested sub sees undefined before a non-shadowing my runs';
    my $y;
    sub declare_later_fresh {
        $y;
    }
}

# A block containing a nested sub, but where the shadowed name is a typed
# `@`/`%` slot reused from a sibling block, must not have its type check
# clobbered by the reset (regression guard: this fix is scoped to plain
# scalars only).
{
    my Int @a = 1, 2, 3;
}
{
    my @a = 4, 5, 6;
    sub reads_a { @a }
    is-deeply reads_a(), [4, 5, 6], 'typed-slot reuse with a nested sub does not throw';
}

done-testing;
