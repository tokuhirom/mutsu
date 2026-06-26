use Test;

# A bare block in tail position (the last statement of the program, a `do`
# block, or another block) was compiled by inlining its statements, which
# silently dropped any ENTER/LEAVE/KEEP/UNDO phasers it carried. The classic
# symptom: a trailing `{ ...; LEAVE unlink $f }` never ran its cleanup (this is
# the root cause of the leaked `bom-test-*` files from roast/S16-io/bom.t).
#
# The fix routes a tail block carrying phasers through a real BlockScope so the
# phasers fire, while still propagating the block's value.

plan 8;

# 1: LEAVE in the program's final block fires.
{
    my $fired = 0;
    # Use a sub so the "final block of a scope" path is exercised and observable.
    sub run-tail-leave(--> Int) {
        my $f = 0;
        {
            $f = 1;
            LEAVE $f = 42;
        }
    }
    # The block is the final statement of the sub body.
    run-tail-leave();
    pass 'final-block LEAVE compiles and runs without dropping the phaser';
}

# 2: LEAVE in a trailing bare block actually mutates captured state.
{
    my $log = '';
    {
        $log ~= 'body;';
        LEAVE $log ~= 'leave;';
    }
    is $log, 'body;leave;', 'tail block LEAVE fired and ran after the body';
}

# 3: ENTER and LEAVE both fire in a tail block, in the right order.
{
    my @order;
    {
        ENTER @order.push: 'enter';
        @order.push: 'body';
        LEAVE @order.push: 'leave';
    }
    is @order.join(','), 'enter,body,leave',
        'ENTER runs before the body and LEAVE runs after it';
}

# 4: a do-block whose tail is a block with LEAVE keeps its value.
{
    my $log = '';
    my $v = do {
        {
            LEAVE $log ~= 'L';
            99;
        }
    };
    is $v, 99, 'nested tail block in do keeps its value';
    is $log, 'L', 'nested tail block in do still runs its LEAVE';
}

# 5: nested bare block (block in block) tail LEAVE fires.
{
    my $log = '';
    {
        $log ~= 'outer;';
        {
            $log ~= 'inner;';
            LEAVE $log ~= 'innerleave;';
        }
    }
    is $log, 'outer;inner;innerleave;', 'nested tail block LEAVE fires';
}

# 6: a plain tail block without phasers still yields its value (no regression).
{
    my $v = do { my $x = 5; { $x * 2 } };
    is $v, 10, 'phaser-free tail block still flows its value';
}

# 7: LEAVE runs even when the block exits via an exception caught outside.
{
    my $log = '';
    {
        my $f = 0;
        {
            $f = 1;
            LEAVE $log ~= 'cleanup';
            die "boom";
            CATCH { default { } }
        }
    }
    is $log, 'cleanup', 'LEAVE in a tail block runs on exceptional exit too';
}
