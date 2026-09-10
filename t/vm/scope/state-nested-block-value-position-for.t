use Test;

plan 6;

# `state` restart is decided by block CLONING: a nested `{ ... }` inside a loop
# body is a block literal the body re-clones on every iteration, so its `state`
# restarts each time. mutsu models that with an `OpCode::ResetStateLocals`
# bracket, emitted by `compile_bare_block_inline`.
#
# The value-collecting `for` body (`do for ... { ... }`) compiled its tail
# `Stmt::Block` through the raw `compile_block_inline`, which emits no such
# bracket, so it was the one path where a nested block's `state` survived across
# iterations.

{
    my @r = do for ^3 { { state $x = 0; $x++ } };
    is-deeply @r, [0, 0, 0],
        'a nested block in a value-position for restarts its state each iteration';
}

# The statement-position form was already correct -- keep it that way.
{
    my @r;
    for ^3 { { state $x = 0; @r.push($x++) } }
    is-deeply @r, [0, 0, 0],
        'a nested block in a statement-position for restarts its state each iteration';
}

# ... as was a value-position block outside any loop.
{
    my sub g { do { { state $x = 0; $x++ } } }
    is-deeply [g(), g()], [0, 0],
        'a nested block in a value-position do restarts its state each call';
}

# The sole-block statement-modifier form is the OPPOSITE case and must not
# regress: there the block IS the loop's body, cloned once for the whole loop,
# so its state persists across iterations.
{
    my @r;
    { state $n = 0; @r.push($n++) } for 1..3;
    is-deeply @r, [0, 1, 2],
        'a sole-block loop body keeps its state across iterations';
}

# The loop body's own (non-nested) state likewise persists.
{
    my @r = do for ^3 { state $w = 0; $w++ };
    is-deeply @r, [0, 1, 2],
        "a value-position for body's own state persists across iterations";
}

# Routing the tail block through the bare-block compile must not drop a phaser
# it carries, nor the value it yields.
{
    my @log;
    my @r = do for ^2 { { LEAVE { @log.push('L') }; 'v' } };
    is-deeply [@r, @log], [['v', 'v'], ['L', 'L']],
        'a tail block carrying a LEAVE still runs it and still yields its value';
}
