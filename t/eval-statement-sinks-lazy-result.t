use Test;

# A bare `EVAL '...';` statement sinks its return value, like any other bare
# call statement -- and sinking a deferred `gather`/lazy-IO-lines result must
# FORCE it, exactly as `SinkPop` forces one for an ordinary bare statement.
# `OpCode::ExecCall`/`ExecCallPairs` (the "statement-level call, no return
# value kept" bytecode forms `EVAL` compiles to) never reached `SinkPop`'s
# forcing logic at all, so `EVAL 'gather { ... }';` silently never ran the
# gather body. This matters for the real, vendored `Test.rakumod`: its
# `throws-like` runs `EVAL $code, context => $ctx;` as exactly this kind of
# bare, named-arg statement call, so `throws-like 'gather { return 1 }', ...`
# never even entered the gather body (`todo/deep/vendor-real-test-module.md`,
# `t/throws-like-gather-sink.t`).

use MONKEY-SEE-NO-EVAL;

plan 3;

my $marker = "tmp/eval-sink-marker.txt".IO;

# Positional-only EVAL statement call (compiles to OpCode::ExecCall): the
# gather body's side effect (a spurt) must run.
{
    $marker.unlink if $marker.e;
    EVAL 'gather { "tmp/eval-sink-marker.txt".IO.spurt("ran"); take 1 }';
    is $marker.e ?? $marker.slurp !! 'not run', 'ran',
        'a bare, positional-only EVAL of a gather statement forces the body';
    $marker.unlink;
}

# Named-arg EVAL statement call (compiles to OpCode::ExecCallPairs, keep_value:
# false) -- the exact shape `Test.rakumod`'s `throws-like` uses: a bare,
# MID-BODY (not tail-position) `EVAL $code, context => $ctx;` statement,
# followed by more statements. A gather whose forced body hits an escaping
# `return` throws.
{
    my $ctx = CALLER::;
    my $died = False;
    my $reached = False;
    try {
        EVAL 'gather { return 1 }', context => $ctx;
        $reached = True; # keeps the EVAL call in non-tail (mid-body) position
        CATCH { default { $died = True; } }
    }
    ok $died,
        'a bare, named-arg EVAL of a gather-with-return forces the body and throws';
}

# A lazy IO-lines result must also be forced (drained) when EVAL sinks it --
# not left as a still-open, never-iterated handle.
{
    my $path = "tmp/eval-sink-lazy-lines.txt".IO;
    $path.spurt("L1\nL2\n");
    lives-ok { EVAL '"tmp/eval-sink-lazy-lines.txt".IO.open(:r).lines' },
        'a bare EVAL of a lazy IO .lines statement does not hang/crash';
    $path.unlink;
}
