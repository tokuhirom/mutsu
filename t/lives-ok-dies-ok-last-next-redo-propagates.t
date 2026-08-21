use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# `lives-ok`/`dies-ok` run their block through a nested-run boundary
# (`eval_block_value`/`with_nested_registers`). A `last`/`next`/`redo`
# executed inside that block is NOT necessarily "the block died": when the
# block calls a closure whose loop-control statement targets a real
# lexically-enclosing loop dynamically further out (e.g. a `for` loop that
# also called `lives-ok`/`dies-ok`), the signal must keep propagating past
# `lives-ok`/`dies-ok`'s own pass/fail reporting -- exactly like Rakudo,
# where the enclosing loop reacts immediately and the assertion is never
# recorded at all (verified against `raku`).
#
# This is the `last`/`next`/`redo` counterpart of
# t/lives-ok-dies-ok-return-propagates.t, closing the gap left open by
# todo/tickets/lives-ok-dies-ok-last-next-redo-not-propagated.md. Unlike
# `return`, `last`/`next`/`redo` have no compile-time "is there an enclosing
# loop" check (`src/runtime/loop_handler_depth.rs` explains why the check
# must be dynamic): instead the `Last`/`Next`/`Redo` opcodes already convert
# a genuinely homeless signal into a typed `X::ControlFlow` at the raise
# site, and `RuntimeError::is_illegal_control()` is what
# `is_live_nonlocal_control` in `src/runtime/test_functions/eval_exception.rs`
# reads to distinguish that case from a live, still-in-flight signal.
#
# Since `last`/`next`/`redo` (unlike `return`) don't carry a value the test
# can observe with `is`, each case here spawns a subprocess (`is_run`) and
# checks the *shape* of its full TAP output/exit status against what `raku`
# produces for the same script.
plan 6;

# `last` escapes a `lives-ok` block to a real enclosing `for` loop: the loop
# runs its first iteration, `last` fires and unwinds past `lives-ok`
# (recording neither a pass nor a fail), and the loop stops -- so only
# "start 1" is ever printed, never "after 1", and the plan is under-run.
is_run(
    q:to/CODE/,
    use Test;
    plan 1;
    for 1..3 -> $i {
        say "start $i";
        my $cb = -> { last };
        lives-ok { $cb() }, "x";
        say "after $i";
    }
    say "done";
    CODE
    { out => "1..1\nstart 1\ndone\n", status => 255 },
    'last inside a lives-ok block escapes to a real enclosing for loop, breaking it after one iteration',
);

# `next` escapes the same way, but only skips the rest of *that* iteration:
# the loop still runs all three iterations ("start 1/2/3" all print), "after
# $i" never prints (skipped every time), and the assertion is never
# recorded.
is_run(
    q:to/CODE/,
    use Test;
    plan 1;
    for 1..3 -> $i {
        say "start $i";
        my $cb = -> { next };
        lives-ok { $cb() }, "x";
        say "after $i";
    }
    say "done";
    CODE
    { out => "1..1\nstart 1\nstart 2\nstart 3\ndone\n", status => 255 },
    'next inside a lives-ok block escapes to a real enclosing for loop, skipping the rest of each iteration',
);

# `redo` re-runs the current loop iteration from the top. Bound the re-runs
# with a counter (`$n`) so the escaping `redo` only fires once per real
# iteration, then the surrounding `for` proceeds normally.
is_run(
    q:to/CODE/,
    use Test;
    plan 3;
    my $n = 0;
    for 1..3 -> $i {
        $n++;
        last if $n > 5;
        my $cb = -> { redo };
        if $n == 1 {
            lives-ok { $cb() }, "x";
        } else {
            ok True, "iter $n";
        }
    }
    say "n=$n";
    CODE
    { out => "1..3\nok 1 - iter 2\nok 2 - iter 3\nok 3 - iter 4\nn=4\n", status => 0 },
    'redo inside a lives-ok block escapes to a real enclosing for loop and re-runs that iteration',
);

# `dies-ok` gets the same propagation: `last` from a real enclosing loop
# unwinds past `dies-ok` too.
is_run(
    q:to/CODE/,
    use Test;
    plan 1;
    for 1..3 -> $i {
        say "start $i";
        my $cb = -> { last };
        dies-ok { $cb() }, "x";
        say "after $i";
    }
    say "done";
    CODE
    { out => "1..1\nstart 1\ndone\n", status => 255 },
    'last inside a dies-ok block escapes to a real enclosing for loop, breaking it after one iteration',
);

# A genuinely homeless `last`/`next` (no loop anywhere on the dynamic chain)
# is NOT a live signal -- it is converted to a typed X::ControlFlow at the
# opcode itself, so `lives-ok` must still record it as a normal failed
# assertion (matching raku: "last without loop construct"), not let it
# escape uncaught.
is_run(
    q:to/CODE/,
    use Test;
    plan 1;
    lives-ok { last }, "bare last, no loop anywhere";
    say "after";
    CODE
    # The "# Failed test ..." / "# at <file> line N" diagnostics go to
    # stderr (embedding an unpredictable temp-file path, since is_run runs
    # the code as a script, not `-e`), so only stdout and the exit status
    # are asserted here.
    { out => "1..1\nnot ok 1 - bare last, no loop anywhere\nafter\n", status => 1 },
    'bare last with no enclosing loop is still reported as a failed lives-ok assertion, not an uncaught abort',
);

# The homeless case is a legitimate "die" from dies-ok's point of view: the
# block really did throw (a typed X::ControlFlow), so the assertion passes.
is_run(
    q:to/CODE/,
    use Test;
    plan 1;
    dies-ok { next }, "bare next, no loop anywhere";
    say "after";
    CODE
    { out => "1..1\nok 1 - bare next, no loop anywhere\nafter\n", status => 0 },
    'bare next with no enclosing loop is a genuine throw, so dies-ok passes',
);
