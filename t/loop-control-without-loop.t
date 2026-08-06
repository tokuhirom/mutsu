# A `next`/`last`/`redo` that finds no construct to act on is an ordinary,
# catchable `X::ControlFlow` in rakudo. mutsu raised a control *signal*, and
# `try`/`CATCH` deliberately pass control signals through, so it escaped every
# handler and surfaced only at the top of the program — uncatchable.
#
# The discriminator has to be dynamic (a signal legitimately crosses routine and
# EVAL boundaries), so every construct that handles one now raises a
# thread-local depth for the extent in which it would catch, and the raise site
# consults it. The second half of this file is the regression guard for that
# sweep: a construct whose guard is missing would turn a working `next` into a
# thrown exception.
use Test;

plan 24;

# --- no construct to act on: a real, catchable exception ------------------
# Written out rather than looped: rakudo's own `throws-like` nests its subtests
# wrongly when called from inside a `for` whose EVAL'd argument raises a
# loop-control exception, so the loop form fails under `raku` for reasons that
# have nothing to do with what is being tested here.
throws-like 'my $i; { $i++; next; $i--; }', X::ControlFlow,
    illegal => 'next', enclosing => 'loop construct', 'bare next is X::ControlFlow';
throws-like 'my $i; { $i++; last; $i--; }', X::ControlFlow,
    illegal => 'last', enclosing => 'loop construct', 'bare last is X::ControlFlow';
throws-like 'my $i; { $i++; redo; $i--; }', X::ControlFlow,
    illegal => 'redo', enclosing => 'loop construct', 'bare redo is X::ControlFlow';
{
    my $i;
    try { { $i++; next; $i--; } };
    is $!.message, 'next without loop construct', 'the message names the construct';
    is $i, 1, 'the statements after next did not run';
}
# A labelled bare block / labelled `do` block is a block, not a loop
# construct — `last`/`next`/`redo` naming its own label must raise
# X::ControlFlow ("labeled ... without loop construct"), not iterate once and
# exit the block as if it were a one-iteration loop.
{
    my $n = 0;
    try { LAB: { $n++; last LAB; $n += 100 } };
    is $!.message, 'labeled last without loop construct',
        'last naming a labelled bare block is X::ControlFlow';
    is $n, 1, 'the statements after last did not run';
}
{
    my $n = 0;
    try { A: do { $n++; last A; $n += 100 } };
    is $!.message, 'labeled last without loop construct',
        'last naming a labelled do block is X::ControlFlow';
    is $n, 1, 'the statements after last did not run';
}
# A CONTROL block still sees it — it is a control exception, just a catchable one.
{
    my $handled = '';
    try { CONTROL { $handled = 'ok' }; next; };
    is $handled, 'ok', 'CONTROL still catches a loop-less next';
}

# --- a signal still crosses routine and EVAL boundaries -------------------
{
    my $n = 0;
    sub bump-next { next }
    for 1..3 { $n++; bump-next; $n += 100 }
    is $n, 3, 'next raised inside a called routine reaches the caller loop';
}
{
    my $n = 0;
    for 1..3 { $n++; EVAL 'next'; $n += 100 }
    is $n, 3, 'next raised inside EVAL reaches the enclosing loop';
}

# --- every construct that handles the signal must still handle it ---------
{
    my $n = 0;
    for 1..4 { next if $_ %% 2; $n++ }
    is $n, 2, 'for';
}
{
    my ($i, $n) = 0, 0;
    while ++$i <= 4 { next if $i %% 2; $n++ }
    is $n, 2, 'while';
}
{
    my ($i, $n) = 0, 0;
    until ($i += 1) > 4 { next if $i %% 2; $n++ }
    is $n, 2, 'until';
}
{
    my $n = 0;
    loop (my $i = 0; $i < 4; $i++) { next if $i %% 2; $n++ }
    is $n, 2, 'C-style loop';
}
{
    my ($i, $n) = 0, 0;
    repeat { $i++; next if $i %% 2; $n++ } while $i < 4;
    is $n, 2, 'repeat/while';
}
is-deeply (1..4).map({ next if $_ %% 2; $_ }).List, (1, 3), 'map';
is-deeply (1..4).grep({ next if $_ %% 2; True }).List, (1, 3), 'grep';
is-deeply (1, 2, 3, 4).deepmap({ next if $_ %% 2; $_ }).List, (1, 3), 'deepmap';
{
    my @seen = gather for 1..4 { next if $_ %% 2; take $_ };
    is-deeply @seen.List, (1, 3), 'gather + for';
}
{
    my $n = 0;
    LOOP: while $n < 3 { $n++; next LOOP; $n += 100 }
    is $n, 3, 'a labelled next reaches its labelled loop';
}
{
    my $n = 0;
    for 1..4 { last if $_ == 3; $n++ }
    is $n, 2, 'last';
}
{
    my ($tries, $n) = 0, 0;
    for 1..2 { $tries++; redo if $tries == 1 && $_ == 1; $n++ }
    is $n, 2, 'redo re-runs the iteration without advancing';
}
