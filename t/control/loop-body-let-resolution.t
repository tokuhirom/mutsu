use Test;

# GH-7677: `let`/`temp` resolve their saves at the end of the enclosing BLOCK,
# and a loop body IS one — resolved once per ITERATION, not once for the whole
# loop. `for`/`while`/`until`/C-style `loop`/`repeat` bodies used to be compiled
# by none of the three lowerings that implement the resolution (the bare block
# and value-position `do` via `OpCode::LetBlock`, and routine-frame teardown via
# GH-7646), so a `temp` inside any loop body survived the loop entirely and the
# next iteration observed the previous one's temporized value.
#
# The frame is now emitted INSIDE the loop's own body range, so the loop opcodes
# re-execute it — and so re-mark — on every iteration. A `temp`-only body keeps
# its ordinary sink lowering (`temp` always restores, so the frame needs no
# value); a body with a real `let` has its tail statement compiled for value, on
# the value stack rather than through the topic, because a loop body's topic is
# the loop variable and must not be clobbered.
#
# Every assertion below was measured against `raku` (v2026.07) first.

plan 28;

# --- `temp` is restored at the end of the loop body, in every loop form ------

{
    my $x = 1;
    for 1..2 { temp $x = 2; }
    is $x, 1, '`for` body restores a `temp`';
}

{
    my $x = 1;
    my $c = 0;
    while $c++ < 2 { temp $x = 2; }
    is $x, 1, '`while` body restores a `temp`';
}

{
    my $x = 1;
    my $c = 0;
    until $c++ >= 2 { temp $x = 2; }
    is $x, 1, '`until` body restores a `temp`';
}

{
    my $x = 1;
    loop (my $i = 0; $i < 2; $i++) { temp $x = 2; }
    is $x, 1, 'C-style `loop` body restores a `temp`';
}

{
    my $x = 1;
    my $c = 0;
    repeat { temp $x = 2; } while $c++ < 1;
    is $x, 1, '`repeat` body restores a `temp`';
}

{
    my $x = 1;
    for 1..2 -> $e { temp $x = 2; }
    is $x, 1, 'a `for` with a named parameter restores a `temp`';
}

# --- the resolution is PER ITERATION, not per loop ---------------------------

{
    my $x = 1;
    my @seen;
    for 1..3 { @seen.push($x); temp $x = 2; }
    is @seen, [1, 1, 1],
        'each `for` iteration starts from the restored value, not the previous temporized one';
}

{
    my $x = 1;
    my @seen;
    my $c = 0;
    while $c++ < 3 { @seen.push($x); temp $x = 2; }
    is @seen, [1, 1, 1], 'each `while` iteration starts from the restored value';
}

{
    my $x = 1;
    my @seen;
    loop (my $i = 0; $i < 3; $i++) { @seen.push($x); temp $x = 2; }
    is @seen, [1, 1, 1], 'each C-style `loop` iteration starts from the restored value';
}

# --- a real `let` judges the ITERATION's own value ---------------------------

{
    my $x = 1;
    for 1..2 { let $x = 2; Nil }
    is $x, 1, 'a `for` iteration yielding an undefined value rolls back a `let`';
}

{
    my $x = 1;
    for 1..2 { let $x = 2; 42 }
    is $x, 2, 'a `for` iteration yielding a defined value commits a `let`';
}

{
    my $x = 1;
    for 1..2 { let $x = 2 }
    is $x, 2, 'the `let` assignment is itself a defined tail value, so it commits';
}

{
    my $x = 1;
    my $c = 0;
    while $c++ < 2 { let $x = 2; Nil }
    is $x, 1, 'a `while` iteration yielding an undefined value rolls back a `let`';
}

{
    my $x = 1;
    my $c = 0;
    while $c++ < 2 { let $x = 2; 42 }
    is $x, 2, 'a `while` iteration yielding a defined value commits a `let`';
}

{
    # The iteration's value comes off the value stack, NOT the topic: writing the
    # topic would clobber the loop variable the iteration runs under.
    my $x = 1;
    my @seen;
    for 1..2 { @seen.push($_); let $x = 2; Nil }
    is @seen, [1, 2], 'routing the tail value out does not disturb the loop variable';
    is $x, 1, '...and the `let` still rolls back';
}

{
    my $x = 1;
    for 1..2 { let $x = 2; Nil }
    is $_.defined, False, 'the enclosing topic is untouched by a `let` loop body';
}

# --- an iteration that exits early is an UNSUCCESSFUL exit -------------------

{
    my $x = 1;
    for 1..3 { temp $x = 2; next if $_ == 2; }
    is $x, 1, '`next` out of an iteration still restores its `temp`';
}

{
    my $x = 1;
    for 1..2 { temp $x = 2; last }
    is $x, 1, '`last` out of an iteration still restores its `temp`';
}

{
    my $x = 1;
    for 1..2 { let $x = 2; next }
    is $x, 1, '`next` is an unsuccessful exit, so it rolls back a `let`';
}

{
    my $x = 1;
    for 1..2 { let $x = 2; last }
    is $x, 1, '`last` is an unsuccessful exit, so it rolls back a `let`';
}

{
    my $x = 1;
    sub returns-from-loop() { for 1..2 { let $x = 5; return 7 } }
    is returns-from-loop(), 7, 'a `return` out of a loop body still returns its value';
    is $x, 1, '...and is an unsuccessful block exit, so it rolls back a `let`';
}

# --- the save is owned by the body, wherever in it the `temp` sits -----------

{
    my $x = 1;
    for 1..2 { if True { temp $x = 2 } }
    is $x, 1, 'a `temp` in a nested `if` branch is resolved by the loop body';
}

{
    my $x = 1;
    my @seen;
    for 1..2 { temp $x = 2; { @seen.push($x) } }
    is @seen, [2, 2], 'a nested bare block still sees the temporized value';
    is $x, 1, '...and the loop body restores it at the end of the iteration';
}

# --- a value-collecting `for` collects the value BEFORE the restore ----------

{
    my $x = 1;
    my @collected = do for 1..2 { temp $x = 9; $x + 0 };
    is @collected, [9, 9], 'a collecting `for` yields each iteration its temporized value';
    is $x, 1, '...and still restores the `temp` per iteration';
}
