use Test;

plan 12;

# The loop-phaser lowering appends `NEXT`/`LEAVE` bodies to the END of the loop
# body. In statement position that is invisible, because the body's trailing
# value is sunk. In expression position -- `do for ... { ... }`, which collects
# each iteration's trailing value -- it made the PHASER body's value the
# iteration's result.
#
# The capture-into-a-temp mechanism already existed for KEEP/UNDO/POST; it was
# keyed off which phasers were present rather than off whether the caller wanted
# the value, and its re-emit sat BEFORE the appended phaser bodies rather than
# after them.

# --- the bug --------------------------------------------------------------

{
    my @seen;
    my @doubled = do for 1, 2, 3 {
        NEXT { @seen.push: 'next' }
        $_ * 2
    };
    is-deeply @doubled, [2, 4, 6], 'a NEXT phaser does not clobber the collected value';
    is @seen.elems, 3, '... and the NEXT phaser still ran every iteration';
}

# LEAVE is spliced by the same rewrite, so it had the same defect.
{
    my @left;
    my @r = do for 1, 2 {
        LEAVE { @left.push: 'leave' }
        $_ + 100
    };
    is-deeply @r, [101, 102], 'a LEAVE phaser does not clobber the collected value';
    is @left.elems, 2, '... and the LEAVE phaser still ran every iteration';
}

# --- the cases that were already correct, and must stay so ----------------

{
    my @s;
    for 1, 2, 3 { NEXT { @s.push('n') }; @s.push($_ * 2) }
    is-deeply @s, [2, 'n', 4, 'n', 6, 'n'],
        'statement position is unchanged (value sunk, phaser still runs)';
}

{
    my @f = do for 1, 2 { FIRST { 99 }; $_ * 10 };
    is-deeply @f, [10, 20], 'FIRST is prepended as a guard and never was at risk';
    my @l = do for 1, 2 { LAST { 99 }; $_ * 10 };
    is-deeply @l, [10, 20], 'LAST lands in the post-loop statements';
}

{
    my @plain = do for 1, 2, 3 { $_ * 2 };
    is-deeply @plain, [2, 4, 6], 'a phaser-free value-position for is unchanged';
}

# KEEP/UNDO already allocated the capture temp; the re-emit had to move after
# the appended phaser bodies without disturbing them.
{
    my @k = do for 1, 2 { KEEP { 88 }; $_ * 3 };
    is-deeply @k, [3, 6], 'KEEP does not clobber the collected value';
    my @nk = do for 1, 2 { NEXT { 5 }; KEEP { 6 }; $_ + 1 };
    is-deeply @nk, [2, 3], 'NEXT and KEEP together do not clobber it either';
}

# A `take`n body value must NOT be re-emitted (sinking a taken Failure throws).
{
    my @g = gather for 1, 2 { NEXT { 1 }; take $_ * 7 };
    is-deeply @g, [7, 14], 'a gather/take body with NEXT still takes the right value';
}

# `next` as control flow (not the phaser) skips the iteration entirely.
{
    my @n = do for 1, 2, 3 { next if $_ == 2; $_ * 2 };
    is-deeply @n, [2, 6], 'a `next` control exception still skips its iteration';
}
