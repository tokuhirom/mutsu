use Test;

# Slice F coherence pin: an X/Z meta-op whose operand is a thunk
# (`(($s++,),) Xxx 3`, `1 Zand ($s++,)`) mutates a *captured-outer* caller
# lexical when the thunk fires. The thunk runs via a native builtin
# (`__mutsu_reverse_xx` / `__mutsu_*_shortcircuit`) which writes the mutation
# into env by name; the caller's local slot was previously reconciled only by
# the blanket reverse `sync_locals_from_env` pull. These cases must stay coherent
# WITHOUT that pull (run with MUTSU_NO_REVERSE_SYNC=1 to verify), and the
# short-circuit thunks must still NOT run when the operator does not need them.

plan 12;

# 1-2. Xxx: thunk fires once per repeat; captured-outer counter visible.
{
    my $s = 0;
    (($s++,),) Xxx 1;
    is $s, 1, 'Xxx thunk runs once: captured-outer counter visible';
}
{
    my $s = 0;
    (($s++,),) Xxx 9;
    is $s, 9, 'Xxx thunk runs repeatedly: captured-outer counter visible';
}

# 3. Xxx with count 0: thunk must NOT run.
{
    my $s = 0;
    (($s++,),) Xxx 0;
    is $s, 0, 'Xxx with count 0 does not run the thunk';
}

# 4-5. Zand short-circuit: runs the thunk only when the left side is true.
{
    my Mu $s = 0;
    1 Zand ($s++,);
    is $s, 1, 'Zand runs the thunk when the left operand is true';
}
{
    my Mu $s = 0;
    0 Zand ($s++,);
    is $s, 0, 'Zand does not run the thunk when the left operand is false';
}

# 6-7. Zor short-circuit: runs the thunk only when the left side is false.
{
    my Mu $s = 0;
    0 Zor ($s++,);
    is $s, 1, 'Zor runs the thunk when the left operand is false';
}
{
    my Mu $s = 0;
    1 Zor ($s++,);
    is $s, 0, 'Zor does not run the thunk when the left operand is true';
}

# 8. Z&& alias of Zand.
{
    my Mu $s = 0;
    1 Z&& ($s++,);
    is $s, 1, 'Z&& runs the thunk when the left operand is true';
}

# 9. Z|| alias of Zor.
{
    my Mu $s = 0;
    0 Z|| ($s++,);
    is $s, 1, 'Z|| runs the thunk when the left operand is false';
}

# 10-11. Xand / Xor cross short-circuit.
{
    my $s = 0;
    (1,) Xand ($s++,);
    is $s, 1, 'Xand runs the thunk when needed';
}
{
    my $s = 0;
    (0,) Xor ($s++,);
    is $s, 1, 'Xor runs the thunk when needed';
}

# 12. Zxx: list left side thunked, captured-outer counter visible.
{
    my $s = 0;
    ($s++,) Zxx 3;
    is $s, 3, 'Zxx thunk runs repeatedly: captured-outer counter visible';
}
