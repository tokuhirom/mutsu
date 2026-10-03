use Test;

# An ordinary exception that unwinds out of a loop iteration runs that
# iteration's UNDO and LEAVE phasers, after the CATCH handlers have run and
# only when none of them `.resume`s it (#10580). Expected strings are rakudo's.

plan 12;

{
    my $log = '';
    try { for 1..2 { LEAVE { $log ~= "l$_ " }; UNDO { $log ~= "u$_ " }; die "x" } }
    is $log, 'u1 l1 ', 'die out of a for body runs UNDO then LEAVE';
}

{
    my $log = '';
    try { for 1..2 { KEEP { $log ~= "k$_ " }; UNDO { $log ~= "u$_ " }; $_ == 2 ?? die "x" !! 1 } }
    is $log, 'k1 u2 ', 'the dying iteration runs UNDO, the completed one KEEP';
}

{
    my $log = '';
    for 1..2 { LEAVE { $log ~= "l$_ " }; die "y"; CATCH { default { $log ~= "c " } } }
    is $log, 'c l1 c l2 ', 'a CATCH inside the body runs before the LEAVE';
}

{
    my $log = '';
    {
        for 1..2 { LEAVE { $log ~= "l$_ " }; die "x"; $log ~= "a$_ " }
        CATCH { default { $log ~= "c "; .resume } }
    }
    is $log, 'c a1 l1 c a2 l2 ', 'a resumed exception does not run LEAVE early';
}

{
    my $log = '';
    for 1..2 { LEAVE { $log ~= "l$_ " }; die "y"; $log ~= "z$_ "; CATCH { default { $log ~= "c "; .resume } } }
    is $log, 'c z1 l1 c z2 l2 ', 'resumed from a CATCH inside the body';
}

{
    my $log = '';
    sub dies { die "q" }
    try { for 1..2 { LEAVE { $log ~= "l$_ " }; dies() } }
    is $log, 'l1 ', 'an exception from a called sub runs LEAVE';
}

{
    my $log = '';
    sub loops { for 1..2 { LEAVE { $log ~= "l$_ " }; die "x" } }
    { loops(); CATCH { default { $log ~= "c " } } }
    is $log, 'c l1 ', 'handler in the caller runs before the LEAVE';
}

{
    my $log = '';
    my $i = 0;
    try { while $i++ < 3 { LEAVE { $log ~= "w$i " }; die "x" if $i == 2 } }
    is $log, 'w1 w2 ', 'while loop';
}

{
    my $log = '';
    try { loop (my $j = 0; $j < 3; $j++) { LEAVE { $log ~= "L$j " }; die if $j == 1 } }
    is $log, 'L0 L1 ', 'C-style loop';
}

{
    my $log = '';
    my $n = 0;
    try { repeat { LEAVE { $log ~= "r$n " }; $n++; die if $n == 2 } while $n < 5 }
    is $log, 'r1 r2 ', 'repeat loop';
}

{
    my $log = '';
    for 1..2 -> $a {
        for 1..2 -> $b { LEAVE { $log ~= "i$a$b " }; die "q" if $b == 2 }
        LEAVE { $log ~= "o$a " }
        CATCH { default { $log ~= "c " } }
    }
    is $log, 'i11 c i12 o1 i21 c i22 o2 ', 'nested loops unwind innermost first';
}

{
    try { for 1..2 { LEAVE { die "leave" }; die "body" } }
    is $!.message, 'leave', 'a LEAVE that dies replaces the exception';
}
