use Test;
plan 12;

# A loop iteration that exits early runs its NEXT (for a `next` aimed at this
# loop), UNDO and LEAVE phasers however the control signal got there: written
# in the body, raised inside a `try`, or raised by a closure or sub the body
# called. Only the first used to work, because the phasers were wired to each
# `next` statement at compile time (#10566). Outputs verified against raku.

{
    my $out = '';
    for 1..3 { NEXT { $out ~= "n$_ " }; my &c = { next if $_ == 2 }; c(); $out ~= "b$_ " }
    is $out, 'b1 n1 n2 b3 n3 ', 'next from a closure the body calls runs NEXT';
}

{
    my $out = '';
    for 1..3 {
        NEXT { $out ~= "n$_ " }; LEAVE { $out ~= "l$_ " }; UNDO { $out ~= "u$_ " };
        my &c = { next if $_ == 2 }; c(); $out ~= "b$_ "; 42
    }
    is $out, 'b1 l1 n1 n2 u2 l2 b3 l3 n3 ', 'a dynamic next runs NEXT, then UNDO, then LEAVE';
}

{
    my $out = '';
    for 1..3 { NEXT { $out ~= "n$_ " }; LEAVE { $out ~= "l$_ " }; my &c = { last if $_ == 2 }; c(); $out ~= "b$_ " }
    is $out, 'b1 l1 n1 l2 ', 'last from a closure runs LEAVE but not NEXT';
}

{
    my $out = '';
    sub f { next }
    for 1..2 { NEXT { $out ~= "n$_ " }; f(); $out ~= 'b' }
    is $out, 'n1 n2 ', 'next from a called sub runs NEXT';
}

{
    my $out = '';
    for 1..2 { NEXT { $out ~= "n$_ " }; LEAVE { $out ~= "l$_ " }; try { next }; $out ~= 'b' }
    is $out, 'n1 l1 n2 l2 ', 'next raised inside a try runs NEXT and LEAVE';
}

{
    my $out = '';
    LBL: for 1..2 -> $i {
        for 1..2 { NEXT { $out ~= "n$i$_ " }; LEAVE { $out ~= "l$i$_ " }; next LBL if $_ == 1; $out ~= 'b ' }
    }
    is $out, 'l11 l21 ', 'a next aimed at an outer loop runs the inner LEAVE but not its NEXT';
}

{
    my $out = '';
    my $i = 0;
    while $i++ < 3 { NEXT { $out ~= "n$i " }; my &c = { next if $i == 2 }; c(); $out ~= "b$i " }
    is $out, 'b1 n1 n2 b3 n3 ', 'while: next from a closure runs NEXT';
}

{
    my $out = '';
    loop (my $j = 0; $j < 3; $j++) { NEXT { $out ~= "n$j " }; my &c = { next if $j == 1 }; c(); $out ~= "b$j " }
    is $out, 'b0 n0 n1 b2 n2 ', 'loop: next from a closure runs NEXT';
}

{
    my $out = '';
    for 1..3 { NEXT { $out ~= "n$_ " }; (1, 2).map({ next }); $out ~= "b$_ " }
    is $out, 'b1 n1 b2 n2 b3 n3 ', 'a next that ends a .map iteration does not leave the loop';
}

{
    my $out = '';
    my @r = do for 1..3 { NEXT { $out ~= 'n ' }; my &c = { next if $_ == 2 }; c(); $_ * 10 };
    is-deeply [@r, $out], [[10, 30], 'n n n '], 'do for: a dynamic next drops the value and runs NEXT';
}

{
    my $out = '';
    sub g { for 1..2 { LEAVE { $out ~= "l$_ " }; UNDO { $out ~= "u$_ " }; return 5 } }
    is-deeply [g(), $out], [5, 'u1 l1 '], 'return out of a loop body runs UNDO and LEAVE';
}

{
    my $out = '';
    my $r = 0;
    for 1..2 { LEAVE { $out ~= "l$_ " }; NEXT { $out ~= "n$_ " }; if $r++ == 0 { redo }; $out ~= 'b' }
    is $out, 'l1 bl1 n1 bl2 n2 ', 'redo runs LEAVE but not NEXT';
}
