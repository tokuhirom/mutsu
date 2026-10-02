use Test;

# `OUTER::` names one lexical scope, so a write through it reaches that
# scope's binding even when a scope in between shadows the name -- in the
# same frame (an inner block's `my $x`) or across a routine boundary (the
# routine's own `my $x`). Such writes used to be stored under a literal
# `OUTER::x` key and lost, and a read past a routine's own `my $x` returned
# the routine's binding (#10827).

plan 29;

# --- across a routine boundary ---

{
    my $x = 1;
    sub r1 { my $x = 3; my $y; $OUTER::x := $y; $y = 5; $x }
    is r1(), 3, 'sub: the shadow is untouched by the rebind';
    is $x, 5, 'sub: $OUTER::x := $y past the sub\'s own $x aliases the outer $x';
}

{
    my $x = 1;
    sub r2 { my $x = 3; $OUTER::x = 8; $x }
    is r2(), 3, 'sub: the shadow is untouched by the assignment';
    is $x, 8, 'sub: $OUTER::x = v past the sub\'s own $x';
}

{
    my $x = 1;
    sub r3 { my $x = 2; $OUTER::x }
    is r3(), 1, 'sub: a read past the sub\'s own $x sees the outer $x';
    $x = 5;
    is r3(), 5, 'sub: ... and tracks a later outer assignment';
}

{
    my $n = 0;
    sub r4 { my $n = 100; $OUTER::n++ }
    r4() for ^3;
    is $n, 3, 'sub: $OUTER::n++ past the sub\'s own $n, repeated calls';
}

{
    my $x = 1;
    my $c = sub { my $x = 2; $OUTER::x = 4; $x };
    is $c(), 2, 'anonymous sub: the shadow is untouched';
    is $x, 4, 'anonymous sub: the write reaches the outer $x';
}

{
    my $x = 1;
    sub r5 { my $x = 2; my &c = { $OUTER::OUTER::x = 9 }; c(); $x }
    is r5(), 2, 'closure in a sub: $OUTER::OUTER::x skips the sub\'s $x';
    is $x, 9, 'closure in a sub: the write reaches two frames out';
}

{
    my $x = 1;
    my $y = 10;
    sub r6 { my $x = 2; $OUTER::x := $y }
    r6();
    $y = 11;
    is $x, 11, 'sub rebind: the outer $x follows its new source';
    $x = 12;
    is $y, 12, 'sub rebind: ... and writes through to it';
}

{
    my $x = 1;
    my $snap = 0;
    sub r7 { my $x = 2; my &c = { $x }; $snap = $OUTER::x; $x }
    is r7(), 2, 'sub with an inner closure over its own $x';
    is $snap, 1, 'sub with an inner closure: $OUTER::x still reads the outer $x';
}

# --- within one frame ---

{
    my $x = 1;
    { my $x = 2; { my $y; $OUTER::OUTER::x := $y; $y = 5 }; is $x, 2, 'block: the shadow is untouched' }
    is $x, 5, 'block: $OUTER::OUTER::x := $y past an inner $x';
}

{
    my $x = 1;
    { my $x = 2; $OUTER::x = 7; is $x, 2, 'block: assignment leaves the shadow alone' }
    is $x, 7, 'block: $OUTER::x = v past an inner $x';
}

{
    my $x = 1;
    { my $x = 2; $OUTER::x++; $OUTER::x += 10; is $x, 2, 'block: ++ and += leave the shadow alone' }
    is $x, 12, 'block: $OUTER::x++ and $OUTER::x += v past an inner $x';
}

{
    my $x = "a";
    { my $x = 2; $OUTER::x ~= "b" }
    is $x, "ab", 'block: $OUTER::x ~= v';
}

{
    my $x = 1;
    { my $x = 2; ($OUTER::x = 8) }
    is $x, 8, 'block: expression-position assignment';
}

{
    my $x = 0;
    for 1..3 { my $x = 10; $OUTER::x += $_ }
    is $x, 6, 'loop body: the write survives the loop exit';
}

{
    my $x = 1;
    sub reader { $x }
    { my $x = 2; $OUTER::x = 7 }
    is reader(), 7, 'block: a named sub over the outer $x sees the write';
}

{
    my $x = 1;
    my &g = { $x };
    { my $x = 2; $OUTER::x = 7 }
    is g(), 7, 'block: a closure created earlier sees the write';
}

{
    my $x = 1;
    { my $x = 2; $OUTER::x = 7 }
    is EVAL('$x'), 7, 'block: EVAL of the plain name sees the write';
}

{
    my $x = 1;
    { my $x = 2; my $y; $OUTER::x := $y; }
    { my $z = 2; $x++ }
    is $x, 1, 'block: after a rebind to an undefined source, ++ steps the new binding';
}

# --- a binding cell stepped by name ---

{
    my $a = 1;
    my &c = { $a++ };
    my $b = 5;
    $a := $b;
    c();
    is "$a $b", '6 6', '++ in a closure steps the container of a rebound capture';
}
