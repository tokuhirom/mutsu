use Test;

plan 9;

# `is rw` parameter re-bound to an outer scalar (`$a := $arg`) must create a
# *live* shared cell, so writes through any alias (caller var, the re-bound
# outer var) are observed by all. Mirrors roast/S03-binding/scalars.t 24/27.
{
    my $a;
    my $b = sub ($arg is rw) { $a := $arg };
    my $val = 42;
    $b($val);
    is $a, 42, "bound rw sub param was bound correctly (1)";
    $val++;
    is $a, 43, "caller-side mutation propagates through the shared cell";
    lives-ok { $a = 23 }, "remains rw";
    is $a, 23, "write through alias updates itself";
    is $val, 23, "write through alias propagates back to caller var";
}

# Readonly param must SNAPSHOT (no aliasing), even when re-bound.
{
    my $a2;
    my $b2 = sub ($arg) { $a2 := $arg };
    my $v2 = 42;
    $b2($v2);
    $v2++;
    is $a2, 42, "readonly param re-bind is a snapshot, not a live alias";
}

# Direct rw mutation without re-bind keeps working (value-writeback path).
{
    sub bump($x is rw) { $x++ }
    my $n = 5;
    bump($n);
    is $n, 6, "direct rw param mutation writes back to caller";
}

# Named sub (not a closure) rw param re-bind shares the cell too.
{
    my $outer;
    sub rebind($arg is rw) { $outer := $arg }
    my $v = 10;
    rebind($v);
    $v++;
    is $outer, 11, "named-sub rw re-bind shares a live cell";
    $outer = 99;
    is $v, 99, "write through named-sub alias reaches caller var";
}
