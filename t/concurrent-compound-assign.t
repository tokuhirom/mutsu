use v6;
use Test;

# Track C slice 3: scalar compound assignment (`$x OP= rhs`) on a `start`-captured
# lexical is an atomic read-modify-write on the shared cell, so concurrent threads
# don't lose updates. raku itself does NOT guarantee atomic `+=`; mutsu is
# deterministic here on purpose (matching the slice-1 `++` "more deterministic
# than raku" choice). See memory next-compound-assign-atomicity.

plan 8;

# --- `+=` accumulation across 100 threads x 100 iterations -> 10000 ---
for ^3 {
    my $c = 0;
    await (^100).map: { start { for ^100 { $c += 1 } } };
    is $c, 10000, 'concurrent += accumulates without lost updates';
}

# --- `-=` from a base, exact ---
{
    my $c = 10000;
    await (^100).map: { start { for ^100 { $c -= 1 } } };
    is $c, 0, 'concurrent -= is exact';
}

# --- `*=` doubling, serialized count -> 2 ** 10 ---
{
    my $c = 1;
    await (^10).map: { start { $c *= 2 } };
    is $c, 1024, 'concurrent *= doubling reaches 2 ** 10';
}

# --- single-threaded sanity: a mix of compound ops ---
{
    my $x = 10;
    $x += 5;   # 15
    $x -= 3;   # 12
    $x *= 2;   # 24
    is $x, 24, 'single-threaded numeric compound chain';
}

# --- string concat compound on an undefined scalar ---
{
    my $s;
    $s ~= "a";
    $s ~= "b";
    is $s, "ab", 'concat compound on initially-undefined scalar';
}

# --- compound assign inside a method mutating a captured-outer scalar,
#     with a variable rhs (the env-propagation case) ---
{
    my $acc = 0;
    my $step = 3;
    class CompoundPusher { method go() { $acc += $step } }
    my $p = CompoundPusher.new;
    $p.go(); $p.go();
    is $acc, 6, 'compound assign in method propagates captured-outer scalar';
}
