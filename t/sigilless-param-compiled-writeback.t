use Test;

# ADR-0019 C6e-2: sigilless scalar params (`\x`) run through the compiled
# routine entry (the OTF/plan-bytecode gate no longer excludes them). The
# caller-alias writeback must survive every calling context: same-frame,
# across an EVAL boundary (the compiled return path flushes the
# `__mutsu_sigilless_alias::` chain before the caller-env merge), and when
# the callee dies after the write (write-through, not return-merge-only).

plan 10;

sub set2(\x, \y) { x = 11; y = 22 }
sub read-then-set(\x) { my $t = x; x = $t + 1 }
sub swap(\x, \y) { my $z = y; y = x; x = $z }
sub poke(\x) { x = 99; die "boom" }

# Same-frame calls
{
    my $a = 1; my $b = 2;
    set2($a, $b);
    is "$a|$b", '11|22', 'direct: assignments through two sigilless aliases';
}
{
    my $c = 5;
    read-then-set($c);
    is $c, 6, 'direct: read-then-assign through a sigilless alias';
}
{
    my $d = 5; my $e = 3;
    swap($d, $e);
    is "$d|$e", '3|5', 'direct: swap through sigilless aliases';
}
{
    my $f = 5;
    try { poke($f) }
    is $f, 99, 'direct: write-through survives a die after the assignment';
}

# EVAL-boundary calls: the callee's alias writeback must reach the EVAL
# frame and propagate to the enclosing scope.
{
    my $a = 1; my $b = 2;
    EVAL 'set2($a, $b)';
    is "$a|$b", '11|22', 'EVAL: assignments through two sigilless aliases';
}
{
    my $c = 5;
    EVAL 'read-then-set($c)';
    is $c, 6, 'EVAL: read-then-assign through a sigilless alias';
}
{
    my $d = 5; my $e = 3;
    EVAL 'swap($d, $e)';
    is "$d|$e", '3|5', 'EVAL: swap through sigilless aliases';
}
{
    my $f = 5;
    try { EVAL 'poke($f)' }
    is $f, 99, 'EVAL: write-through survives a die after the assignment';
}

# Repeated calls keep aliasing fresh arguments (no stale alias leak).
{
    my $x = 1; my $y = 10;
    read-then-set($x);
    read-then-set($y);
    is "$x|$y", '2|11', 'sequential calls alias their own arguments';
}

# A sigilless param still binds non-lvalue arguments read-only-safely.
{
    sub ident(\v) { v }
    is ident(42), 42, 'literal argument binds fine through a sigilless param';
}
