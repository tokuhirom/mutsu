use Test;

# Slice F (env<->locals coherence): an eager `.map`/`.grep` over a literal list
# (or Range / Seq) runs the block body through the interpreter's `run_reuse`
# loop rather than `call_compiled_closure_with_topic`, so a captured outer
# lexical mutated inside the block must still be written through to the caller's
# local slot without relying on the reverse `sync_locals_from_env` pull.
#
# `@a.map` over a concrete array variable already went through the VM native
# path (#3307); the gap was the literal-list / Range / Seq receiver, which falls
# back to the eager interpreter loop. Results below must hold identically with
# the reverse-sync pull enabled (default) and disabled (MUTSU_NO_REVERSE_SYNC=1).

plan 14;

# --- map: scalar accumulation over a literal list ---
{
    my $sum = 0;
    (1, 2, 3).map({ $sum += $_ });
    is $sum, 6, 'literal-list map: captured scalar accumulation';
}

# --- map: ++ on a captured scalar ---
{
    my $count = 0;
    (1, 2, 3, 4).map({ $count++ });
    is $count, 4, 'literal-list map: captured scalar ++';
}

# --- map: push onto a captured array ---
{
    my @collected;
    (10, 20, 30).map({ @collected.push($_ * 2) });
    is-deeply @collected, [20, 40, 60], 'literal-list map: captured array push';
}

# --- map: string concatenation onto a captured scalar ---
{
    my $s = '';
    ('x', 'y', 'z').map({ $s ~= $_ });
    is $s, 'xyz', 'literal-list map: captured string concat';
}

# --- map over a Range mutates a captured scalar ---
{
    my $acc = 0;
    (1 .. 4).map({ $acc += $_ });
    is $acc, 10, 'Range map: captured scalar accumulation';
}

# --- map over a Seq mutates a captured scalar ---
{
    my $tot = 0;
    (1, 2, 3).Seq.map({ $tot += $_ });
    is $tot, 6, 'Seq map: captured scalar accumulation';
}

# --- grep: scalar accumulation while filtering ---
{
    my $seen = 0;
    my @evens = (1, 2, 3, 4).grep({ $seen += $_; $_ %% 2 });
    is $seen, 10, 'literal-list grep: captured scalar accumulation';
    is-deeply @evens, [2, 4], 'literal-list grep: filter result is correct';
}

# --- grep: ++ counter ---
{
    my $calls = 0;
    (<a b c d>).grep({ $calls++; .chars });
    is $calls, 4, 'word-list grep: captured counter ++';
}

# --- grep over a Range mutates a captured scalar ---
{
    my $sum = 0;
    my @big = (1 .. 5).grep({ $sum += $_; $_ > 3 });
    is $sum, 15, 'Range grep: captured scalar accumulation';
    is-deeply @big, [4, 5], 'Range grep: filter result is correct';
}

# --- result of the map/grep is still correct alongside the side effect ---
{
    my $sum = 0;
    my @doubled = (1, 2, 3).map({ $sum += $_; $_ * 2 });
    is $sum, 6, 'map: side effect and return value coexist (side effect)';
    is-deeply @doubled.List, (2, 4, 6), 'map: side effect and return value coexist (result)';
}

# --- the array-var native path stays coherent too (regression guard) ---
{
    my $sum = 0;
    my @a = 1, 2, 3;
    @a.map({ $sum += $_ });
    is $sum, 6, 'array-var map: captured scalar accumulation (native path)';
}

# vim: expandtab shiftwidth=4
