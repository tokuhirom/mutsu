use Test;

# Exercises the VM-native `.sort` fast path (src/vm/vm_native_sort.rs): the
# no-comparator sort and simple `{ $^a <=> $^b }` / `cmp` comparator blocks now
# run in the VM instead of falling back to the interpreter. Results (including
# sort stability) must remain identical.

plan 16;

# --- no comparator ---
is-deeply (3, 1, 2).sort.List, (1, 2, 3), "Int sort, no comparator";
is-deeply <c a b>.sort.List, <a b c>.List, "Str sort, no comparator";
is-deeply (3, 1.5, 2, 0.5).sort.List, (0.5, 1.5, 2, 3), "mixed Int/Rat sort";
is-deeply [5, 3, 8, 1].sort.List, (1, 3, 5, 8), "Array (square-bracket) sort";
is-deeply ().sort.List, ().List, "empty sort";
is-deeply (42,).sort.List, (42,), "single-element sort";

# --- simple numeric comparator block ---
is-deeply (3, 1, 4, 1, 5).sort({ $^a <=> $^b }).List, (1, 1, 3, 4, 5), "ascending numeric comparator";
is-deeply (3, 1, 4, 1, 5).sort({ $^b <=> $^a }).List, (5, 4, 3, 1, 1), "descending numeric comparator";

# --- simple string comparator block ---
is-deeply <banana apple cherry>.sort({ $^a cmp $^b }).List, <apple banana cherry>.List, "ascending cmp";
is-deeply <banana apple cherry>.sort({ $^b cmp $^a }).List, <cherry banana apple>.List, "descending cmp";

# --- stability: equal keys preserve input order ---
{
    my @p = [1, 'a'], [2, 'b'], [1, 'c'], [2, 'd'], [1, 'e'];
    my @sorted = @p.sort({ $^a[0] <=> $^b[0] });
    is-deeply @sorted.map(*[1]).List, <a c e b d>.List, "sort is stable for equal keys";
}

# --- negative numbers ---
is-deeply (-3, 5, -1, 0, 2).sort.List, (-3, -1, 0, 2, 5), "sort with negatives";
is-deeply (-3, 5, -1, 0, 2).sort({ $^b <=> $^a }).List, (5, 2, 0, -1, -3), "descending with negatives";

# --- returns a Seq, leaves the source unchanged ---
{
    my @orig = 3, 1, 2;
    my @s = @orig.sort;
    is-deeply @s.List, (1, 2, 3), "sort result is sorted";
    is-deeply @orig.List, (3, 1, 2), "sort leaves the source array unchanged";
}

# --- chaining onto sort ---
is-deeply (3, 1, 2).sort.reverse.List, (3, 2, 1), "sort then reverse";
