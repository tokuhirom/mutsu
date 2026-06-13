use Test;

# A plain (non-`is copy`) named for-loop variable binds each element by alias,
# just like a plain `@`/`%`/`$` parameter: mutating it through the loop var
# propagates to the source array. The implicit topic `$_` already aliased;
# named params (`-> @row` / `-> $row`) did not.

plan 18;

# --- named @-param: element-assign, push, nested ---------------------------
{
    my @m = [1, 2], [3, 4];
    for @m -> @row { @row[0] *= 10 }
    is @m.gist, '[[10 2] [30 4]]', '-> @row: element-assign propagates';
}
{
    my @m = [1, 2], [3, 4];
    for @m -> @row { @row.push(9) }
    is @m.gist, '[[1 2 9] [3 4 9]]', '-> @row: .push propagates';
}
{
    my @m = [1, 2], [3, 4];
    for @m -> @row { for @row { $_++ } }
    is @m.gist, '[[2 3] [4 5]]', '-> @row: nested topic mutation propagates';
}

# --- named $-param binding a container -------------------------------------
{
    my @m = [1, 2], [3, 4];
    for @m -> $row { $row.push(9) }
    is @m.gist, '[[1 2 9] [3 4 9]]', '-> $row: .push on bound container propagates';
}
{
    my @m = [1, 2], [3, 4];
    for @m -> $row { $row[0] = 99 }
    is @m.gist, '[[99 2] [99 4]]', '-> $row: element-assign propagates';
}

# --- named %-param ---------------------------------------------------------
{
    my @hs = { a => 1 }, { a => 2 };
    for @hs -> %r { %r<a> = 99 }
    is @hs.gist, '[{a => 99} {a => 99}]', '-> %r: hash element-assign propagates';
}

# --- `is copy` does NOT propagate ------------------------------------------
{
    my @m = [1, 2], [3, 4];
    for @m -> @row is copy { @row[0] *= 10 }
    is @m.gist, '[[1 2] [3 4]]', '-> @row is copy: no propagation';
}

# --- `<->` rw still works (separate machinery) -----------------------------
{
    my @m = [1, 2], [3, 4];
    for @m <-> @row { @row[0] *= 10 }
    is @m.gist, '[[10 2] [30 4]]', '<-> @row: rw still propagates';
}

# --- topic `$_` (regression: must still propagate AND stay fast) ----------
{
    my @a = 1, 2, 3;
    for @a { $_++ }
    is @a.gist, '[2 3 4]', 'topic $_++ propagates';
}
{
    my @a = [1], [2];
    for @a { $_.push(9) }
    is @a.gist, '[[1 9] [2 9]]', 'topic container .push propagates';
}
{
    my @a = <a b c>;
    for @a { $_ ~= "!" }
    is @a.gist, '[a! b! c!]', 'topic string concat propagates';
}

# --- read-only loops leave the source unchanged ----------------------------
{
    my @a = 1, 2, 3;
    my $sum = 0;
    for @a { $sum += $_ }
    is @a.gist, '[1 2 3]', 'read-only topic loop leaves source unchanged';
    is $sum, 6, 'read-only topic loop still reads correctly';
}
{
    my @a = 1, 2, 3;
    my $sum = 0;
    for @a -> $x { $sum += $x }
    is @a.gist, '[1 2 3]', 'read-only named loop leaves source unchanged';
    is $sum, 6, 'read-only named loop reads correctly';
}

# --- `<->` rw writeback also propagates and stays O(n) --------------------
{
    my @a = 1, 2, 3;
    for @a <-> $x { $x *= 10 }
    is @a.gist, '[10 20 30]', '<-> rw mutation propagates';
}

# --- O(n) performance: a read-only loop over a large array must be fast.
# (Pre-fix the topic AND `<->` rw writebacks rebuilt the whole array every
# iteration, making these O(n^2): ~46s for 40k. They should now finish in well
# under a second.)
{
    my @big = (1 .. 20000).Array;
    my $s = 0;
    for @big { $s += $_ }
    is $s, 200010000, 'large read-only topic loop computes correct sum (O(n))';
}
{
    my @big = (1 .. 20000).Array;
    my $s = 0;
    for @big <-> $x { $s += $x }
    is $s, 200010000, 'large read-only <-> rw loop computes correct sum (O(n))';
}
