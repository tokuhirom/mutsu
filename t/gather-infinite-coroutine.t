use Test;

# Lazy access on an infinite gather coroutine must not force the whole
# (infinite) list. Covers .head(n), .head, .first(pred), indexing, and the
# bare-loop / while / C-style-loop coroutine resume paths.

plan 21;

# --- bare `loop { take ... }` --------------------------------------------
{
    my $g = gather { my $i = 0; loop { take $i++ } };
    is-deeply $g.head(5).List, (0, 1, 2, 3, 4), 'loop: .head(5)';
    is $g.head, 0, 'loop: .head (no arg)';
    is $g[3], 3, 'loop: index [3]';
    is $g.first(* > 7), 8, 'loop: .first(* > 7)';
}

# Side effects run only as far as the first match (no over-pull).
{
    my $c = 0;
    my $g = gather { my $i = 0; loop { $c++; take $i++ } };
    is $g.first(* >= 3), 3, 'loop: .first body returns match';
    is $c, 4, 'loop: .first runs body exactly to the match';
}

# Incremental indexing resumes the coroutine repeatedly.
{
    my $g = gather { my $i = 0; loop { take $i++ } };
    is $g[0], 0, 'loop: incremental [0]';
    is $g[1], 1, 'loop: incremental [1]';
    is $g[2], 2, 'loop: incremental [2]';
    is $g[5], 5, 'loop: incremental [5]';
}

# --- `while True { take ... }` -------------------------------------------
{
    my $g = gather { my $i = 0; while True { take $i++ } };
    is-deeply $g.head(4).List, (0, 1, 2, 3), 'while: .head(4)';
    is $g.first(* > 5), 6, 'while: .first(* > 5)';
    is $g[2], 2, 'while: index [2]';
}

# --- `until False { take ... }` ------------------------------------------
{
    my $g = gather { my $i = 0; until False { take $i++ } };
    is-deeply $g.head(3).List, (0, 1, 2), 'until: .head(3)';
}

# --- C-style `loop (init; ; step)` ---------------------------------------
{
    my $g = gather { loop (my $i = 0; ; $i++) { take $i } };
    is-deeply $g.head(4).List, (0, 1, 2, 3), 'c-style: .head(4)';
    is $g[5], 5, 'c-style: index [5]';
}

# --- `for 0..Inf` (already worked; guard against regression) -------------
{
    my $g = gather { for 0..Inf -> $i { take $i * $i } };
    is-deeply $g.head(4).List, (0, 1, 4, 9), 'for-Inf: .head(4)';
    is $g.first(* > 10), 16, 'for-Inf: .first(* > 10)';
}

# --- finite gather: .head / .first still correct -------------------------
{
    my $g = gather { take 1; take 2; take 3 };
    is-deeply $g.head(2).List, (1, 2), 'finite: .head(2)';
    is-deeply $g.head(5).List, (1, 2, 3), 'finite: .head past end';
    is $g.first(* > 9), Nil, 'finite: .first no match -> Nil';
}
