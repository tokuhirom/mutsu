use Test;

# Lever C Slice 2: a closure captures the *container* of a closed-over lexical
# scalar, not a frozen value. Mutation after capture is visible, and sibling
# closures over the same lexical share one cell, while loop-body `my` stays
# per-iteration fresh.

plan 15;

# --- intra-iteration mutation after capture (was: frozen value) ---
{
    my @c;
    for 1 { my $x = 1; my $cl = { $x }; $x = 2; @c.push($cl) }
    is @c[0](), 2, 'mutation after capture is visible to the closure';
}

# --- repeated mutation of a loop-body local is observed through the cell ---
{
    my @r;
    for 1 { my $x = 1; my $cl = { $x }; $x = 2; @r.push($cl()); $x = 5; @r.push($cl()) }
    is @r.join(','), '2,5', 'closure observes successive mutations of the shared cell';
}

# --- sibling closures over a *non-loop* (sub-body) local share one container.
# Deferred: Slice 2 boxes only loop-body locals; general "capture the container"
# for non-loop siblings needs a broader ContainerRef-handling audit (Slice 3+). ---
{
    sub make-pair {
        my $v = 1;
        my $g = { $v };
        my $s = -> $n { $v = $n };
        return $g, $s;
    }
    my ($g, $s) = make-pair();
    $s(42);
    todo 'non-loop sibling-closure container sharing (deferred to a later slice)';
    is $g(), 42, 'sibling closures share the same lexical container';
}

# --- counter factory: mutating + reading sibling share state (non-loop, deferred) ---
{
    sub mk { my $n = 0; return { $n++ }, { $n } }
    my ($inc, $get) = mk();
    $inc(); $inc();
    todo 'non-loop sibling-closure container sharing (deferred to a later slice)';
    is $get(), 2, 'mutating and reading sibling closures share state';
}

# --- cross-iteration per-iteration binding preserved (Slice 1) ---
{
    my @c;
    for 1..3 { my $x = $_; @c.push({ $x }) }
    is @c[0](), 1, 'for-loop body local: iter 1 frozen';
    is @c[1](), 2, 'for-loop body local: iter 2 frozen';
    is @c[2](), 3, 'for-loop body local: iter 3 frozen';
}

# --- per-iteration freshness AND intra-iteration sibling sharing together ---
{
    my @c;
    for 1..2 {
        my $v = 0;
        my $g = { $v };
        my $s = -> $n { $v = $n };
        @c.push(($g, $s));
    }
    @c[0][1](10);
    @c[1][1](20);
    is @c[0][0](), 10, 'iter 1 cell mutated independently';
    is @c[1][0](), 20, 'iter 2 cell mutated independently';
}

# --- non-loop late binding to a shared lexical still works ---
{
    my $g = 1;
    my $c = { $g };
    $g = 9;
    is $c(), 9, 'non-loop closure sees later mutation of shared lexical';
}

# --- mutating loop closure keeps its own accumulated state ---
{
    my @c;
    for 1..2 { my $x = $_; @c.push({ $x++; $x }) }
    is @c[0](), 2, 'mutating loop closure iter 1 first call';
    is @c[1](), 3, 'mutating loop closure iter 2 first call';
    is @c[0](), 3, 'mutating loop closure iter 1 accumulates';
}

# --- while-loop body local per-iteration capture ---
{
    my @c;
    my $i = 0;
    while $i < 3 { my $x = $i; @c.push({ $x }); $i++ }
    is "{@c[0]()}{@c[1]()}{@c[2]()}", '012', 'while-loop body local per-iteration capture';
}

# --- array capture is reference-shared, never boxed ---
{
    my @a = 1, 2, 3;
    my $c = { @a.elems };
    @a.push(4);
    is $c(), 4, 'array free variable stays reference-shared';
}
