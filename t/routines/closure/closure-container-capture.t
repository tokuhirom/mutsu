use Test;

# Lever C (Phase 1): a closure captures the *container* of a closed-over lexical
# scalar, not a frozen value. Mutation after capture is visible, and sibling
# closures over the same lexical share one cell (loop-body AND non-loop, the
# latter via the compiler's >=2-sibling-closure signal), while loop-body `my`
# stays per-iteration fresh.

plan 24;

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
# Phase 1: a local captured by >=2 sibling closures is boxed into a shared cell
# (compiler `multi_captured_mutated_locals` signal), so reads/writes round-trip
# through the same ContainerRef even after the declaring frame returns. ---
{
    sub make-pair {
        my $v = 1;
        my $g = { $v };
        my $s = -> $n { $v = $n };
        return $g, $s;
    }
    my ($g, $s) = make-pair();
    $s(42);
    is $g(), 42, 'sibling closures share the same lexical container';
}

# --- counter factory: mutating + reading sibling share state (non-loop) ---
{
    sub mk { my $n = 0; return { $n++ }, { $n } }
    my ($inc, $get) = mk();
    $inc(); $inc();
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

# --- escape analysis (step 1): a SINGLE closure that ESCAPES the frame (assigned
# to an outer variable) shares the captured container, even though only one
# closure captures it (the old >=2-sibling proxy missed this). ---
{
    my $f;
    { my $a = 3; $f = sub { $a++ } }
    is $f(), 3, 'single escaping closure: first call reads captured value';
    is $f(), 4, 'single escaping closure: state persists through the shared cell';
}

# --- immediately-invoked closures (call arguments) are NOT boxed: the captured
# topic/local flows through the non-escaping path. This is the #2746 perf guard;
# here we just assert correctness is unaffected. ---
{
    my $sum = 0;
    (1, 2, 3).map({ $sum += $_ });
    is $sum, 6, 'immediately-invoked map closure mutates outer var correctly';
}

# --- a returned closure factory keeps independent per-instance state ---
{
    sub counter { my $n = 0; return sub { $n++ } }
    my $a = counter();
    my $b = counter();
    $a(); $a();
    is $b(), 0, 'returned closure factory: instances have independent state';
}

# --- bareword `f()` call to a `&f` closure that captured a lexical from a
# now-EXITED block. The closure's captured container must be observed on EVERY
# call (the bareword path reads the closure's captured env, which previously lost
# the captured value on the second call -> returned Nil/0). ---
{
    my &f;
    { my $a = 3; &f = sub { $a++ } }
    is f(), 3, 'bareword call to exited-block closure: first call';
    is f(), 4, 'bareword call to exited-block closure: state persists';
}
{
    my &g;
    { my $a = 3; &g = sub { $a } }
    is g(), 3, 'bareword call, read-only captured lexical: first call';
    is g(), 3, 'bareword call, read-only captured lexical: second call (not Nil)';
}
# bareword `f()` and ampersand `&f()` observe the same shared container.
{
    my &h;
    { my $a = 10; &h = sub { $a++ } }
    is "{h()}{&h()}{h()}", '101112', 'bareword and &-call share the captured container';
}
