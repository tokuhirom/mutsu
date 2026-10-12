use Test;

# A block-declared sub's body calling a *sibling* sub of the same block by
# name, from a closure that outlived the block (#13037, ADR-12529 phase 2).
# Each closure keeps the routines of the activation it was created in.

plan 8;

{
    my @c;
    for 1, 2 -> $v {
        sub a1 { "a$v" ~ b1() }
        sub b1 { "b$v" }
        @c.push(-> { a1() });
    }
    is @c.map({ $_() }).join(' '), 'a1b1 a2b2', 'loop body: sibling called after the iteration';
}

{
    my @c;
    for 1, 2 -> $v {
        sub a2 { b2() ~ '!' }
        sub b2 { "b$v" }
        @c.push(-> { a2() });
    }
    is @c.map({ $_() }).join(' '), 'b1! b2!', 'caller with no free variable of its own';
}

{
    my @c;
    for 1, 2 -> $v {
        sub a3 { b3() }
        sub b3 { c3() }
        sub c3 { "c$v" }
        @c.push(-> { a3() });
    }
    is @c.map({ $_() }).join(' '), 'c1 c2', 'a chain of sibling calls';
}

{
    my $f;
    {
        my $x = 'inner';
        sub a4 { b4() }
        sub b4 { $x }
        $f = -> { a4() };
    }
    is $f(), 'inner', 'bare block: sibling called after the block exited';
}

{
    my @c;
    for 1, 2 -> $v {
        sub ev($n) { $n == 0 ?? "e$v" !! od($n - 1) }
        sub od($n) { $n == 0 ?? "o$v" !! ev($n - 1) }
        @c.push(-> { ev(0) ~ ev(3) });
    }
    is @c.map({ $_() }).join(' '), 'e1o1 e2o2', 'mutually recursive siblings';
}

{
    my @c;
    for 1, 2 -> $v {
        sub plain-a { 'pa' ~ plain-b() }
        sub plain-b { 'pb' }
        @c.push(-> { plain-a() });
    }
    is @c.map({ $_() }).join(' '), 'papb papb', 'siblings with no free variables';
}

{
    my @c;
    for 1, 2 -> $v {
        sub na { nb(:k<x>) }
        sub nb(:$k) { "$k$v" }
        sub ra { my $z = 1; rb($z); $z }
        sub rb($p is rw) { $p = $v * 10 }
        @c.push(-> { na() ~ '/' ~ ra() });
    }
    is @c.map({ $_() }).join(' '), 'x1/10 x2/20', 'named and is-rw arguments to a sibling';
}

{
    my @c;
    for 1, 2 -> $v {
        sub ma { (1,).map({ mb() }).head }
        sub mb { "m$v" }
        @c.push(-> { ma() });
    }
    is @c.map({ $_() }).join(' '), 'm1 m2', 'sibling called from a closure in the body';
}
