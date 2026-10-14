use Test;

plan 6;

# A `my sub (...)` / `my method (...)` closure written directly as an
# argument shares the cells of the variables it writes, so a wrapper
# re-entered through `callsame` sees the outer invocation's updates (#12803).

{
    my $n = 0;
    my @seen;
    sub f($k) { $k > 0 ?? f($k - 1) !! 0 }
    &f.wrap(my sub (|c) { $n++; @seen.push($n); callsame });
    f(3);
    is $n, 4, 'a re-entered `my sub` wrapper counts every entry';
    is @seen.join(' '), '1 2 3 4', 'each nested entry sees the previous write';
}

{
    my $n = 0;
    sub g($k) { $k > 0 ?? g($k - 1) !! 0 }
    &g.wrap(sub (|c) { $n++; callsame });
    g(3);
    is $n, 4, 'an anonymous `sub` wrapper (control)';
}

{
    grammar G { token TOP { '.' <TOP>? | \d } }
    my $n = 0;
    G.^find_method('TOP').wrap(my method (|c) { $n++; callsame });
    ok G.parse('..4'), 'the wrapped self-recursive token still parses';
    is $n, 3, 'a `my method` wrapper of a recursive token counts every entry';
}

{
    my $q = 0;
    sub run(&c) { c() }
    run(my sub () { $q++; run(my sub () { $q++ }) });
    is $q, 2, 'a `my sub` argument writes through to the outer variable';
}
