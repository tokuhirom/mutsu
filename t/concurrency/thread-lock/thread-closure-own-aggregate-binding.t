use Test;

# A closure run on another thread -- a `start` block, or a `.then` callback on
# a promise that is still pending -- reads the `@`/`%` binding it captured, not
# the one a LATER call of the same routine bound to the same name (#9723,
# ADR-0129).

plan 7;

{
    sub h($n) { my @p = ^$n; start { sleep 0.2; @p.elems } }
    is-deeply (await h(1), h(2), h(3)), (1, 2, 3),
        'start block reads its own call\'s array';
}

{
    sub h($n) {
        my @p = ^$n;
        my $k = Promise.new;
        my $r = $k.then({ @p.elems });
        ($k, $r)
    }
    my @pairs = h(1), h(2), h(3);
    .[0].keep(1) for @pairs;
    is-deeply (await @pairs.map(*.[1])), (1, 2, 3),
        'deferred .then reads its own call\'s array';
}

{
    sub h($n) {
        my %p = (^$n).map({ $_ => 1 });
        my $k = Promise.new;
        my $r = $k.then({ %p.elems });
        ($k, $r)
    }
    my @pairs = h(1), h(2), h(3);
    .[0].keep(1) for @pairs;
    is-deeply (await @pairs.map(*.[1])), (1, 2, 3),
        'deferred .then reads its own call\'s hash';
}

{
    # Writes after the re-declaration land in the captured binding.
    sub h($n) { my @p = ^$n; start { sleep 0.2; @p.push(9); @p.elems } }
    is-deeply (await h(1), h(2)), (2, 3),
        'start block pushes to its own call\'s array';
}

{
    # Two children of one binding keep sharing it after the routine is called
    # again and re-declares the name.
    sub h($n) {
        my @p = ^$n;
        my $go = Promise.new;
        my $a = $go.then({ @p.push(10); 1 });
        my $b = $go.then({ await $a; @p.elems });
        ($go, $b)
    }
    my ($go1, $b1) = h(1);
    my ($go2, $b2) = h(5);
    $go1.keep(1);
    $go2.keep(1);
    is-deeply (await $b1, $b2), (2, 6),
        'siblings of one binding still see each other\'s writes';
}

{
    # The caller's own same-named declaration, still being initialised when
    # the callees spawn, must not overwrite what the callees' callbacks read
    # (the Tinky `validate-apply` hang).
    sub helper($n) {
        my sub run() {
            my @promises = (^$n).map({ start { sleep 0.1; 1 } });
            Promise.allof(@promises).then({ @promises.elems });
        }
        run();
    }
    sub outer() {
        my @promises = helper(0), helper(1), helper(2);
        Promise.allof(@promises).then({ @promises.map(*.result) });
    }
    my $p = outer();
    await Promise.anyof($p, Promise.in(20));
    is-deeply ($p.status ~~ Kept ?? $p.result !! 'hung'), (0, 1, 2),
        'caller\'s in-flight declaration does not clobber a callee\'s binding';
}

{
    # A genuinely shared outer array is still shared.
    my @acc;
    await (^4).map: -> $i { start { @acc.push($i) } };
    is-deeply @acc.sort.List, (0, 1, 2, 3), 'outer array still shared';
}
