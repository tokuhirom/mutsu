use Test;

# `Supply.on-demand(&producer)` completes only when the producer calls `done`,
# unlike a `supply { }` block. A producer that returns without it and emits
# later -- from a timer tap, a `start` block -- must still reach the tap, and a
# `.grep`/`.map` over such a supply must follow it live rather than snapshot
# what the producer emitted synchronously (#9493, Chronic `t/040-at.t`).

plan 10;

{
    my @got;
    my $done = Promise.new;
    my $v = $done.vow;
    my $s = Supply.on-demand(-> $p {
        $p.emit(1);
        start { sleep 0.1; $p.emit(2); $p.done };
    });
    $s.tap({ @got.push($_) }, done => { $v.keep(True) });
    await Promise.anyof($done, Promise.in(10));
    is-deeply @got, [1, 2], 'a value the producer emits after returning reaches the tap';
    ok $done, 'done fires when the producer calls it, not when it returns';
}

{
    my $got = Promise.new;
    my $v = $got.vow;
    my $s = Supply.on-demand(-> $p {
        Supply.interval(0.05).tap({ $p.emit($_) });
    });
    $s.tap({ $v.keep($_) if $_ >= 2 && $got.status ~~ Planned });
    await Promise.anyof($got, Promise.in(10));
    is $got.status, Kept, 'a producer fed by Supply.interval keeps emitting';
}

{
    my $got = Promise.new;
    my $v = $got.vow;
    my $s = Supply.on-demand(-> $p {
        Supply.interval(0.05).tap({ $p.emit($_) });
    });
    $s.grep(* == 3).tap({ $v.keep($_) if $got.status ~~ Planned });
    await Promise.anyof($got, Promise.in(10));
    is $got.status, Kept, '.grep over a live on-demand supply sees later emits';
    is $got.result, 3, '... and filters them';
}

{
    my @got;
    my $done = Promise.new;
    my $v = $done.vow;
    my $s = Supply.on-demand(-> $p {
        start { sleep 0.1; $p.emit($_) for 1..3; $p.done };
    });
    $s.map(* * 10).tap({ @got.push($_) }, done => { $v.keep(True) });
    await Promise.anyof($done, Promise.in(10));
    is-deeply @got, [10, 20, 30], '.map over a live on-demand supply sees later emits and done';
}

{
    # The producer runs once per tap of the derived supply, not at .grep time.
    my $runs = 0;
    my $s = Supply.on-demand(-> $p { $runs++; $p.emit($_) for 1..4; $p.done });
    my $g = $s.grep(* %% 2);
    is $runs, 0, '.grep does not run the producer';
    my @a; my @b;
    $g.tap({ @a.push($_) });
    $g.tap({ @b.push($_) });
    is $runs, 2, 'each tap of the derived supply runs the producer';
    is-deeply (@a, @b), ([2, 4], [2, 4]), 'each tap gets the filtered values';
}

{
    my @got;
    my $m = supply { emit 1; emit 2 }.map(* + 1);
    $m.tap({ @got.push($_) });
    is-deeply @got, [2, 3], '.map over a supply block still completes synchronously';
}
