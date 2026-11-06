use Test;

plan 6;

# A `$*SCHEDULER.cue(:catch)` handler runs in the dynamic scope of the `die`,
# so `.resume` continues the dying code (as rakudo does).
{
    my @got;
    my $done = Promise.new;
    $*SCHEDULER.cue(
        { my $x = die "boom"; @got.push("after"); $done.keep(1) },
        :catch({ @got.push(.message); try .resume }),
    );
    await Promise.anyof($done, Promise.in(5));
    is $done.status, 'Kept', 'resumed code ran to the end';
    is-deeply @got.List, ("boom", "after"), 'handler ran before the code resumed';
}

# Resume from inside a nested call and a block.
{
    my @got;
    my $done = Promise.new;
    sub inner { die "deep"; "resumed" }
    $*SCHEDULER.cue(
        { my @r = (1, 2).map({ inner() }); @got.push(@r.elems); $done.keep(1) },
        :catch({ @got.push(.message); try .resume }),
    );
    await Promise.anyof($done, Promise.in(5));
    is $done.status, 'Kept', 'nested die resumed';
    is-deeply @got.List, ("deep", "deep", 2), 'both dies were handled, map completed';
}

# A handler that does not resume abandons the code, and runs once.
{
    my @got;
    my $p = Promise.new;
    $*SCHEDULER.cue(
        { die "stop"; @got.push("not reached") },
        :catch({ @got.push(.message); $p.keep(1) }),
    );
    await Promise.anyof($p, Promise.in(5));
    sleep 0.2;
    is-deeply @got.List, ("stop",), 'non-resuming handler ran once; code abandoned';
    is $p.status, 'Kept', 'handler completed';
}
