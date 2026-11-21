use Test;

# `done` in a `whenever` body completes its own supply block even when the
# value arrived synchronously while the block was still opening that source:
# a source supply block that emits during its own setup (#18472; Red
# t/190-resultseq-supply, a `head` stage over a snapshot-then-live source,
# feeding a `sort` stage that flushes from `LAST`).

plan 8;

{
    my $src = supply { emit 3; whenever Supplier.new.Supply { } };
    my $b = supply { whenever $src -> $v { emit $v; done } };
    my @got;
    my $done = False;
    $b.tap(-> $v { @got.push($v) }, done => { $done = True });
    is-deeply @got, [3], 'the value emitted before done arrives';
    ok $done, 'done during the source setup completes the block';
}

{
    my $src = supply { emit 3 };
    my $c = supply {
        whenever $src -> $v { emit $v; done }
        whenever Supplier.new.Supply { }
    };
    my $done = False;
    $c.tap(-> $ { }, done => { $done = True });
    ok $done, 'done completes the block although another whenever is still live';
}

{
    my $src = supply { emit 3; emit 1; emit 7; whenever Supplier.new.Supply { } };
    my $head = supply { whenever $src -> $v { emit $v; done if $v == 1 } };
    my @got;
    $head.tap(-> $v { @got.push($v) }, done => { @got.push('done') });
    is-deeply @got, [3, 1, 'done'], 'a head-like stage stops at its done';

    my $sorted = supply {
        my @snap;
        whenever $head -> $v {
            LAST { emit $_ for @snap.sort }
            @snap.push($v);
        }
    };
    my @out;
    my $finished = False;
    $sorted.tap(-> $v { @out.push($v) }, done => { $finished = True });
    is-deeply @out, [1, 3], 'a downstream whenever runs its LAST when the stage is done';
    ok $finished, 'and the downstream block completes';
}

{
    # The ordinary live case keeps working.
    my $s = Supplier.new;
    my $src = supply { whenever $s.Supply { emit $_ } };
    my $d = supply { whenever $src -> $v { emit $v; done } };
    my @got;
    my $done = False;
    $d.tap(-> $v { @got.push($v) }, done => { $done = True });
    $s.emit(5);
    $s.emit(6);
    is-deeply @got, [5], 'a later live value still completes the block';
    ok $done, 'with its done';
}
