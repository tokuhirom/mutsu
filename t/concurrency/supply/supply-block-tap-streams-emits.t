use Test;

# A plain `.tap` of a `supply { }` block runs the tap callback inside each
# `emit`, while the block is still running -- not once the whole block has
# returned (#11434).

plan 8;

{
    my @log;
    my $s = supply {
        emit 'a';
        @log.push: 'after a';
        emit 'b';
        @log.push: 'after b';
    }
    $s.tap: { @log.push: "got $_" };
    is-deeply @log, ['got a', 'after a', 'got b', 'after b'],
        'each emit reaches the tap before the block goes on';
}

{
    # The block waits for the tap to have seen its first value.
    my $seen = Promise.new;
    my $vow  = $seen.vow;
    my @got;
    supply {
        emit 'a';
        await Promise.anyof($seen, Promise.in(5));
        emit $seen ?? 'seen' !! 'timeout';
    }.tap: { @got.push: $_; $vow.keep(True) if $_ eq 'a' };
    is-deeply @got, ['a', 'seen'], 'the block can wait on what its tap did';
}

{
    my @log;
    my $s = supply {
        emit 1;
        @log.push: 'after 1';
        emit 2;
    }
    $s.do({ @log.push: "do $_" }).tap: { @log.push: "tap $_" };
    is-deeply @log, ['do 1', 'tap 1', 'after 1', 'do 2', 'tap 2'],
        '.do callbacks stream with the tap';
}

{
    my @got;
    supply {
        whenever Supply.from-list(1, 2) { emit $_ }
        emit 3;
    }.tap: { @got.push: $_ };
    is-deeply @got, [3, 1, 2], 'a whenever starts only once the block has run';
}

{
    my @got;
    supply {
        emit 0;
        whenever Supply.from-list(1, 2) { emit $_ }
        emit 3;
    }.tap: { @got.push: $_ };
    is-deeply @got, [0, 3, 1, 2], 'emits around a whenever keep that order';
}

{
    my @log;
    my $s = supply {
        emit 1;
        @log.push: 'after 1';
        emit 2;
    }
    my $err;
    try {
        $s.tap: { @log.push: "got $_"; die "boom" }, quit => { @log.push: 'quit' };
        CATCH { default { $err = .message } }
    }
    is $err, 'boom', 'a tap callback that dies throws out of .tap';
    is-deeply @log, ['got 1'], 'the block stops at that emit, and quit is not called';
}

{
    my @log;
    my $inner = supply { emit 1; @log.push: 'inner after 1'; emit 2 }
    supply {
        whenever $inner { emit $_ * 10 }
    }.tap: { @log.push: "got $_" };
    is-deeply @log, ['got 10', 'inner after 1', 'got 20'],
        'a whenever over a supply block streams through to the outer tap';
}
