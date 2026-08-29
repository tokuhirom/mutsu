use Test;

plan 12;

# `my @a = hyper for LIST { BODY }` used to parse as `.hyper(do for ...)`:
# the loop ran sequentially on the main thread and `.hyper` only wrapped the
# already-computed list. It must keep ForMode::Hyper, collect in order, and
# actually run the body on worker threads.

{
    my @got = hyper for 1..4 { $_ * 2 };
    is-deeply @got, [2, 4, 6, 8], 'hyper for in assignment collects in order';
}

{
    my @got = hyper for 1..6 { $_ }.rotor(2);
    is @got.elems, 3, 'postfix .rotor applies to the hyper-for result';
    is-deeply @got[0].List, (1, 2), '... first rotor chunk';
}

{
    my $main = $*THREAD.id;
    my $saw = False;
    my @got = hyper for ^200 {
        $saw = True if $*THREAD.id != $main;
        $_
    };
    ok $saw, 'assignment hyper for runs the body off the main thread';
    is @got.elems, 200, '... and still collects every item';
    is @got[199], 199, '... preserving order';
}

{
    my $main = $*THREAD.id;
    my $saw = False;
    hyper for ^200 {
        $saw = True if $*THREAD.id != $main;
    }
    ok $saw, 'statement hyper for still sees another thread';
}

{
    my $main = $*THREAD.id;
    my $saw = False;
    my @got = (^2000).hyper.map({
        $saw = True if $*THREAD.id != $main;
        $_
    });
    ok $saw, 'HyperSeq.map of 2000 items stays parallel (no 1000-item cutoff)';
    is @got.elems, 2000, '... and maps every item';
    is @got[1999], 1999, '... preserving order';
}

{
    my $r = race for ^10 -> $n { $n if $n %% 2 };
    is $r.elems, 5, 'race for in assignment still skips false if-modifier values';
    is $r.sort.join(','), '0,2,4,6,8', '... with the expected values';
}
