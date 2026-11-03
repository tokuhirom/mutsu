use Test;

# `Proc::Async.Supply(:bin)` delivers the merged stdout and stderr as `Buf`
# chunks, not `Str` (#15647, Shell::DSL's `Pipeline.capture`). Checked against
# rakudo.

plan 8;

{
    my $p = Proc::Async.new('echo', 'hi');
    my @chunks;
    $p.Supply(:bin).tap: -> $x { @chunks.push: $x };
    await $p.start;
    is @chunks.elems, 1, ':bin: one chunk for a one-line command';
    ok @chunks[0] ~~ Buf, '...which is a Buf';
    is-deeply @chunks[0].list, (104, 105, 10), '...holding the output bytes';
}

{
    my $p = Proc::Async.new('sh', '-c', 'printf abc; printf def >&2');
    my $all = Buf.new;
    $p.Supply(:bin).tap: -> $x { $all ~= $x };
    await $p.start;
    is $all.elems, 6, 'both streams are merged into the Buf chunks';
    is $all.decode.chars, 6, '...and decode to six characters';
}

{
    my $p = Proc::Async.new('echo', 'hi');
    my @chunks;
    $p.Supply.tap: -> $x { @chunks.push: $x };
    await $p.start;
    is-deeply @chunks, ["hi\n"], 'without :bin the Supply still delivers Str';
}

{
    my $p = Proc::Async.new('echo', 'hi');
    my $done = False;
    $p.Supply(:bin).tap: -> $ {}, done => { $done = True };
    await $p.start;
    ok $done, 'the :bin Supply still runs its done callback';
}

{
    my $p = Proc::Async.new('echo', 'hi');
    $p.Supply(:bin);
    throws-like { $p.stdout(:bin) }, X::Proc::Async::SupplyOrStd,
        'asking for stdout after the Supply is still refused';
}
