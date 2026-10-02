use Test;

plan 8;

# Issue #10839: raku's Supply.share taps its source when `.share` is called,
# so a shared supply block runs right then -- not on the first consumer's tap.

{
    my $runs = 0;
    my $sup = supply { $runs++; emit 1 }.share;
    is $runs, 1, 'the shared block runs at .share time';
    my @a;
    $sup.tap({ @a.push($_) });
    is $runs, 1, 'tapping does not run the block again';
    is-deeply @a, [], 'values emitted before anyone tapped are lost';
}

{
    my $s = Supplier.new;
    my $runs = 0;
    my $sup = supply { $runs++; whenever $s.Supply -> $d { emit $d } }.share;
    $s.emit('early');
    my (@a, @b);
    $sup.tap({ @a.push($_) });
    $sup.map(* ~ '!').tap({ @b.push($_) });
    $s.emit('x');
    is $runs, 1, 'the block runs once across every consumer';
    is-deeply @a, ['x'], 'a direct tap sees values emitted after it tapped';
    is-deeply @b, ['x!'], 'a derived tap joins the running block';
}

{
    my $sup;
    lives-ok { $sup = supply { die 'boom' }.share },
        'a block dying at .share time does not throw out of .share';
    my @got;
    $sup.tap({ @got.push($_) });
    is-deeply @got, [], '... and the shared supply delivers nothing afterwards';
}
