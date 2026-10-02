use Test;

plan 7;

# Issue #10831: closing the tap that started a `.share`d supply block must not
# tear the block down for every other tap. raku's Supply.share taps its source
# once and keeps it running; closing a consumer drops only that consumer.

{
    my $s = Supplier.new;
    my $sup = supply { whenever $s.Supply -> $d { emit $d } }.share;
    my @a; my @b;
    my $t = $sup.tap({ @a.push($_) });
    $sup.tap({ @b.push($_) });
    $s.emit(1);
    $t.close;
    $s.emit(2);
    is-deeply @a, [1], 'the closed starting tap stops receiving';
    is-deeply @b, [1, 2], 'the other tap keeps receiving after the starter closes';
}

{
    my $s = Supplier.new;
    my $closed = 0;
    my $sup = supply { whenever $s.Supply -> $d { emit $d }; CLOSE { $closed++ } }.share;
    my @a; my @b; my @c;
    my $t = $sup.map(* + 1).tap({ @a.push($_) });
    my $t2 = $sup.tap({ @b.push($_) });
    $s.emit(1);
    $t.close;
    $s.emit(2);
    is-deeply @a, [2], 'a closed derived tap stops receiving';
    is-deeply @b, [1, 2], 'closing a derived starting tap leaves the others running';
    $t2.close;
    is $closed, 0, 'closing every consumer does not run the shared block CLOSE phaser';
    $s.emit(3);
    my $t3 = $sup.tap({ @c.push($_) });
    $s.emit(4);
    is-deeply @c, [4], 'a later tap joins the still-running shared block';
    $t3.close;
    is-deeply @b, [1, 2], 'a closed tap receives nothing more';
}
