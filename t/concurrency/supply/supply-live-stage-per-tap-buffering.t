use Test;

# ADR-18443 slice 2: `lines`, `reduce`, `unique` and `squish` over a live
# source (or over a per-tap recipe) are per-tap recipes too (#18443).

plan 9;

my $n = 0;
{
    my $s = Supplier.new;
    my $u = $s.Supply.unique(:as({ $n++; $_ }));
    $s.emit(1);
    is $n, 0, 'an untapped unique does not run';
}

{
    my $s = Supplier.new;
    my @a; my @b;
    my $u = $s.Supply.unique;
    $u.tap({ @a.push: $_ });
    $u.tap({ @b.push: $_ });
    $s.emit($_) for 1, 1, 2;
    is-deeply (@a, @b), ([1, 2], [1, 2]), 'each tap keeps its own unique state';
}

{
    my $s = Supplier.new;
    my @r;
    $s.Supply.map(* * 2).reduce(&[+]).tap({ @r.push: $_ });
    $s.emit($_) for 1..3;
    $s.done;
    is-deeply @r, [12], 'reduce over a live map';
}

{
    my $s = Supplier.new;
    my @l;
    $s.Supply.lines.tap({ @l.push: $_ });
    $s.emit("a\nb");
    $s.emit("c\n");
    $s.done;
    is-deeply @l, ['a', 'bc'], 'lines over a live source';
}

{
    my $s = Supplier.new;
    my @l;
    $s.Supply.lines(:!chomp).grep(*.chars > 2).tap({ @l.push: $_ });
    $s.emit("ab\nx\ncd\n");
    is-deeply @l, ["ab\n", "cd\n"], 'a stage over lines';
}

{
    my $s = Supplier.new;
    my @q;
    my $t = $s.Supply.squish.tap({ @q.push: $_ });
    $s.emit($_) for 1, 1, 2, 2, 1;
    $t.close;
    $s.emit(9);
    is-deeply @q, [1, 2, 1], 'squish, and nothing after close';
}

{
    my $s = Supplier.new;
    nok $s.Supply.lines.live, 'lines over a live source is not live';
    nok $s.Supply.unique.live, 'unique neither';
    nok $s.Supply.reduce(&[+]).live, 'reduce neither';
}
