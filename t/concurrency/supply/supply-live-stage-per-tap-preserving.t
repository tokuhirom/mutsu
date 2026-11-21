use Test;

# ADR-18443 slice 2c: a stage over a `Supplier::Preserving` or a
# `schedule-on` source, and `unique(:expires)` over a live one, are per-tap
# recipes too (#18443): nothing runs until a tap, and closing the tap
# releases the source.

plan 8;

my $n = 0;

{
    my $p = Supplier::Preserving.new;
    my $m = $p.Supply.map({ $n++; $_ * 2 });
    $p.emit(1);
    $p.emit(2);
    is $n, 0, 'an untapped stage over a preserving source runs nothing';
    my @a;
    my $t = $m.tap({ @a.push: $_ });
    $p.emit(3);
    is-deeply ($n, @a), (3, [2, 4, 6]), 'its first tap replays the backlog through the stage';
    $t.close;
    $p.emit(4);
    is $n, 3, 'closing the tap releases the preserving source';
}

{
    my $p = Supplier::Preserving.new;
    my $b = $p.Supply.unique.batch(:elems(2));
    $p.emit($_) for 1, 1, 2, 3;
    my @a;
    $b.tap({ @a.push: $_.List });
    $p.emit(4);
    is-deeply @a, [(1, 2), (3, 4)], 'a chain of stages over a preserving source';
}

{
    $n = 0;
    my $s = Supplier.new;
    my $m = $s.Supply.schedule-on($*SCHEDULER).map({ $n++; $_ });
    $s.emit(1);
    is $n, 0, 'an untapped stage over a schedule-on source runs nothing';
}

{
    my $s = Supplier.new;
    my $u = $s.Supply.unique(:expires(1));
    my (@a, @b);
    $u.tap({ @a.push: $_ });
    $s.emit(1);
    $u.tap({ @b.push: $_ });
    $s.emit(1);
    sleep 1.5;
    $s.emit(1);
    is-deeply (@a, @b), ([1, 1], [1, 1]), 'each unique(:expires) tap has its own clock';
}

{
    $n = 0;
    my $s = Supplier.new;
    my $u = $s.Supply.map({ $n++; $_ }).unique(:expires(10));
    $s.emit(1);
    is $n, 0, 'an untapped unique(:expires) runs nothing';
}

{
    my $p = Supplier::Preserving.new;
    $p.emit($_) for 1..3;
    my @got;
    my $fin = start {
        react whenever $p.Supply.map(* * 10) -> $v {
            @got.push: $v;
            done if @got == 3;
        }
    }
    await Promise.anyof($fin, Promise.in(10));
    is-deeply @got, [10, 20, 30], 'a react over a stage of a preserving source sees its backlog';
}
