use Test;

# `delayed` and `schedule-on` over a live per-tap recipe mark the recipe
# itself, so each tap delays (or schedules) its own instance of the chain
# below: a stage runs once per tap per value, and a closed tap releases it
# (ADR-18443, #18579). They used to instantiate the chain once, at call
# time, and share it between taps.

plan 5;

{
    my $n = 0;
    my $s = Supplier.new;
    my $d = $s.Supply.map({ $n++; $_ }).delayed(0.05);
    my (@a, @b);
    $d.tap({ @a.push: $_ });
    $d.tap({ @b.push: $_ });
    $s.emit(1);
    $s.emit(2);
    sleep 0.4;
    is "@a[] | @b[]", '1 2 | 1 2', 'both taps of a delayed recipe see every value';
    is $n, 4, 'each tap runs its own map';
    my $t = $d.tap({;});
    $t.close;
    $s.emit(3);
    sleep 0.3;
    is $n, 6, 'a closed tap releases its own chain';
}

{
    my $n = 0;
    my $s = Supplier.new;
    my $sc = $s.Supply.map({ $n++; $_ }).schedule-on($*SCHEDULER);
    my @c;
    $sc.tap({ @c.push: $_ });
    $sc.tap({;});
    $s.emit(5);
    sleep 0.3;
    is @c.join, '5', 'a scheduled recipe delivers';
    is $n, 2, 'each tap of a scheduled recipe runs its own map';
}
