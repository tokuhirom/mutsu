use Test;

# `Supply.list` / `.Seq` tap the supply and return a lazy list reified as the
# supply emits (Type/Supply.rakudoc). They used to collect every value until
# the supply was done -- up to a 30 s deadline -- so a `supply` block with a
# `whenever Supply.interval(60)` (EventSource::Server's `out-supply`) made
# `for $s.list { last if ... }` stall for 30 s.

plan 7;

{
    my $s = supply {
        whenever (1, 2, 3).Supply -> $m { emit $m }
        whenever Supply.interval(60) { }
    }
    my $t0 = now;
    my @got;
    for $s.list -> $m {
        @got.push: $m;
        last if $m == 3;
    }
    is-deeply @got, [1, 2, 3], 'a never-done supply block yields its values through .list';
    ok now - $t0 < 10, '... and `last` ends the loop without waiting for done';
}

{
    my $s = supply { whenever Supply.interval(0.05) -> $v { emit $v * 10 } };
    my @got;
    for $s.Seq -> $v {
        @got.push: $v;
        last if $v == 20;
    }
    is-deeply @got, [0, 10, 20], '.Seq of a live supply block is lazy';
}

{
    my $s = supply { emit 1; emit 2; done };
    is-deeply $s.list.List, (1, 2), 'a finite supply block still lists all its values';
    isa-ok $s.list, List, '.list answers a List';
}

{
    my $s = supply { emit 1; emit 2; die "oops" };
    my @got;
    my $err;
    try { for $s.list { @got.push: $_ }; CATCH { default { $err = .message } } }
    is-deeply @got, [1, 2], 'values emitted before a quit are listed';
    is $err, 'oops', '... then the quit is thrown';
}
