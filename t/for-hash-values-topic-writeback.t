use Test;

# `$_ = X for %h.values` aliases the hash value and writes back by key order.
plan 9;

{
    my %h = a => 1, b => 2, c => 3;
    $_ = $_ * 10 for %h.values;
    is %h.sort».kv.flat.join(","), "a,10,b,20,c,30", 'topic assign writes back';
}

{
    my %h = a => 1, b => 2;
    $_++ for %h.values;
    is %h.sort».kv.flat.join(","), "a,2,b,3", 'topic ++ writes back';
}

{
    my %h = a => 1, b => 2;
    for %h.values { $_ = $_ + 100 }
    is %h.sort».kv.flat.join(","), "a,101,b,102", 'block form topic assign writes back';
}

{
    # read-only iteration must not corrupt the hash
    my %h = a => 5, b => 6;
    my $sum = 0;
    $sum += $_ for %h.values;
    is $sum, 11, 'read-only sum over values';
    is %h.sort».kv.flat.join(","), "a,5,b,6", 'read-only values leaves hash unchanged';
}

{
    # bare `for %h` iterates Pairs and must NOT value-writeback
    my %h = a => 1, b => 2;
    for %h { }
    is %h.sort».kv.flat.join(","), "a,1,b,2", 'bare for over hash leaves it unchanged';
}

{
    # rw named param over .values still writes back
    my %h = a => 1, b => 2, c => 3;
    for %h.values -> $v is rw { $v *= 2 }
    is %h.sort».kv.flat.join(","), "a,2,b,4,c,6", 'rw named param over values writes back';
}

{
    # .kv rw still works
    my %h = a => 1, b => 2;
    for %h.kv -> $k, $v is rw { $v += 10 }
    is %h.sort».kv.flat.join(","), "a,11,b,12", '.kv rw writeback';
}

{
    # plain named param over .values is read-only (raku throws)
    my %h = a => 1, b => 2;
    dies-ok { for %h.values -> $v { $v = 9 } }, 'plain named param over values is read-only';
}
