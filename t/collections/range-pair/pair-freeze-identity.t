use Test;

# A Pair that holds a container has the identity of the Pair object: stable
# across reads and across `.freeze`, distinct for a `.clone`. `.freeze`
# changes that object, so every holder sees the read-only value.

plan 10;

{
    my $value = 17;
    my $pair = number => $value;
    my $before = $pair.WHICH;
    is $pair.WHICH, $before, '.WHICH is stable across reads';
    isa-ok $before, ObjAt, '... and is an ObjAt';
    $pair.freeze;
    is $pair.WHICH, $before, '.freeze keeps the identity';
}

{
    my $v = 1;
    my $p = a => $v;
    isnt $p.clone.WHICH, $p.WHICH, 'a clone is a different object';
}

{
    my $v = 1;
    my $p = a => $v;
    my $q = $p;
    is $p.freeze, 1, '.freeze answers the value';
    $v = 5;
    is $p.value, 1, 'the alias to $v is severed';
    throws-like { $q.value = 3 }, X::Assignment::RO,
        'another holder of the same Pair sees it frozen';
}

{
    my $p = Pair.new("foo", my Int $);
    $p.value = 42;
    $p.freeze;
    throws-like { $p.value = 666 }, X::Assignment::RO, 'a frozen typed value';
    is $p.value, 42, '... keeps its value';
}

{
    my $x = 3;
    is (a => $x).freeze, 3, '.freeze on a temporary Pair';
}
