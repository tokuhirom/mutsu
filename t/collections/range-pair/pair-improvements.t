use Test;

plan 10;

# Pair.new with named arguments
{
    my $p = Pair.new(:key<foo>, :value<bar>);
    is $p.key, 'foo', 'Pair.new(:key<foo>, :value<bar>) has correct key';
    is $p.value, 'bar', 'Pair.new(:key<foo>, :value<bar>) has correct value';
}

# Pair.new with positional arguments
{
    my $p = Pair.new("hello", "world");
    is $p.key, 'hello', 'Pair.new("hello", "world") has correct key';
    is $p.value, 'world', 'Pair.new("hello", "world") has correct value';
}

# .Pair identity method
{
    my $p = :foo<bar>;
    ok $p.Pair === $p, '.Pair on Pair:D is identity';
    ok Pair.Pair === Pair, '.Pair on Pair:U is identity';
}

# cmp-ok with Pair values as arguments
{
    my $p = :foo<bar>;
    cmp-ok $p, '===', $p, 'cmp-ok works with Pair arguments';
    # NB: the expected Pair must be spelled `'foo' => 'bar'`, not `:foo<bar>` —
    # a colonpair in an argument list is a NAMED argument, so the latter would
    # not reach `cmp-ok`'s third positional at all (raku rejects the call).
    cmp-ok $p, 'eqv', ('foo' => 'bar'), 'cmp-ok eqv works with Pair arguments';
}

# .gist on list of Pairs uses => separator
{
    is (1 => "a", 2 => "b").sort.gist, '(1 => a 2 => b)', 'Pair elements in list .gist use => separator';
    is (:a(1), :b(2)).sort.gist, '(a => 1 b => 2)', 'Colonpair elements in list .gist use => separator';
}
