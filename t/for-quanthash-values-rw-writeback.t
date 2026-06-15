use Test;

# Writing back to a *mutable* QuantHash (MixHash/BagHash) weight through a
# `.values`/`.kv` for-loop alias. `$_ = X for $b.values` and
# `for $b.kv -> \k, \v { v = X }` mutate the QuantHash by key; a non-numeric
# string raises X::Str::Numeric and a zero weight removes the key. An *immutable*
# Bag/Mix/Set still throws X::Assignment::RO.

plan 16;

# --- topic `.values` ---
{
    my $b = (a => 1, b => 2, c => 3).BagHash;
    $_ = $_ * 10 for $b.values;
    is "$b<a> $b<b> $b<c>", "10 20 30", 'topic .values assign writes back (Bag)';
}
{
    my $b = <a a a>.BagHash;
    for $b.values { $_-- }
    is $b<a>, 2, 'topic .values $_-- decrements weight';
    for $b.values { $_ = 0 }
    is $b.elems, 0, 'topic .values $_ = 0 removes the key';
}
{
    my $m = (a => 2.5, b => 1.0).MixHash;
    $_ = $_ * 2 for $m.values;
    is "$m<a> $m<b>", "5 2", 'topic .values assign writes back (Mix, Real weights)';
}

# --- named rw `.values` ---
{
    my $b = (a => 1, b => 2).BagHash;
    for $b.values -> $v is rw { $v += 100 }
    is "$b<a> $b<b>", "101 102", '.values -> $v is rw writes back';
}

# --- `.kv` rw ---
{
    my $b = (p => 5, q => 6).BagHash;
    for $b.kv -> \k, \v { v = v + 1 }
    is "$b<p> $b<q>", "6 7", '.kv -> \\k, \\v sigilless assign writes back';
}
{
    my $m = (x => 1.5, y => 2.0).MixHash;
    for $m.kv -> $k, $v is rw { $v = $v * 4 }
    is "$m<x> $m<y>", "6 8", '.kv -> $k, $v is rw writes back (Mix)';
}
{
    my $b = (a => 3).BagHash;
    for $b.kv -> \k, \v { v = 0 }
    is $b.elems, 0, '.kv setting weight 0 removes the key';
}

# --- X::Str::Numeric on a non-numeric assignment ---
{
    my $b = (a => 1).BagHash;
    dies-ok { $_ = "foo" for $b.values }, 'topic .values = non-numeric Str dies';
    my $err;
    { $_ = "foo" for $b.values; CATCH { default { $err = .^name } } }
    is $err, 'X::Str::Numeric', 'and it is X::Str::Numeric';
    is $b<a>, 1, 'the weight is unchanged after the throw';
}
{
    my $b = (a => 1).BagHash;
    dies-ok { for $b.kv -> \k, \v { v = "foo" } }, '.kv = non-numeric Str dies';
}

# --- immutable Bag/Mix/Set still read-only ---
{
    my $b = (a => 1).Bag;
    dies-ok { $_ = 5 for $b.values }, 'immutable Bag .values assign is read-only';
    is $b<a>, 1, 'immutable Bag weight unchanged';
}

# --- read-only iteration leaves the QuantHash intact ---
{
    my $b = (a => 2, b => 3).BagHash;
    my $sum = 0;
    $sum += $_ for $b.values;
    is $sum, 5, 'read-only sum over .values';
    is "$b<a> $b<b>", "2 3", 'read-only .values leaves the Bag unchanged';
}
