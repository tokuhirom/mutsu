use Test;

# `.value = X` / `.value--` on a Pair yielded by a mutable QuantHash's `.pairs`
# writes the new weight back to the source container (BagHash/MixHash/SetHash).
# Weight 0 (or False for Set) removes the key; a non-numeric Str raises
# X::Str::Numeric. Immutable Bag/Mix/Set remain read-only.

plan 23;

# --- Parser: `.value--` is `.value` then postfix `--`, not a method `value--` ---
{
    my $p = a => 5;
    $p.value--;
    is $p.value, 4, '$pair.value-- decrements the pair value';
}
{
    my @seen;
    for (a => 1, b => 2) { @seen.push(.value) };
    is-deeply @seen, [1, 2], '.value as a topic method still reads the pair value';
}

# --- BagHash .pairs .value writeback ---
{
    my $b = (a => 5).BagHash;
    .value = 999 for $b.pairs;
    is $b<a>, 999, 'BagHash .value = N from a .pairs alias';
}
{
    my $bh = <a a b b b c>.BagHash;
    for $bh.pairs { .value-- }
    is $bh<a>, 1, 'BagHash .value-- (a: 2 -> 1)';
    is $bh<b>, 2, 'BagHash .value-- (b: 3 -> 2)';
    is $bh<c>, 0, 'BagHash .value-- removing last occurrence (c: 1 -> 0)';
}
{
    my $bh = <a a b>.BagHash;
    for $bh.pairs { .value = 42 }
    is $bh<a>, 42, 'BagHash .value = 42 sets the weight';
    is $bh<b>, 42, 'BagHash .value = 42 sets the weight (b)';
}
{
    my $bh = <a a b>.BagHash;
    for $bh.pairs { .value = 0 }
    is $bh.elems, 0, 'BagHash .value = 0 removes all elements';
}
# A Bag weight that goes negative removes the element (unlike a Mix).
{
    my $bh = <a b b c d e f>.BagHash;
    .value = -1 for $bh.pairs;
    is-deeply $bh, ().BagHash, 'BagHash .value = -1 removes elements (negative weight)';
}
{
    my $bh = <a b b c>.BagHash;
    .value-- for $bh.pairs;
    .value-- for $bh.pairs;
    is-deeply $bh, ().BagHash, 'BagHash repeated .value-- past zero removes elements';
}
# A Mix keeps negative weights.
{
    my $m = (a => 1.0, b => 2.0).MixHash;
    .value = -2.5 for $m.pairs;
    is $m<a>, -2.5, 'MixHash retains a negative weight';
}

# --- MixHash .pairs .value writeback ---
{
    my $m = (a => 1.5, b => 2.0).MixHash;
    .value = 9.5 for $m.pairs;
    is $m<a>, 9.5, 'MixHash .value = 9.5 from a .pairs alias';
}

# --- SetHash .pairs .value writeback ---
{
    my $sh = <a b c>.SetHash;
    for $sh.pairs { .value = False if .key eq 'b' }
    is $sh<b>, False, 'SetHash .value = False removes the element';
    is $sh<a>, True, 'SetHash other elements unaffected';
}

# --- Coercion / immutability ---
{
    my $bh = (a => 3).BagHash;
    throws-like { .value = "foo" for $bh.pairs },
      X::Str::Numeric,
      'non-numeric Str on a mutable BagHash .pairs alias raises X::Str::Numeric';
}
{
    my $b = (a => 5).Bag;
    throws-like { .value = 9 for $b.pairs },
      X::Assignment::RO,
      'immutable Bag .pairs .value is read-only';
}
{
    my $s = <a b>.Set;
    throws-like { .value = 0 for $s.pairs },
      X::Assignment::RO,
      'immutable Set .pairs .value is read-only';
}

# --- Typed BagHash element autovivification clamps at 0 ---
{
    my BagHash $bh;
    is $bh<poinc>++, 0, 'BagHash $bh<k>++ returns 0 on autoviv';
    is $bh<poinc>, 1, 'BagHash $bh<k>++ leaves 1';
}
{
    my BagHash $bh;
    is $bh<podec>--, 0, 'BagHash $bh<k>-- returns 0 on autoviv';
    is $bh<podec>, 0, 'BagHash $bh<k>-- stays at 0 (no negative counts)';
}
{
    my BagHash $bh;
    # Prefix -- on an absent key returns the clamped new value (0), not -1.
    is --$bh<prdec>, 0, 'BagHash --$bh<k> returns 0 (clamped), not -1';
}
