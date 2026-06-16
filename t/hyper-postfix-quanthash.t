use Test;

# `$b>>++` / `$b>>--` on a mutable QuantHash applies the postfix op to each
# weight: it returns the *original* QuantHash and writes the in/decremented
# weights back to the source variable. Bag counts reaching <= 0 (Mix weights
# reaching 0.0) are removed. Immutable Bag/Mix/Set have read-only weights.

plan 10;

# BagHash >>-- (the rakudo #5057 case).
{
    my $b := <a b c d e a b>.BagHash;
    is-deeply $b>>--, <a b c d e a b>.BagHash, 'BagHash >>-- returns the original';
    is-deeply $b, <a b>.BagHash, 'BagHash >>-- decrements the source (0-weights removed)';
}

# MixHash >>-- (the rakudo #5057 case).
{
    my $m := <a b c d e a b>.MixHash;
    is-deeply $m>>--, <a b c d e a b>.MixHash, 'MixHash >>-- returns the original';
    is-deeply $m, <a b>.MixHash, 'MixHash >>-- decrements the source';
}

# BagHash >>++ increments each weight.
{
    my $b := <a b>.BagHash;
    is-deeply $b>>++, <a b>.BagHash, 'BagHash >>++ returns the original';
    is-deeply $b, <a a b b>.BagHash, 'BagHash >>++ increments the source';
}

# A plain `my` (non-bound) BagHash also writes back.
{
    my $b = <a a b>.BagHash;
    $b>>--;
    is-deeply $b, <a>.BagHash, 'my-bound BagHash >>-- writes back';
}

# SetHash >>-- drops every element (weight 1 -> 0).
{
    my $s := <a b c>.SetHash;
    $s>>--;
    is-deeply $s, ().SetHash, 'SetHash >>-- empties the set';
}

# Immutable Bag/Mix weights are read-only.
{
    my $b = <a a b>.Bag;
    dies-ok { $b>>-- }, 'immutable Bag >>-- dies (read-only weights)';
}
{
    my $m = (a => 1.5).Mix;
    dies-ok { $m>>-- }, 'immutable Mix >>-- dies (read-only weights)';
}
