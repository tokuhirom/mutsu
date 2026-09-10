use v6;
use Test;

plan 20;

# SetHash slice assignment applies membership (Bool) semantics per key, keeps
# untouched members, and does NOT replace the container with a plain Hash.
{
    my $fruits = <peach apple orange>.SetHash;
    my @r = ($fruits<apple kiwi> = False, True);
    is @r.raku, '[Bool::False, Bool::True]', 'SetHash slice assign returns per-key Bool';
    is $fruits.keys.sort.raku, '("kiwi", "orange", "peach").Seq',
        'SetHash slice assign removes False keys, adds True keys, keeps the rest';
}

# Short rvalue pads with Nil (no cycling): only the first key is set True.
{
    my $s = SetHash.new;
    my @r = ($s<a b c> = True);
    is @r.raku, '[Bool::True, Bool::False, Bool::False]', 'SetHash slice pads with Nil (Bool::False)';
    is $s.keys.sort.raku, '("a",).Seq', 'SetHash slice: only first key set';
}

# BagHash slice assignment applies count semantics per key.
{
    my $b = BagHash.new;
    my @r = ($b<a x> = 3, 2);
    is @r.raku, '[3, 2]', 'BagHash slice assign returns per-key counts';
    is $b<a>, 3, 'BagHash slice: a => 3';
    is $b<x>, 2, 'BagHash slice: x => 2';
}
{
    my $b = <a b c>.BagHash;
    my @r = ($b<a x> = 0, 2);
    is @r.raku, '[0, 2]', 'BagHash slice with 0 removes, keeps expr value 0';
    is $b.keys.sort.raku, '("b", "c", "x").Seq', 'BagHash slice: a removed (0), x added';
}
{
    my $b = BagHash.new;
    my @r = ($b<x y z> = 5);
    is @r.raku, '[5, 0, 0]', 'BagHash slice pads short rvalue with 0';
    is $b.keys.sort.raku, '("x",).Seq', 'BagHash slice: only x set (padded 0 => absent)';
}

# MixHash slice assignment applies weight semantics per key.
{
    my $m = MixHash.new;
    my @r = ($m<a x> = 0.5, 2);
    is @r.raku, '[0.5, 2]', 'MixHash slice assign returns per-key weights';
    is $m<a>, 0.5, 'MixHash slice: a => 0.5';
    is $m<x>, 2, 'MixHash slice: x => 2';
}
{
    my $m = MixHash.new;
    my @r = ($m<a b> = 1.5, -2);
    is @r.raku, '[1.5, -2]', 'MixHash slice keeps negative weights';
    is $m<b>, -2, 'MixHash slice: negative weight stored';
}

# Hyper-increment/decrement over a QuantHash slice.
{
    my SetHash $fruits .= new;
    $fruits<apple banana kiwi>»++;
    $fruits<banana kiwi>»--;
    is $fruits.keys.sort.raku, '("apple",).Seq', 'SetHash hyper ++/-- over a slice';
}

# Immutable Set/Bag/Mix slice assignment throws (RO), not a silent replace.
{
    my $s = <a b>.Set;
    dies-ok { $s<a c> = True, True }, 'immutable Set slice assign dies';
    my $b = <a b>.Bag;
    dies-ok { $b<a c> = 1, 1 }, 'immutable Bag slice assign dies';
    my $m = <a b>.Mix;
    dies-ok { $m<a c> = 1, 1 }, 'immutable Mix slice assign dies';
}
