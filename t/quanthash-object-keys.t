use Test;

# Assigning a non-string object as a SetHash/BagHash/MixHash key must retain the
# object, not stringify it: `$sh{1001} = 42` keeps the Int `1001` as the key
# (closes roast/S02-types/sethash.t "retains object, not container").

plan 14;

# SetHash: the key stays the Int object even after the source var mutates
{
    my $o = SetHash.new;
    my $i = 1001;
    $o{$i} = 42;
    $i++;
    is-deeply $o.keys, (1001,).Seq, 'SetHash retains Int object key';
    is $o.keys[0].WHAT.gist, '(Int)', 'SetHash key is an Int';
}

# BagHash
{
    my $b = BagHash.new;
    my $i = 1001;
    $b{$i} = 42;
    $i++;
    is-deeply $b.keys, (1001,).Seq, 'BagHash retains Int object key';
    is $b.keys[0].WHAT.gist, '(Int)', 'BagHash key is an Int';
}

# MixHash
{
    my $m = MixHash.new;
    $m{1001} = 2.5;
    is-deeply $m.keys, (1001,).Seq, 'MixHash retains Int object key';
    is $m.keys[0].WHAT.gist, '(Int)', 'MixHash key is an Int';
}

# Bool object key
{
    my $o = SetHash.new;
    $o{True} = 1;
    is $o.keys[0].WHAT.gist, '(Bool)', 'Bool object key retained';
    is $o.keys.raku, '(Bool::True,).Seq', 'Bool key renders as Bool::True';
}

# String keys are unaffected
{
    my $o = SetHash.new;
    $o<abc> = True;
    is-deeply $o.keys, ("abc",).Seq, 'string key stays a string';
    is $o.keys[0].WHAT.gist, '(Str)', 'string key is a Str';
}

# Removing an object key drops it (and its original-key record)
{
    my $o = SetHash.new;
    $o{1001} = 42;
    $o{1001} = 0;
    is-deeply $o.keys, ().Seq, 'removed object key is gone';
}

# Multiple object keys
{
    my $o = SetHash.new;
    $o{10} = 1;
    $o{20} = 1;
    is-deeply $o.keys.sort, (10, 20).Seq, 'multiple Int object keys';
    is $o.elems, 2, 'two elements';
}

# BagHash weight still works with an object key
{
    my $b = BagHash.new;
    $b{7} = 3;
    is $b{7}, 3, 'BagHash weight retained for object key';
}
