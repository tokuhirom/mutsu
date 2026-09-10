use Test;

plan 22;

# A `:delete` through a container held in a `$` used to resolve the container
# from the env mirror only. A scalar-held container lives in its local slot and
# leaves that mirror at the `my`-declaration seed (a type object), so the delete
# found `Any`, removed nothing, and then wrote that `Any` back over the
# variable — losing the whole container.

# --- hash held in a `$` -------------------------------------------------
{
    my $h = { a => 1, b => 2 };
    is ($h<a>:delete), 1, 'delete through a $-held hash returns the value';
    is $h.raku, '${:b(2)}', 'and the hash keeps its remaining keys';
}

{
    my $h = { a => 1, b => 2 };
    $h<a>:delete;
    is $h.raku, '${:b(2)}', 'the bare-statement form deletes too';
}

{
    my $h = { a => 1, b => 2 };
    is ($h{'a'}:delete), 1, 'the {} subscript spelling deletes as well';
    is $h.raku, '${:b(2)}', 'and leaves the rest of the hash alone';
}

{
    my $h = { a => 1, b => 2 };
    is ($h<a b>:delete).raku, '(1, 2)', 'a slice delete returns every value';
    is $h.raku, '${}', 'and empties the hash';
}

{
    my $h = { a => 1 };
    is ($h<zz>:delete).raku, 'Any', 'deleting an absent key yields the Any hole';
    is $h.raku, '${:a(1)}', 'and leaves the hash untouched';
}

{
    my $h = { a => { b => 1, c => 2 } };
    is ($h<a><b>:delete), 1, 'a nested delete returns the value';
    is $h.raku, '${:a(${:c(2)})}', 'and mutates the inner hash in place';
}

# --- array held in a `$` ------------------------------------------------
{
    my $a = [1, 2, 3];
    $a[1]:delete;
    is $a.raku, '$[1, Any, 3]', 'deleting an interior element leaves a hole';
}

{
    my $a = [1, 2, 3];
    is ($a[2]:delete), 3, 'deleting the last element returns it';
    is $a.raku, '$[1, 2]', 'and trims the trailing hole';
}

{
    my $a = [1, 2, 3];
    is ($a[0, 2]:delete).raku, '(1, 3)', 'a slice delete returns every element';
    is $a.raku, '$[Any, 2]', 'and holes out each addressed slot';
}

# --- quanthashes held in a `$` -----------------------------------------
{
    my $s = <a b>.SetHash;
    $s<a>:delete;
    is $s.raku, 'SetHash.new("b")', 'a $-held SetHash loses only the deleted key';
}

{
    my $b = <a b b>.BagHash;
    is ($b<b>:delete), 2, 'a $-held BagHash delete returns the weight';
    is $b.raku, '("a"=>1).BagHash', 'and drops just that element';
}

# --- the immutability guards are reachable through a `$` too ------------
{
    my $s = Set.new(<a b>);
    throws-like { $s<a>:delete }, X::Assignment::RO,
        'an immutable Set held in a $ refuses the removal';
    is $s.raku, 'Set.new("a","b")', 'and keeps both elements';
}

{
    my $b = <a a b>.Bag;
    throws-like { $b<a>:delete }, X::Assignment::RO,
        'an immutable Bag held in a $ refuses the removal';
}
