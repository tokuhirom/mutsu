use Test;

# #9277 (same-frame half): a `:=` rebind decides the name's writability from
# what it is bound to NOW. Bound to an immutable value, assignment dies; bound
# to a container, assignment writes through it.

plan 6;

{
    my $v = 1;
    $v := 42;
    throws-like { $v = 5 }, X::AdHoc,
        message => 'Cannot assign to an immutable value', 'rebind to a literal, then assign';
    is $v, 42, 'the value is unchanged';
}

{
    my $v = 1;
    $v := "str";
    throws-like { $v = 5 }, X::AdHoc,
        message => 'Cannot assign to an immutable value', 'rebind to a Str literal, then assign';
}

{
    my $y := 5;
    my $z = 10;
    $y := $z;
    lives-ok { $y = 3 }, 'rebind from an immutable value to a container, then assign';
    is "$y $z", "3 3", 'the assignment wrote through to the new container';
}

{
    my $a = 1;
    my $b = 2;
    $a := $b;
    $a = 7;
    is $b, 7, 'rebind to another variable keeps writing through';
}
