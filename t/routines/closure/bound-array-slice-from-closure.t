use Test;

# `my @s := @a[1, 2]` makes @s a fixed-arity view over @a's elements: a later
# whole-array assignment writes through them. The "this is a bound slice"
# mark lives on the bound value itself (ADR-0097), not under a name-keyed env
# marker of the declaring frame, so a closure or routine that assigns to the
# captured @s writes through too (it used to replace the binding instead).

plan 10;

{
    my @a = 1 .. 5;
    my @s := @a[1, 2];
    my $c = { @s = 8, 9, 10 };
    $c();
    is-deeply @a, [1, 8, 9, 4, 5], 'a closure assigning to a bound slice writes through';
    is ~@s, '8 9', 'the bound slice keeps its arity';
}

{
    my @a = 1 .. 5;
    my @s := @a[1, 2];
    sub h { @s = 7, 7 }
    h();
    is-deeply @a, [1, 7, 7, 4, 5], 'a routine assigning to a bound slice writes through';
}

{
    my @a = 1 .. 5;
    my @s := @a[1, 2];
    my $c = { @s = (6,) };
    $c();
    is @a[1], 6, 'a short RHS fills the first slot';
    is @a[2].WHAT, Any, 'and pads the rest with Any';
}

{
    my %h = a => 1, b => 2, c => 3;
    my @s := %h<a b>;
    { @s = 10, 20, 30 }();
    is-deeply %h, %(a => 10, b => 20, c => 3), 'a closure writes through a bound hash slice';
}

{
    my @a = 1 .. 5;
    for ^2 -> $i {
        my @t := @a[3, 4];
        @t = $i, $i;
    }
    is-deeply @a, [1, 2, 3, 1, 1], 'a fresh bound slice per iteration';
}

{
    my @a = 1 .. 3;
    my @s := @a[0, 1];
    my @copy = @s;
    @copy = 9, 9, 9;
    is-deeply @a, [1, 2, 3], 'a copy of a bound slice is a plain array';
    is-deeply @copy, [9, 9, 9], 'which takes the whole RHS';
}

{
    my @a = 1 .. 3;
    my @b := @a;
    @b = 4, 5, 6, 7;
    is-deeply @a, [4, 5, 6, 7], 'binding a whole array is not a slice';
}
