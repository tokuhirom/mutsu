use Test;

plan 16;

# A declaring list assignment `my ($x, $y) = RHS` evaluates to its LHS after
# the assignment -- the declared targets -- not to the RHS (#9342).

{
    my $r = (my ($x, $y) = 1, 2, 3);
    is $r.raku, '$(1, 2)', 'value is the LHS list, not the RHS';
    $r[0] = 9;
    is $x, 9, 'the value holds the declared containers themselves';
}

{
    my $r = (my ($a, @b, %h) = 1, 2, 3);
    is $r.raku, '$(1, [2, 3], {})', 'array and hash targets appear as containers';
}

{
    my $r = (my Int ($i, $j) = 1);
    is $r.raku, '$(1, Int)', 'a typed target that ran out of values reports its default';
}

{
    my $r = (my (\q, $w) = 5, 6);
    is $r.raku, '$(5, 6)', 'a sigilless target is included';
}

{
    my $r = (my ($u, "foo") = 5, "foo");
    is $r.raku, '$(5, "foo")', 'a literal postconstraint element yields its value';
}

# An infinite RHS must not leak out as the statement's value: sinking it would
# force the infinite list.
{
    my ($x, $y) = (1..*).map(* + 1);
    pass 'infinite mapped RHS: the next statement runs';
    is $x, 2, '... $x';
    is $y, 3, '... $y';
}

{
    my ($x, $y) = 1 xx *;
    pass 'infinite xx RHS: the next statement runs';
    is "$x $y", '1 1', '... values';
}

{
    my ($x, $y) = (1..*) Z (1..*);
    pass 'infinite Z RHS: the next statement runs';
    is $x.raku, '$(1, 1)', '... $x';
    is $y.raku, '$(2, 2)', '... $y';
}

{
    my $r = (my ($x, $y) = (1..*).map(* + 1));
    is $r.raku, '$(2, 3)', 'value of an infinite-RHS assignment is the finite LHS';
}

{
    my $out = '';
    if my ($a, $b) = 1, 2 { $out = "$a $b" }
    is $out, '1 2', 'destructuring declaration still works as a condition';
}
