use Test;

# A pointy parameter of a `with` / `without` block over a non-variable
# condition binds the once-evaluated condition the way a signature parameter
# does: `Nil` stays `Nil`, the parameter is readonly, and a `$` parameter
# holds a list as an item (#18390).

plan 10;

{
    my $got;
    without Nil -> $s { $got = $s.raku }
    is $got, 'Nil', 'without Nil -> $s binds Nil, not Any';
}

{
    my $got;
    without Failure.new -> $f { $got = $f.^name }
    is $got, 'Failure', 'without binds a Failure as itself';
}

{
    sub foo { 42 }
    my $got;
    with foo() -> $x { $got = $x }
    is $got, 42, 'with a call result -> $x';
}

{
    $_ = 'outer';
    my $got;
    with 7 -> $x { $got = "$x $_" }
    is $got, '7 outer', 'an explicit signature leaves the outer topic alone';
}

{
    my ($elems, $var);
    with (1, 2) -> $s { my @a = $s; $elems = @a.elems; $var = $s.VAR.^name }
    is $elems, 1, 'a list bound to a $ parameter stays one item';
    is $var, 'Scalar', 'and the parameter is a Scalar';
}

throws-like { with 5 -> $x { $x = 6 } }, Exception,
    'the parameter is readonly';

{
    my $got;
    with 'a' -> $x is copy { $x ~= 'b'; $got = $x }
    is $got, 'ab', 'is copy is writable';
}

{
    my $got;
    with (1, 2, 3) -> ($a, $b, $c) { $got = $c }
    is $got, 3, 'a sub-signature still destructures';
}

{
    my @r = do with 4 -> $x { $x * 2 };
    is @r.join, '8', 'the block value is the do value';
}
