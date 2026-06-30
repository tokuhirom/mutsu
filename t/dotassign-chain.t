use Test;

plan 11;

# Chained `.=` on a scalar (statement form): `$s .= uc .= flip` means
# `$s = $s.uc.flip` — each step's result feeds the next, assigned back once.
{
    my $s = "abc";
    $s .= uc .= flip;
    is $s, "CBA", 'chained .= on scalar (statement)';
}

{
    my $s = "abcd";
    $s .= uc .= flip .= lc;
    is $s, "dcba", 'triple-chained .= on scalar';
}

# Chained `.=` in expression context.
{
    my $s = "abc";
    ($s .= uc .= flip);
    is $s, "CBA", 'chained .= on scalar (expression)';
}

# Chained `.=` on an indexed array element writes back to the element.
{
    my @a = "Hello", "World";
    @a[0] .= lc .= flip;
    is @a[0], "olleh", 'chained .= on array element';
    is @a[1], "World", 'sibling element untouched';
}

{
    my @a = "a", "b", "c";
    @a[$_] .= uc .= flip for ^@a;
    is-deeply @a, ["A", "B", "C"], 'chained .= per-element in for-modifier';
}

# A single (non-chained) `.=` still works.
{
    my $s = "abc";
    $s .= uc;
    is $s, "ABC", 'single .= still works';
}

# Mixed chains preserve evaluation order.
{
    my $s = "Hello World";
    $s .= words .= elems;
    is $s, 2, 'chained .= producing a number';
}

# `.=` chain after a method with args.
{
    my $s = "a,b,c";
    $s .= split(",") .= elems;
    is $s, 3, 'chained .= with method args';
}

# A chain whose first step changes the type.
{
    my $x = "42";
    $x .= Int .= succ;
    is $x, 43, 'chained .= across a type change';
}

# `$_ .= ...` chained.
{
    $_ = "abc";
    $_ .= uc .= flip;
    is $_, "CBA", 'chained .= on the topic';
}
