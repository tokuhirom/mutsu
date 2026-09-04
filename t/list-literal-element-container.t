use Test;

# A `List` literal holds the CONTAINER of each element that has one, so a later
# alias of a list element reaches the source. That already worked when the
# element expression was a scalar variable; an array or hash ELEMENT has no
# source *name* for the `WrapVarRef` tag the list compile used, so the list
# stored a dereferenced copy and the alias refused the write with
# "Cannot modify an immutable Int".

plan 15;

# --- 1. an element alias through a list literal writes through --------------
{
    my @a = 1, 2;
    my (\p, \q) := (@a[0], @a[1]);
    p = 9;
    is-deeply @a, [9, 2], 'a destructured list-literal element aliases the array element';
    q = 8;
    is-deeply @a, [9, 8], 'and so does the second one';
}
{
    my @a = 1, 2;
    my $l := (@a[0], @a[1]);
    my \r := $l[0];
    r = 9;
    is-deeply @a, [9, 2], 'aliasing the list element afterwards reaches the array too';
}
{
    my @a = 1, 2;
    my $l := (@a[0],);
    $l[0] = 9;
    is-deeply @a, [9, 2], 'assigning through the list element writes through';
}
{
    my %h = a => 1;
    my (\p) := (%h<a>,);
    p = 9;
    is %h<a>, 9, 'a hash element behaves the same way';
}
{
    my @a = 1, 2;
    my \idx = 1;
    my (\p) := (@a[idx],);
    p = 9;
    is-deeply @a, [1, 9], 'a computed index aliases its element too';
}

# --- 2. the scalar-variable case it was modelled on (control) ---------------
{
    my $x = 1;
    my $l := ($x, 6);
    my \a := $l[0];
    a = 10;
    is $x, 10, 'control: a scalar-variable element still aliases its container';
}

# --- 3. ... while everything a list literal RENDERS or COPIES is unchanged --
{
    my @a = 1, 2;
    is-deeply (@a[0], @a[1]).raku, '(1, 2)', 'the list renders its values, not its cells';
    is (@a[0], @a[1]).WHAT.gist, '(List)', 'and it is still a List';
    is (@a[0], @a[1]).elems, 2, 'with the right length';
}
{
    my @a = 1, 2;
    my @b = (@a[0], @a[1]);
    @b[0] = 9;
    is-deeply @a, [1, 2], 'assigning the list into an array COPIES, as raku does';
    is-deeply @b, [9, 2], 'and the copy is independent';
}
{
    my @a = 1, 2;
    sub f(*@x) { @x }
    is-deeply f(@a[0], @a[1]), [1, 2], 'a slurpy parameter still receives values';
}
{
    my @a = 1, 2;
    is-deeply [(@a[0], @a[1])], [1, 2], 'a bracket array decontainerizes';
}

# --- 4. ... and a missing element is not vivified by CONSTRUCTING the list --
{
    my @a;
    my $l := (@a[5],);
    is-deeply @a, [], 'building the list does not grow the array';
    # (Writing THROUGH such an alias should vivify — rakudo gives
    #  `[Any, Any, Any, Any, Any, 9]` — but that is the direct
    #  `my \p := @a[5]; p = 9` gap, which fails the same way and is tracked in
    #  todo/tickets/alias-to-a-missing-element-does-not-vivify.md.)
}
