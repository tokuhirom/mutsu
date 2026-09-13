use Test;

# Itemization is a property of the CONTAINER, not of the object, and `.clone`
# copies the object -- so the clone comes back de-itemized. mutsu carried the
# `ArrayKind`'s itemization through verbatim, so `my @a = $v.clone` nested the
# list instead of flattening it. Crane's `Crane::In.in(container, @path) =
# $value.clone` depends on exactly that.

plan 8;

{
    my $v = <a b c>;
    is $v.raku, '$("a", "b", "c")', 'the itemized source renders itemized';
    is $v.clone.raku, '("a", "b", "c")', 'its clone does not';
}

{
    my $v = <a b c>;
    my @a = $v.clone;
    is-deeply @a, ['a', 'b', 'c'], 'assigning the clone to an array flattens it';
}

{
    # `= $v` (no clone) keeps the itemization, so it does NOT flatten.
    my $v = <a b c>;
    my @a = $v;
    is @a.elems, 1, 'assigning the itemized value itself still nests';
}

{
    my $a = [1, 2];
    is $a.raku, '$[1, 2]', 'an itemized Array renders itemized';
    is $a.clone.raku, '[1, 2]', 'and its clone does not';
}

{
    # The clone is still an independent copy of the elements.
    my $v = [1, 2];
    my $c = $v.clone;
    $c[0] = 9;
    is-deeply $v, $[1, 2], 'the clone is independent of the source';
}

{
    # A plain (non-itemized) array clones unchanged.
    my @a = 1, 2;
    is @a.clone.raku, '[1, 2]', 'a plain array clones unchanged';
}
