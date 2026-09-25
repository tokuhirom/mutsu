use Test;

# An element `temp` (`temp @a[i] = v`, `temp $t[1]<k>[1] = v`) saves and
# restores just that element, in place, as rakudo does: the rest of the
# container keeps any write made inside the scope, and a name bound to part of
# it still sees the same container afterwards (#9434).

plan 16;

{
    my $t = ["x", { key => ["y", 42] }];
    my $alias := $t[1]<key>;
    { temp $t[1]<key>[1] = 23; is $alias[1], 23, 'the temporized write is visible through a bound alias' }
    is $alias[1], 42, 'multi-level: the restore reaches a name bound to the inner array';
    ok $alias === $t[1]<key>, 'multi-level: the inner array keeps its identity';
}

{
    my @c = [1, [2, 3]];
    { temp @c[1][0] = 9; @c[0] = 5 }
    is @c.raku, '[5, [2, 3]]', 'multi-level: a write elsewhere in the container survives the restore';
}

{
    my $t = [];
    { temp $t[1]<k>[1] = 3; is $t.raku, '$[Any, {:k($[Any, 3])}]', 'a missing path is vivified by the assignment' }
    is $t.raku, '$[Any, {:k($[Any, Any])}]', 'the vivified element is restored to Any';
}

{
    my %h;
    { temp %h<a><b> = 1 }
    is %h.raku, '{:a(${:b(Any)})}', 'multi-level hash path: restored to Any, vivified path kept';
}

{
    my @b = [1, [2]];
    { temp @b[1][5] = 9 }
    is @b[1].elems, 6, 'an element past the end is restored to Any; the array stays extended';
}

{
    my @n = [1, [2, 3]];
    { temp @n[1][*-1] = 9; is @n.raku, '[1, [2, 9]]', 'a WhateverCode index names the last element' }
    is @n.raku, '[1, [2, 3]]', 'and that element is restored';
}

{
    my @b = 1, 2;
    { temp @b[0] = 9; @b[1] = 7 }
    is @b.raku, '[1, 7]', 'single-level: a write to another element survives the restore';
}

{
    my @c = 1, 2;
    my $x := @c[0];
    { temp @c[0] = 9 }
    is $x, 1, 'single-level: a name bound to the element sees the restore';
}

{
    my %h;
    { temp %h<a> = 1 }
    is %h.raku, '{:a(Any)}', 'single-level hash: a new key is restored to Any, not deleted';
}

{
    my @a = 1, 2;
    { temp @a[5] = 1 }
    is @a.raku, '[1, 2, Any, Any, Any, Any]', 'single-level: an element past the end is restored to Any';
}

{
    my %g = a => { b => 1 };
    sub f { temp %g<a><b> = 2; %g<a><b> }
    is f(), 2, 'inside the routine the element holds the temporized value';
    is %g<a><b>, 1, 'the routine exit restores the element';
}
