use Test;

# An assignment expression evaluates to the assigned scalar's container, so a
# method that mutates its receiver writes that scalar (#18824).

plan 9;

{
    my $x = "ab";
    ($x = "cd").subst-mutate(/c/, "Z");
    is $x, "Zd", '($x = EXPR).subst-mutate writes $x';
}

{
    ($ = "1234567").subst-mutate(:g, /../, "XX");
    is $/.elems, 3, '($ = EXPR).subst-mutate on an anonymous state scalar';
}

{
    my $r = ($ = 1234567).subst-mutate(:g, /../, "XX");
    is $r.elems, 3, 'Int receiver through the anonymous scalar';
}

{
    my $x;
    is ($x = "cd").chars, 2, 'a non-mutating method sees the assigned value';
    is $x, "cd", '... and the assignment happened once';
}

{
    my $x = [1];
    ($x = [5]).push(6);
    is-deeply $x, [5, 6], '($x = [..]).push grows the assigned array';
}

{
    my @a;
    (@a = 1, 2).push(3);
    is-deeply @a, [1, 2, 3], '(@a = ...).push';
}

{
    sub f { ($ = "ab").subst-mutate(/a/, "Z"); ~$/ }
    is f(), "a", 'each call reassigns the anonymous scalar';
    is f(), "a", '... a second time too';
}
