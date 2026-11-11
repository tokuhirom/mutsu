use Test;

# `$a[$i]` reads of a hole, of an index past the end and of a `:=`-bound
# element answer from the element-read fast path (#17429). These pin what each
# of them reads as, and the shapes that keep their own answer.

plan 14;

{
    my $d = [];
    $d[3] = 1;
    is-deeply $d[1], Any, 'a hole in a $-held Array reads as Any';
    is-deeply $d[9], Any, '... and so does an index past its end';
    is $d[3], 1, '... while the stored element reads back';
}

{
    my @a;
    @a[2] = 5;
    is-deeply @a[0], Any, 'a hole in an @-array reads as Any';
    is-deeply @a[7], Any, '... and so does an index past its end';
}

{
    my $l = (1, 2);
    is-deeply $l[5], Nil, 'past the end of a List is Nil';
}

{
    my @b = 1, Any, Int;
    is-deeply @b[1], Any, 'a stored Any reads back';
    is-deeply @b[2], Int, 'a stored type object reads back';
}

{
    my $x = 7;
    my @c;
    @c[0] := $x;
    $x = 8;
    is @c[0], 8, 'a bound element reads its source';
}

{
    my Int @t;
    @t[3] = 1;
    is-deeply @t[0], Int, 'a typed array hole reads as its type';
    is-deeply @t[9], Int, '... and so does an index past its end';
}

{
    my @dd is default(42);
    @dd[3] = 1;
    is @dd[0] + @dd[9], 84, 'an is default array reads its default';
}

{
    my $i = $[1, 2];
    is-deeply ($i[1], $i[5]), (2, Any), 'an itemized Array indexes like the Array';
}

{
    my @h;
    @h[1] = Nil;
    is-deeply @h[1], Any, 'an element assigned Nil reads as Any';
}
