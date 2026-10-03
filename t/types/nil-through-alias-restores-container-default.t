use Test;

# A Nil assigned through an alias (an `is rw` parameter, a `for` alias, a `:=`
# binding, the topic) decays to the default of the container the alias writes
# into, not to the alias's own (absent) default (#9831). The default belongs to
# the container, like its `of`-type.

plan 16;

{
    my Int $x is default(42) = -1;
    for $x -> $v is rw { $v = Nil }
    is $x, 42, 'typed scalar through a for `is rw` alias';
}

{
    my $y is default(7) = 1;
    sub s($p is rw) { $p = Nil }
    s($y);
    is $y, 7, 'scalar through an `is rw` parameter';
}

{
    my @a is default(3) = 1;
    for @a -> $v is rw { $v = Nil }
    is-deeply @a, [3], 'array element through a for `is rw` alias';
}

{
    my @a is default(4) = 1, 2;
    $_ = Nil for @a;
    is-deeply @a, [4, 4], 'array elements through the topic';
}

{
    my Int @a is default(6) = 1;
    for @a -> $v is rw { $v = Nil }
    is-deeply @a, Array[Int].new(6), 'typed array element through a for alias';
}

{
    my %h is default(5) = a => 1;
    for %h.values -> $v is rw { $v = Nil }
    is %h<a>, 5, 'hash value through a for alias';
}

{
    my $z is default(9) = 1;
    my $r := $z;
    $r = Nil;
    is $z, 9, 'scalar through a `:=` binding';
}

{
    my $w is default(11) = 1;
    $_ = Nil for $w;
    is $w, 11, 'scalar through the topic';
}

{
    my @a is default(3) = 1, 2;
    my $r := @a[0];
    $r = Nil;
    is-deeply @a, [3, 2], 'array element through a `:=` binding';
}

{
    my @b is default(4) = 1;
    sub f($x is rw) { $x = Nil }
    f(@b[0]);
    is-deeply @b, [4], 'array element through an `is rw` parameter';
}

{
    my %h is default(5) = a => 1;
    sub g($x is rw) { $x = Nil }
    g(%h<a>);
    is %h<a>, 5, 'hash element through an `is rw` parameter';
}

{
    my $y is default(7) = 1;
    my $t;
    for $y <-> $v { $t = ($v = Nil) }
    is $t, 7, 'expression-position assignment yields the default';
    is $y, 7, 'expression-position assignment stores the default';
}

{
    my $z is default(8) = 1;
    for $z -> $v is rw { my &c = { $v = Nil }; c() }
    is $z, 8, 'through a closure capturing a for alias';
}

{
    my $u = 1;
    for $u -> $v is rw { $v = Nil }
    is $u.raku, 'Any', 'an undefaulted scalar still resets to Any';
}

{
    my @n = 1;
    for @n -> $v is rw { $v = Nil }
    is @n.raku, '[Any]', 'an undefaulted array element still resets to Any';
}
