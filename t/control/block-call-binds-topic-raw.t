use Test;

# A bare block called with an argument binds its implicit `$_` RAW to that
# argument, so `my $b = { $_ = 9 }; $b($v)` writes the CALLER's `$v`.
#
# `todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` section B
# producer 3. The topic machinery could already alias (`for $v { $_ = 9 }`
# worked); the gap was the block-CALL path, on both halves of the argument: a
# plain scalar variable (no container was handed over) and a subscript (whose
# element container had no producer for a callee with no compile-time name).

plan 18;

{
    my $v = 1;
    my $b = { $_ = 9 };
    $b($v);
    is $v, 9, 'a block call aliases $_ to a scalar variable argument';
}

{
    my $v = 1;
    my $b = { $_ = 9 };
    $b.($v);
    is $v, 9, '...through the .() spelling too';
}

{
    my $v = 1;
    my &b = { $_ = 9 };
    b($v);
    is $v, 9, '...and through a &-sigil code variable';
}

{
    my $v = 1;
    my $b = { $_++ };
    $b($v);
    is $v, 2, 'a mutating method on the topic reaches the caller too';
}

{
    my @a = 1, 2, 3;
    my $b = { $_ = 9 };
    $b(@a[0]);
    is-deeply @a, [9, 2, 3], 'a block call aliases $_ to an array element argument';
}

{
    my %h = a => 1, b => 2;
    my $b = { $_ = 9 };
    $b(%h<a>);
    is-deeply %h, {a => 9, b => 2}, '...and to a hash element argument';
}

# The same missing producer, refusing instead of losing the write: an explicit
# `is rw` parameter of a callee with no compile-time name.

{
    my @a = 1, 2;
    my $b = -> $x is rw { $x = 9 };
    $b(@a[0]);
    is-deeply @a, [9, 2], 'an is-rw pointy block binds an array element argument';
}

{
    my @a = 1, 2;
    sub g($y is rw) { $y = 9 }
    my $r = &g;
    $r(@a[0]);
    is-deeply @a, [9, 2], 'an is-rw sub called through a code value binds one too';
}

{
    my @a = 1, 2;
    sub g($y is rw) { $y = 9 }
    &g(@a[0]);
    is-deeply @a, [9, 2], '...and through the &-sigil call spelling';
}

{
    my @a = 1, 2;
    class S { method take($y is rw) { $y = 9 } }
    S.new.take(@a[0]);
    is-deeply @a, [9, 2], 'an is-rw method parameter binds an array element argument';
}

{
    my %h = a => 1;
    class T { method take($y is rw) { $y = 9 } }
    T.new.take(%h<a>);
    is-deeply %h, {a => 9}, '...and a hash element argument';
}

# ... and an ordinary call is untouched.

{
    my $v = 1;
    my $b = { $_ };
    is $b($v), 1, 'a block that only reads the topic still reads it';
    is $v, 1, '...and leaves the caller alone';
}

{
    my $v = 1;
    sub f($x) { $x }
    is f($v), 1, 'a readonly parameter still binds by value';
    is $v, 1, '...and leaves the caller alone';
}

{
    my @a = 1, 2, 3;
    my $b = { $_ + 1 };
    is $b(@a[0]), 2, 'a subscript argument still reads as its value';
    is-deeply @a, [1, 2, 3], '...and an unwritten element is unchanged';
}

{
    # A literal argument has no container at all; assigning to the topic is an
    # error, not a silent write.
    my $b = { $_ = 9 };
    dies-ok { $b(7) }, 'a container-less argument still refuses a topic write';
}
