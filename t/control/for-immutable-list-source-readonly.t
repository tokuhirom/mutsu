use Test;

# A List's elements are values, not Scalar containers, so a `for` alias of one
# is not assignable -- unless the item happens to BE a container (a list built
# from variables). mutsu used to promote the List's slots to cells, or rebuild
# the List from the loop variable, and so wrote into an immutable List (#10349).
# Every expectation here was measured against rakudo.

plan 37;

# --- The four statements of the report ---------------------------------------
{
    my @l := (1, 2);
    throws-like { $_ = 42 for @l }, X::AdHoc, 'topic of a bound List is read-only';
    is-deeply @l, (1, 2), '... and the List is untouched';
}
{
    my $l = (1, 2);
    throws-like { $_ = 42 for $l.list }, X::AdHoc, 'topic of $scalar.list of a List is read-only';
    is-deeply $l, (1, 2), '... and the List is untouched';
}
{
    my @l := (1, 2);
    throws-like { $_ = 42 for @l.list }, X::AdHoc, 'topic of @bound.list is read-only';
    is-deeply @l, (1, 2), '... and the List is untouched';
    throws-like { for @l.kv -> \k, \v { v = 42 } }, X::Assignment::RO,
        'a sigilless .kv value of a List is not assignable';
    is-deeply @l, (1, 2), '... and the List is untouched';
}

# --- Other ways to alias an item of a List -----------------------------------
{
    my @l := (1, 2);
    throws-like { for @l -> $x is rw { $x = 5 } }, X::Parameter::RW,
        'an `is rw` parameter cannot bind a List item';
    throws-like { for @l -> $x is rw { } }, X::Parameter::RW,
        '... the bind fails even when the body never assigns';
    throws-like { for @l <-> $x { $x = 5 } }, X::Parameter::RW, 'a `<->` parameter cannot bind it either';
    throws-like { for @l -> \x { x = 5 } }, X::Assignment::RO, 'a sigilless parameter binds, then the write fails';
    lives-ok { for @l -> \x { } }, '... but binding it without writing is fine';
    throws-like { for @l.kv -> $k, $v is rw { $v = 5 } }, X::Parameter::RW,
        'an `is rw` .kv value cannot bind a List item';
    throws-like { for @l -> $x, $y is rw { $y = 5 } }, X::Parameter::RW,
        'nor a chunked `is rw` parameter';
    throws-like { $_ = 5 for @l.values }, X::AdHoc, 'the topic of .values';
    throws-like { $_ = 5 for @l.reverse }, X::AdHoc, 'the topic of .reverse';
    throws-like { for @l -> $x { $x = 5 } }, X::AdHoc, 'a plain named parameter is read-only anyway';
    is-deeply @l, (1, 2), 'none of the above wrote into the List';
}

# --- Bindings that do not write ----------------------------------------------
{
    my @l := (1, 2);
    my $sum = 0;
    $sum += $_ for @l;
    is $sum, 3, 'reading a List through the topic works';
    for @l -> $x is copy { $x = 9 }
    is-deeply @l, (1, 2), 'an `is copy` parameter owns its own container';
    for @l.kv -> $k, $v is copy { $v = 9 }
    is-deeply @l, (1, 2), 'so does an `is copy` .kv value';
    for @l -> $x, $y is copy { $y = 9 }
    is-deeply @l, (1, 2), 'and a chunked one';
}

# --- A List parameter and a scalar-held List ---------------------------------
{
    sub bump(@l) { $_ = 5 for @l }
    throws-like { bump((1, 2)) }, X::AdHoc, 'a `@` parameter bound to a List is read-only';
    my $s = (1, 2);
    throws-like { $_ = 9 for @$s }, X::AdHoc, 'the deref of a scalar-held List';
    throws-like { for $s.list -> $x is rw { $x = 9 } }, X::Parameter::RW, '... and an `is rw` parameter over it';
    throws-like { for $s.list.kv -> \k, \v { v = 9 } }, X::Assignment::RO, '... and its sigilless .kv value';
    is-deeply $s, (1, 2), 'the scalar-held List is untouched';
}

# --- Items that ARE containers stay writable ---------------------------------
{
    my $a = 1;
    my $b = 2;
    my @l := ($a, $b);
    $_ = 9 for @l;
    is "$a $b", '9 9', 'the topic aliases the variables a List was built from';
    for @l -> $x is rw { $x = 7 }
    is "$a $b", '7 7', 'so does an `is rw` parameter';
    for @l.kv -> \k, \v { v = 5 }
    is "$a $b", '5 5', 'and a sigilless .kv value';
    my $c = 1;
    my @mixed := ($c, 2);
    throws-like { $_ = 9 for @mixed }, X::AdHoc, 'a mixed List fails at its first bare item';
    is $c, 9, '... after having written the container before it';
}

# --- Mutable sources are unaffected ------------------------------------------
{
    my @a = 1, 2;
    $_ = 9 for @a;
    is-deeply @a, [9, 9], 'the topic of a real Array is written';
    for @a -> $x is rw { $x++ }
    is-deeply @a, [10, 10], 'an `is rw` parameter over an Array is written';
    for @a.kv -> $k, $v is rw { $v = $k }
    is-deeply @a, [0, 1], 'and its .kv value';
    my $r = [1, 2];
    $_ .= Str for @$r;
    is-deeply $r, ['1', '2'], 'and the deref of a scalar-held Array';
}

