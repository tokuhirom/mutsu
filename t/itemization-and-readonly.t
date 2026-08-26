use Test;

plan 37;

# ---------------------------------------------------------------------------
# A readonly marking must not survive the binding that created it.
#
# `my $zz := 42` marks the bare name `zz` immutable. `readonly_vars` is keyed by
# bare name and is not unwound at block exit, so a later, unrelated `my $zz :=
# $y` used to inherit that marking and reject `$zz = 23`. A declaration now
# resets the name's readonly state and the bind re-establishes it.
# ---------------------------------------------------------------------------

{
    try { my $zz := 42; $zz = 23 };
    my $y = 1;
    my $zz := $y;
    lives-ok { $zz = 23 }, 'a fresh := binding is writable after an aborted one';
    is $y, 23, 'and it writes through to the bound source';
}

{
    if 1 { my $bb := 42; }
    my $y = 1;
    my $bb := $y;
    $bb = 5;
    is $y, 5, 'an if-branch := to a literal does not poison a later same name';
}

{
    for 1..1 { my $cc := 42; }
    my $y = 1;
    my $cc := $y;
    $cc = 6;
    is $y, 6, 'a for-body := to a literal does not poison a later same name';
}

{
    { my $dd := 42; }
    my $y = 1;
    my $dd := $y;
    $dd = 7;
    is $y, 7, 'a bare-block := to a literal does not poison a later same name';
}

# The marking itself still has to work.
{
    my $lit := 5;
    dies-ok { $lit = 6 }, 'a scalar bound to a literal stays immutable';
    is (my $expr := 7), 7, 'a literal bind still evaluates to the bound value';
    my $src = 3;
    is (my $alias := $src), 3, 'a variable bind still evaluates to the bound value';
}

{
    my constant PI = 3.14;
    dies-ok { PI = 5 }, 'a constant is still immutable';
}

{
    my @bound := (1, 2, 3);
    dies-ok { @bound[0] = 9 }, 'an element of a bound immutable List is still rejected';
}

# ---------------------------------------------------------------------------
# A `$_` pointy parameter on a conditional is a FRESH topic scope, not an
# ordinary lexical declaration: it must not survive the block.
# ---------------------------------------------------------------------------

{
    $_ = 1;
    my $seen;
    if 5 -> $_ { $seen = $_ }
    is $seen, 5, 'if COND -> $_ binds the topic inside the block';
    is $_, 1, 'if COND -> $_ restores the enclosing topic';
}

{
    $_ = 1;
    my $seen;
    if 0 { } else -> $_ { $seen = $_ }
    is $seen, 0, 'else -> $_ binds the topic inside the block';
    is $_, 1, 'else -> $_ restores the enclosing topic';
}

{
    $_ = 1;
    my $seen;
    unless 0 -> $_ { $seen = $_ }
    is $seen, 0, 'unless COND -> $_ binds the topic inside the block';
    is $_, 1, 'unless COND -> $_ restores the enclosing topic';
}

{
    $_ = 1;
    my $seen;
    if 0 { } elsif 7 -> $_ { $seen = $_ }
    is $seen, 7, 'elsif COND -> $_ binds the topic inside the block';
    is $_, 1, 'elsif COND -> $_ restores the enclosing topic';
}

{
    $_ = 1;
    my $seen;
    with 5 -> $_ { $seen = $_ }
    is $seen, 5, 'with LITERAL -> $_ binds the topic inside the block';
    is $_, 1, 'with LITERAL -> $_ restores the enclosing topic';
}

{
    $_ = 1;
    my $x = 5;
    my $seen;
    with $x -> $_ { $seen = $_ }
    is $seen, 5, 'with VAR -> $_ binds the topic inside the block';
    is $_, 1, 'with VAR -> $_ restores the enclosing topic';
}

{
    $_ = 1;
    my $seen;
    with Nil -> $_ { } else -> $_ { $seen = 'else' }
    is $seen, 'else', 'with/else -> $_ runs the else branch';
    is $_, 1, 'with ... else -> $_ restores the enclosing topic';
}

{
    $_ = 1;
    my $seen;
    if 5 -> $v { $seen = $v }
    is $seen, 5, 'a named pointy parameter still binds';
    is $_, 1, 'and leaves the topic alone';
}

{
    $_ = 3;
    if 0 -> $_ { }
    is $_, 3, 'an untaken if COND -> $_ leaves the topic alone';
}

# ---------------------------------------------------------------------------
# A `for` over a list built only out of literals aliases each item directly:
# there is no container, so `$_ = ...` is rejected and `.VAR` reports the
# item's own type.
# ---------------------------------------------------------------------------

dies-ok { for 1, 2 { $_ = 5 } }, 'the topic of `for 1, 2` is immutable';
dies-ok { for (1, 2) { $_ = 5 } }, 'the topic of `for (1, 2)` is immutable';
dies-ok { for <a b> { $_ = 5 } }, 'the topic of `for <a b>` is immutable';
dies-ok { my %h = a => 1; for %h.keys { $_ = 5 } }, 'the topic of `for %h.keys` is immutable';

{
    my @names;
    for 1, 2 { @names.push($_.VAR.^name); last }
    is @names[0], 'Int', '`for 1, 2` reports the item type from .VAR';
}

{
    my %h = a => 1;
    my @names;
    for %h.keys { @names.push($_.VAR.^name); last }
    is @names[0], 'Str', '`for %h.keys` reports the item type from .VAR';
}

# A container source keeps a writable topic. Identity is asserted by OBSERVING
# the mutation through the source, not with `=:=` (which compares equal Ints in
# distinct containers as identical in mutsu).
{
    my @a = 1, 2;
    for @a { $_ = 5 }
    is @a.join(','), '5,5', '`for @a` still writes back through the element';
}

{
    my @a = 1, 2;
    my @names;
    for @a { @names.push($_.VAR.^name); last }
    is @names[0], 'Scalar', '`for @a` still reports Scalar from .VAR';
}

{
    my $p = 1;
    my $q = 2;
    for $p, $q { $_ = 9 }
    is "$p $q", '9 9', 'a list of scalar variables still aliases their containers';
}

# A `.VAR` read on a bare topic must not be answered from a cached meta object
# built for a *container* topic of the same name (both live under the key `_`).
{
    my @a = 1, 2;
    my @names;
    for @a { @names.push($_.VAR.^name); last }
    for 1, 2 { @names.push($_.VAR.^name); last }
    is @names.join(','), 'Scalar,Int', '.VAR is not answered from a stale per-name cache';
}
