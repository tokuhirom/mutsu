use Test;

# `with @container { ... }` topicalizes the container by alias (same as
# `given`): a container *mutation* through `$_` (`.push`, element assign)
# propagates to the source. The topic is read-only, so `$_ = ...` (whole
# reassign) must fail. A bare scalar variable (`with $x`) aliases `$x` rw.
# `with` only runs the body when the topic is defined.

plan 13;

# --- container mutation through the topic propagates ----------------------
{
    my @a = 1, 2, 3;
    with @a { .push(4) }
    is @a.gist, '[1 2 3 4]', 'with @a { .push } propagates';
}
{
    my @a = 1, 2, 3;
    with @a { $_[0] = 99 }
    is @a.gist, '[99 2 3]', 'with @a { $_[0] = v } propagates';
}
{
    my %h = a => 1;
    with %h { $_<b> = 2 }
    is %h.sort.gist, '(a => 1 b => 2)', 'with %h { $_<k> = v } propagates';
}
{
    my %h = a => 1;
    with %h { .{'b'} = 2 }
    is %h.sort.gist, '(a => 1 b => 2)', 'with %h { .{k} = v } propagates';
}

# --- the topic is read-only: whole reassign must die ----------------------
{
    my @a = 1, 2;
    dies-ok { with @a { $_ = (7, 8, 9) } },
        'with @container { $_ = ... } dies (read-only topic)';
}

# --- a bare scalar variable topic is rw (aliases the variable) ------------
{
    my $x = 5;
    with $x { $_ = 99 }
    is $x, 99, 'with $x { $_ = v } is rw and propagates to $x';
}

# --- read-only with leaves the source unchanged and still reads -----------
{
    my @a = 1, 2, 3;
    my $sum = 0;
    with @a { $sum += .sum }
    is @a.gist, '[1 2 3]', 'read-only with leaves the source unchanged';
    is $sum, 6, 'read-only with still reads the topic';
}

# --- with only runs when the topic is defined -----------------------------
{
    my $u;
    my $ran = 0;
    with $u { $ran = 1 }
    is $ran, 0, 'with undefined scalar skips the body';
}

# --- a defined (even empty) array always runs with / never runs without ---
{
    my @e;
    my $ran = 0;
    with @e { $ran = 1 }
    is $ran, 1, 'with defined (empty) array runs the body';
}
{
    my @e;
    my $ran = 0;
    without @e { $ran = 1 }
    is $ran, 0, 'without defined array does not run';
}

# --- nested with keeps inner/outer topics distinct ------------------------
{
    my @outer = 1, 2;
    my @inner = 10, 20;
    with @outer {
        with @inner { .push(30) }
        .push(3);
    }
    is @outer.gist, '[1 2 3]', 'nested with: outer push propagates';
    is @inner.gist, '[10 20 30]', 'nested with: inner push propagates';
}
