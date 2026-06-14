use Test;

# `given @container { ... }` topicalizes the container by alias: a container
# *mutation* through `$_` (`.push`, element assign) propagates to the source.
# The topic is read-only, so `$_ = ...` (whole reassign) must fail — except a
# bare scalar variable (`given $x`), which aliases `$x` rw.

plan 12;

# --- container mutation through the topic propagates ----------------------
{
    my @a = 1, 2, 3;
    given @a { .push(4) }
    is @a.gist, '[1 2 3 4]', 'given @a { .push } propagates';
}
{
    my @a = 1, 2, 3;
    given @a { $_[0] = 99 }
    is @a.gist, '[99 2 3]', 'given @a { $_[0] = v } propagates';
}
{
    my %h = a => 1;
    given %h { $_<b> = 2 }
    is %h.sort.gist, '(a => 1 b => 2)', 'given %h { $_<k> = v } propagates';
}

# --- the topic is read-only: whole reassign must die ----------------------
{
    my @a = 1, 2;
    dies-ok { given @a { $_ = (7, 8, 9) } },
        'given @container { $_ = ... } dies (read-only topic)';
}
{
    dies-ok { given 42 { $_ = 99 } },
        'given <literal> { $_ = ... } dies (read-only topic)';
}

# --- a bare scalar variable topic is rw (aliases the variable) ------------
{
    my $x = 5;
    given $x { $_ = 99 }
    is $x, 99, 'given $x { $_ = v } is rw and propagates to $x';
}

# --- read-only given leaves the source unchanged and still reads ----------
{
    my @a = 1, 2, 3;
    my $sum = 0;
    given @a { $sum += .sum }
    is @a.gist, '[1 2 3]', 'read-only given leaves the source unchanged';
    is $sum, 6, 'read-only given still reads the topic';
}

# --- given/when dispatch is unaffected ------------------------------------
{
    my $r = do given 5 {
        when * > 3 { "big" }
        default { "small" }
    };
    is $r, 'big', 'given/when dispatch still works';
}
{
    my @a = <a b c>;
    my @seen;
    given @a { @seen.push(.elems) }
    is @seen.gist, '[3]', 'given @a topicalizes the whole array (1 item)';
}

# --- nested given keeps inner/outer topics distinct -----------------------
{
    my @outer = 1, 2;
    my @inner = 10, 20;
    given @outer {
        given @inner { .push(30) }
        .push(3);
    }
    is @outer.gist, '[1 2 3]', 'nested given: outer push propagates';
    is @inner.gist, '[10 20 30]', 'nested given: inner push propagates';
}
