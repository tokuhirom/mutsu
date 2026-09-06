use Test;

# A plain nested block does NOT bind a topic of its own: inside
# `given $x { if COND { $_ = 'new' }; say $_ }` the `if` body's `$_` IS the
# enclosing topic, so an assignment through it must survive the block. mutsu's
# block-scope restore used to drop every `$_` write on exit, so the very next
# statement read the pre-block value — while the write still reached `$x`
# through the topic's container, which made the two disagree. Reduced from
# `Template6`'s `Context.get-template-block`, whose
# `given $template { if $_ !~~ Callable { $_ = $.parser.compile($_) }; %blocks{...} = $_ }`
# cached the raw template text instead of the compiled closure.

plan 15;

{
    my $x = 'old';
    given $x {
        if True { $_ = 'new' }
        is $_, 'new', 'given: $_ assigned in a nested if is visible afterwards';
    }
    is $x, 'new', 'given: the topic source variable is updated too';
}

{
    my $x = 'old';
    given $x {
        { $_ = 'new' }
        is $_, 'new', 'given: $_ assigned in a bare nested block is visible afterwards';
    }
}

{
    my $x = 'old';
    with $x {
        if True { $_ = 'new' }
        is $_, 'new', 'with: $_ assigned in a nested if is visible afterwards';
    }
    is $x, 'new', 'with: the topic source variable is updated too';
}

{
    my $x = 'old';
    for $x {
        if True { $_ = 'new' }
        is $_, 'new', 'for: $_ assigned in a nested if is visible afterwards';
    }
    is $x, 'new', 'for: the topic source variable is updated too';
}

# An `is rw` `$_` parameter is the same shape.
sub bump($_ is rw) {
    if True { $_ = 'new' }
    $_
}
{
    my $x = 'old';
    is bump($x), 'new', 'rw $_ parameter: the nested write is visible in the sub';
    is $x, 'new', 'rw $_ parameter: the caller sees the write';
}

# A block that DOES bind its own topic still keeps it to itself.
{
    my $x = 'outer';
    given $x {
        for 'inner1', 'inner2' { }
        is $_, 'outer', 'an inner for does not leak its topic to the enclosing given';
    }
    is $x, 'outer', 'and does not touch the topic source variable';
}

{
    my $x = 'outer';
    given $x {
        given 'inner' { }
        is $_, 'outer', 'an inner given restores the enclosing topic';
    }
}

{
    for ('outer',) {
        if True {
            for 'inner' { }
        }
        is $_, 'outer', 'a for nested two blocks deep still restores the topic';
    }
}

# The mainline topic is not disturbed by a block that never touches it.
$_ = 'mainline';
{ my $unused = 1 }
is $_, 'mainline', 'a block that never assigns $_ leaves the topic alone';

{
    my $x = 'old';
    given $x {
        if False { $_ = 'never' }
        is $_, 'old', 'an untaken branch does not change the topic';
    }
}
