use Test;

plan 20;

# A multi-dim subscript whose target is an EXPRESSION (a subscript chain rooted
# at a variable) used to be compiled to `MultiDimIndexAssignGeneric`, which
# mutated a detached copy of the target value and dropped the write whenever the
# chain had to autovivify a level. The chain is now carried to the VM as
# root-name + prefix, so the assignment lands in the real container.

{
    my %o;
    %o<inner>{1;2} = 5;
    is %o.raku, '{:inner(${"1" => ${"2" => 5}})}',
        'associative multi-dim through a hash-element chain autovivifies';
    is %o<inner><1><2>, 5, 'the leaf is reachable by the chained spelling';
}

{
    my %o;
    %o<a><b>{1;2} = 5;
    is %o.raku, '{:a(${:b(${"1" => ${"2" => 5}})})}',
        'a two-level chain prefix autovivifies both levels';
}

{
    my @a;
    @a[0]{1;2} = 5;
    is @a.raku, '[{"1" => ${"2" => 5}},]',
        'an array element autovivifies to the associative walk';
}

{
    my @a;
    @a[0]<k>{1;2} = 5;
    is @a.raku, '[{:k(${"1" => ${"2" => 5}})},]',
        'a mixed positional/associative chain prefix';
}

{
    my %o;
    %o<i>{1;2;3} = 5;
    is %o.raku, '{:i(${"1" => ${"2" => ${"3" => 5}}})}',
        'three associative dimensions under a chain prefix';
}

{
    my %o;
    my $k = 'i';
    %o{$k}{1;2} = 5;
    is %o.raku, '{:i(${"1" => ${"2" => 5}})}', 'a runtime chain key works too';
}

# Under 6.d an all-scalar associative multi-dim subscript is still a multislice,
# so the assignment distributes the RHS list element-wise: only `1` reaches the
# single leaf. The chain prefix must not change that.
{
    my %o;
    %o<inner>{1;2} = [1, 2, 3];
    is %o.raku, '{:inner(${"1" => ${"2" => 1}})}',
        'the 6.d multislice rule survives a chain prefix';
    is %o<inner>{1;2}.raku, '(1,)', 'the 6.d read is still a one-element List';
}

# An already-existing container behind the chain keeps working (this used to
# survive only because the popped value happened to share its backing store).
{
    my %o;
    %o<i> = [];
    %o<i>[0;1] = 5;
    is %o.raku, '{:i($[[Any, 5],])}', 'positional multi-dim into a defined array element';
}

{
    my @a;
    @a[0] = [];
    @a[0][1;2] = 5;
    is @a.raku, '[[Any, [Any, Any, 5]],]', 'positional multi-dim into a defined nested array';
}

{
    my @a;
    my @s[2;2];
    @a[0] := @s;
    @a[0][1;1] = 7;
    is @a[0][1;1], 7, 'a shaped array bound into an element still bounds-checks';
}

# A positional multi-dim subscript does NOT autovivify: rakudo has no
# `ASSIGN-POS` candidate taking more than one index on an undefined invocant.
# Dropping the write silently (what mutsu used to do) is the bug this file pins.
{
    my %o;
    throws-like { %o<i>[0;1] = 5 }, X::Multi::NoMatch,
        'positional multi-dim on an absent hash element throws';
    is %o.raku, '{}', 'and nothing was written';
}

{
    my $x;
    throws-like { $x[0;1] = 5 }, X::Multi::NoMatch,
        'positional multi-dim on an undefined scalar throws';
}

# A refused assignment must leave the container exactly as it found it -- the
# chain walk rolls back every level it autovivified on the way down.
{
    my @a;
    try { @a[0][1;2] = 5 };
    is @a.elems, 0, 'a refused chain leaves no autovivified array slot';
    my %o;
    try { %o<a><b>[0;1] = 5 };
    is %o.raku, '{}', 'a refused chain leaves no autovivified hash keys';
}

# The associative spelling does autovivify through an undefined scalar --
# `ASSIGN-KEY` is defined on `Any:U`.
{
    my $x;
    $x{1;2} = 5;
    is $x.raku, '${"1" => ${"2" => 5}}', 'associative multi-dim autovivifies a scalar';
}

# The write must be visible through the shared container cell a closure capture
# leaves behind, not only in the writing frame's env snapshot.
{
    my %o;
    my $set = sub { %o<inner>{1;2} = 5 };
    $set();
    is %o.raku, '{:inner(${"1" => ${"2" => 5}})}',
        'a chained multi-dim write from a closure reaches the captured hash';
}

{
    my %o;
    %o<inner>{1;2} = 5;
    %o<inner>{1;3} = 6;
    is %o<inner><1>.raku, '${"2" => 5, "3" => 6}',
        'a second write reuses the level the first one autovivified';
}
