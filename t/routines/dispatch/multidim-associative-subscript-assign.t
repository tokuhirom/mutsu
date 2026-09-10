use Test;

# An ASSOCIATIVE multi-dimensional subscript (`%h{1;2}`) is a chain of nested
# Hash keys, not a shape: every level autovivifies a Hash and stringifies its
# key. Before this was tracked, mutsu treated every `{a;b}` subscript like the
# positional (shaped) spelling and died with
# "Invalid index for multi-dim assignment" on an integer key.

plan 18;

{
    my %h;
    %h{1;2} = 5;
    is %h.raku, '{"1" => ${"2" => 5}}', 'integer keys autovivify nested hashes';
    is %h{1}{2}, 5, 'the leaf is reachable through the chained spelling';
    # An associative multi-dim subscript is a slice even for all-scalar keys.
    is %h{1;2}.raku, '(5,)', 'the read hands back a one-element List';
    is %h{1;2}.VAR.^name, 'List', 'and its .VAR is that List';
}

{
    my %h;
    %h{1;2;3} = 9;
    is %h.raku, '{"1" => ${"2" => ${"3" => 9}}}', 'three associative dimensions';
    %h{1;2} = 4;
    is %h.raku, '{"1" => ${"2" => 4}}', 'assigning an intermediate level replaces it';
}

{
    my %h;
    %h{"x";"y"} = 1;
    is %h.raku, '{:x(${:y(1)})}', 'string keys nest the same way';
}

{
    my %h;
    %h{1;2} = 5;
    %h{1;3} = 6;
    is-deeply %h{1; 2,3}.List, (5, 6), 'a slice dimension selects several leaves';
}

{
    my %h;
    %h{1; 4,5} = 7, 8;
    is %h.raku, '{"1" => ${"4" => 7, "5" => 8}}',
        'a slice dimension distributes the RHS element-wise';
}

{
    my %h;
    %h{1;2} = 5;
    %h{1;3} = 6;
    is %h{1;*}.sort.List.raku, '(5, 6)', 'Whatever selects every existing key';
}

# A `$` scalar root autovivifies into a Scalar container, so it itemizes --
# same as the single-subscript spelling `$x{1}{2}`.
{
    my $x;
    $x{1;2} = 5;
    is $x.raku, '${"1" => ${"2" => 5}}', 'a scalar root itemizes';
}

# The POSITIONAL spelling is unchanged: `[...]` walks a (possibly shaped)
# Positional and an integer-looking key stays an array index.
{
    my @a;
    @a[0;1] = 3;
    is @a.raku, '[[Any, 3],]', 'positional multidim still autovivifies arrays';
}

{
    my @sh[2;2];
    @sh[0;0] = 7;
    is @sh[0;0], 7, 'shaped array assignment still works';
}

# Compound assignment goes through the same lowering.
{
    my %h;
    %h{1;2} = 5;
    %h{1;2} += 3;
    # `%h{1;2}` reads as `(5,)`, so `+` sees a 1-element list: 1 + 3.
    is %h{1}{2}, 4, 'compound assignment goes through the List read';
}

# The lvalue is a List, so the assignment is a LIST assignment: the RHS is
# distributed element-wise over the selected leaves. The positional spelling
# keeps single-element semantics and stores the whole array.
{
    my %h;
    %h{1;2} = [1, 2, 3];
    is %h.raku, '{"1" => ${"2" => 1}}', 'an associative leaf takes the first RHS element';
    my @a;
    @a[0;1] = [1, 2, 3];
    is @a.raku, '[[Any, [1, 2, 3]],]', 'a positional leaf takes the whole array';
}

{
    my %h;
    %h{1;2} = (7, 8);
    is %h.raku, '{"1" => ${"2" => 7}}', 'a parenthesised RHS list distributes too';
}

# The 6.e side of the split (the multislice is gone there) lives in
# t/multidim-associative-subscript-6e.t -- a version pragma is per compilation
# unit, so it needs its own file.

# A value stored at an associative leaf lives in a Scalar container.
{
    my %h;
    %h{1; 2,3} = [1,2], [3,4];
    is %h.raku, '{"1" => ${"2" => $[1, 2], "3" => $[3, 4]}}',
        'an associative leaf itemizes what it stores';
}

# NOTE: an EXPRESSION target (`%outer<inner>{1;2} = 5`) still drops the write --
# `MultiDimIndexAssignGeneric` descends into a throwaway copy of the read value.
# That gap predates the associative walk (the positional spelling drops it too)
# and is tracked in todo/tickets/multidim-assign-to-an-expression-target-is-dropped.md.
