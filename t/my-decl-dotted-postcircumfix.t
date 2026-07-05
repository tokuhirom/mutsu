use Test;

# `my @a.[2] = 42` is `(my @a).[2] = 42`: the dotted postcircumfix applies to
# the just-declared variable (no shaped array is created). mutsu previously
# dropped the `.[2] = 42`, parsing it as a separate `$_.[2] = 42` statement and
# leaving `@a` empty.

plan 10;

{
    my @arr1.[2] = 42;
    is-deeply @arr1, [Any, Any, 42], 'my @a.[2] = 42 assigns the declared array';
}
{
    my @arr2\  .[2] = "foo";
    is-deeply @arr2, [Any, Any, "foo"], 'unspace form my @a\ .[2] = ...';
}
{
    my @a.[0] = 1;
    is-deeply @a, [1], 'index 0';
}
{
    my %h.<k> = 5;
    is-deeply %h, {k => 5}, 'dotted hash postcircumfix .<k>';
}
{
    my %h2.{"x"} = 9;
    is %h2<x>, 9, 'dotted hash postcircumfix .{...}';
}

# The variable really lands in the enclosing scope (not a nested block)
{
    my @a.[1] = 7;
    @a[3] = 9;
    is-deeply @a, [Any, 7, Any, 9], 'declared @a is usable afterwards';
}

# Plain and shaped declarations are unaffected
{
    my @a = 1, 2, 3;
    is-deeply @a, [1, 2, 3], 'plain array decl unaffected';
}
{
    my @a[3] = 1, 2, 3;
    is @a.shape.gist, '(3)', 'shaped decl (bracket) unaffected';
}
{
    my %h = a => 1;
    is %h<a>, 1, 'plain hash decl unaffected';
}
{
    my $x = 5;
    is $x, 5, 'scalar decl unaffected';
}
