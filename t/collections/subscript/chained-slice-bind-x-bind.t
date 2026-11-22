use Test;

# A `:=` into a chained subscript with a slice (#18535). A last-level slice
# binds element by element into a container that takes the bind (an Array for
# a positional slice, a Hash for an associative one); any other receiver, and
# a slice at an inner level, throws X::Bind ("Cannot use bind operator with
# this left-hand side"), as a statement and inside parentheses.

plan 14;

# Refused.
{
    my @s = 1, 2;
    throws-like { @s[0][0,1] := (7, 8) }, X::Bind,
        message => 'Cannot use bind operator with this left-hand side',
        'into an Int element';
    throws-like { say (@s[0][0,1] := (7, 8)) }, X::Bind, 'in parentheses too';
    is @s.raku, '[1, 2]', 'nothing was bound';
}
{
    my @s = [1, 2], [3, 4];
    my @l = (1, 2),;
    my %h = a => { x => 1 };
    throws-like { @s[0,1][0] := 7 }, X::Bind, 'a slice at an inner level';
    throws-like { my $x = @s[0,1][0] := 3 }, X::Bind, 'as an initializer';
    throws-like { @l[0][0,1] := (7, 8) }, X::Bind, 'into a List element';
    throws-like { %h<a>[0,1] := (7, 8) }, X::Bind, 'a positional slice of a Hash element';
    throws-like { @s[0]<x y> := (7, 8) }, X::Bind, 'an associative slice of an Array element';
    throws-like { @s[0][*-1] := 7 }, X::Bind::Slice, 'a Whatever index stays X::Bind::Slice';
}

# Bound element by element.
{
    my @s = [1, 2], [3, 4];
    @s[0][0,1] := (7, 8);
    is @s.raku, '[[7, 8], [3, 4]]', 'a positional slice into an Array element';
    throws-like { @s[0][1] = 9 }, X::AdHoc, 'the bound elements are not containers';
    my ($a, $b) = 1, 2;
    @s[1][0,1] := ($a, $b);
    $a = 5;
    is @s[1][0], 1, 'the values are bound, not the variables';
    is-deeply (@s[0][0,1] := (3, 4)), (3, 4), 'in parentheses, the bound values';
    my %h = a => { x => 1 };
    %h<a><x y> := (7, 8);
    is-deeply %h<a>, { x => 7, y => 8 }, 'an associative slice into a Hash element';
    my @t = [[1, 2],];
    @t[0][0][0,1] := (5, 6);
    is @t.raku, '[[[5, 6],],]', 'three levels';
    @t[0][0][0] := 9;
    is @t[0][0][0], 9, 'a single-element chained bind is unchanged';
}
