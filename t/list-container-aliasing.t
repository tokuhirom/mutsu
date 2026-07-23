use Test;

# A List (`($a, $b)`) holds the *container* of each scalar variable element,
# not a snapshot of its value: a later mutation of the variable is visible when
# the List is read. See raku-doc/doc/Language/traps.rakudoc
# ("list-element container aliasing").

plan 13;

# Basic aliasing: List bound to a scalar keeps the containers.
{
    my $a = 2;
    my $l = ($a, $a);
    $a = 99;
    is $l.gist, '(99 99)', 'List bound to scalar aliases the variable container';
}

# The two elements share the same container.
{
    my $y = 1;
    my $sl = ($y, $y);
    ok $sl[0] === $sl[1], 'repeated var in a List shares one container';
}

# The classic Capture-reification trap.
{
    my $a = 2;
    is (join ",", ($a, ++$a)), '3,3', 'pre-increment is visible in the earlier list element';
}

# The fibonacci push trap from traps.rakudoc.
{
    my @arr;
    my ($x, $y) = (1, 1);
    my @seen;
    for ^3 {
        ($x, $y) = ($y, $x + $y);
        @arr.push: ($x, $y);
        @seen.push: @arr.gist;
    }
    is @seen[0], '[(1 2)]', 'push round 1';
    is @seen[1], '[(2 3) (2 3)]', 'push round 2 reflects the later mutation';
    is @seen[2], '[(3 5) (3 5) (3 5)]', 'push round 3 reflects the later mutation';
}

# Pushing a parenthesized list keeps the containers.
{
    my @arr;
    my $p = 1; my $q = 2;
    @arr.push: ($p, $q);
    $p = 100;
    is @arr.gist, '[(100 2)]', 'pushed List aliases the variable container';
}

# Array assignment decontainerizes (a copy of the value).
{
    my $m = 1;
    my @z = ($m, $m);
    $m = 50;
    is @z.gist, '[1 1]', 'array assignment decontainerizes the List elements';
}

# Bracket arrays decontainerize on construction.
{
    my $a = 2;
    my $l = [$a, $a];
    $a = 99;
    is $l.gist, '[2 2]', 'bracket array does not alias';
}

# `.item` / a value-producing expression breaks the alias.
{
    my $a = 2;
    my $l = (+$a, ++$a);
    is $l.gist, '(2 3)', '+$a forces a value, so it is not aliased';
}

# Passing a List to a function is by value.
{
    sub g($p, $q) { "$p $q" }
    my $m = 5; my $n = 6;
    my $r = g($m, $n);
    $m = 99;
    is $r, '5 6', 'function arguments are passed by value';
}

# Nested lists alias at every level.
{
    my $c = 1;
    my $l = (($c, $c), $c);
    $c = 7;
    is $l.gist, '((7 7) 7)', 'nested List aliases at every level';
}

# A scalar holding an array is one element (not flattened) and still aliases.
{
    my $arr = [1, 2, 3];
    my $l = ($arr, $arr);
    is $l.elems, 2, 'a scalar element is not flattened in a List';
}
