use Test;

# #9173: `temp`/`let` snapshot an `@`/`%` variable one level deep, as raku's
# `.clone` does, and a `$` variable not at all. The snapshot used to be a
# recursive deep copy, O(total nodes) per save.

plan 13;

{
    my @a = [1, 2], [3, 4];
    { temp @a; @a[0][0] = 99; @a[1] = 5; }
    is-deeply @a, [[99, 2], [3, 4]], 'temp @a restores its elements but shares nested containers';
}

{
    my %h = a => [1];
    { temp %h; %h<a>[0] = 7; %h<b> = 1 }
    is-deeply %h, {a => [7]}, 'temp %h restores its keys but shares nested containers';
}

{
    my @b = 1, 2;
    { temp @b; @b[0] = 9; @b.push(3) }
    is-deeply @b, [1, 2], 'temp @a undoes element writes and pushes';
}

{
    my $x = 1;
    my @c;
    @c[0] := $x;
    { temp @c; @c[0] = 5 }
    is @c[0], 1, 'a bound element is snapshot by value';
}

{
    my Int %t{Str};
    %t<a> = 1;
    { temp %t; %t<a> = 2 }
    is %t.raku, '(my Int %{Str} = :a(1))', 'temp keeps a typed, keyed hash its identity';
}

{
    my $s = [1, [2]];
    { temp $s; $s[1][0] = 9; $s[0] = 7 }
    is-deeply $s, [7, [9]], 'temp $s snapshots the value, not a copy of it';
}

{
    my $s = [1, 2];
    { temp $s = [5] }
    is-deeply $s, [1, 2], 'temp $s = ... restores the old value';
}

{
    my $q = [1, 2];
    { temp $q[0] = 9; is-deeply $q, [9, 2], 'element temp writes the element' }
    is-deeply $q, [1, 2], 'element temp restores the element';
}

{
    my $struct = [ "x", { key => [ "y", 42 ] } ];
    { temp $struct[1]<key>[1] = 23 }
    is $struct[1]<key>[1], 42, 'multi-level element temp restores the nested element';
}

{
    my @a = (^100).map({ [^200] });
    my $n = 0;
    for ^50 { temp @a; $n++ }
    is $n, 50, 'temp of a big nested array in a loop';
}

# `state` in a loop body re-initializes per execution of the loop statement.
{
    my $out = '';
    for ^3 { for ^2 { state $n = 0; $n++; $out ~= $n } }
    is $out, '121212', 'state in a nested loop body resets per statement entry';
}

is-deeply (1..3).map({ state $s = 0; $s += $_ }).List, (1, 3, 6), 'state in a map callback';
