use Test;

# The sequence operator calls its generator and endpoint closures with as many
# trailing elements as the closure's `.count` (all of them for a slurpy), the
# way Rakudo does (#10119).

plan 9;

{
    my @seen;
    my @s = 1, 2, 3 ... -> $a, $b { @seen.push("$a $b"); $b > 2 };
    is-deeply @s, [1, 2, 3], 'two-parameter endpoint stops the sequence';
    is-deeply @seen, ['1 2', '2 3'], 'two-parameter endpoint is not consulted before two elements exist';
}

{
    my @seen;
    my @s = 5 ... -> $a, $b, $c { @seen.push("$a $b $c"); $c > 7 };
    is-deeply @s, [5, 6, 7, 8], 'three-parameter endpoint over a deduced sequence';
    is-deeply @seen, ['5 6 7', '6 7 8'], 'endpoint receives the last three elements, never Nil padding';
}

{
    my $i = 0;
    my @s = { ++$i } ... * > 3;
    is-deeply @s, [1, 2, 3, 4], 'seedless generator with an optional topic';
    is $i, 4, 'generator side effects reach the enclosing scope';
}

is-deeply (1, 1, -> *@a { @a[*-1] + @a[*-2] } ... *)[^7], (1, 1, 2, 3, 5, 8, 13),
    'slurpy generator receives every element so far';
is-deeply (1, { @_.elems } ... *)[^5], (1, 1, 2, 3, 4), '@_ generator receives every element so far';
is-deeply (1, 2, 4 ... { @_.elems > 2 }), (1, 2, 4), '@_ endpoint receives every element so far';
