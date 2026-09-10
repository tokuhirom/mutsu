use Test;

# A trailing comma in a pointy-block signature: `-> $a, { ... }`,
# `-> IO() :$file, { ... }`. Raku allows a trailing comma in a signature; the
# pointy multi-param loop must stop at the `{` (or `-->`) instead of trying to
# parse it as another parameter. Regression for the Configuration dist
# (`&config-run => -> IO() :$file, :$watch is copy, Signal() :$signal, :&block, { ... }`).

plan 8;

# Single positional param with a trailing comma.
{
    my $f = -> $a, { "got $a" };
    is $f(5), 'got 5', 'trailing comma after a single positional pointy param';
}

# Multiple positional params with a trailing comma.
{
    my $f = -> $a, $b, { "$a-$b" };
    is $f(1, 2), '1-2', 'trailing comma after multiple positional params';
}

# Trailing comma before an explicit return type.
{
    my $g = -> $x, --> Int { $x * 2 };
    is $g(5), 10, 'trailing comma before an explicit --> return type';
}

# Named param with a trailing comma.
{
    my $f = -> :$name, { "hi $name" };
    is $f(name => 'x'), 'hi x', 'trailing comma after a named pointy param';
}

# Typed named param with a trailing comma.
{
    my $f = -> Int :$n, { $n // -1 };
    is $f(n => 9), 9, 'trailing comma after a typed named pointy param';
}

# `is copy` named param + trailing comma (as in Configuration).
{
    my $f = -> :$watch is copy, { $watch = 'set'; $watch };
    is $f(watch => 'orig'), 'set', 'trailing comma after an `is copy` named param';
}

# No trailing comma still works (no regression).
{
    my $f = -> $p, $q { "$p+$q" };
    is $f(3, 4), '3+4', 'a pointy param list without a trailing comma still works';
}

# The full Configuration-shaped lambda: mixed typed named params across lines
# with a trailing comma before the block.
{
    my $f = ->
        IO()  :$file,
              :$watch is copy,
              :&block,
    {
        my @parts;
        @parts.push("file={$file // 'none'}");
        @parts.push("watch={$watch // 'none'}");
        @parts.join(',');
    };
    is $f(file => 'a', watch => 'b'), 'file=a,watch=b',
        'multi-line mixed typed named params with a trailing comma';
}
