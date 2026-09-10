use Test;

plan 8;

# Raku interpolates a method call WITH parens in an s/// replacement
# (`$x.meth()`), but leaves a bare `$x.meth` (no parens) literal — matching
# double-quoted string interpolation rules.

{
    my $x = "hello";
    my $name = "wor";
    $x ~~ s/hello/$name.uc()/;
    is $x, "WOR", 'method call with parens is interpolated in replacement';
}

{
    my $y = "hello";
    my $name = "wor";
    $y ~~ s/hello/$name.uc/;
    is $y, "wor.uc", 'bare method (no parens) is left literal';
}

# `$/.chars()` — the Match variable with a method call, bound per match.
{
    my $foo = "barbaz";
    $foo ~~ s/.+/$/.chars()/;
    is $foo, "6", '$/.chars() interpolates the match length';
}

# The over-sharing-of-$/ idiom from roast S17-promise/start.t.
{
    is ([+] map -> $n {
            my $foo = "barbaz" x $n;
            $foo ~~ s/.+/$/.chars()/;
            $foo || 0
        }, ^100),
        29700,
        '$/.chars() across many substitutions sums correctly';
}

# Method-call interpolation works with :g (global) too, re-evaluated per match.
{
    my $s = "ab cd ef";
    my $sep = "-";
    $s ~~ s:g/\s/$sep.uc()/;
    is $s, "ab-cd-ef", 'method-call replacement re-runs per match under :g';
}

# Chained postfix: `$x[0].uc()`.
{
    my @a = "foo", "bar";
    my $t = "X";
    $t ~~ s/X/@a[0].uc()/;
    is $t, "FOO", 'indexed method call is interpolated';
}

# A plain variable replacement (no method) still works.
{
    my $t = "AB";
    my $r = "z";
    $t ~~ s/A/$r/;
    is $t, "zB", 'plain variable replacement unaffected';
}

# A `{...}` code block replacement still works.
{
    my $t = "test";
    $t ~~ s/es/{"X" x 2}/;
    is $t, "tXXt", 'code-block replacement unaffected';
}
