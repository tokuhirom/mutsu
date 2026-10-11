use Test;

# #11217: rendering a structure nested deeper than the native stack holds must
# not crash the process. It either succeeds or raises the catchable ADR-0100
# "Too deep recursion" error -- which one depends on the build's frame sizes,
# so each check accepts both. (Dropping such structures:
# stack-deep-nesting-drop.t; a cyclic Pair: stack-deep-cyclic-pair.t.)

plan 4;

sub rendered-or-too-deep(&render, $expected-chars) {
    my $chars = try render();
    return $chars == $expected-chars if $chars.defined;
    so $!.message ~~ /'Too deep recursion'/;
}

{
    my $p = 1;
    for ^100_000 { $p = 1 => $p<> }
    ok rendered-or-too-deep({ $p.Str.chars }, 200_001),
        '.Str of 100k nested pairs renders or raises a catchable error';
    ok rendered-or-too-deep({ "$p".chars }, 200_001),
        'interpolating 100k nested pairs renders or raises a catchable error';
}

{
    my $a = [];
    for ^30_000 { $a = [$a] }
    ok rendered-or-too-deep({ $a.gist.chars }, 60_002),
        '.gist of 30k nested arrays renders or raises a catchable error';
    ok rendered-or-too-deep({ $a.raku.chars }, 90_003),
        '.raku of 30k nested arrays renders or raises a catchable error';
}
