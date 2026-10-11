use Test;

# #11217: a structure nested deeper than the native stack holds, built at run
# time with no parser involved, must not crash the process. Dropping it always
# succeeds; rendering it either succeeds or raises the catchable ADR-0100
# "Too deep recursion" error -- which one depends on the build's frame sizes,
# so the render checks accept both.

plan 8;

sub rendered-or-too-deep(&render, $expected-chars) {
    my $chars = try render();
    return $chars == $expected-chars if $chars.defined;
    so $!.message ~~ /'Too deep recursion'/;
}

{
    my $a = [];
    for ^1_000_000 { $a = [$a] }
    $a = Nil;
    pass 'dropping a million nested arrays does not overflow';
}

{
    my %h = a => 1;
    for ^200_000 { %h = (x => %h.clone) }
    %h = ();
    pass 'dropping 200k nested hashes does not overflow';
}

{
    my $p = 1;
    for ^200_000 { $p = 1 => $p<> }
    $p = Nil;
    pass 'dropping 200k nested pairs does not overflow';
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
    for ^200_000 { $a = [$a] }
    ok rendered-or-too-deep({ $a.gist.chars }, 400_002),
        '.gist of 200k nested arrays renders or raises a catchable error';
    ok rendered-or-too-deep({ $a.raku.chars }, 600_003),
        '.raku of 200k nested arrays renders or raises a catchable error';
}

{
    # `1 => $a` holds $a's container, so this is a cycle (rakudo never
    # finishes `.Str` on it); it must not take the process down.
    my $a = 1;
    for ^3 { $a = 1 => $a }
    my $r = try $a.Str;
    ok !$r.defined && $!.message ~~ /'Too deep recursion'/,
        '.Str of a cyclic pair raises a catchable error';
}
