use Test;

# Two related junction-threading gaps from the 2026-09-09 doc-diff sweep.
# See https://github.com/tokuhirom/mutsu/issues/7755
#
# Every assertion captures the Junction result into a variable and calls
# `.raku` on it BEFORE handing it to `is` — a raw Junction argument would
# autothread the `is` call itself, comparing each eigenstate separately
# instead of checking the junction's own shape.
plan 11;

# 1. `~` over two SAME-kind junctions threads into one flat junction, not a
# junction of junctions. `.raku` lists every eigenstate, so this is also the
# width check: 9 eigenstates (3x3), not 3 nested junctions of 3.
{
    my $odd = 1|3|5;
    my $even = 2|4|6;
    my $merged = $odd ~ $even;
    is $merged.raku, 'any("12", "14", "16", "32", "34", "36", "52", "54", "56")',
        '~ over two any-junctions is one flat, width-correct any(...) of 9';
}
{
    my $merged = (1|3) ~ (2|4);
    is $merged.raku, 'any("12", "14", "32", "34")',
        '(1|3) ~ (2|4) has four eigenstates, not two junctions';
}

# Comparison against the nested form still works (control: this survived
# even with the old nested bug, so it must keep working).
{
    my $merged = (1|3|5) ~ (2|4|6);
    ok 34 == $merged, 'comparison against the merged junction still matches';
}

# Mismatched kinds are NOT flattened — Rakudo keeps them genuinely nested
# because the two kinds' short-circuit semantics cannot collapse into one.
{
    my $merged = (1|2) ~ (3&4);
    is $merged.raku, 'all(any("13", "14"), any("23", "24"))',
        'mismatched-kind ~ (any and all) stays nested, unlike same-kind';
}

# Other operators sharing eval_binary_with_junctions are UNAFFECTED controls:
# Rakudo keeps arithmetic genuinely nested even for same-kind junctions
# (only ~ flattens), so this must not regress.
{
    my $merged = (1|3) + (2|4);
    is $merged.raku, 'any(any(3, 5), any(5, 7))',
        '+ over two any-junctions stays nested (unlike ~, which flattens)';
}

# 2. A junction inside a list threads through `.join` instead of being
# stringified in place.
{
    my $j = ("a"|"b", "c", "d").join;
    is $j.raku, 'any("acd", "bcd")',
        'a junction element threads through the method-call .join';
}
{
    my @a = "a"|"b", "c", "d";
    my $j = @a.join;
    is $j.raku, 'any("acd", "bcd")', 'a junction element threads through @array.join';
}
{
    my $j = join(",", "a"|"b", "c", "d");
    is $j.raku, 'any("a,c,d", "b,c,d")',
        'a junction as a direct join() argument threads (function-call form)';
}
{
    my $j = ("a"|"b", "c"|"d", "e").join;
    is $j.raku, 'any("ace", "ade", "bce", "bde")',
        'two same-kind junctions in a list flatten into one, width-correct';
}

# Control: a list with no junction is unaffected.
is ("a", "b", "c").join, "abc", 'join with no junction is unaffected';
is ("a", "b", "c").join(","), "a,b,c", 'join(sep) with no junction is unaffected';

done-testing;
