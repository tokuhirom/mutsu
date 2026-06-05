use Test;

# Match.gist (and the gist of a Match nested inside a container) renders the
# corner-quoted matched text plus positional/named sub-captures, ordered by
# match position and nested recursively — matching Rakudo. Regression: nested
# Match values (e.g. the values of `$/.caps` or a `m:g//` result list) used to
# gist as the bare matched text, and a Match's own positional captures were not
# shown at all.

plan 9;

{
    "a1" ~~ /(\w)(\d)/;
    is $/.gist, "｢a1｣\n 0 => ｢a｣\n 1 => ｢1｣",
        "positional captures appear in the gist";
}

{
    "abc" ~~ /(\w)+/;
    is $/.gist, "｢abc｣\n 0 => ｢a｣\n 0 => ｢b｣\n 0 => ｢c｣",
        "quantified capture repeats under the same index";
}

{
    "a1b2" ~~ /((\w)(\d))+/;
    is $/.gist,
        "｢a1b2｣\n 0 => ｢a1｣\n  0 => ｢a｣\n  1 => ｢1｣\n 0 => ｢b2｣\n  0 => ｢b｣\n  1 => ｢2｣",
        "nested captures indent one space per depth";
}

{
    "abc123" ~~ /(\w+?)(\d+)/;
    is $/.caps.gist, "(0 => ｢abc｣ 1 => ｢123｣)",
        "Match values nested in .caps gist as corner-quoted text";
    is $/.chunks.gist, "(0 => ｢abc｣ 1 => ｢123｣)",
        ".chunks values gist as corner-quoted text";
}

{
    my @m = "abcabc" ~~ m:g/a/;
    is @m.map(*.gist).join(" "), "｢a｣ ｢a｣",
        "each m:g result Match gists with corner quotes";
}

{
    # A simple Match with no sub-captures gists as just the corner-quoted text.
    "hello" ~~ /ell/;
    is $/.gist, "｢ell｣", "captureless Match gist is just the matched text";
}

{
    # The .Str of a Match is still the plain matched text (no corner quotes).
    "hello" ~~ /ell/;
    is $/.Str, "ell", "Match.Str is the plain matched text";
}

{
    # gist used implicitly by `say` over a list of Matches.
    my @m = "x1y2" ~~ m:g/<[xy]>\d/;
    is @m.map(*.Str).gist, "(x1 y2)", "mapped Match .Str values are plain";
}
