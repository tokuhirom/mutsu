use Test;

# An interpolation in a regex is read when the cursor reaches it, as Rakudo
# does, not before the match starts (#14510): a code block earlier in the
# pattern can change what it matches, and a quantified `"..."` atom is
# evaluated once per iteration.

plan 9;

# A dynamic variable.
{
    my $*v = "a";
    is ~("xb" ~~ / x { $*v = "b" } $*v /), 'xb', '$*dyn set by an earlier code block';
}
grammar G { token TOP { x { $*w = "b" } $*w } }
{
    my $*w = "a";
    is ~G.parse("xb"), 'xb', '$*dyn in a grammar token';
}
{
    my $*t = "b";
    grammar H { token TOP { x $*t } }
    is ~H.parse("xb"), 'xb', '$*dyn bound by the caller';
}

# `<$var>` after a code block.
{
    my $o = "a";
    is ~("xb" ~~ / x { $o = "b" } <$o> /), 'xb', '<$var> set by an earlier code block';
    my $p = "b";
    is ~("xb" ~~ / x <$p> /), 'xb', '<$var> without a code block';
}

# A `"..."` atom runs when reached.
{
    my $n = 0;
    "aaa" ~~ / [ "{ $n++; 'a' }" ]+ /;
    is $n, 4, 'a quantified "..." atom is evaluated once per iteration';
    my $q = "a";
    my $r = rx/ x "$q" /;
    $q = "b";
    is ~("xb" ~~ $r), 'xb', 'a "..." atom reads the variable at match time';
}

# The same `"..."`-only pattern matched twice: the atom's text-scan fallback
# must not be cached as the pattern's own parse.
{
    class Q {}
    my @got = (^2).map({ ~("Q" ~~ /"{ Q.^name }"/) });
    is-deeply @got, ['Q', 'Q'], 'a "..." atom that is the whole pattern, matched twice';
    nok "x" ~~ /"{ Q.^name }"/, '... and it still fails to match other text';
}
