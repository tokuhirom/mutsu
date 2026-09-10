use Test;

# `:samecase` (:ii) and `:samemark` (:mm) are independent transforms and must
# both apply when requested together. Previously the substitution code used an
# `if samecase {} else if samemark {}` chain, silently dropping samemark whenever
# samecase was also present (roast S05-substitution/subst.t tests 100, 104).

plan 9;

{
    my $a = "Ä";
    $a ~~ s:ii:mm/A/x/;
    is $a, "Ẍ", ':ii:mm applies both samecase and samemark';
}

{
    my $a = "Ä";
    $a ~~ s:mm/A/x/;
    is $a, "ẍ", ':mm alone applies samemark';
}

{
    my $a = "FOO";
    $a ~~ s:ii/foo/bar/;
    is $a, "BAR", ':ii alone applies samecase';
}

{
    # samespace + samecase + samemark together, multi-word.
    $_ = "Ä\nb\tć D";
    s:ss:ii:mm/a ḇ?   c D/w x y z/;
    is $_, "Ẅ\nx\tý Z", ':ss:ii:mm preserves whitespace, case, and marks';
}

{
    # sigspace + samecase + samemark together, multi-word.
    $_ = "Å\nḇ\tć d";
    s:s:ii:mm/a  B+  c   D/w x y z/;
    is $_, "W̊ x̱ ý z", ':s:ii:mm preserves case and marks';
}

{
    # The .subst method shares the same chokepoint.
    is "Ä".subst(/:i:m a/, "x", :samecase, :samemark), "Ẍ",
        '.subst with :samecase and :samemark applies both';
}

# In Raku, `:sigspace` (and therefore `:samespace` / the `ss///` operator) also
# implies `:samemark` — the matched text's combining marks are copied onto the
# replacement even without an explicit `:mm` (roast S05-substitution/subst.t #102).
{
    $_ = "ḇ";
    s:s/ḇ/x/;
    is $_, "x̱", ':s (sigspace) implies samemark on a literal mark match';
}

{
    $_ = "a\nḇ\tĆ d";
    ss:i:m/Å b C d/w x y z/;
    is $_, "w\nx̱\tý z", 'ss:i:m preserves whitespace and copies marks';
}

{
    # sigspace does NOT imply samecase.
    $_ = "B";
    s:s:i/b/x/;
    is $_, "x", ':s (sigspace) does not imply samecase';
}
