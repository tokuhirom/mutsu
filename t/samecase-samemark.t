use Test;

# `:samecase` (:ii) and `:samemark` (:mm) are independent transforms and must
# both apply when requested together. Previously the substitution code used an
# `if samecase {} else if samemark {}` chain, silently dropping samemark whenever
# samecase was also present (roast S05-substitution/subst.t tests 100, 104).

plan 6;

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
