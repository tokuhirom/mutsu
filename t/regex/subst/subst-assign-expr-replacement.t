use Test;

# Substitution with an assignment-syntax replacement (`s{pat}=EXPR` /
# `S{pat}=EXPR`) where EXPR is a compound expression or references the topic
# `$_`, the match `$/`, captures `$0`, or an enclosing placeholder `$^a`.
#
# Previously the replacement parser grabbed only the first primary term, so a
# compound replacement like `"<" ~ $/ ~ ">"` left `~ $/ ~ ">"` dangling to be
# (mis)parsed as an outer binary expression -- silently dropped from the
# substitution. And the expression path wrapped the replacement in an AnonSub
# closure that rebound `$_` to the match and captured `$^a` itself instead of
# letting it belong to the enclosing block.

plan 14;

# --- non-destructive S{pat}=EXPR -----------------------------------------

{
    $_ = "abcXYZdef";
    is (S{<[A..Z]>+} = "<" ~ $/ ~ ">"), "abc<XYZ>def", 'S{}: compound repl with $/';
}
{
    $_ = "abcXYZdef";
    is (S{<[A..Z]>+} = "[" ~ $/.lc ~ "]"), "abc[xyz]def", 'S{}: $/.lc in compound repl';
}
{
    $_ = "12345";
    is (S{5} = $_.chars), "12345", 'S{}: $_ is the original topic, not the match';
}
{
    $_ = "a1b2c3";
    is (S:g{(\d)} = $0 + 10), "a11b12c13", 'S{}:g per-match capture arithmetic';
}
{
    $_ = "12345";
    is-deeply ([3,4].map: { S{5} = $^a }), ('12343', '12344'),
        'S{}: placeholder belongs to the enclosing block';
}
{
    my $c = 0;
    $_ = "xxx";
    is (S:g{x} = $c++), "012", 'S{}:g per-match side effect runs each match';
}
{
    $_ = "12345";
    is (S{5} = "X"), "1234X", 'S{}: plain literal replacement still works';
}

# --- destructive s{pat}=EXPR ---------------------------------------------

{
    $_ = "abcXYZdef";
    s{<[A..Z]>+} = "<" ~ $/ ~ ">";
    is $_, "abc<XYZ>def", 's{}: compound repl with $/';
}
{
    $_ = "12345";
    s{5} = $_.chars;
    is $_, "12345", 's{}: $_ is the original topic';
}
{
    $_ = "a1b2c3";
    s:g{(\d)} = $0 + 10;
    is $_, "a11b12c13", 's{}:g per-match capture arithmetic';
}
{
    my $c = 0;
    $_ = "xxx";
    s:g{x} = $c++;
    is $_, "012", 's{}:g per-match side effect';
}
{
    is-deeply ([3,4].map: { my $x = "12345"; $x ~~ s{5} = $^a; $x }),
        ('12343', '12344'), 's{}: placeholder belongs to enclosing block';
}
{
    $_ = "12345";
    s{5} = "X";
    is $_, "1234X", 's{}: plain literal replacement still works';
}

# --- slash-delimited forms unaffected ------------------------------------

{
    $_ = "abcXYZdef";
    is (S/<[A..Z]>+/{ "[" ~ $/.lc ~ "]" }/), "abc[xyz]def",
        'S/.../{...}/ block replacement still works';
}
