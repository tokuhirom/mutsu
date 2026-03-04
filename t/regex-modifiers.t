use Test;

plan 25;

# --- :i / :ignorecase ---
{
    ok "Hello" ~~ m:i/hello/, ':i adverb matches case-insensitively';
    ok "HELLO" ~~ m:i/hello/, ':i adverb with all caps';
    ok "abc" ~~ / :i ABC /, 'inline :i matches case-insensitively';
    ok "ABC" ~~ / :i abc /, 'inline :i works both ways';
    nok "abc" ~~ / ABC /, 'without :i, case matters';
}

# --- :g / :global ---
{
    my @matches = "aabaa" ~~ m:g/a/;
    is +@matches, 4, ':g finds all 4 "a" in "aabaa"';

    my @matches2 = "aaaa" ~~ m:g/aa/;
    is +@matches2, 2, ':g returns non-overlapping matches (2 "aa" in "aaaa")';

    my @matches3 = "abcabc" ~~ m:g/abc/;
    is +@matches3, 2, ':g finds all non-overlapping "abc"';

    my @matches4 = "xyz" ~~ m:g/a/;
    nok @matches4, ':g returns Nil when no matches';
}

# --- :ov / :overlap ---
{
    my @matches = "aaaa" ~~ m:ov/aa/;
    is +@matches, 3, ':ov returns overlapping matches (3 "aa" in "aaaa")';
}

# --- :ex / :exhaustive ---
{
    my @matches = "aabaa" ~~ m:ex/a/;
    is +@matches, 4, ':ex finds all possible "a" matches';
}

# --- :s / :sigspace ---
{
    ok "a  b   c" ~~ m:s/a b c/, ':s treats whitespace as flexible';
    nok "abc" ~~ m:s/a b c/, ':s requires whitespace between tokens';
}

# --- Combinations ---
{
    my @matches = "Hello hello HELLO" ~~ m:g:i/hello/;
    is +@matches, 3, ':g:i finds all case-insensitive matches';
}

# --- :g with longer patterns ---
{
    my @matches = "xyzxyzxyz" ~~ m:g/xyz/;
    is +@matches, 3, ':g finds all non-overlapping "xyz"';
}

# --- :ignorecase long form ---
{
    ok "Hello" ~~ m:ignorecase/hello/, ':ignorecase long form works';
}

# --- :global long form ---
{
    my @matches = "aaa" ~~ m:global/a/;
    is +@matches, 3, ':global long form works';
}

# --- :overlap long form ---
{
    my @matches = "aaaa" ~~ m:overlap/aa/;
    is +@matches, 3, ':overlap long form finds overlapping matches';
}

# --- :exhaustive long form ---
{
    my @matches = "aaa" ~~ m:exhaustive/a/;
    is +@matches, 3, ':exhaustive long form works';
}

# --- :sigspace long form ---
{
    ok "a  b" ~~ m:sigspace/a b/, ':sigspace long form works';
}

# --- m:i correctly creates Match object ---
{
    "Hello World" ~~ m:i/hello/;
    ok $/.defined, ':i match sets $/';
    is ~$/, "Hello", ':i match captures correct text';
}

# --- :g with captures ---
{
    my @m = "abc def" ~~ m:g/(\w+)/;
    is +@m, 2, ':g with capture group finds 2 words';
}

# --- :ii / :samecase ---
{
    ok "Hello" ~~ m:ii/hello/, ':ii implies :i for matching';
}

# --- :ss / :samespace ---
{
    ok "a  b" ~~ m:ss/a b/, ':ss implies :s for matching';
}

done-testing;
