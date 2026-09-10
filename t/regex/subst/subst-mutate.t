use Test;

# Str.subst-mutate substitutes in place (like s///) and returns what s/// sets
# in $/: a Match for a single hit, the Any type object when nothing matched, or
# a List of Matches under :g. Previously mutsu had no such method.

plan 20;

# --- single substitution, in place + Match return ---
{
    my $s = "aaa";
    my $r = $s.subst-mutate(/a/, "b");
    is $s, "baa", "single subst-mutate replaces the first match in place";
    ok $r ~~ Match, "single subst-mutate returns a Match";
    is ~$r, "a", "the returned Match is the replaced text";
}

# --- global substitution returns a List of Matches ---
{
    my $s = "aaa";
    my $r = $s.subst-mutate(/a/, "b", :g);
    is $s, "bbb", ":g subst-mutate replaces all matches";
    ok $r ~~ List, ":g subst-mutate returns a List";
    is $r.elems, 3, ":g returns one Match per replacement";
    ok $r[0] ~~ Match, ":g list elements are Match objects";
}

# --- string (non-regex) matcher ---
{
    my $s = "hello";
    $s.subst-mutate("l", "L");
    is $s, "heLlo", "string matcher replaces the first occurrence";
}

# --- no match: string untouched; Any (single) / empty List (:g) ---
{
    my $s = "abc";
    my $r = $s.subst-mutate(/z/, "x");
    is $s, "abc", "no match leaves the string unchanged";
    nok $r.defined, "single no-match returns an undefined value";
    ok $r === Any, "single no-match returns the Any type object";

    my $t = "abc";
    my $rg = $t.subst-mutate(/z/, "x", :g);
    is $t, "abc", ":g no match leaves the string unchanged";
    ok $rg ~~ List, ":g no match still returns a List";
    is $rg.elems, 0, ":g no match returns an empty List";
}

# --- adverbs: :nth, :samecase, closure replacement ---
{
    my $s = "abcabc";
    $s.subst-mutate(/b/, "X", :2nd);
    is $s, "abcaXc", ":2nd replaces only the second match";

    my $t = "hello world";
    $t.subst-mutate(/o/, "0", :samecase);
    is $t, "hell0 world", ":samecase adverb is honored";

    my $u = "Hello";
    $u.subst-mutate(/l/, { "L" });
    is $u, "HeLlo", "closure replacement works";
}

# --- :g over a character class; whole-list capture of :g result ---
{
    my $s = "a1b2";
    $s.subst-mutate(/\d/, "X", :g);
    is $s, "aXbX", ":g over a character class";

    my $t = "xyz";
    my @r = $t.subst-mutate(/<[xyz]>/, "_", :g);
    is @r.elems, 3, ":g result flattens to one element per replacement";
    is $t, "___", ":g replaced every character";
}
