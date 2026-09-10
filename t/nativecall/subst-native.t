use Test;

# Exercises the VM-native `.subst` fast path (src/vm/vm_native_subst.rs).
# These all use a Str invocant with a plain regex/string pattern and a string
# (or absent) replacement -- the common cases now handled in the VM instead of
# falling back to the interpreter. Results must remain identical to the
# interpreter path.

plan 20;

# --- regex pattern, single match ---
is "aXbXc".subst(/X/, "-"), "a-bXc", "regex single replaces first match only";
is "hello".subst(/l/, "L"), "heLlo", "regex single, first of repeated";
is "abc".subst(/Z/, "-"), "abc", "regex no match returns original";
is "abc".subst(/b/, ""), "ac", "regex empty replacement deletes match";
is "abc".subst(/b/), "ac", "regex absent replacement deletes match";

# --- regex pattern, global (:g) ---
is "aXbXc".subst(/X/, "-", :g), "a-b-c", "regex :g replaces all";
is "aaa".subst(/a/, "bb", :g), "bbbbbb", "regex :g overlapping-free expansion";
is "abc".subst(/Z/, "-", :g), "abc", "regex :g no match returns original";

# --- string pattern ---
is "hello".subst("l", "L"), "heLlo", "string single replaces first";
is "hello".subst("l", "L", :g), "heLLo", "string :g replaces all";
is "a.b.c".subst(".", "-", :g), "a-b-c", "string :g treats pattern literally";
is "abc".subst("z", "-"), "abc", "string no match returns original";

# --- $/ after subst ---
{
    my $s = "foo123bar";
    $s.subst(/(\d+)/, "X");
    ok $/.defined, '$/ is set after single regex subst';
    is $/.Str, "123", '$/ holds the matched text';
    is $/[0].Str, "123", '$/[0] holds the first capture';
}
{
    "abc".subst(/Z/, "-");
    nok $/.defined, '$/ is Nil after a no-match subst';
}

# --- unicode (char vs byte indices) ---
is "café".subst(/é/, "e"), "cafe", "unicode single subst";
is "naïve café".subst(/<[éï]>/, "_", :g), "na_ve caf_", "unicode :g subst";

# --- chaining returns a fresh string, invocant unchanged ---
{
    my $orig = "a,b,c";
    my $new = $orig.subst(/","/, ";", :g);
    is $new, "a;b;c", "subst returns substituted copy";
    is $orig, "a,b,c", "subst leaves the invocant unchanged";
}
