use Test;

# A plain `$var` in a regex is read when the cursor reaches it on every match
# entry point, not only on a smartmatch (#14510, ADR-12917 direction 2): a
# code block earlier in the pattern can change what the atom matches.
# `s///`, `.match`, `.comb`, `.subst`, `m:g`, `m:i` and `.split` used to splice
# the value into the pattern before the match started. Expectations were
# measured against rakudo 2026.09.

plan 13;

my $x;

$x = "a";
{
    my $s = "xbab";
    $s ~~ s/x { $x = "b" } $x/Y/;
    is $s, 'Yab', 's/// reads the value the block wrote';
}
$x = "a"; is ~"xb".match(/x { $x = "b" } $x/), 'xb', '.match';
$x = "a"; is "xbxb".comb(/x { $x = "b" } $x/).join(','), 'xb,xb', '.comb';
$x = "a"; is "xbxb".subst(/x { $x = "b" } $x/, "Z", :g), 'ZZ', '.subst(:g)';
$x = "a"; is ("xbxb" ~~ m:g/x { $x = "b" } $x/).elems, 2, 'm:g';
$x = "a"; is ~("XB" ~~ m:i/x { $x = "b" } $x/), 'XB', 'm:i';
$x = "a"; is-deeply "1xb2".split(/x { $x = "b" } $x/).List, ("1", "2"), '.split';

# Values that are not changed during the match behave as before.
{
    my @r;
    for <a b c> -> $v { @r.push: "abc".subst(/$v/, "_") }
    is @r.join(','), '_bc,a_c,ab_', 'a loop variable, one value per iteration';
}
{
    my $re = /\d+/;
    is ~"a12b".match(/a $re b/), 'a12b', 'a Regex value';
}
{
    my $y = ".";
    is "a.b".subst(/$y/, "!"), 'a!b', 'a metacharacter is a literal';
}
{
    my $j = any("b", "c");
    is "acb".subst(/$j/, "_", :g), 'a__', 'a junction is an alternation';
}
{
    constant $C = "q";
    is "aqb".subst(/$C/, "Q"), 'aQb', 'a constant';
}
{
    my $s = "aXbX";
    my $sep = "X";
    $s ~~ s:g/$sep/-/;
    is $s, 'a-b-', 's:g with a plain value';
}
