use Test;

# A sigil alias on an interpolated `$var` (`$<a>=$var`, `@<a>=$var`,
# `$0=$var`) names the interpolation itself, as rakudo's `subrule_alias`
# renames the interpolation node (#14510): the alias holds the whole value's
# Match (a Regex value's nested captures included), a quantifier repeats the
# interpolation (one Match per iteration, whatever the value's type), and
# `@<a>=` lists it. A multi-character Str value used to give the alias only
# its first character on the paths that splice the value into the pattern.
# Expectations were measured against rakudo 2026.09.

plan 22;

my $t = "abc";
my $re = /(\d)<alpha>/;

# Match-time path (smartmatch).
is ("xabc" ~~ /x $<a>=$t/)<a>.Str, 'abc', 'the alias names the whole Str value';
is ("abcabc" ~~ /$<a>=$t+/)<a>.elems, 2, 'a quantifier repeats the interpolation';
is ("abcabc" ~~ /$<a>=$t+/)<a>[1].from, 3, 'one Match per iteration';
is ("abc" ~~ /@<a>=$t/)<a>.elems, 1, '@<a>= lists the interpolation';
is ("abc" ~~ /$0=$t/)[0].Str, 'abc', 'a positional alias names the whole value';
is ("1z" ~~ /$<a>=$re/)<a>[0].Str, '1', 'a Regex value keeps its positional captures';
is ("1z" ~~ /$<a>=$re/)<a><alpha>.Str, 'z', 'and its named ones';
is ("AbC" ~~ /:i $<a>=$t/)<a>.Str, 'AbC', ':i reaches the aliased value';
is ("AbC" ~~ m:i/$<a>=$t/)<a>.Str, 'AbC', 'so does m:i';
{
    my $j = any("x", "yy");
    is ("yy" ~~ /$<a>=$j/)<a>.Str, 'yy', 'a junction value is an alternation';
}
{
    my $x = "a";
    is ("xb" ~~ / x { $x = "b" } $<q>=$x /)<q>.Str, 'b',
        'the aliased value is read when the cursor reaches it';
}
{
    my $*D = "q";
    is ("q" ~~ /$<d>=$*D/)<d>.Str, 'q', 'a dynamic variable';
}
is ("ab" ~~ /:my $m = "ab"; $<z>=$m/)<z>.Str, 'ab', 'an in-regex :my lexical';

# A rule's parameter.
grammar G {
    token TOP { <t("12")> }
    token t($p) { $<v>=$p }
}
is G.parse("12")<t><v>.Str, '12', 'a Str rule parameter under an alias';
grammar H {
    token TOP { <t(/<digit>+/)> }
    token t($p) { $<v>=$p }
}
is H.parse("12")<t><v><digit>.elems, 2, 'a Regex rule parameter keeps its captures';

# Substitution and the other entry points.
{
    my $u = "xabc";
    $u ~~ s/$<a>=$t/[$<a>]/;
    is $u, 'x[abc]', 's/// sees the whole value under the alias';
}
{
    my $u = "abcabc";
    $u ~~ s/$<a>=$t+/[{$<a>.elems}]/;
    is $u, '[2]', 's/// repeats it per iteration';
}
{
    my $u = "abc";
    $u ~~ s/@<a>=$t/{$<a>.elems}/;
    is $u, '1', 's/// lists an @<a>= alias';
}
{
    my $u = "abc";
    $u ~~ s/$0=$t/<$0>/;
    is $u, '<abc>', 's/// with a positional alias';
}
is "xabc".match(/$<a>=$t/)<a>.Str, 'abc', '.match';
is "xabc".subst(/$<a>=$t/, { "<$<a>>" }), 'x<abc>', '.subst';
is "xABC".match(/:i $<a>=$t/)<a>.Str, 'ABC', '.match with :i';
