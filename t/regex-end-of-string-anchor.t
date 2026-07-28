use Test;

plan 9;

# Raku's `$` is end-of-STRING wherever it appears; only `$$` is end-of-line.
# A `$` followed by another atom (a code block, an assertion) used to be matched
# with end-of-line semantics, which fails at the end of a string that ends in a
# newline — so YAMLish's `Schema::Core` `token plain { ^ .* $ { make … } }` never
# matched a multi-line scalar.
my $multi = "Hello,\n  World\n";

ok $multi ~~ / ^ $<value>=.* $ /, 'trailing $ anchors the end of the string';
ok $multi ~~ / ^ $<value>=.* $ { 1 } /, '$ before a code block is still end-of-string';
ok $multi ~~ / ^ .* $ <?{ True }> /, '$ before a code assertion is still end-of-string';
ok $multi ~~ / ^ 'Hello,' \n '  World' \n $ { 1 } /, '$ after literals, before a code block';

grammar G {
    token plain {
        ^ $<value>=.* $
        { make ~$<value> }
    }
}
my $m = G.subparse($multi, :rule<plain>);
ok $m, 'a grammar token ending in `$ { ... }` matches a multi-line string';
is $m.ast, $multi, 'and its action sees the whole string';

# `$$` keeps end-of-LINE semantics: it matches before a newline, and (unlike `$`)
# not at the very end of a string that already ended with one.
ok "ab\ncd" ~~ / ^ 'ab' $$ /, '$$ matches before a newline';
nok "ab\n" ~~ / ^ 'ab' \n $$ { 1 } /, '$$ does not match after a trailing newline';
ok "ab\n" ~~ / ^ 'ab' \n $ { 1 } /, 'but $ does';
