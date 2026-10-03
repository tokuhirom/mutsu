use Test;

plan 5;

# A regex literal nested in a regex's code block / code assertion keeps its
# quote characters as regex text: `<["']>` does not open a string.
grammar G {
    rule a { (\w+) <!{ $0.Str ~~ / 'x' <["']>? $/ }> }
    token quote { <["']> }
}
is ~G.parse('abc', :rule<a>), 'abc', 'rule with a nested regex in <!{ }> parses and matches';
ok G.parse('"', :rule<quote>), 'and the grammar still has the token after it';

ok 'a' ~~ / <!{ 'x' ~~ / <["]> / }> a /, 'nested regex with a lone " in a char class';
ok 'b' ~~ / <?{ 6 / 2 == 3 }> b /, 'a / after a term is still division';
ok 'c' ~~ / c <?{ $/ }> /, '$/ in a code assertion is still the match variable';
