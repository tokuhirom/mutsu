use Test;

# A character class range whose two ends are surrogate code points names no
# scalar value: rakudo accepts it as an empty range (ANTLR4::Grammar's
# generated Java.g4 `JavaLetter`, #16592). A genuinely reversed range is still
# refused.

plan 7;

my $r = / <[ \x[D800] .. \x[DBFF] ]> /;
ok $r.defined, 'a surrogate-only range compiles in a literal';
nok ('a' ~~ $r).so, 'and matches nothing';

grammar G { token t { <[ \x[D800] .. \x[DBFF] ]> | 'b' } }
ok G.parse('b', :rule<t>).so, 'in a grammar token the other branch still matches';
nok G.parse('a', :rule<t>).so, 'and the empty class does not';

my $mixed = / <[ a \x[D800] .. \x[DFFF] z ]> /;
is ('a' ~~ $mixed).Str ~ ('z' ~~ $mixed).Str, 'az', 'other members of the class still match';
nok ('m' ~~ $mixed).so, 'a non-member does not';

throws-like { EVAL 'my $x = / <[ z .. a ]> /; 1' }, Exception, 'a reversed range is still refused';

done-testing;
