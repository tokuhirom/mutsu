use v6;
use Test;

plan 8;

# The regex of a `token`/`rule`/`regex` declaration or term that holds no code is
# carried through finalization as the parser built it (#8033, ADR-15760 slice 2).

grammar G {
    token TOP { <ws> <num> <[+-]> <num> }
    token num { \d+ }
    token ws { \s* }
    rule r { a b | c }
    regex q { x+ }
}
ok G.parse('1+2'), 'a grammar with a character class parses';
nok G.parse('12 - 3x'), 'trailing text fails the parse';
ok G.parse('c', :rule<r>), 'a rule with alternation';
is G.parse('xxx', :rule<q>).Str, 'xxx', 'a regex declaration';

my $t = token { <[+-]> \d+ };
is ('x-5' ~~ /x <$t>/).Str, 'x-5', 'an anonymous token interpolated into a regex';
my $r = rule { a b };
ok 'a b' ~~ $r, 'an anonymous rule term';
my $q = regex { x+ };
ok so('xx' ~~ $q), 'an anonymous regex term';

# A code block keeps the conversion and its line information.
my @seen;
'ab' ~~ / a { @seen.push(1) } b /;
is @seen.elems, 1, 'a regex with code still runs its block';
