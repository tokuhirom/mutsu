use Test;

# A name captured by both sides of a separated quantifier, or by both sides
# of a `~` goal match, lists its entries in match order, as rakudo does
# (#10574). Positional captures keep their source numbering.

plan 8;

grammar J { token TOP { <x>+ % <x> }; token x { \w } }
is J.parse("abc")<x>».Str.join("|"), 'a|b|c', 'separated quantifier: atom and separator share a name';

grammar J2 { token TOP { <x>+ %% <x> }; token x { \w } }
is J2.parse("abcd")<x>».Str.join("|"), 'a|b|c|d', '%% with a trailing separator';

grammar J3 { token TOP { [<x> <y>]+ % [<y> <x>] }; token x { \w }; token y { \d } }
my $m = J3.parse("a12bc3");
is $m<x>».Str.join("|"), 'a|b|c', 'grouped sides: first shared name';
is $m<y>».Str.join("|"), '1|2|3', 'grouped sides: second shared name';

is ("abc" ~~ / <alpha>+ % <alpha> /)<alpha>».Str.join("|"), 'a|b|c', 'in a plain regex';

grammar Q { token TOP { "(" ~ <a> <a> }; token a { \w } }
is Q.parse("(bc")<a>».Str.join("|"), 'b|c', 'goal match: the inner pattern before the goal';

my $g = "(a1" ~~ / "(" ~ (\d) (\w) /;
is ~$g[0], '1', 'goal match: the goal\'s group is still $0';
is ~$g[1], 'a', 'goal match: the inner group is $1';
