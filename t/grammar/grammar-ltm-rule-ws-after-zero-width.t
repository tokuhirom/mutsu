use v6;
use Test;

# Found via the Math::Symbolic distribution. In `rule a { \s* <x> }` the implicit
# whitespace comes after the `\s*`, so it is not the rule's leading whitespace
# even though `\s*` matched nothing: it ends the declarative prefix at 0, as in
# Rakudo, and `|` does not rank `a` above a branch whose prefix is longer.

plan 4;

grammar G {
    token TOP { <a> | <b> }
    rule a { \s* <x> }
    token x { \w '=' \w\w }
    token b { \w '=' \w }
}
is G.subparse('y=mz').Str, 'y=m', 'a rule whose whitespace follows \s* has prefix 0';

grammar H {
    token TOP { <a> | <b> }
    rule a { <x> }
    token x { \w '=' \w\w }
    token b { \w '=' \w }
}
is H.subparse('y=mz').Str, 'y=mz', 'a rule whose first atom is the subrule keeps its prefix';

grammar Eq {
    token TOP { <equation> | <expression> }
    token equation { <before <-[ = ]>+ '=' > <expression> '=' <expression> }
    rule expression { \s* [<operation>|<term>] }
    token variable { <alpha> <alnum>* }
    rule term { \s* <variable> }
    token operation { <chain> }
    rule chain { <before .+? '*'><t> [ '*' <t> ]+ }
    token t { <term> }
}
ok Eq.parse('y=m*x'), 'equation | expression alternation parses an equation with a product';
is Eq.parse('y=m*x')<equation>.Str, 'y=m*x', 'and it is the equation branch';
