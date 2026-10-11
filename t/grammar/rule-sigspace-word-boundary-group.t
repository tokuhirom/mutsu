use Test;

# From Grammar::Modelica (t/Equations.rakutest): the `|` of `<|w>` inside a
# `rule`'s `[ ... ]` group with trailing significant whitespace was taken for
# an alternation branch boundary by the implicit-`<.ws>` injector.
plan 6;

ok 'e' ~~ rule { [<|w>'e' ] }, 'group with <|w> and trailing space';
ok 'a e' ~~ rule { 'a' [ <|w> 'e' ] }, 'leading and trailing space in group';
ok 'a e c end' ~~ rule { 'a' [ <|w>'e' 'c' ]* 'end' }, 'quantified group';
nok 'a ec' ~~ rule { 'a' [ <|w>'e' ] 'c' }, 'boundary still enforced';

grammar G { rule TOP { ^ 'a' [ <|w>'e'<|w> 'c' ]* 'end' $ } }
ok G.parse('a e c e c end'), 'grammar rule';
ok G.parse('a end'), 'zero iterations';
