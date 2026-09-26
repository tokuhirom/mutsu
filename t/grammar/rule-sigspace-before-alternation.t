use Test;

plan 8;

# In a `rule`, whitespace after an atom and before a `|` / `||` is
# significant: it is a `<.ws>`, like whitespace between two atoms. Whitespace
# right after the pipe is not. mutsu dropped the one before the pipe, so a
# branch that ended in an optional atom could not step past the space that
# followed it. (ANTLR4::Grammar's `element` rule; #9491)

grammar T { token TOP { <e> 'x' }; rule e { a | b } }
ok  T.parse('a x'), 'space before | is a <.ws> in the first branch';
nok T.parse('ax'),  '... which is required';
ok  T.parse('b x'), 'the last branch keeps its trailing <.ws>';

grammar U { token TOP { <e> 'x' }; rule e { a| b } }
nok U.parse('a x'), 'no space before | means no <.ws>';
ok  U.parse('ax'),  '... so none is required';

grammar V { token TOP { <e> 'x' }; rule e {
    a
  | b
} }
nok V.parse('ax'), 'a newline before | is significant too';

grammar E { token TOP { <e>* }; rule e { \w+ '?'? | \w+ 'Z' } }
ok E.parse('a? b'), 'a quantified rule can step past the space after an optional atom';

grammar L { token TOP { <e> 'x' }; rule e { | a | b } }
ok L.parse('a x'), 'a leading pipe stays stylistic';
