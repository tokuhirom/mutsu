use v6;
use Test;

# A named item inside a regex character class (e.g. `<-restricted>`, `<+foo>`)
# that is not a built-in character class falls back to resolving it as a grammar
# token. That fallback runs once per character checked against the class, so it
# must use the cheap static token-pattern resolver (pattern read straight from
# the token def) rather than building a fresh scratch interpreter per character.
# This test pins that both a literal-body token (static path) and a
# non-literal-body token (eval fallback) still resolve correctly.

plan 6;

grammar Lit {
  token r   { <[a..f]> }        # literal body -> static resolver
  token TOP { <-r>+ }           # negated class referencing the token
}
ok  Lit.parse('XYZ789').defined, 'negated literal-body token: all-out-of-class matches';
nok Lit.parse('abc').defined,    'negated literal-body token: in-class chars fail';
nok Lit.parse('aXY').defined,    'negated literal-body token: one in-class char fails';

grammar Dyn {
  token d   { <?{ True }> <[0..9]> }   # non-literal body (code assertion) -> eval fallback
  token TOP { <+d>+ }                  # positive class referencing the token
}
ok  Dyn.parse('42').defined,  'positive non-literal-body token: digits match';
nok Dyn.parse('4a').defined,  'positive non-literal-body token: non-digit fails';

# The class-item token resolution must also see tokens inherited via MRO.
grammar Base { token b { <[x..z]> } }
grammar Derived is Base { token TOP { <-b>+ } }
ok Derived.parse('ABC').defined, 'inherited token resolved in class-item fallback';
