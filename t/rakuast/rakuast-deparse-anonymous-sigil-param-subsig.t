use Test;
use experimental :rakuast;

plan 4;

# An anonymous `%` / `@` parameter with a sub-signature is a `ParameterTarget::Var`
# named just by its sigil (the parser's internal name does not leak).
is Q[sub f(% (:$c)) { 1 }].AST.DEPARSE.lines.head, 'sub f (% (:$c)) {', 'anonymous % with a sub-signature';
is Q[sub f(@ ($a, $b)) { 1 }].AST.DEPARSE.lines.head, 'sub f (@ ($a, $b)) {', 'anonymous @ with a sub-signature';
is Q[-> % (:$c) { 1 }].AST.DEPARSE.trim.lines.head, '-> % (:$c) {', 'a pointy block';
ok !Q[sub f(% (:$c)) { 1 }].AST.gist.contains('ANON'), 'no internal name in the tree';
