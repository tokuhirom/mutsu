use Test;

# Sigilled constants in RakuAST, measured on rakudo 2026.09: a
# `VarDeclaration::Constant` whose `name` carries the sigil. EVAL of the tree
# declares them as the parsed program does.

plan 12;

sub decl($src) { $src.AST.statements.head.expression }

is decl(Q[constant @a = 1, 2]).name, '@a', 'an array constant keeps its `@`';
is decl(Q[constant %h = a => 1]).name, '%h', 'a hash constant its `%`';
is decl(Q[my constant $x = 3]).name, '$x', 'a scalar constant its `$`';
is decl(Q[constant &f = { 1 }]).name, '&f', 'a code constant its `&`';
is decl(Q[constant y = 4]).name, 'y', 'a sigilless constant has none';
is decl(Q[constant term:<$bar> = 42]).name, 'term:<$bar>', 'a sigiled term is named as a term';

sub run($src) { EVAL($src.AST) }
is run(Q[constant @a = 1, 2, 3; "{@a.elems} @a[1]"]), '3 2', 'an array constant';
is run(Q[constant %h = a => 1, b => 2; %h<b>]), 2, 'a hash constant';
is run(Q[my constant $x = 3; $x + 1]), 4, 'a scalar constant';
is run(Q[constant &twice = -> $n { $n * 2 }; twice(5)]), 10, 'a code constant is callable by name';
is run(Q[constant @e = <p q>; @e.join]), 'pq', 'a word-list initializer';
is run(Q[constant term:<$bar> = 42; $bar + 1]), 43, 'a sigiled term reads its value';
