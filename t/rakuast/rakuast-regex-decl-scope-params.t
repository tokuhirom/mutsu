use Test;

# Scoped and parameterised regex declarations in RakuAST, measured on rakudo
# 2026.09: `scope` leads the declaration (`has` is the default and renders
# none), and a parameter list is a method-style `signature`. EVAL of the tree
# declares and matches them as the parsed program does.

plan 10;

my $mine = Q[my token t { a }].AST.statements.head.expression;
isa-ok $mine, RakuAST::TokenDeclaration, 'a `my token`';
is $mine.scope, 'my', 'is `my`-scoped';

my $decl = Q[grammar G { token r($x, :$y) { a } }].AST.statements.head.expression
    .body.body.statement-list.statements.head.expression;
is $decl.scope, 'has', 'a grammar token keeps the default scope';
is $decl.signature.parameters.elems, 2, 'and carries its parameters';
is $decl.signature.parameters[1].names, ('y',), 'a named one among them';

sub run($src) { EVAL($src.AST) }
is run(Q[my token d3 { \d ** 3 }; ~("ab123" ~~ / <d3> /)]), '123', 'a `my token` matches';
is run(Q[my regex w { \w+ }; ~("hi there" ~~ / <w> /)]), 'hi', 'so does a `my regex`';
is run(Q[grammar G1 { token TOP { <rep('x')> }; token rep($c) { $c $c } }; ~G1.parse('xx')]),
    'xx', 'a positional parameter reaches the token';
is run(Q[grammar G2 { rule TOP { <pair('=')> }; rule pair($sep) { \w+ $sep \w+ } }; ~G2.parse('a = b')]),
    'a = b', 'and a rule\'s';
is run(Q[grammar G3 { token TOP { <named(:k<z>)> }; token named(:$k) { $k } }; ~G3.parse('z')]),
    'z', 'a named parameter takes its argument';
