use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 5;

# `< + \< \> >`: a backslash escapes the angle brackets inside a regex word
# list; rakudo keeps the unescaped text in one StrLiteral segment.
my $decl = Q[token op14609 { < + \< \> > }].AST.statements.tail.expression;
isa-ok $decl, RakuAST::TokenDeclaration, 'the declaration converts';
like $decl.gist, /'processors => ("words",)'/, 'the list is a words quote';
like $decl.gist, /'StrLiteral.new(" + < > ")'/, 'escaped brackets are unescaped in the segment';

my $g = EVAL(Q[grammar G14609 { token TOP { <op> }; token op { < + \< \> > } }; G14609].AST);
ok $g.parse('<'), 'an escaped open bracket matches after the round trip';
ok $g.parse('>'), 'an escaped close bracket matches after the round trip';
