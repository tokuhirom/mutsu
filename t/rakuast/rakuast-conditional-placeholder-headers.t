use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# An elsif/orwith condition is part of its enclosing block's placeholder
# signature after an AST round trip. A nested if in an ordinary else block is
# a different scope. These shapes and results match Rakudo 2026.11.
plan 8;

my $with-ast = Q[my &f = { with $^v { "v $_" } orwith $^w { "w $_" } else { "none" } }; &f].AST;
my $with-node = $with-ast.statements[0].expression.initializer.expression.body.statement-list.statements[0];
isa-ok $with-node, RakuAST::Statement::With, 'the first clause is With';
isa-ok $with-node.elsifs[0], RakuAST::Statement::Orwith, 'the next clause is Orwith';
my &choose = EVAL($with-ast);
is &choose.signature.params.elems, 2, 'both conditions contribute placeholders';
is choose(Any, 5), 'w 5', 'orwith receives the second placeholder';
is choose(1, 2), 'v 1', 'with receives the first placeholder';

my &branch = EVAL(Q[my &f = { if $^a { "a" } elsif $^b { "b" } }; &f].AST);
is &branch.signature.params.elems, 2, 'an elsif condition contributes a placeholder';
is branch(0, 1), 'b', 'elsif reads that placeholder';

my &nested = EVAL(Q[my &f = { if $^a { 1 } else { if $^b { 2 } } }; &f].AST);
is &nested.signature.params.elems, 1, 'an ordinary else block owns its nested if';
