use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# `unless` written as a term is the same statement as the `unless` statement:
# `Statement::Unless` over the condition as written (rakudo 2026.09). This
# file passes under both mutsu and raku.

plan 4;

my $node = Q[(unless 0 { 42 })].AST.statements[0].expression.semilist.statements[0];
is $node.^name, 'RakuAST::Statement::Unless', 'an unless term is Statement::Unless';
is $node.condition.raku, 'RakuAST::IntLiteral.new(0)', 'its condition is as written';

is EVAL(Q[(unless 0 { 42 })].AST), 42, 'a false condition runs the body';
is EVAL(Q[(unless 1 { 42 }).elems].AST), 0, 'a true condition yields an empty list';
