use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# `%(:foo, :bar)` keeps its bare colonpairs: ColonPair::True nodes in the
# AST, `%(:foo, :bar)` from .DEPARSE, and the hash still holds foo/bar => True.
# Checked against rakudo.

plan 5;

my $ast = Q[%(:foo, :bar)].AST;
ok $ast.raku.contains('RakuAST::ColonPair::True.new("foo")'), 'the AST has ColonPair::True';
nok $ast.raku.contains('FatArrow'), 'and no FatArrow';
is $ast.DEPARSE.trim, '%(:foo, :bar)', 'DEPARSE keeps the colonpairs';
is-deeply EVAL(Q[%(:foo, :bar)]), %(foo => True, bar => True), 'source value';
is-deeply EVAL($ast), %(foo => True, bar => True), 'EVAL($ast) value';
