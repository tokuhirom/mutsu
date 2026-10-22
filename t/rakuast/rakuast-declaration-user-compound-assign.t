use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 4;

# `my $x foo= 6` with a user-declared infix: the declaration is the left side
# of an ApplyInfix whose infix is MetaInfix::Assign(Infix("foo")).
my $source = Q[multi sub infix:<foo>($a, $b) { $a + $b }; multi sub infix:<foo>() { 0 }; my $x foo= 6];
my $assign = $source.AST.statements.tail.expression;
isa-ok $assign, RakuAST::ApplyInfix, 'the statement is one infix application';
isa-ok $assign.left, RakuAST::VarDeclaration::Simple, 'the declaration is the left operand';
isa-ok $assign.infix, RakuAST::MetaInfix::Assign, 'the infix is a meta assignment';
is EVAL(Q[multi sub infix:<foo>($a, $b) { $a + $b }; multi sub infix:<foo>() { 0 }; my $x foo= 6; $x].AST), 6,
    'the user operator is applied to the declared variable';
