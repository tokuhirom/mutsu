use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 16;

my $negated = Q[4 !< 5].AST.statements[0].expression;
is $negated.^name, 'RakuAST::ApplyInfix', 'a negated infix is an application';
is $negated.infix.^name, 'RakuAST::MetaInfix::Negate',
    'the negation is an explicit meta-infix';
is $negated.infix.infix.^name, 'RakuAST::Infix', 'negation retains its base infix';
is $negated.infix.infix.operator, '<', 'the base operator is preserved';
is EVAL(Q[4 !< 5].AST), False, 'a negated infix round-trips';

my $hand = RakuAST::ApplyInfix.new(
    left => RakuAST::IntLiteral.new(4),
    infix => RakuAST::MetaInfix::Negate.new(RakuAST::Infix.new('<')),
    right => RakuAST::IntLiteral.new(5),
);
is EVAL($hand), False, 'a hand-built negated infix lowers';

my $symbolic = Q[sub infix:<§>($a, $b) { $a + $b }; 2 § 3].AST;
is $symbolic.statements[1].expression.infix.^name, 'RakuAST::Infix',
    'a declared symbolic infix uses the ordinary Infix class';
is $symbolic.statements[1].expression.infix.operator, '§',
    'the declared operator keeps its spelling';
is EVAL($symbolic), 5, 'a declared symbolic infix round-trips';

my $bracketed = Q[my $x = 42; $x [[+]]= 6; $x].AST;
my $assign = $bracketed.statements[1].expression;
is $assign.infix.^name, 'RakuAST::MetaInfix::Assign',
    'bracketed compound assignment keeps its assign meta-infix';
is $assign.infix.infix.^name, 'RakuAST::BracketedInfix',
    'the first bracket pair is a model node';
is $assign.infix.infix.infix.^name, 'RakuAST::BracketedInfix',
    'the nested bracket pair remains distinct';
is $assign.infix.infix.infix.infix.operator, '+',
    'the bracketed form retains the base operator';
is EVAL($bracketed), 48, 'a nested bracketed compound assignment round-trips';

my $immutable = Q[sub called($ignored) { die 'called' }; called pi = 4].AST;
is $immutable.statements[1].expression.args.args[0].left.^name,
    'RakuAST::Term::Name', 'a read-only setting term retains its assignment shape';
throws-like { EVAL($immutable) }, X::Assignment::RO,
    'a read-only setting term throws before the surrounding call';
