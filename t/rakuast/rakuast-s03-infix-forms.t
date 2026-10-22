use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 24;

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

my $reverse = Q[my $a = 12; $a [R-]= 42; $a].AST;
is $reverse.statements[1].expression.infix.infix.^name,
    'RakuAST::BracketedInfix', 'a bracketed reverse assignment keeps its brackets';
is $reverse.statements[1].expression.infix.infix.infix.^name,
    'RakuAST::MetaInfix::Reverse', 'the reverse metaoperator remains inside the brackets';
is EVAL($reverse), 30, 'a bracketed reverse assignment round-trips';
is EVAL(Q[my $a = 12; ((10 R+= ($a [R-]= 42)) //= 100) += 1000; $a].AST),
    1040, 'nested reverse meta assignments retain their lvalue semantics';

my $cross = Q[my @a = 1, 2; @a [X+]= (3, 4); @a.join(',')].AST;
is $cross.statements[1].expression.infix.infix.infix.^name,
    'RakuAST::MetaInfix::Cross', 'a bracketed cross assignment retains its metaoperator';
is EVAL($cross), '4,5,5,6', 'a bracketed cross assignment round-trips';

my $zip = Q[my @a = 1, 2; @a [Z+]= (3, 4); @a.join(',')].AST;
is $zip.statements[1].expression.infix.infix.infix.^name,
    'RakuAST::MetaInfix::Zip', 'a bracketed zip assignment retains its metaoperator';
is EVAL($zip), '4,6', 'a bracketed zip assignment round-trips';
