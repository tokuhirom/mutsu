use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# `supply { }`, a hyper assignment over a literal list of lvalues and an inline
# `subset` term are kept as written by the parser and lowered once; the
# expansion the compiler runs is not what `.AST` renders.

plan 9;

my $supply = Q[supply { emit 1; emit 2; done }].AST.statements[0].expression;
isa-ok $supply, RakuAST::StatementPrefix::Supply, 'supply is a statement prefix';
is $supply.blorst.body.statement-list.statements.elems, 3,
    'its block holds the three statements as written';
is EVAL(Q[supply { emit 1; emit 2; done }].AST).list.join(','), '1,2',
    'the written supply emits when evaluated';

my $hyper = Q[(my $a, my $b) »=» (3, 4)].AST.statements[0].expression;
isa-ok $hyper, RakuAST::ApplyInfix, 'a hyper assignment is an infix application';
isa-ok $hyper.infix, RakuAST::MetaInfix::Hyper, 'its infix is the hyper meta-infix';
is EVAL(Q[my ($x, $y); ($x, $y) »=» (5, 6); "$x$y"].AST), '56',
    'the written hyper assignment distributes its value';

my $subset = Q[my $v = subset :: of Int where * > 2; 3 ~~ $v].AST;
my $type = $subset.statements[0].expression.initializer.expression;
isa-ok $type, RakuAST::Type::Subset, 'an inline subset term is a subset type';
is EVAL($subset), True, 'the written subset term constrains by its where clause';

is EVAL(Q[my $s = supply { emit 9 }; my @got; $s.tap({ @got.push($_) }); @got[0]].AST), 9,
    'a supply bound to a variable runs through the same lowering';
