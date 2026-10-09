use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 10;

my $while = Q[1 while False].AST.statements[0];
isa-ok $while.loop-modifier, RakuAST::StatementModifier::While,
    'postfix while is a loop modifier';
isa-ok $while.loop-modifier.expression, RakuAST::Term::Enum,
    'the while modifier exposes its written condition';

my $until = Q[1 until True].AST.statements[0];
isa-ok $until.loop-modifier, RakuAST::StatementModifier::Until,
    'postfix until is a loop modifier';
isa-ok $until.loop-modifier.expression, RakuAST::Term::Enum,
    'the until modifier exposes its written condition';

my $block = Q[{ 1 } while False].AST.statements[0];
isa-ok $block.expression, RakuAST::Block,
    'a bare block stays the modified expression';

is EVAL(Q[my $i = 0; my @r = ({ 1 } while $i++ < 2); @r.map(*.^name).join(",")].AST),
    'Block,Block', 'postfix while evaluates the block as a value';
is EVAL(Q[my $i = 0; my @r = ({ 1 } until $i++ >= 2); @r.map(*.^name).join(",")].AST),
    'Block,Block', 'postfix until evaluates the block as a value';
is EVAL(Q[my $i = 0; my @r = (7 while $i++ < 2); @r.join(",")].AST),
    '7,7', 'postfix while round-trips a non-block operand';
is EVAL(Q[my $i = 0; my @r = (7 until $i++ >= 2); @r.join(",")].AST),
    '7,7', 'postfix until round-trips a non-block operand';

is EVAL($while).raku, 'Nil', 'a postfix while node lowers on its own';
