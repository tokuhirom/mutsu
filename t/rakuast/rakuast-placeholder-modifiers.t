use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 11;

my $if = Q[{ $^x } if 7].AST.statements[0];
isa-ok $if.expression, RakuAST::Block, 'the modified placeholder block stays a Block';
isa-ok $if.condition-modifier, RakuAST::StatementModifier::If, 'postfix if stays a condition modifier';
isa-ok $if.condition-modifier.expression, RakuAST::IntLiteral, 'the if modifier exposes its condition';

my $unless = Q[{ $^x } unless 0].AST.statements[0];
isa-ok $unless.condition-modifier, RakuAST::StatementModifier::Unless, 'postfix unless stays a condition modifier';

my $given = Q[{ $^x } given 69].AST.statements[0];
isa-ok $given.loop-modifier, RakuAST::StatementModifier::Given, 'postfix given stays a loop modifier';
isa-ok $given.loop-modifier.expression, RakuAST::IntLiteral, 'the given modifier exposes its topic';

is EVAL(Q[my $a; { $a = $^x } if 7; $a].AST), 7,
    'postfix if binds its condition to the placeholder';
is EVAL(Q[my $a = "oops"; { $a = $^x } unless 0; $a].AST), 0,
    'postfix unless binds the written condition';
is EVAL(Q[my $a; { $a = $^x } given 69; $a].AST), 69,
    'postfix given binds its topic';
is EVAL(Q[my $a = "keep"; { $a = $^x } if 0; $a].AST), 'keep',
    'a false modifier does not run the placeholder block';
is EVAL(Q[my $a = "keep"; { $a = $^x } unless 1; $a].AST), 'keep',
    'a true unless condition does not run the placeholder block';
