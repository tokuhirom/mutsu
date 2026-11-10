use Test;
use experimental :rakuast;

# `has-any-phasers`, `has-loop-phasers` and `has-placeholder-parameters` on the
# code-block nodes (Red's RakuAST compiler asks a block for them before it
# inlines it into a query).
plan 20;

sub block(Str $body) { ('-> { ' ~ $body ~ ' }').AST.statements[0].expression }

for <ENTER LEAVE KEEP UNDO PRE POST> -> $p {
    ok block("$p \{ 1 }").has-any-phasers, "$p is a block phaser";
    nok block("$p \{ 1 }").has-loop-phasers, "$p is no loop phaser";
}
ok block('FIRST { 1 }').has-loop-phasers, 'FIRST is a loop phaser';
nok block('BEGIN { 1 }').has-any-phasers, 'BEGIN belongs to the unit';
nok block('1').has-any-phasers, 'a plain block has none';

my $nested = '{ ENTER { 1 } }'.AST.statements[0].expression;
ok $nested.has-any-phasers, 'a braced block with ENTER';
my $outer = '{ { ENTER { 1 } } }'.AST.statements[0].expression;
nok $outer.has-any-phasers, 'a nested block keeps its own phasers';

ok '{ $^a + 1 }'.AST.statements[0].expression.has-placeholder-parameters, 'a placeholder parameter';
nok '{ 1 }'.AST.statements[0].expression.has-placeholder-parameters, 'no placeholder';
nok '{ { $^a } }'.AST.statements[0].expression.has-placeholder-parameters, 'the inner block owns it';
